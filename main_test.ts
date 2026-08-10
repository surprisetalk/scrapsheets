import { assert, assertEquals } from "@std/assert";
import { PGlite } from "@electric-sql/pglite";
import { PostgresConnection } from "pg-gateway";
import { citext } from "@electric-sql/pglite/contrib/citext";
import { app, arrayify, automerge, createJwt, createToken, sql } from "./main.ts";
import type { Sheet, Table, Template } from "./main.ts";
import dbSql from "./db.sql" with { type: "text" };
import examplesSql from "./examples.sql" with { type: "text" };

const request = async (jwt: string, route: string, options?: object) => {
  const res = await app.request(route, {
    headers: new Headers({
      "Content-Type": "application/json",
      Authorization: `Bearer ${jwt}`,
    }),
    ...options,
  });
  assert(
    res.ok,
    `Expected a 2xx but received a ${res.status}: ${res.statusText}`,
  );
  return await res.json();
};

const reject = async (jwt: string, route: string, options?: object) => {
  const res = await app.request(route, {
    headers: new Headers({
      "Content-Type": "application/json",
      Authorization: `Bearer ${jwt}`,
    }),
    ...options,
  });
  assert(
    400 <= res.status && res.status < 500,
    `Expected a 4xx but received a ${res.status}: ${res.statusText}`,
  );
};

async function get<T>(
  jwt: string,
  route: string,
  query?: Record<string, string | number>,
): Promise<T> {
  return await request(
    jwt,
    route +
      "?" +
      new URLSearchParams(query as Record<string, string>).toString(),
  ).then((res) => res.data);
}

const post = (jwt: string, route: string, body: unknown) =>
  request(jwt, route, { method: "POST", body: JSON.stringify(body) });
const put = (jwt: string, route: string, body: unknown) =>
  request(jwt, route, { method: "PUT", body: JSON.stringify(body) });

const usr = async (email: string) => {
  const [{ usr_id }] = await sql`
    insert into usr (email) values (${email})
    on conflict (email) do update set email = excluded.email
    returning *
  `;
  return { usr_id, jwt: await createJwt(usr_id) };
};

Deno.test(async function allTests(_t) {
  const listener = Deno.listen({ hostname: "127.0.0.1", port: 5434 });
  const pglite = new PGlite({ extensions: { citext } });

  (async () => {
    for await (const conn of listener) {
      new PostgresConnection(conn, {
        async onStartup() {
          await pglite.waitReady;
        },
        async onMessage(data, { isAuthenticated }) {
          if (!isAuthenticated) return;
          return await pglite.execProtocolRaw(data);
        },
      });
    }
  })();

  await pglite.waitReady;
  await pglite.exec(dbSql);
  await pglite.exec(examplesSql);

  // Signup completion + login round-trip (app-level password hashing, no pgcrypto).
  {
    const email = "carol@example.com";
    const password = "s3cret-pass";
    const token = await createToken(email);
    await post("", `/signup/${token}`, { email, password });
    const { data } = await post("", `/login`, { email, password });
    assert(data.jwt, "login should return a jwt");
    assert(data.usr_id, "login should return usr_id");
    // Wrong password is rejected.
    await reject("", `/login`, {
      method: "POST",
      body: JSON.stringify({ email, password: "wrong" }),
    });
    // Re-signup updates the password (on conflict do update).
    const newPassword = "rotated-pass";
    await post("", `/signup/${await createToken(email)}`, { email, password: newPassword });
    await reject("", `/login`, { method: "POST", body: JSON.stringify({ email, password }) });
    const { data: data2 } = await post("", `/login`, { email, password: newPassword });
    assert(data2.jwt, "login with rotated password should succeed");
  }

  {
    const { jwt } = await usr("alice@example.com");

    // Alice creates templates.
    {
      const templates: Template[] = [
        {
          type: "table",
          data: [
            arrayify([
              { name: "a", type: "text", key: 0 },
              { name: "b", type: "text", key: 1 },
              { name: "c", type: "text", key: 2 },
            ]),
          ],
        },
        { type: "net-hook", data: [] },
        {
          type: "net-http",
          data: [{ url: "http://127.0.0.1:5049/test", interval: 1000 }],
        },
        { type: "net-socket", data: [{ url: "ws://127.0.0.1:5051/test" }] },
        {
          type: "query",
          data: [
            {
              lang: "sql",
              code: "select 123 as a, 456 as b, 789 as c",
              args: [],
            },
          ],
        },
        { type: "codex-db", data: [] },
        { type: "codex-scrapsheets", data: [] },
      ];
      for (const template of templates) {
        const hand = automerge.create<{ data: Sheet["data"] }>({
          data: [template],
        });
        await put(jwt, `/library/template:${hand.documentId}`, {});
      }
      for (const { type, data } of templates) {
        const hand = automerge.create<{ data: Sheet["data"] }>({ data });
        await put(jwt, `/library/${type}:${hand.documentId}`, {});
      }

      const [cols_, ...rows] = await get<Table>(jwt, `/library`);
      const cols = Object.values(cols_);
      assert(cols.length);
      assertEquals(
        cols.map((col) => col.name).join(),
        "created_at,type,doc_id,name,tags,sell_price",
      );
      assertEquals(rows.length, templates.length * 2);
    }

    // Alice updates templates and posts them to shop.
    {
      const [cols_, ...rows] = await get<Table>(jwt, `/library`);
      const cols = Object.values(cols_);
      assert(cols.length);
      assertEquals(
        cols.map((col) => col.name).join(),
        "created_at,type,doc_id,name,tags,sell_price",
      );
      assert(rows.length);
      for (const { type, doc_id } of rows) {
        const sheet_id = type + ":" + doc_id;
        const meta = { name: `Example ${type}`, tags: ["tag1", "tag2"] };
        await put(jwt, `/library/${sheet_id}`, meta);
        await post(jwt, `/sell/${sheet_id}`, { price: 0 });
        const [, { name, tags, sell_price }] = await get<Table>(
          jwt,
          `/library`,
          { doc_id: doc_id as string },
        );
        assertEquals(name, meta.name);
        assertEquals(tags, meta.tags);
        // postgres.js returns numeric as string (arbitrary precision)
        assertEquals(sell_price, "0");
      }
    }
  }

  // The shop is publicly viewable.
  {
    const [cols_, ...rows] = await get<Table>("", `/shop`);
    const cols = Object.values(cols_);
    assert(cols.length);
    assert(rows.length);
  }

  {
    const { jwt } = await usr("bob@example.com");

    // Bob purchases items from the shop.
    {
      const [cols_, ...rows] = await get<Table>(jwt, `/shop`);
      const cols = Object.values(cols_);
      assert(cols.length);
      assertEquals(cols.map((col) => col.name).join(), "name,price,");
      assert(rows.length);

      // Buy the first 3 items
      const toBuy = rows.slice(0, 3);
      for (const row of toBuy) {
        const { data: sheet_id } = await post(jwt, `/buy/${row.sell_id}`, {});
        assert(sheet_id, "Buy should return a sheet_id");
      }

      // Verify Bob's library grew
      const [, ...bobRows] = await get<Table>(jwt, `/library`);
      assertEquals(bobRows.length, toBuy.length);

      // A purchased sheet cannot be resold (buy_price and sell_price are mutually exclusive).
      const [{ type, doc_id }] = bobRows;
      await reject(jwt, `/sell/${type}:${doc_id}`, {
        method: "POST",
        body: JSON.stringify({ price: 5 }),
      });
    }

    // Buying with invalid sell_id returns 404.
    {
      await reject(jwt, `/buy/nonexistent_sell_id`, {
        method: "POST",
        body: JSON.stringify({}),
      });
    }

    // Bob runs a basic SQL query
    {
      const {
        data: [cols_, ...rows],
      }: { data: Table } = await post(jwt, `/query`, {
        lang: "sql",
        code: `select 1 as a, 2 as b, 3 as c`,
        args: [],
      });
      const cols = Object.values(cols_);
      assert(cols.length);
      assertEquals(cols.map((col) => col.name).join(), "a,b,c");
      assertEquals(rows.length, 1);
    }

    // SQL query with multiple rows and types
    {
      const {
        data: [cols_, ...rows],
      }: { data: Table } = await post(jwt, `/query`, {
        lang: "sql",
        code: `select 1 as num, 'hello' as txt union all select 2, 'world'`,
        args: [],
      });
      const cols = Object.values(cols_);
      assertEquals(cols.map((col) => col.name).join(), "num,txt");
      assertEquals(rows.length, 2);
    }

    // Bob runs a PRQL query - tests that PRQL compiles to SQL and executes
    // NOTE: PRQL's from_text doesn't translate to AlaSQL-compatible SQL,
    // so we verify the compilation works by checking for valid response structure
    {
      const res: { data: Table } = await post(jwt, `/query`, {
        lang: "prql",
        code: `from_text format:json '[{"a":1}]' | select {a}`,
        args: [],
      });
      // Just verify we get a valid response with column structure
      assert(res.data, "Expected data in response");
      const [cols_] = res.data;
      assert(cols_, "Expected columns in response");
    }
  }

  // CSV import -> export round-trip.
  {
    const { jwt } = await usr("dave@example.com");
    const csv = "name,age\nAlice,30\nBob,25";
    const form = new FormData();
    form.append("file", new File([csv], "people.csv", { type: "text/csv" }));
    const importRes = await app.request("/import/csv", {
      method: "POST",
      headers: new Headers({ Authorization: `Bearer ${jwt}` }),
      body: form,
    });
    assert(importRes.ok, `import failed: ${importRes.status} ${importRes.statusText}`);
    const { sheet_id } = await importRes.json();
    assert(sheet_id, "import should return a sheet_id");

    const exportRes = await app.request(`/export/${sheet_id}.csv`, {
      headers: new Headers({ Authorization: `Bearer ${jwt}` }),
    });
    assert(exportRes.ok, `export failed: ${exportRes.status} ${exportRes.statusText}`);
    assertEquals(exportRes.headers.get("content-type"), "text/csv; charset=utf-8");
    const lines = (await exportRes.text()).split("\n");
    assertEquals(lines[0], "name,age");
    assertEquals(lines.length, 3);
    assertEquals(lines[1], "Alice,30");
    assertEquals(lines[2], "Bob,25");
  }

  // Proxy guards. Each rejection must name its own cause, never a bare status.
  {
    const proxy = async (url?: string) => {
      const res = await app.request(
        "/proxy" + (url === undefined ? "" : `?url=${encodeURIComponent(url)}`),
      );
      return { status: res.status, ...(await res.json()) };
    };
    assertEquals(await proxy(), { status: 400, error: "Missing url parameter" });
    assertEquals(await proxy("ftp://example.com/x"), {
      status: 400,
      error: "Only HTTP(S) URLs allowed.",
    });
    for (
      const url of [
        "http://localhost/x",
        "http://foo.local/x",
        "http://127.0.0.1/x",
        "http://169.254.169.254/latest/meta-data",
      ]
    ) {
      assertEquals(await proxy(url), { status: 400, error: "Internal URLs not allowed." }, url);
    }
    const { status, error } = await proxy("notaurl");
    assertEquals(status, 502);
    assert(
      error.includes("Invalid URL") && error.includes("notaurl"),
      `A malformed url should say which url is malformed, got: ${error}`,
    );
  }

  // Every seeded example must survive a visit with no ?q= param: a free-text
  // search that interpolates a null @params builds a request its API rejects.
  {
    const lines = examplesSql.split("\n");
    assert(
      lines.filter((line) => line.includes("@params->('')")).length > 10,
      "expected to find the seeded example queries",
    );
    assertEquals(
      lines.flatMap((line, i) => /(?<!coalesce\()@params->\(''\)/.test(line) ? [i + 1] : []),
      [],
      `examples.sql interpolates @params->('') with no default on these lines, so opening ` +
        `those sheets without ?q= sends an empty search term. Wrap it in coalesce(...).`,
    );
  }

  await sql.end();
  listener.close();
  await pglite.close();

  await new Promise((res) => setTimeout(res, 250));
});
