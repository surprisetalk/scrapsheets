import { assert, assertEquals, assertObjectMatch } from "jsr:@std/assert";
import { PGlite } from "npm:@electric-sql/pglite";
import { PostgresConnection } from "npm:pg-gateway";
import { citext } from "npm:@electric-sql/pglite/contrib/citext";
import { app, sql, createJwt, automerge } from "./main.ts";
import type { Sheet, Col, Template, Table } from "./main.ts";
import * as AM from "npm:@automerge/automerge-repo";

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
  ).then(res => res.data);
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

Deno.test(async function allTests(t) {
  const listener = Deno.listen({ port: 5434 });
  const pglite = new PGlite({ extensions: { citext } });

  (async () => {
    for await (const conn of listener)
      new PostgresConnection(conn, {
        async onStartup() {
          await pglite.waitReady;
        },
        async onMessage(data, { isAuthenticated }) {
          if (!isAuthenticated) return;
          return await pglite.execProtocolRaw(data);
        },
      });
  })();

  await pglite.waitReady;
  await pglite.exec(await Deno.readTextFile("./db.sql"));

  {
    const { jwt } = await usr("alice@example.com");

    // Alice creates templates.
    {
      // TODO: Even more complicated queries, etc.
      const templates: Template[] = [
        // ["template", ["net-hook", []]],
        [
          "table",
          [
            ["a", "string", 0],
            ["b", "string", 1],
            ["c", "string", 2],
          ],
        ],
        ["net-hook", []],
        ["net-http", ["http://127.0.0.1:5049/test", 1000]],
        ["net-socket", ["ws://127.0.0.1:5051/test"]],
        ["query", ["sql", "select 123 as a, 456 as b, 789 as c", []]],
        ["codex-db", []],
        ["codex-scrapsheets", []],
      ];
      for (const template of templates) {
        const hand = automerge.create<{ data: Sheet["data"] }>({
          data: [template],
        });
        await put(jwt, `/library/template:${hand.documentId}`, {});
      }
      for (const [type, row_0] of templates) {
        const hand = automerge.create<{ data: Sheet["data"] }>({
          data: [row_0] as unknown as [], // TODO:
        });
        await put(jwt, `/library/${type}:${hand.documentId}`, {});
      }

      const [cols, ...rows] = await get<Table>(jwt, `/library`);
      assert(cols.length);
      assertEquals(
        cols.map(col => col[0]).join(),
        "created_at,type,doc_id,name,tags,sell_price",
      );
      assertEquals(rows.length, templates.length * 2);
    }

    // Alice updates templates and posts them to shop.
    {
      const [cols, ...rows] = await get<Table>(jwt, `/library`);
      assert(cols.length);
      assertEquals(
        cols.map(col => col[0]).join(),
        "created_at,type,doc_id,name,tags,sell_price",
      );
      assert(rows.length);
      for (const [, type, doc_id] of rows) {
        const sheet_id = type + ":" + doc_id;
        const meta = { name: `Example ${type}`, tags: ["tag1", "tag2"] };
        await put(jwt, `/library/${sheet_id}`, meta);
        await post(jwt, `/sell/${sheet_id}`, { price: 0 });
        const [, [, , , name, tags, sell_price]] = await get<Table>(
          jwt,
          `/library`,
          { doc_id: doc_id as string },
        );
        assertEquals(name, meta.name);
        assertEquals(tags, meta.tags);
        assertEquals(sell_price, "0"); // TODO: should return number
      }
    }
  }

  // The shop is publicly viewable.
  {
    const [cols, ...rows] = await get<Table>("", `/shop`);
    assert(cols.length);
    assert(rows.length);
  }

  {
    const { jwt } = await usr("bob@example.com");

    // Bob purchases everything in the shop.
    {
      const [cols, ...rows] = await get<Table>(jwt, `/shop`);
      assert(cols.length);
      assertEquals(
        cols.map(col => col[0]).join(),
        "created_at,sell_id,sell_type,sell_price,name",
      );
      assert(rows.length);
      for (const [, sell_id] of rows) {
        const { data: sheet_id } = await post(jwt, `/buy/${sell_id}`, {});
        const [type, doc_id] = sheet_id.split(":");
        switch (type) {
          case "table": {
            const hand = await automerge.find<{ data: Table }>(
              doc_id as AM.AnyDocumentId,
            );
            hand.change(d => d.data.push([1, 2, 3], [4, 5, 6], [7, 8, 9]));
            break;
          }
          case "portal": {
            const [cols, ...rows] = await get<Table>(jwt, `/portal/${doc_id}`);
            assert(cols.length);
            // TODO:
            break;
          }
          case "net-socket":
          // TODO: Send socket message.
          case "net-http":
          case "net-hook": {
            await post(jwt, `/net/${type}:${doc_id}`, { foo: "bar" });
            const [cols, ...rows] = await get<Table>(
              jwt,
              `/net/${type}:${doc_id}`,
            );
            assert(cols.length);
            assertEquals(cols.map(col => col[0]).join(), "created_at,body");
            assert(rows.length);
            break;
          }
          case "query": {
            const hand = await automerge.find<{ data: Table }>(
              doc_id as AM.AnyDocumentId,
            );
            const [[lang, code, args]] = hand.doc().data;
            const {
              data: [cols, ...rows],
            }: { data: Table } = await post(jwt, `/query`, {
              lang,
              code,
              args,
            });
            assert(cols.length);
            assertEquals(cols.map(col => col[0]).join(), "a,b,c");
            assert(rows.length);
            break;
          }
          case "codex-db": {
            await post(
              jwt,
              `/codex-db/${doc_id}`,
              "postgresql://postgres:postgres@127.0.0.1:5434/postgres",
            );
            const [cols, ...rows] = await get<Table>(
              jwt,
              `/codex/codex-db:${doc_id}`,
            );
            assert(cols.length);
            assertEquals(cols.map(col => col[0]).join(), "name,columns");
            assert(rows.length);
            assertEquals(
              rows.map(col => col[0]).join(),
              "db,net,sheet,sheet_usr,usr",
            );
            assertEquals(
              rows
                .map((col: any) => col[1][0].map((c: any) => c[0]).join())
                .join(""),
              "name,type,i".repeat(rows.length),
            );
            break;
          }
          case "codex-scrapsheets": {
            const [cols, ...rows] = await get<Table>(
              jwt,
              `/codex/codex-scrapsheets:${doc_id}`,
            );
            assert(cols.length);
            assertEquals(cols.map(col => col[0]).join(), "name,columns");
            assert(rows.length);
            assertEquals(rows.map(col => col[0]).join(), "shop,library");
            assertEquals(
              rows
                .map((col: any) => col[1][0].map((c: any) => c[0]).join())
                .join(""),
              "name,type,i".repeat(rows.length),
            );
            break;
          }
          default:
            throw new Error(`Not implemented: ${type}`);
        }

        // Charlie does not have access to the purchased sheets.
        {
          const { jwt } = await usr("charlie@example.com");
          await reject(jwt, `/${type}/${sheet_id}`, {});
        }
      }
    }

    // Bob runs a query on his doc.
    {
      // TODO: Create a new doc instead of reusing the one randomly updated from the shop.
      const [, [, type, doc_id]] = await get<Table>(jwt, `/library`, {
        type: "table",
      });
      assertEquals(type, "table");
      assert(AM.isValidDocumentId(doc_id));
      const {
        data: [cols, ...rows],
      }: { data: Table } = await post(jwt, `/query`, {
        lang: "sql",
        code: `select * from @table:${doc_id}`,
        args: [],
      });
      assert(cols.length);
      assertEquals(cols.map(col => col[0]).join(), "a,b,c");
      assert(rows.length);
    }
  }

  await sql.end();
  listener.close();
  await pglite.close();

  await new Promise(res => setTimeout(res, 250));
});
