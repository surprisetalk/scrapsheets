import { assert, assertEquals, assertObjectMatch } from "jsr:@std/assert";
import { PGlite } from "npm:@electric-sql/pglite";
import { PostgresConnection } from "npm:pg-gateway";
import { citext } from "npm:@electric-sql/pglite/contrib/citext";
import { app, sql, createJwt, automerge, arrayify } from "./main.ts";
import type { Sheet, Col, Template, Table, Row } from "./main.ts";
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
        {
          type: "table",
          data: [
            arrayify([
              { name: "a", type: "string", key: 0 },
              { name: "b", type: "string", key: 1 },
              { name: "c", type: "string", key: 2 },
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
        cols.map(col => col.name).join(),
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
        cols.map(col => col.name).join(),
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
        assertEquals(sell_price, "0"); // TODO: should return number
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

    // Bob purchases everything in the shop.
    {
      const [cols_, ...rows] = await get<Table>(jwt, `/shop`);
      const cols = Object.values(cols_);
      assert(cols.length);
      assertEquals(
        cols.map(col => col.name).join(),
        "created_at,sell_id,sell_type,sell_price,name",
      );
      assert(rows.length);
      for (const { sell_id } of rows) {
        const { data: sheet_id } = await post(jwt, `/buy/${sell_id}`, {});
        const [type, doc_id] = sheet_id.split(":");
        switch (type) {
          case "table": {
            const hand = await automerge.find<{ data: Table }>(
              doc_id as AM.AnyDocumentId,
            );
            hand.change(d =>
              d.data.push(
                ...[
                  [1, 2, 3],
                  [4, 5, 6],
                  [7, 8, 9],
                ].map(arrayify),
              ),
            );
            break;
          }
          case "portal": {
            const [cols_, ...rows] = await get<Table>(jwt, `/portal/${doc_id}`);
            const cols = Object.values(cols_);
            assert(cols.length);
            // TODO:
            break;
          }
          case "net-socket":
          // TODO: Send socket message.
          case "net-http":
          case "net-hook": {
            await post(jwt, `/net/${type}:${doc_id}`, { foo: "bar" });
            const [cols_, ...rows] = await get<Table>(
              jwt,
              `/net/${type}:${doc_id}`,
            );
            const cols = Object.values(cols_);
            assert(cols.length);
            assertEquals(cols.map(col => col.name).join(), "created_at,body");
            assert(rows.length);
            break;
          }
          case "query": {
            const hand = await automerge.find<{ data: Row[] }>(
              doc_id as AM.AnyDocumentId,
            );
            const [{ lang, code, args }] = hand.doc().data;
            assertEquals(lang, "sql");
            const {
              data: [cols_, ...rows],
            }: { data: Table } = await post(jwt, `/query`, {
              lang,
              code,
              args,
            });
            const cols = Object.values(cols_);
            assert(cols.length);
            assertEquals(cols.map(col => col.name).join(), "a,b,c");
            assert(rows.length);
            break;
          }
          case "codex-db": {
            await post(
              jwt,
              `/codex-db/${doc_id}`,
              "postgresql://postgres:postgres@127.0.0.1:5434/postgres",
            );
            const [cols_, ...rows] = await get<Table>(
              jwt,
              `/codex/codex-db:${doc_id}`,
            );
            const cols = Object.values(cols_);
            assert(cols.length);
            assertEquals(cols.map(col => col.name).join(), "name,columns");
            assert(rows.length);
            assertEquals(
              rows.map(col => col.name).join(),
              "db,net,sheet,sheet_usr,usr",
            );
            assertEquals(
              rows
                .map((col: any) =>
                  col.columns[0].map((c: any) => c.name).join(),
                )
                .join(""),
              "name,type,key".repeat(rows.length),
            );
            break;
          }
          case "codex-scrapsheets": {
            const [cols_, ...rows] = await get<Table>(
              jwt,
              `/codex/codex-scrapsheets:${doc_id}`,
            );
            const cols = Object.values(cols_);
            assert(cols.length);
            assertEquals(cols.map(col => col.name).join(), "name,columns");
            assert(rows.length);
            assertEquals(rows.map(col => col.name).join(), "shop,library");
            assertEquals(
              rows
                .map((col: any) =>
                  col.columns[0].map((c: any) => c.name).join(),
                )
                .join(""),
              "name,type,key".repeat(rows.length),
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
      const [, { type, doc_id } = {}] = await get<Table>(jwt, `/library`, {
        type: "table",
      });
      assertEquals(type, "table");
      assert(AM.isValidDocumentId(doc_id));
      const {
        data: [cols_, ...rows],
      }: { data: Table } = await post(jwt, `/query`, {
        lang: "sql",
        code: `select * from @table:${doc_id}`,
        args: [],
      });
      const cols = Object.values(cols_);
      assert(cols.length);
      assertEquals(cols.map(col => col.name).join(), "a,b,c");
      assert(rows.length);
    }
  }

  await sql.end();
  listener.close();
  await pglite.close();

  await new Promise(res => setTimeout(res, 250));
});
