import { assert, assertEquals, assertObjectMatch } from "jsr:@std/assert";
import { PGlite } from "npm:@electric-sql/pglite";
import { PostgresConnection } from "npm:pg-gateway";
import { citext } from "npm:@electric-sql/pglite/contrib/citext";
import { app, sql, createJwt, automerge } from "./main.ts";
import type { Sheet, Row, Page, Query, LibraryItem } from "./main.ts";
import * as AM from "npm:@automerge/automerge-repo";

const emptyPage: Sheet = { type: "page", doc: { cols: [], rows: [] } };

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
async function get1<T>(
  jwt: string,
  route: string,
  query: Record<string, string | number>,
): Promise<T> {
  return await get<T[]>(jwt, route, query as Record<string, string>)
    .then(xs => xs?.[0])
    .then(
      x => (
        assert(
          x,
          `Expected one but got none: ${route} ${JSON.stringify(query)}`,
        ),
        x
      ),
    );
}

const post = (jwt: string, route: string, body: unknown) =>
  request(jwt, route, { method: "POST", body: JSON.stringify(body) });
const patch = (jwt: string, route: string, body: unknown) =>
  request(jwt, route, { method: "PATCH", body: JSON.stringify(body) });
const del = (jwt: string, route: string, body: unknown) =>
  request(jwt, route, { method: "DELETE", body: JSON.stringify(body) });

// Test setup helpers
const setupTestDb = async () => {
  const listener = Deno.listen({ port: 5434 });
  const pglite = new PGlite({ extensions: { citext } });

  // Start postgres connection handler
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

  return { listener, pglite };
};

const createTestUser = async (email: string) => {
  const [{ usr_id }] = await sql`
    insert into usr (email) values (${email}) 
    on conflict (email) do update set email = excluded.email 
    returning *
  `;
  return { usr_id, jwt: await createJwt(usr_id) };
};

const createTestSheet = async (jwt: string, sheet: Sheet) => {
  const { data: sheet_id } = await post(jwt, `/library`, sheet);
  assert(sheet_id);
  const [type, doc_id] = sheet_id.split(":");
  assert(doc_id);
  return { sheet_id, type, doc_id };
};

Deno.test(async function allTests(t) {
  const { listener, pglite } = await setupTestDb();

  await t.step(async function viewShopSheets(t) {
    // TODO: /shop/sheet
  });

  await t.step(async function viewShopTools(t) {
    // TODO: /shop/tool
  });

  const { jwt } = await createTestUser("alice@example.com");

  await t.step(async function createSheet(t) {
    await post(jwt, `/library`, emptyPage);
  });

  await t.step(async function viewLibrary(t) {
    await post(jwt, `/library`, emptyPage);
    const sheets = await get<LibraryItem[]>(jwt, `/library`);
    assert(sheets.length);
  });

  await t.step(async function editSheetMetadata(t) {
    const { data: id } = await post(jwt, `/library`, emptyPage);
    const meta = { name: "Example", tags: ["tag1", "tag2"] };
    await patch(jwt, `/library/${id}`, meta);
    const sheets = await get<LibraryItem[]>(jwt, `/library`, { sheet_id: id });
    assertEquals(sheets?.[0]?.sheet_id, id);
    assertEquals(sheets?.[0]?.name, meta.name);
    assertEquals(sheets?.[0]?.tags, meta.tags);
  });

  await t.step(async function editPage(t) {
    const { sheet_id, type, doc_id } = await createTestSheet(jwt, emptyPage);
    assertEquals(type, emptyPage.type);
    const row: Row = { a: 1 };
    {
      const hand = await automerge.find<Page>(doc_id);
      const sheet = hand.doc();
      assert(sheet.rows.length === 0);
      hand.change(doc => doc.rows.push(row));
    }
    {
      const hand = await automerge.find<Page>(doc_id);
      const sheet = hand.doc();
      assertEquals(sheet.rows?.[0], row);
    }
  });

  await t.step(async function runPortal(t) {
    const sheets: Sheet[] = [
      {
        type: "template",
        doc: {
          type: "page",
          doc: {
            cols: [{ name: "a", type: "int" }],
            rows: [{ a: 1 }],
          },
        },
      },
      {
        type: "page",
        doc: {
          cols: [{ name: "a", type: "int" }],
          rows: [{ a: 1 }],
        },
      },
      // TODO: {
      // TODO:   type: "query",
      // TODO:   doc: {
      // TODO:     db_id: null,
      // TODO:     lang: "sql",
      // TODO:     code: "select 1 as a",
      // TODO:   },
      // TODO: },
      // TODO: {
      // TODO:   type: "agent",
      // TODO:   doc: {},
      // TODO: },
    ];
    for (const sheet_ of sheets) {
      let sheet_id = "";
      await t.step(async function sellSheet(t) {
        const { jwt } = await createTestUser("bob@example.com");
        const { data: sheet_id_ } = await post(jwt, `library`, sheet_);
        assert(sheet_id_);
        const { data } = await post(jwt, `/shop/sheet/sell/${sheet_id_}`, {});
        assert(data);
        sheet_id = data;
      });
      assert(sheet_id);
      const [type, doc_id] = sheet_id.split(":");
      assert(doc_id);
      assertEquals(type, "portal");
      assert(!AM.isValidDocumentId(doc_id));
      await reject(jwt, `/portal/${doc_id}`);
      await post(jwt, `/shop/sheet/buy/${sheet_id}`, {});
      let sheet: Sheet = await get<Sheet>(jwt, `/portal/${doc_id}`);
      if (sheet.type === "template") sheet = sheet.doc;
      assertObjectMatch(sheet, {
        type: "page",
        doc: {
          cols: [{ type: "int", name: "a" }],
          rows: [{ a: 1 }],
        },
      });
    }
  });

  // TODO: /agent/:id  -- crawler? timer?
  // TODO: /agent/:id/webform
  await t.step(async function runAgent(t) {
    const sheet_: Sheet = {
      type: "agent",
      doc: {
        type: "webhook",
      },
    };
    const { sheet_id, type, doc_id } = await createTestSheet(jwt, sheet_);
    assertEquals(type, sheet_.type);
    await post("", `/agent/${sheet_id}/webhook`, "hello world");
    const sheet = await get<Sheet>(jwt, `/agent/${sheet_id}`);
    assertObjectMatch(sheet, {
      rows: [{ body: "hello world" }],
    });
  });

  await t.step(async function saveQuery(t) {
    const sheet_: Sheet = {
      type: "query",
      doc: {
        db_id: null,
        lang: "sql",
        code: "select 1 as a",
      },
    };
    const { sheet_id, type, doc_id } = await createTestSheet(jwt, sheet_);
    assertEquals(type, sheet_.type);
    const hand = await automerge.find<Query>(doc_id);
    const sheet = hand.doc();
    const { data }: { data: Sheet } = await post(jwt, `/query`, sheet);
    assertObjectMatch(data, {
      cols: [{ type: "int", name: "a" }],
      rows: [{ a: 1 }],
    });
  });

  await t.step(async function runQuery(t) {
    const { data: sheet_id } = await post(jwt, `/library`, {
      type: "page",
      doc: {
        cols: [{ name: "a", type: "int" }],
        rows: [{ a: 1 }],
      },
    });
    const { data: db_id } = await post(
      jwt,
      `/db`,
      "postgresql://127.0.0.1:5434/postgres",
    );
    const queries: { query: Query; page: Page }[] = [
      {
        query: { db_id: null, lang: "sql", code: "select 1 as a" },
        page: { cols: [{ name: "a", type: "int" }], rows: [{ a: 1 }] },
      },
      {
        query: {
          db_id: null,
          lang: "sql",
          code: `select a from sheet('${sheet_id}')`,
        },
        page: { cols: [{ name: "a", type: "int" }], rows: [{ a: 1 }] },
      },
      {
        query: {
          db_id: db_id,
          lang: "sql",
          code: `select a from sheet('${sheet_id}')`,
        },
        page: { cols: [{ name: "a", type: "int" }], rows: [{ a: 1 }] },
      },
    ];
    for (const { query, page } of queries)
      await t.step(async function execQuery(t) {
        const { data } = await post(jwt, `/query`, query);
        assertObjectMatch(data, page);
      });
  });

  await t.step(async function purchaseTool(t) {
    // TODO: /shop/tool
  });

  await t.step(async function viewLedger(t) {
    // TODO: /ledger
  });

  await sql.end();
  listener.close();
  await pglite.close();
});
