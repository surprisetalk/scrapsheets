import { assert, assertEquals } from "jsr:@std/assert";
import { PGlite } from "npm:@electric-sql/pglite";
import { PostgresConnection } from "npm:pg-gateway";
import { citext } from "npm:@electric-sql/pglite/contrib/citext";
import { app, sql, createJwt, automerge } from "./main.ts";
import type { Sheet, Row, LibraryItem } from "./main.ts";

const emptySheet: Sheet = { type: "page", cols: [], rows: [] };

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

const post = (jwt: string, route: string, body: object) =>
  request(jwt, route, { method: "POST", body: JSON.stringify(body) });
const patch = (jwt: string, route: string, body: object) =>
  request(jwt, route, { method: "PATCH", body: JSON.stringify(body) });
const del = (jwt: string, route: string, body: object) =>
  request(jwt, route, { method: "DELETE", body: JSON.stringify(body) });

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

  await t.step(async function viewShopSheets(t) {
    // TODO: /shop/sheet
  });

  await t.step(async function viewShopTools(t) {
    // TODO: /shop/tool
  });

  const [{ usr_id }] =
    await sql`insert into usr (email) values ('taylor@example.com') returning *`;
  const jwt = await createJwt(usr_id);

  await t.step(async function createSheet(t) {
    await post(jwt, `/library`, emptySheet);
  });

  await t.step(async function viewLibrary(t) {
    await post(jwt, `/library`, emptySheet);
    const sheets = await get<LibraryItem[]>(jwt, `/library`);
    assert(sheets.length);
  });

  await t.step(async function editSheetMetadata(t) {
    const { data: id } = await post(jwt, `/library`, emptySheet);
    const meta = { name: "Example", tags: ["tag1", "tag2"] };
    await patch(jwt, `/library/${id}`, meta);
    const sheets = await get<LibraryItem[]>(jwt, `/library`, { sheet_id: id });
    assertEquals(sheets?.[0]?.sheet_id, id);
    assertEquals(sheets?.[0]?.name, meta.name);
    assertEquals(sheets?.[0]?.tags, meta.tags);
  });

  await t.step(async function editSheetContent(t) {
    const { data: sheet_id } = await post(jwt, `/library`, emptySheet);
    assert(sheet_id);
    const row: Row = { a: 1 };
    {
      const hand = await automerge.find<Sheet>(sheet_id);
      const sheet = hand.doc();
      assertEquals(sheet.type, "page");
      assert(sheet.type === "page" && sheet.rows.length === 0);
      hand.change(doc => doc.type === "page" && doc.rows.push(row));
    }
    {
      const hand = await automerge.find<Sheet>(sheet_id);
      const sheet = hand.doc();
      assertEquals(sheet.type, "page");
      assertEquals(sheet.type === "page" && sheet.rows?.[0], row);
    }
  });

  await t.step(async function purchaseSheet(t) {
    // TODO: /shop/sheet/:id
  });

  await t.step(async function purchaseTool(t) {
    // TODO: /shop/tool/:id
  });

  await t.step(async function viewLedger(t) {
    // TODO: /ledger
  });

  await sql.end();
  listener.close();
  await pglite.close();

  // Give time for all resources to clean up
  await new Promise(resolve => setTimeout(resolve, 500));
});
