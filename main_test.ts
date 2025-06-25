import { assert, assertEquals } from "jsr:@std/assert";
import { PGlite } from "npm:@electric-sql/pglite";
import { PostgresConnection } from "npm:pg-gateway";
import { citext } from "npm:@electric-sql/pglite/contrib/citext";
import { app, sql, createJwt } from "./main.ts";

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

  await t.step(async function viewLibrary(t) {
    post(jwt, `/library`, {});
    const sheets = await get<[]>(jwt, `/library`);
    assert(sheets.length);
  });

  await t.step(async function editSheetMetadata(t) {
    // TODO: /library/:id
  });

  await t.step(async function editSheetContent(t) {
    // TODO: /library/:id
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
});
