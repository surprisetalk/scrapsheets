import { assert } from "jsr:@std/assert";

import { PGlite } from "npm:@electric-sql/pglite";
import { PostgresConnection } from "npm:pg-gateway";
import { citext } from "npm:@electric-sql/pglite/contrib/citext";

import pg from "https://deno.land/x/postgresjs@v3.4.5/mod.js";

Deno.test(async function allTests(t) {
  const pglite = new PGlite({ extensions: { citext } });
  const listener = Deno.listen({ port: 5434 });
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

  const sql = pg({
    host: "127.0.0.1",
    port: 5434,
    database: "postgres",
    username: "postgres",
    password: "",
    ssl: false,
    prepare: false,
    fetch_types: true,
    onnotice: msg => msg.severity !== "DEBUG" && console.log(msg),
  });

  await t.step(async function example(t) {
    console.log(await sql`select 1`);
    assert(true);
  });

  /*
  TODO:
  g /shop/sheet
  g /shop/tool
  p /signup
  p /password
  p /login
  p /shop/sheet/id
  p /shop/tool/id
  g /ledger
  g /library
  p /library/:id
  for [template, page, portal, agent, query]:
    p /library
    w /library/:id
  */

  await sql.end();
  await pglite.close();
  listener.close();
});
