// Boots the stack main_test.ts and server_test.ts share: PGlite behind pg-gateway, the codex PGlite, schema, seed.
// Each file runs in its own process (see the test task), so each binds its own ports.
import { assert } from "@std/assert";
import { PGlite } from "@electric-sql/pglite";
import { PostgresConnection } from "pg-gateway";
import { citext } from "@electric-sql/pglite/contrib/citext";
import {
  app,
  CODEX_PREVIEW_MAX,
  createJwt,
  FRAME_BURST,
  hookSecret,
  hookSign,
  pollAlertSheet,
  pollNetSheet,
  pollTimer,
  rateLimitBuckets,
  seed,
  sql,
} from "./main.ts";
import dbSql from "./schema/db.sql" with { type: "text" };

// The port comes from DATABASE_URL, which the test task sets for the second process.
export const DB_PORT = Number(URL.parse(Deno.env.get("DATABASE_URL") ?? "")?.port || 5434);
export const CODEX_PORT = DB_PORT + 1;
export const OWN_DSN = `postgresql://postgres@127.0.0.1:${DB_PORT}/postgres`;
export const CODEX_DSN = `postgresql://postgres@127.0.0.1:${CODEX_PORT}/postgres`;
// Ports nobody listens on: past the other process's pair, which listens too.
export const DEAD_PORTS = [DB_PORT + 4, DB_PORT + 5];

export const request = async (jwt: string, route: string, options?: object) => {
  // Every request this suite makes arrives from one address, which no real
  // client does, so the address bucket is shared by the whole run and empties
  // over the course of it. It refills on the wall clock, so whether a step
  // passed depended on how slow the suite had been up to that point -- making
  // the suite faster is what surfaced it. This helper is the one for requests
  // that are meant to succeed; the limiter itself is exercised in its own steps,
  // which set the bucket they are about by hand and go straight to
  // `app.request`, so clearing here cannot hide what they assert.
  rateLimitBuckets.clear();
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

export const reject = async (jwt: string, route: string, options?: object) => {
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

export async function get<T>(
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

export const post = (jwt: string, route: string, body: unknown) =>
  request(jwt, route, { method: "POST", body: JSON.stringify(body) });
export const put = (jwt: string, route: string, body: unknown) =>
  request(jwt, route, { method: "PUT", body: JSON.stringify(body) });

// Every delivery is signed, so the tests sign it the way a sender does: through
// the server's own hookSign, rather than a second implementation that could
// agree with a bug in the first. The target is signed alongside the body, so it
// is passed in rather than assumed.
export const deliver = async (sheet_id: string, body: string, query = "") =>
  await app.request(`/net/${sheet_id}${query}`, {
    method: "POST",
    headers: new Headers({
      "Content-Type": "application/json",
      "scrapsheets-signature": await hookSign(await hookSecret(sheet_id), `/net/${sheet_id}${query}`, body),
    }),
    body,
  });

// One feed, through the very function the tick hands each due sheet -- not
// the tick itself, which walks every net-http sheet the earlier steps left
// and would re-poll the whole pile for one sheet's assertion.
export const pollFeed = (sheet_id: string, fetcher?: Parameters<typeof pollNetSheet>[2], at?: number) =>
  pollNetSheet(sheet_id, sheet_id.split(":")[1], fetcher, at);

// One alert, through the same function, for the same reason: the tick walks
// every alert sheet the earlier steps left.
export const pollAlert = async (
  sheet_id: string,
  send?: Parameters<typeof pollAlertSheet>[1],
  at?: number,
  fetcher?: Parameters<typeof pollAlertSheet>[3],
) => {
  const [alert] = await sql`
    select sheet_id, doc_id, name, created_by from sheet where sheet_id = ${sheet_id}
  `;
  await pollAlertSheet(alert, send, at, fetcher);
};

export const usr = async (email: string) => {
  const [{ usr_id }] = await sql`
    insert into usr (email) values (${email})
    on conflict (email) do update set email = excluded.email
    returning *
  `;
  return { usr_id, jwt: await createJwt(usr_id) };
};

// A flood must exceed what a bucket allows by a margin: twice the burst, since the bucket empties at one burst.
export const FLOOD = 2 * FRAME_BURST;

export const boot = async () => {
  // The production poller would fire with the real safeFetch once a slow run
  // passes 15 seconds and write DNS-failure rows over fixtures on `.test`
  // hosts; every step here drives the poller itself.
  clearInterval(pollTimer);
  // Booting a PGlite from nothing runs initdb: a second and a half, the largest
  // single cost in this file. The empty cluster it makes depends only on the
  // PGlite build, so the first run on a machine saves it, keyed by the resolved
  // package path, which carries the version, and every later run loads it.
  const key = Array.from(
    new Uint8Array(
      await crypto.subtle.digest("SHA-256", new TextEncoder().encode(import.meta.resolve("@electric-sql/pglite"))),
    ),
    (b) => b.toString(16).padStart(2, "0"),
  ).join("").slice(0, 16);
  const cached = `data/pglite-${key}.tar`;
  const saved = await Deno.readFile(cached).catch((e) => {
    if (e instanceof Deno.errors.NotFound) return undefined;
    throw e;
  });
  const pglite = new PGlite({ extensions: { citext }, ...(saved && { loadDataDir: new Blob([saved]) }) });
  await pglite.waitReady.catch((e) => {
    throw new Error(`PGlite would not load the saved empty cluster at ${cached}. Delete that file and run again.`, {
      cause: e,
    });
  });
  // The empty cluster, taken before our own schema is applied. Booting a PGlite
  // is about a second and a half of WebAssembly bringing up a Postgres data
  // directory from nothing, and this suite needs two of them -- so the second is
  // loaded from the first's bytes instead of doing that work again, which is a
  // quarter of a second rather than another second and a half, and two of them
  // are no longer contending for the same cores while they do it.
  //
  // Taken before `exec(dbSql)` deliberately: the codex database is a *foreign*
  // one, and a clone holding our own tables would have `codexTables()` listing
  // them.
  const bare = saved ? new Blob([saved]) : await pglite.dumpDataDir("none");
  if (!saved) {
    // Written aside and renamed, so a run killed mid-write or a second run
    // beside this one never reads half a cluster.
    const aside = `${cached}.${crypto.randomUUID()}`;
    await Deno.mkdir("data", { recursive: true });
    await Deno.writeFile(aside, new Uint8Array(await bare.arrayBuffer()));
    await Deno.rename(aside, cached);
  }
  // The database a codex sheet points at: its own PGlite, behind its own
  // address, because the gateway hands every connection onto one PGlite
  // session -- a codex connection setting its session read only would set it
  // for main.ts's own connection too, and the next insert anywhere in the
  // suite would be refused. Started here and awaited on connect, so its load
  // overlaps the run rather than adding to it. 5435 and not 5434 is also what
  // makes it external at all: GET /codex/:id refuses a dsn on this server's
  // own host and port.
  // Built on the first connection to it and not before: most of this suite never
  // opens a codex sheet, and a second Postgres coming up in the background is
  // cores the steps that are running want.
  let started: Promise<PGlite> | undefined;
  const external = () =>
    started ??= (async () => {
      const db = new PGlite({ loadDataDir: bare });
      await db.waitReady;
      await db.exec(`
        create table widget (widget_id int, name text);
        insert into widget values (1, 'bolt'), (2, 'nut');
        create table "odd.name" (x int);
        create table gadget (gadget_id int, note text);
        insert into gadget values (1, null), (2, 'v1.5'), (3, 'x');
        create table crowd (n int, tag text);
        insert into crowd select i, case when i = 7 then 'in stock' else 'x' end from generate_series(1, ${
        CODEX_PREVIEW_MAX + 1
      }) i;
      `);
      return db;
    })();

  const serve = (port: number, of: PGlite | (() => Promise<PGlite>)) => {
    const listener = Deno.listen({ hostname: "127.0.0.1", port });
    // pg-gateway hands every socket's messages to one PGlite session. Postgres
    // gives each connection its own backend, so two connections' extended-
    // protocol exchanges never interleave there; here they would, and one side
    // reads 'portal "" cannot be run'. So one connection holds the session until
    // its Terminate ('X', what postgres.js's end() sends). The timeout frees a
    // connection that never sends one: main.ts's own pool connection lives as
    // long as the suite.
    let queue: Promise<void> = Promise.resolve();
    (async () => {
      for await (const conn of listener) {
        // Linux pairs Nagle on this side with delayed ACK on postgres.js's
        // side: each query through the gateway stalls about 40 ms per round
        // trip, 126 ms against 0.8 ms measured. macOS does not show it.
        conn.setNoDelay(true);
        const db = await (typeof of === "function" ? of() : of);
        let held: Promise<void> | undefined;
        let release: () => void = () => {};
        new PostgresConnection(conn, {
          async onStartup() {
            await db.waitReady;
          },
          async onMessage(data, { isAuthenticated }) {
            if (!isAuthenticated) return;
            if (!held) {
              held = queue;
              queue = new Promise<void>((resolve) => {
                const timer = setTimeout(resolve, 2_000);
                release = () => {
                  clearTimeout(timer);
                  resolve();
                };
              });
              await held;
            }
            const result = await db.execProtocolRaw(data);
            if (data[0] === 0x58) release();
            return result;
          },
        });
      }
    })();
    return listener;
  };
  const listeners = [serve(DB_PORT, pglite), serve(CODEX_PORT, external)];

  await pglite.exec(dbSql);
  // seed() grants the error log to this address. It is set before seed() runs
  // because that is the only moment the grant is written.
  Deno.env.set("OPERATOR_EMAIL", "erin@example.com");
  await seed();

  return {
    close: async () => {
      await sql.end();
      for (const listener of listeners) listener.close();
      await pglite.close();
      // Only if something opened it: a run that never touched a codex sheet never
      // built it, and there is nothing to close.
      if (started) await (await started).close();
    },
  };
};
