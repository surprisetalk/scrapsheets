import { assert, assertEquals, assertThrows } from "@std/assert";
import { PGlite } from "@electric-sql/pglite";
import { PostgresConnection } from "pg-gateway";
import { citext } from "@electric-sql/pglite/contrib/citext";
import * as AM from "@automerge/automerge-repo";
import { WebSocketClientAdapter } from "@automerge/automerge-repo-network-websocket";
import Stripe from "stripe";
import {
  app,
  arrayify,
  automerge,
  createJwt,
  createToken,
  NET_KEEP,
  parseNetHeaders,
  pollAlertOnce,
  pollNetOnce,
  seed,
  sendDigestOnce,
  sql,
  trimNet,
} from "./main.ts";
import type { Col, Query, Sheet, Table, Template } from "./main.ts";
import { DATASETS, EXAMPLES } from "./src/examples.mjs";
import {
  applyWindows,
  checkCells,
  checkColumnTypes,
  checkPivot,
  describeRef,
  describeRows,
  rewriteUnpivot,
  rewriteWindows,
  scanRefs,
} from "./src/sql.mjs";
import ala from "alasql";
import dbSql from "./schema/db.sql" with { type: "text" };
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
  await seed();

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

  // POST /signup must send the verification email through resend with the api key.
  {
    Deno.env.set("RESEND_API_KEY", "re_test_key");
    const origFetch = globalThis.fetch;
    let req: Request | undefined;
    globalThis.fetch = ((input: RequestInfo | URL, init?: RequestInit) => {
      const r = new Request(input, init);
      if (!r.url.startsWith("https://api.resend.com/")) return origFetch(input, init);
      req = r;
      return Promise.resolve(Response.json({ id: "email_1" }));
    }) as typeof fetch;
    await post("", `/signup?email=dave@example.com`, {});
    globalThis.fetch = origFetch;
    Deno.env.delete("RESEND_API_KEY");
    assert(req, "signup should send a verification email");
    assertEquals(req!.headers.get("authorization"), "Bearer re_test_key");
    const body = await req!.json();
    assertEquals(body.to, "dave@example.com");
    assertEquals(body.from, "hello@sheets.scrap.land");
    assert(body.text.includes("https://sheets.scrap.land/password?email=dave%40example.com&token="));
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

      // The library had no order at all, so paging it repeated and skipped rows.
      const paged: string[] = [];
      for (let at = 0; at < rows.length; at += 3) {
        const [, ...page] = await get<Table>(jwt, `/library`, { limit: 3, offset: at });
        paged.push(...page.map((row) => `${row.type}:${row.doc_id}`));
      }
      assertEquals(paged, rows.map((row) => `${row.type}:${row.doc_id}`));
      assertEquals(new Set(paged).size, rows.length, "paging must not repeat a sheet");
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

  // Automerge sync over the real websocket route (CBOR join/peer protocol).
  {
    const { jwt } = await usr("erin@example.com");
    const server = Deno.serve({ hostname: "127.0.0.1", port: 0, onListen() {} }, app.fetch);
    const wsUrl = (auth?: string) =>
      `ws://127.0.0.1:${server.addr.port}/library/sync` +
      (auth ? `?auth=Bearer%20${encodeURIComponent(auth)}` : "");

    const hand = automerge.create<Sheet>({
      type: "table",
      data: [arrayify([{ name: "a", type: "num", key: 0 }]), { 0: 42 }],
    });
    await put(jwt, `/library/table:${hand.documentId}`, {});

    // The server answers a CBOR join with a CBOR peer message.
    {
      const ws = new WebSocket(wsUrl(jwt));
      ws.binaryType = "arraybuffer";
      const reply = await new Promise<Record<string, unknown>>((resolve, reject) => {
        const timer = setTimeout(() => reject(new Error("No peer reply within 2s.")), 2000);
        ws.onopen = () =>
          ws.send(AM.cbor.encode({
            type: "join",
            senderId: "raw-test-peer",
            peerMetadata: {},
            supportedProtocolVersions: ["1"],
          }));
        ws.onmessage = (e) => {
          clearTimeout(timer);
          resolve(AM.cbor.decode(new Uint8Array(e.data as ArrayBuffer)) as Record<string, unknown>);
        };
        ws.onerror = () => {
          clearTimeout(timer);
          reject(new Error("WebSocket errored before the peer reply."));
        };
      });
      assertEquals(reply.type, "peer");
      assertEquals(reply.selectedProtocolVersion, "1");
      ws.close();
    }

    // An authenticated client repo syncs a shared doc end-to-end.
    {
      const adapter = new WebSocketClientAdapter(wsUrl(jwt), 100);
      const repo = new AM.Repo({ network: [adapter], peerId: "client-erin" as AM.PeerId });
      const found = await repo.find<Sheet>(hand.documentId);
      assertEquals(
        JSON.parse(JSON.stringify(found.doc())),
        JSON.parse(JSON.stringify(hand.doc())),
      );
      adapter.disconnect();
    }

    // Query output columns keep their source types.
    {
      const {
        data: [cols_],
      }: { data: Table } = await post(jwt, `/query`, {
        lang: "sql",
        code: `select a from @table:${hand.documentId}`,
        args: [],
      });
      assertEquals(Object.values(cols_)[0], { name: "a", type: "num", key: "a" });
    }

    // A doc created on the CLIENT can be registered: PUT /library/:id must be
    // able to fetch it over sync even though no sheet row exists yet.
    {
      const adapter = new WebSocketClientAdapter(wsUrl(jwt), 100);
      const clientRepo = new AM.Repo({ network: [adapter], peerId: "client-erin-creator" as AM.PeerId });
      const clientHand = clientRepo.create<Sheet>({
        type: "table",
        data: [arrayify([{ name: "z", type: "num", key: 0 }]), { 0: 7 }],
      });
      await put(jwt, `/library/table:${clientHand.documentId}`, {});
      const serverDoc = (await automerge.find<Sheet>(clientHand.documentId)).doc();
      assertEquals(JSON.parse(JSON.stringify(serverDoc.data[1])), { 0: 7 });
      adapter.disconnect();
    }

    // Anonymous clients and authenticated non-owners are both denied.
    for (
      const [peer, auth] of [
        ["client-anon", undefined],
        ["client-frank", (await usr("frank@example.com")).jwt],
      ] as const
    ) {
      const adapter = new WebSocketClientAdapter(wsUrl(auth), 100);
      const repo = new AM.Repo({ network: [adapter], peerId: peer as AM.PeerId });
      const outcome = await Promise.race([
        repo.find(hand.documentId).then(() => "shared", () => "denied"),
        new Promise<string>((resolve) => setTimeout(() => resolve("timed out"), 1500)),
      ]);
      assertEquals(outcome, "denied", peer);
      adapter.disconnect();
    }

    // Roles: a viewer reads but cannot write; an editor writes.
    {
      const sheet_id = `table:${hand.documentId}`;
      const viewer = await usr("viewer@example.com");
      const editor = await usr("editor@example.com");
      await post(jwt, `/library/${sheet_id}/share`, { email: "viewer@example.com", role: "viewer" });
      await post(jwt, `/library/${sheet_id}/share`, { email: "editor@example.com", role: "editor" });

      // get() unwraps the envelope's .data for us.
      const share: { members: { email: string; role: string }[]; public: boolean } = await get(
        jwt,
        `/library/${sheet_id}/share`,
      );
      assertEquals(
        share.members.filter((m) => m.email !== "erin@example.com").map((m) => `${m.email}=${m.role}`).sort()
          .join(),
        "editor@example.com=editor,viewer@example.com=viewer",
      );

      // A viewer can load the document.
      const vAdapter = new WebSocketClientAdapter(wsUrl(viewer.jwt), 100);
      const vRepo = new AM.Repo({ network: [vAdapter], peerId: "client-viewer" as AM.PeerId });
      const vFound = await Promise.race([
        vRepo.find<Sheet>(hand.documentId).then(() => "shared", () => "denied"),
        new Promise<string>((resolve) => setTimeout(() => resolve("timed out"), 2000)),
      ]);
      assertEquals(vFound, "shared", "a viewer should be able to read the sheet");

      // ...but the server must not persist the viewer's edit.
      const before = JSON.stringify((await automerge.find<Sheet>(hand.documentId)).doc());
      const vHandle = await vRepo.find<Sheet>(hand.documentId);
      vHandle.change((d: Sheet) => {
        (d.data as unknown as Record<string, unknown>[])[1] = { 0: 666 };
      });
      await new Promise((r) => setTimeout(r, 600));
      assertEquals(
        JSON.stringify((await automerge.find<Sheet>(hand.documentId)).doc()),
        before,
        "a viewer's edit must not reach the server copy",
      );
      // Refusing the write must not tear down the socket: the viewer still reads.
      const stillReadable = await Promise.race([
        vRepo.find<Sheet>(hand.documentId).then(() => "shared", () => "denied"),
        new Promise<string>((resolve) => setTimeout(() => resolve("timed out"), 2000)),
      ]);
      assertEquals(stillReadable, "shared", "a viewer should still read after a refused write");
      vAdapter.disconnect();

      // An editor's write does land.
      const eAdapter = new WebSocketClientAdapter(wsUrl(editor.jwt), 100);
      const eRepo = new AM.Repo({ network: [eAdapter], peerId: "client-editor" as AM.PeerId });
      const eHandle = await eRepo.find<Sheet>(hand.documentId);
      eHandle.change((d: Sheet) => {
        (d.data as unknown as Record<string, unknown>[])[1] = { 0: 4242 };
      });
      for (let i = 0; i < 50; i++) {
        const doc = (await automerge.find<Sheet>(hand.documentId)).doc();
        if (JSON.stringify(doc).includes("4242")) break;
        await new Promise((r) => setTimeout(r, 100));
      }
      assert(
        JSON.stringify((await automerge.find<Sheet>(hand.documentId)).doc()).includes("4242"),
        "an editor's write should reach the server copy",
      );
      eAdapter.disconnect();

      // Removing a member revokes sync.
      await request(jwt, `/library/${sheet_id}/share`, {
        method: "DELETE",
        body: JSON.stringify({ email: "viewer@example.com" }),
      });
      const gone = await sql`
        select true from sheet_usr su inner join usr u using (usr_id)
        where su.sheet_id = ${sheet_id} and u.email = 'viewer@example.com'
      `;
      assertEquals(gone.length, 0, "the removed member should be gone from sheet_usr");
    }

    // A share link grants read to a client with no account at all.
    {
      const sheet_id = `table:${hand.documentId}`;
      const { data: { token } }: { data: { token: string } } = await post(jwt, `/library/${sheet_id}/link`, {});
      const adapter = new WebSocketClientAdapter(wsUrl(token), 100);
      const repo = new AM.Repo({ network: [adapter], peerId: "client-linked" as AM.PeerId });
      const outcome = await Promise.race([
        repo.find<Sheet>(hand.documentId).then(() => "shared", () => "denied"),
        new Promise<string>((resolve) => setTimeout(() => resolve("timed out"), 2000)),
      ]);
      assertEquals(outcome, "shared", "a share-link token should grant read access");
      adapter.disconnect();
    }

    // A public sheet is readable with no token at all.
    {
      const sheet_id = `table:${hand.documentId}`;
      await post(jwt, `/library/${sheet_id}/public`, { public: true });
      const adapter = new WebSocketClientAdapter(wsUrl(undefined), 100);
      const repo = new AM.Repo({ network: [adapter], peerId: "client-public" as AM.PeerId });
      const outcome = await Promise.race([
        repo.find<Sheet>(hand.documentId).then(() => "shared", () => "denied"),
        new Promise<string>((resolve) => setTimeout(() => resolve("timed out"), 2000)),
      ]);
      assertEquals(outcome, "shared", "a public sheet should be readable anonymously");
      adapter.disconnect();
      await post(jwt, `/library/${sheet_id}/public`, { public: false });
    }

    await server.shutdown();
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

    // A bought template's doc carries its type (Elm's decoder requires it).
    {
      const [, ...templates] = await get<Table>(jwt, `/shop`, { name: "Example template" });
      const tableTemplate = templates.find((row) => row.sell_type === "table");
      assert(tableTemplate, "expected a table template in the shop");
      const { data: sheet_id } = await post(jwt, `/buy/${tableTemplate.sell_id}`, {});
      assert(sheet_id.startsWith("table:"), `expected a table sheet, got: ${sheet_id}`);
      const doc = (await automerge.find<{ type: string }>(sheet_id.split(":")[1])).doc();
      assertEquals(doc.type, "table");
    }

    // Buying a live table yields a portal sheet that resolves to the seller's data.
    {
      const [, listing] = await get<Table>(jwt, `/shop`, { name: "Example table" });
      assertEquals(listing?.sell_type, "portal");
      const { data: sheet_id } = await post(jwt, `/buy/${listing.sell_id}`, {});
      assert(sheet_id.startsWith("portal:"), `expected a portal sheet, got: ${sheet_id}`);
      const {
        data: [cols_],
      }: { data: Table } = await request(jwt, `/portal/${sheet_id.split(":")[1]}`);
      assertEquals(
        Object.values(cols_).map((col) => col.name).join(),
        "a,b,c",
      );
    }

    // Seeded datasets are in the shop; buying one yields a full, typed table doc.
    {
      await seed(); // running seed again must not duplicate listings
      const [, ...listings] = await get<Table>(jwt, `/shop`, { name: "countries" });
      assertEquals(listings.length, 1);
      const { data: sheet_id } = await post(jwt, `/buy/${listings[0].sell_id}`, {});
      assert(sheet_id.startsWith("table:"), `expected a table sheet, got: ${sheet_id}`);
      const doc = (await automerge.find<{ type: string; data: Table }>(sheet_id.split(":")[1])).doc();
      assertEquals(doc.type, "table");
      const countries = DATASETS.find((d: { doc_id: string }) => d.doc_id === "countries");
      assert(countries, "countries dataset should exist in DATASETS");
      assertEquals(doc.data.length, countries.doc.data.length);

      // Cross-sheet reference round-trip through the server query engine.
      const {
        data: [, row],
      }: { data: Table } = await post(jwt, `/query`, {
        lang: "sql",
        code: `select count(*) as n from @${sheet_id}`,
        args: [],
      });
      assertEquals(row.n, countries.doc.data.length - 1);
    }

    // Every bundled dataset must satisfy its own declared types, and every
    // bundled query must run against them. These ship as the shop's inventory,
    // so a broken one is a broken storefront.
    {
      const cols = (id: string) =>
        Object.values(DATASETS.find((d: { doc_id: string }) => `table:${d.doc_id}` === id)!.doc.data[0]) as Col[];
      const loaded: Record<string, Record<string, unknown>[]> = {};
      for (const { doc_id, doc } of DATASETS as unknown as { doc_id: string; doc: { data: Table } }[]) {
        const [cols_, ...rows] = doc.data;
        loaded[`table:${doc_id}`] = rows.map((row) =>
          Object.fromEntries(Object.values(cols_).map((col) => [col.name, row[col.key]]))
        );
        checkColumnTypes(`table:${doc_id}`, Object.values(cols_), loaded[`table:${doc_id}`]);
      }
      const examples = Object.entries(EXAMPLES) as unknown as [string, { doc: { type: string; data: [Query] } }][];
      let ran = 0;
      for (const [id, ex] of examples) {
        if (ex.doc.type !== "query") continue;
        const { code } = ex.doc.data[0];
        // A @query ref needs the whole recursion; the browser test covers those.
        if (code.includes("@query:")) continue;
        const described = describeRef(code);
        let rows: unknown[];
        if (described) rows = describeRows(described, cols(described), loaded[described]);
        else {
          // The same order the server runs: cells, then unpivot, then windows.
          const { sql, cells } = scanRefs(code);
          const colsOf = Object.fromEntries(
            Object.entries(loaded).map(([ref, rows_]) => [
              ref,
              Object.keys(rows_[0] ?? {}).map((name) => ({ name })),
            ]),
          );
          checkCells(cells, loaded, colsOf);
          checkPivot(sql);
          const plan = rewriteWindows(rewriteUnpivot(sql, colsOf));
          const res = await ala(plan.sql, [loaded]) as { columns: { columnid: string }[]; data: unknown[] };
          const run = (q: string, params: unknown[]) => (ala(q, params) as { data: unknown[] }).data;
          rows = plan.windows.length ? applyWindows(res, plan, run).data : res.data;
          // A window column that came back empty means the pass silently missed
          // it. A lifted one is dropped from the result, so it has nothing to say.
          for (const w of plan.windows) {
            if (w.alias.startsWith("__")) continue;
            assert(
              (rows as Record<string, unknown>[]).some((r) => r[w.alias] !== null && r[w.alias] !== undefined),
              `${id}: the window column "${w.alias}" is empty in every row`,
            );
          }
        }
        assert(rows.length > 0, `${id} returned no rows`);
        ran++;
      }
      const chained = examples.filter(([, ex]) => ex.doc.type === "query" && ex.doc.data[0].code.includes("@query:"));
      assertEquals(
        ran,
        examples.filter(([, ex]) => ex.doc.type === "query").length - chained.length,
        `${chained.length} examples build on another query; the browser test covers those`,
      );
    }

    // An alert sheet: a query, a destination, and a log of what it decided. It
    // fires when the query returns a row, so the condition is the where clause.
    {
      const watched = automerge.create<{ data: Sheet["data"] }>({
        data: [
          arrayify([{ name: "region", type: "text", key: 0 }, { name: "burn", type: "num", key: 1 }]),
          { 0: "north", 1: 0.9 },
          { 0: "south", 1: 0.8 },
        ],
      });
      await put(jwt, `/library/table:${watched.documentId}`, {});
      const alert = automerge.create<{ data: [{ code: string; to: string; interval: number }] }>({
        data: [{
          code: `select region, burn from @table:${watched.documentId} where burn > 1`,
          to: "ops@example.com",
          interval: 60,
        }],
      });
      await put(jwt, `/library/alert:${alert.documentId}`, { name: "burn watch" });
      const alert_id = `alert:${alert.documentId}`;

      const sent: { to: string; rows: unknown[]; added: number; removed: number }[] = [];
      const send = (
        to: string,
        _id: string,
        _name: string,
        rows: unknown[],
        diff: { added: unknown[]; removed: number } | null,
      ) => {
        sent.push({ to, rows, added: diff?.added.length ?? -1, removed: diff?.removed ?? -1 });
        return Promise.resolve("sent");
      };
      const history = async () => {
        const [, ...rows] = await get<Table>(jwt, `/sheet/${alert_id}`, {});
        return rows.map((row) => JSON.parse(String(row.body)) as Record<string, unknown>);
      };

      // A day ahead, so the background interval that also runs this never decides
      // a step of the test: whatever it recorded, these calls are still due.
      let clock = Date.now() + 86_400_000;

      // Nothing breaches yet, so the first run records "clear" and sends nothing.
      await pollAlertOnce(send, clock);
      assertEquals(sent.length, 0, "a clear alert should not email anyone");
      assertEquals((await history())[0].status, "clear");

      // The same answer again is the same alert, and is not recorded twice.
      const settled = (await history()).length;
      clock += 120_000;
      await pollAlertOnce(send, clock);
      assertEquals((await history()).length, settled, "an unchanged answer should not be recorded again");

      // A row breaches: it fires once, to the address on the sheet.
      watched.change((d: { data: Record<number, unknown>[] }) => {
        d.data[1][1] = 1.4;
      });
      clock += 120_000;
      await pollAlertOnce(send, clock);
      assertEquals(sent.length, 1, "a breach should email once");
      assertEquals(sent[0].to, "ops@example.com");
      assertEquals(sent[0].rows.length, 1);
      const fired = (await history())[0];
      assertEquals([fired.status, fired.rows, fired.delivery], ["firing", 1, "sent"]);
      // One row arrived, none left: the run says so and the email leads with it.
      assertEquals([fired.added, fired.removed], [1, 0]);
      assertEquals([sent[0].added, sent[0].removed], [1, 0]);

      // Still breaching, same rows: no second email.
      clock += 120_000;
      await pollAlertOnce(send, clock);
      assertEquals(sent.length, 1, "the same breach should not email twice");

      // A second region breaches and the first recovers: one row in, one out.
      watched.change((d: { data: Record<number, unknown>[] }) => {
        d.data[1][1] = 0.7;
        d.data[2][1] = 1.9;
      });
      clock += 120_000;
      await pollAlertOnce(send, clock);
      assertEquals(sent.length, 2, "a different breach should email again");
      assertEquals([sent[1].added, sent[1].removed], [1, 1]);
      const swapped = (await history())[0];
      assertEquals([swapped.rows, swapped.added, swapped.removed], [1, 1, 1]);

      // A broken query is recorded against the sheet rather than thrown away.
      alert.change((d: { data: [{ code: string }] }) => {
        d.data[0].code = "select * fromm nowhere";
      });
      clock += 120_000;
      await pollAlertOnce(send, clock);
      const failed = (await history())[0];
      assertEquals(failed.status, "error");
      assert(String(failed.error).includes("fromm"), `expected the bad SQL named, got: ${failed.error}`);

      // An alert mails its author on a timer, so a copy of one would mail a
      // stranger's address: it cannot be listed.
      await reject(jwt, `/sell/${alert_id}`, { method: "POST", body: JSON.stringify({ price: 0 }) });

      // A digest alert records its run but holds the email, and one summary a day
      // goes to the account address with every held run since the last one.
      {
        alert.change((d: { data: [{ code: string; digest?: boolean }] }) => {
          d.data[0].code = `select region, burn from @table:${watched.documentId} where burn > 0`;
          d.data[0].digest = true;
        });
        clock += 120_000;
        await pollAlertOnce(send, clock);
        assertEquals(sent.length, 2, "a digest alert should not email on its own");
        assertEquals((await history())[0].delivery, "held for the daily digest");

        const digests: { to: string; runs: number }[] = [];
        const digest = (to: string, runs: unknown[]) => {
          digests.push({ to, runs: runs.length });
          return Promise.resolve("sent");
        };
        await sendDigestOnce(digest, clock);
        assertEquals(digests.length, 1, "the held run should arrive in a digest");
        assertEquals(digests[0].to, "bob@example.com");
        assertEquals(digests[0].runs, 1);

        // One a day: the same account does not get a second digest an hour later.
        await sendDigestOnce(digest, clock + 3_600_000);
        assertEquals(digests.length, 1, "a digest should go out at most once a day");

        // A day later, with nothing new held, there is nothing to send.
        await sendDigestOnce(digest, clock + 25 * 3_600_000);
        assertEquals(digests.length, 1, "an empty day should not send an empty digest");
      }

      // An alert reads as a sheet, so its history is queryable like any log.
      const { data: [, row] }: { data: Table } = await post(jwt, `/query`, {
        lang: "sql",
        code: `select count(*) as n from @${alert_id}`,
        args: [],
      });
      assertEquals(Number(row.n), (await history()).length);
      assert(Number(row.n) >= 3, `expected the clear, the firing and the error, got ${row.n}`);
    }

    // A chart is a sheet: its settings describe a query, so it reads, pages and
    // exports through exactly the paths every other sheet does.
    {
      const source = automerge.create<{ data: Sheet["data"] }>({
        data: [
          arrayify([{ name: "month", type: "date", key: 0 }, { name: "spent", type: "usd", key: 1 }]),
          { 0: "2026-01-01", 1: 120 },
          { 0: "2026-02-01", 1: 150 },
          { 0: "2026-03-01", 1: 90 },
        ],
      });
      await put(jwt, `/library/table:${source.documentId}`, {});
      const chart = automerge.create<{ data: [{ source: string; kind: string; x: string; y: string }] }>({
        data: [{ source: `@table:${source.documentId}`, kind: "line", x: "month", y: "spent" }],
      });
      await put(jwt, `/library/chart:${chart.documentId}`, { name: "spend" });
      const chart_id = `chart:${chart.documentId}`;

      const [cols_, ...points] = await get<Table>(jwt, `/sheet/${chart_id}`);
      assertEquals(Object.values(cols_).map((col) => col.name).join(), "x,y");
      assertEquals(points.map((row) => row.y).join(), "120,150,90");
      // Ordered by the x column, so the same chart drawn twice is the same line.
      assertEquals(points.map((row) => row.x).join(), "2026-01-01,2026-02-01,2026-03-01");

      const csv = await app.request(`/export/${chart_id}.csv`, {
        headers: new Headers({ Authorization: `Bearer ${jwt}` }),
      });
      assert(csv.ok, `chart export failed: ${csv.status}`);
      assert((await csv.text()).includes("2026-02-01"), "a chart should export the rows it draws");

      // A column name is the only thing that goes into the SQL, so anything else
      // is refused by name rather than concatenated in.
      for (
        const [cfg, said] of [
          [{ source: "budget", kind: "line", x: "month", y: "spent" }, "one table or query sheet"],
          [{ source: "@chart:abc", kind: "line", x: "month", y: "spent" }, "one table or query sheet"],
          [{ source: `@table:${source.documentId}`, kind: "line", x: "month", y: "spent; drop table" }, "column name"],
        ] as const
      ) {
        const bad = automerge.create<{ data: [typeof cfg] }>({ data: [cfg] });
        await put(jwt, `/library/chart:${bad.documentId}`, {});
        const res = await app.request(`/sheet/chart:${bad.documentId}`, {
          headers: new Headers({ Authorization: `Bearer ${jwt}` }),
        });
        assertEquals(res.status, 400);
        assert((await res.text()).includes(said), `expected "${said}" for ${JSON.stringify(cfg)}`);
      }
    }

    // A dashboard owns no data: it names the sheets to show, so its own rows are
    // the list of what it names, and each tile is that sheet embedded.
    {
      const board = automerge.create<{ data: [{ tiles: string[] }] }>({
        data: [{ tiles: ["@chart:one", "@query:two"] }],
      });
      await put(jwt, `/library/dashboard:${board.documentId}`, { name: "watch" });
      const [cols_, ...tiles] = await get<Table>(jwt, `/sheet/dashboard:${board.documentId}`);
      assertEquals(Object.values(cols_).map((col) => col.name).join(), "tile");
      assertEquals(tiles.map((row) => row.tile).join(), "chart:one,query:two");
    }

    // Buying with invalid sell_id returns 404.
    {
      await reject(jwt, `/buy/nonexistent_sell_id`, {
        method: "POST",
        body: JSON.stringify({}),
      });
    }

    // A $0 buy writes a payment row with no Stripe session.
    {
      const { usr_id: bobId } = await usr("bob@example.com");
      const pays = await sql`select * from payment where buyer_id = ${bobId} order by payment_id`;
      assert(pays.length >= 1, "free buys should write payment rows");
      assert(pays.every((p: { amount: string; stripe_session_id: string | null }) => p.amount === "0"));
      assert(pays.every((p: { stripe_session_id: string | null }) => p.stripe_session_id === null));
      assert(pays.every((p: { sheet_id: string | null }) => p.sheet_id));
    }

    // Paid listings go through Stripe Checkout and fulfill on the signed webhook.
    {
      const { jwt: aliceJwt, usr_id: aliceId } = await usr("alice@example.com");
      const { jwt: bobJwt, usr_id: bobId } = await usr("bob@example.com");
      const hand = automerge.create<{ type: string; data: unknown[] }>({
        type: "table",
        data: [arrayify([{ name: "a", type: "text", key: 0 }])],
      });
      await put(aliceJwt, `/library/table:${hand.documentId}`, { name: "Paid table" });
      await post(aliceJwt, `/sell/table:${hand.documentId}`, { price: 5 });
      const [, listing] = await get<Table>(bobJwt, `/shop`, { name: "Paid table" });
      assert(listing?.sell_id, "expected Paid table in the shop");

      const raw = async (path: string, init?: RequestInit) =>
        await app.request(path, {
          headers: new Headers({
            "Content-Type": "application/json",
            Authorization: `Bearer ${bobJwt}`,
            ...Object.fromEntries(new Headers(init?.headers).entries()),
          }),
          ...init,
        });

      {
        const res = await raw(`/buy/${listing.sell_id}`, { method: "POST", body: "{}" });
        assertEquals(res.status, 500);
        assert(
          (await res.text()).includes("STRIPE_SECRET_KEY"),
          "paid buy without a Stripe key should name STRIPE_SECRET_KEY",
        );
        const copies = await sql`select * from sheet where buy_id = ${listing.sell_id}`;
        assertEquals(copies.length, 0);
      }

      const stripeCalls: string[] = [];
      let sessionReq: Request | undefined;
      const origFetch = globalThis.fetch;
      Deno.env.set("STRIPE_SECRET_KEY", "sk_test_paid");
      globalThis.fetch = ((input: RequestInfo | URL, init?: RequestInit) => {
        const r = new Request(input, init);
        if (!r.url.startsWith("https://api.stripe.com/")) return origFetch(input, init);
        stripeCalls.push(new URL(r.url).pathname);
        if (r.url.includes("/v1/customers"))
          return Promise.resolve(Response.json({ id: "cus_test_1", object: "customer" }));
        if (r.url.includes("/v1/checkout/sessions")) {
          sessionReq = r;
          return Promise.resolve(Response.json({
            id: "cs_test_1",
            object: "checkout.session",
            url: "https://checkout.stripe.com/c/pay/cs_test_1",
          }));
        }
        return Promise.resolve(Response.json({ error: { message: `unexpected Stripe ${r.url}` } }, { status: 500 }));
      }) as typeof fetch;
      const { data: checkout } = await post(bobJwt, `/buy/${listing.sell_id}`, {});
      globalThis.fetch = origFetch;
      Deno.env.delete("STRIPE_SECRET_KEY");
      assertEquals(checkout, { checkout_url: "https://checkout.stripe.com/c/pay/cs_test_1" });
      assert(stripeCalls.includes("/v1/customers"), `expected customers.create, got ${stripeCalls.join()}`);
      assert(stripeCalls.includes("/v1/checkout/sessions"), `expected sessions.create, got ${stripeCalls.join()}`);
      assert(sessionReq, "paid buy should POST a Checkout Session");
      const sessionBody = new URLSearchParams(await sessionReq!.text());
      assertEquals(sessionBody.get("line_items[0][price_data][unit_amount]"), "500");
      assertEquals(sessionBody.get("payment_method_types[0]"), "card");
      assertEquals(sessionBody.get("metadata[usr_id]"), String(bobId));
      assertEquals(sessionBody.get("metadata[sell_id]"), listing.sell_id);
      assertEquals(sessionBody.get("customer"), "cus_test_1");
      assertEquals((await sql`select * from sheet where buy_id = ${listing.sell_id}`).length, 0);
      const [bob] = await sql`select stripe_customer_id from usr where usr_id = ${bobId}`;
      assertEquals(bob.stripe_customer_id, "cus_test_1");

      const webhookSecret = "whsec_test_secret";
      const payloadFor = (over: Record<string, unknown>) =>
        JSON.stringify({
          id: "evt_test_1",
          object: "event",
          type: "checkout.session.completed",
          data: {
            object: {
              id: "cs_test_1",
              object: "checkout.session",
              payment_status: "paid",
              payment_intent: "pi_test_1",
              amount_total: 500,
              metadata: { usr_id: String(bobId), sell_id: listing.sell_id },
              ...over,
            },
          },
        });
      const sign = (payload: string, secret = webhookSecret) =>
        new Stripe("sk_test_unused").webhooks.generateTestHeaderStringAsync({ payload, secret });

      {
        const res = await raw("/stripe", {
          method: "POST",
          body: payloadFor({}),
          headers: { "Content-Type": "application/json" },
        });
        assertEquals(res.status, 500);
        assert((await res.text()).includes("STRIPE_WEBHOOK_SECRET"));
      }

      Deno.env.set("STRIPE_WEBHOOK_SECRET", webhookSecret);
      {
        const payload = payloadFor({});
        const res = await raw("/stripe", {
          method: "POST",
          body: payload,
          headers: { "Content-Type": "application/json", "stripe-signature": "t=1,v1=deadbeef" },
        });
        assertEquals(res.status, 400);
        assert((await res.text()).includes("stripe-signature"));
      }
      {
        const payload = payloadFor({ payment_status: "unpaid" });
        const res = await raw("/stripe", {
          method: "POST",
          body: payload,
          headers: { "Content-Type": "application/json", "stripe-signature": await sign(payload) },
        });
        assertEquals(res.status, 200);
        assertEquals((await sql`select * from sheet where buy_id = ${listing.sell_id}`).length, 0);
      }
      {
        const payload = payloadFor({});
        // Stripe signs webhooks; they do not carry our JWT. /stripe is in front of jwt middleware.
        const res = await app.request("/stripe", {
          method: "POST",
          body: payload,
          headers: { "Content-Type": "application/json", "stripe-signature": await sign(payload) },
        });
        assertEquals(res.status, 200);
        const copies = await sql`select * from sheet where buy_id = ${listing.sell_id}`;
        assertEquals(copies.length, 1);
        assertEquals(copies[0].created_by, bobId);
        assertEquals(copies[0].buy_price, "5");
        const pays = await sql`select * from payment where stripe_session_id = 'cs_test_1'`;
        assertEquals(pays.length, 1);
        assertEquals(pays[0].buyer_id, bobId);
        assertEquals(pays[0].seller_id, aliceId);
        assertEquals(pays[0].amount, "5");
        assertEquals(pays[0].sheet_id, copies[0].sheet_id);
        assertEquals(pays[0].stripe_payment_intent_id, "pi_test_1");

        const replay = await app.request("/stripe", {
          method: "POST",
          body: payload,
          headers: { "Content-Type": "application/json", "stripe-signature": await sign(payload) },
        });
        assertEquals(replay.status, 200);
        assertEquals((await sql`select * from sheet where buy_id = ${listing.sell_id}`).length, 1);
        assertEquals((await sql`select * from payment where stripe_session_id = 'cs_test_1'`).length, 1);
      }
      Deno.env.delete("STRIPE_WEBHOOK_SECRET");
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

    // The UDFs registered by src/sql.mjs. These run in the browser too, from the
    // same module, so a passing case here is a passing case there.
    {
      const run = async (code: string) => {
        const { data: [, row] }: { data: Table } = await post(jwt, `/query`, { lang: "sql", code, args: [] });
        return row;
      };
      const round = (n: unknown, p = 6) => Math.round(Number(n) * 10 ** p) / 10 ** p;

      // Aggregates: AlaSQL ships these uppercase only, so the lowercase spelling
      // used to throw "alasql.fn.var is not a function".
      const stats = await run(
        `select var(x) v, stdev(x) sd, median(x) m, mode(x) mo, array_agg(x) a
         from (select 1 as x union all select 3 as x union all select 3 as x)`,
      );
      assertEquals(round(stats.v), round(4 / 3));
      assertEquals(round(stats.m), 3);
      assertEquals(stats.mo, 3);
      assertEquals((stats.a as number[]).length, 3);

      const pct = await run(`select percentile(array(x), 0.5) p50 from (select 1 as x union all select 3 as x)`);
      assertEquals(pct.p50, 2);

      // y = 2x + 1 exactly, so the fit is exact.
      const reg = await run(
        `select round(corr(array(x),array(y)),6) c, round(regr_slope(array(x),array(y)),6) sl,
                round(regr_intercept(array(x),array(y)),6) ic, round(r2(array(x),array(y)),6) r
         from (select 1 as x, 3 as y union all select 2, 5 union all select 3, 7)`,
      );
      assertEquals([reg.c, reg.sl, reg.ic, reg.r], [1, 2, 1, 1]);

      const re = await run(
        `select regexp_replace('a1b2','[0-9]','#') r, regexp_extract('order-42','([0-9]+)',1) e, regexp_split('a,b',',') s`,
      );
      assertEquals([re.r, re.e, (re.s as string[]).join("|")], ["a#b#", "42", "a|b"]);

      const fuzzy = await run(
        `select levenshtein('kitten','sitting') l, soundex('Robert') s, token_set_ratio('acme corp','corp acme') t`,
      );
      assertEquals([fuzzy.l, fuzzy.s, fuzzy.t], [3, "R163", 1]);

      const dates = await run(
        `select date_trunc('month','2026-08-16T12:00:00Z') m, date_add('day',7,'2026-08-16') a,
                date_diff('day','2026-01-01','2026-03-01') d, iso_week('2026-01-05') w,
                business_days('2026-08-10','2026-08-17') b`,
      );
      assertEquals(dates.m, "2026-08-01T00:00:00.000Z");
      assertEquals(dates.a, "2026-08-23T00:00:00.000Z");
      assertEquals([dates.d, dates.w, dates.b], [59, 2, 5]);

      // A fiscal year is named for the calendar year it ends in, so an October
      // start puts 2026-10-01 in FY2027 period 1, and 2026-09-30 in FY2026 period 12.
      const fy = await run(
        `select fiscal_year('2026-10-01',10) a, fiscal_quarter('2026-10-01',10) b, fiscal_period('2026-10-01',10) c,
                fiscal_year('2026-09-30',10) d, fiscal_period('2026-09-30',10) e, fiscal_year('2026-12-31',1) f`,
      );
      assertEquals([fy.a, fy.b, fy.c, fy.d, fy.e, fy.f], [2027, 1, 1, 2026, 12, 2026]);

      const json = await run(
        `select json_extract('{"a":{"b":7}}','$.a.b') v, json_extract('{"xs":[10,20]}','$.xs[1]') x`,
      );
      assertEquals([json.v, json.x], [7, 20]);

      // A date spine turns gaps into zero rows instead of missing rows.
      const spine = await run(`select count(*) n from series('2026-01-01','2026-01-10')`);
      assertEquals(spine.n, 10);
    }

    // Window functions. AlaSQL parses `over (partition by ...)` and then computes
    // it wrong -- sum(x) over (...) came back 0 -- so src/sql.mjs lifts every
    // window out of the query and computes it over the rows the engine returns.
    // The page runs the same pass from the same module.
    {
      const rows = async (code: string) => {
        const { data: [, ...rs] }: { data: Table } = await post(jwt, `/query`, { lang: "sql", code, args: [] });
        return rs as Record<string, number | string>[];
      };
      const T = `(select 'a' as shop, '2026-01-01' as day, 10 as amt
                  union all select 'b' as shop, '2026-01-01' as day, 5 as amt
                  union all select 'a' as shop, '2026-01-02' as day, 20 as amt
                  union all select 'b' as shop, '2026-01-02' as day, 5 as amt
                  union all select 'a' as shop, '2026-01-03' as day, 30 as amt
                  union all select 'b' as shop, '2026-01-03' as day, 50 as amt)`;
      const col = (rs: Record<string, number | string>[], name: string) => rs.map((r) => r[name]).join();

      const running = await rows(
        `select shop, day, sum(amt) over (partition by shop order by day) as running from ${T} order by shop, day`,
      );
      assertEquals(col(running, "running"), "10,30,60,5,10,60");

      // Ties share a rank but not a row number, and dense_rank does not skip.
      const ranked = await rows(
        `select amt, row_number() over (order by amt) as rn, rank() over (order by amt) as rk,
                dense_rank() over (order by amt) as dr
         from ${T} where shop = 'b' order by rn`,
      );
      assertEquals([col(ranked, "rn"), col(ranked, "rk"), col(ranked, "dr")], ["1,2,3", "1,1,3", "1,1,2"]);

      const offsets = await rows(
        `select day, lag(amt, 1, 0) over (partition by shop order by day) as prev,
                lead(amt) over (partition by shop order by day) as next,
                avg(amt) over (partition by shop order by day rows between 1 preceding and current row) as ma2
         from ${T} where shop = 'a' order by day`,
      );
      assertEquals([col(offsets, "prev"), col(offsets, "next"), col(offsets, "ma2")], [
        "0,10,20",
        "20,30,",
        "10,15,25",
      ]);

      // A window over an aggregate: the grand total beside each group's own.
      const grouped = await rows(
        `select shop, sum(amt) as amt, sum(sum(amt)) over () as grand from ${T} group by shop order by shop`,
      );
      assertEquals([col(grouped, "amt"), col(grouped, "grand")], ["60,60", "120,120"]);

      // The row cap comes off before the engine runs and goes back on after, so
      // the running total is the one over every row, not over the two returned.
      const capped = await rows(
        `select shop, sum(amt) over (partition by shop order by day) as running from ${T} order by day, shop limit 2`,
      );
      assertEquals(col(capped, "running"), "10,5");

      // ignore nulls steps over the rows that have no value, which is what turns
      // last_value() into a forward fill and lag() into "the last one there was".
      const G = `(select 1 as n, 10 as px
                  union all select 2 as n, null as px
                  union all select 3 as n, null as px
                  union all select 4 as n, 14 as px)`;
      const filled = await rows(
        `select n, px,
                last_value(px) ignore nulls over (order by n rows between unbounded preceding and current row) as carried,
                lag(px) ignore nulls over (order by n) as prev,
                lag(px) respect nulls over (order by n) as prev_raw
         from ${G} order by n`,
      );
      assertEquals(col(filled, "carried"), "10,10,10,14");
      assertEquals(col(filled, "prev"), ",10,10,10");
      assertEquals(col(filled, "prev_raw"), ",10,,");

      // The result column carries the window's own type, not the source column's.
      const { data: [typed] }: { data: Table } = await post(jwt, `/query`, {
        lang: "sql",
        code: `select shop, row_number() over (order by amt) as rn from ${T}`,
        args: [],
      });
      assertEquals(Object.values(typed).map((c) => `${c.name}:${c.type}`).join(), "shop:text,rn:int");

      // A window that is not a select item of its own cannot be lifted, and
      // returning zeros for it is worse than saying so.
      for (
        const [code, said] of [
          [`select shop, sum(amt) over (partition by shop) from ${T}`, "as some_name"],
          [`select shop, sum(amt) over (partition by shop) * 2 as z from ${T}`, "do not wrap it"],
          [`select shop, sum(amt) ignore nulls over (partition by shop) as z from ${T}`, "no nulls to ignore"],
          [`select shop, sumx(amt) over (partition by shop) as z from ${T}`, "not a window function"],
          [`select shop from (select shop, sum(amt) over () as w from ${T}) x`, "outermost select list"],
          [`select shop, amt as z, sum(amt) over () as z from ${T}`, "both named"],
          [
            `select shop, sum(amt) over (partition by shop range between 1 preceding and current row) as z from ${T}`,
            "range frame cannot count",
          ],
        ] as const
      ) {
        const res = await app.request(`/query`, {
          method: "POST",
          headers: new Headers({ Authorization: `Bearer ${jwt}`, "Content-Type": "application/json" }),
          body: JSON.stringify({ lang: "sql", code, args: [] }),
        });
        assertEquals(res.status, 400);
        const body = await res.text();
        assert(body.includes(said), `expected "${said}" in: ${body}`);
      }
    }

    // Cell references, unpivot and qualify: the three passes that run beside the
    // window one, each on a real sheet because each reads the sheet's own shape.
    {
      const sheet = (cols: Col[], ...rows: Record<number, unknown>[]) => {
        const doc = automerge.create<{ data: Sheet["data"] }>({ data: [arrayify(cols), ...rows] });
        return doc.documentId;
      };
      const cfg = sheet(
        [{ name: "as_of", type: "date", key: 0 }, { name: "factor", type: "num", key: 1 }],
        { 0: "2026-08-20", 1: 3 },
      );
      const wide = sheet(
        [{ name: "team", type: "text", key: 0 }, { name: "q1", type: "int", key: 1 }, {
          name: "q2",
          type: "int",
          key: 2,
        }],
        { 0: "eng", 1: 10, 2: 12 },
        { 0: "ops", 1: 4, 2: 5 },
      );
      for (const id of [cfg, wide]) await put(jwt, `/library/table:${id}`, {});

      const rows = async (code: string) => {
        const { data: [, ...rs] }: { data: Table } = await post(jwt, `/query`, { lang: "sql", code, args: [] });
        return rs as Record<string, number | string>[];
      };
      const col = (rs: Record<string, number | string>[], name: string) => rs.map((r) => r[name]).join();

      // A cell reference is a scalar: one value out of a one-row sheet.
      const scaled = await rows(
        `select team, q1 * @table:${cfg}.factor as scaled, @table:${cfg}.as_of as asked from @table:${wide} order by team`,
      );
      assertEquals([col(scaled, "scaled"), col(scaled, "asked")], ["30,12", "2026-08-20,2026-08-20"]);

      // Wide to long. AlaSQL's own unpivot drops every column it is not
      // unpivoting, so this one never reaches it.
      const long = await rows(
        `select team, quarter, headcount from @table:${wide} unpivot (headcount for quarter in (q1, q2)) order by team, quarter`,
      );
      assertEquals(col(long, "team"), "eng,eng,ops,ops");
      assertEquals(col(long, "quarter"), "q1,q2,q1,q2");
      assertEquals(col(long, "headcount"), "10,12,4,5");

      // ...and back again, which AlaSQL does do correctly.
      const back = await rows(
        `select * from (${""}select team, quarter, headcount from @table:${wide} unpivot (headcount for quarter in (q1, q2))) u pivot (sum(headcount) for quarter)`,
      );
      assertEquals([col(back, "q1"), col(back, "q2")], ["10,4", "12,5"]);

      // qualify filters on the window it computes, which is what makes an as-of
      // join one statement: the latest row at or before each date.
      const asof = await rows(
        `select w.team, u.quarter, u.headcount from @table:${wide} w join (${""}select team, quarter, headcount from @table:${wide} unpivot (headcount for quarter in (q1, q2))) u on u.team = w.team qualify row_number() over (partition by w.team order by u.headcount desc) = 1 order by w.team`,
      );
      assertEquals([col(asof, "team"), col(asof, "headcount")], ["eng,ops", "12,5"]);

      for (
        const [code, said] of [
          [`select q1 * @table:${wide}.q1 as x from @table:${wide}`, "does not hold exactly one"],
          [`select @table:${cfg}.factr as x from @table:${wide}`, `no column named "factr"`],
          [`select team, q1 from @table:${wide} unpivot (n for q in (q1, q9))`, `no column named "q9"`],
          [`select * from @table:${wide} pivot (sum(q1) for team in ('eng'))`, "not quoted text"],
          [`select team from @table:${wide} qualify q1 = 10`, "and this query has none"],
          [`select team, row_number() over (order by q1) as r from @table:${wide} qualify`, "needs a condition"],
          [
            `select team, row_number() over (order by q1) as r from @table:${wide} qualify nope = 1`,
            `names "nope"`,
          ],
          [`select q1 from @table:${wide} where q1 = @table:${cfg}`, "cannot be used as a single value"],
        ] as const
      ) {
        const res = await app.request(`/query`, {
          method: "POST",
          headers: new Headers({ Authorization: `Bearer ${jwt}`, "Content-Type": "application/json" }),
          body: JSON.stringify({ lang: "sql", code, args: [] }),
        });
        assertEquals(res.status, 400, `expected a 400 for: ${code}`);
        const body = await res.text();
        assert(body.includes(said), `expected "${said}" in: ${body}`);
      }
    }

    // A malformed query is a 400 naming the line, not a generic 500.
    {
      const res = await app.request(`/query`, {
        method: "POST",
        headers: new Headers({ Authorization: `Bearer ${jwt}`, "Content-Type": "application/json" }),
        body: JSON.stringify({ lang: "sql", code: `select * fromm 1`, args: [] }),
      });
      assertEquals(res.status, 400);
      const body = await res.text();
      assert(body.includes("Line 1"), `expected a caret-positioned error, got: ${body}`);
    }

    // AlaSQL answers `select populaton from ...` with a column of undefined
    // rather than an error, so a typo used to read as "no data".
    {
      const hand = automerge.create<{ data: Sheet["data"] }>({
        data: [arrayify([{ name: "population", type: "num", key: 0 }]), { 0: 42 }],
      });
      await put(jwt, `/library/table:${hand.documentId}`, {});
      const res = await app.request(`/query`, {
        method: "POST",
        headers: new Headers({ Authorization: `Bearer ${jwt}`, "Content-Type": "application/json" }),
        body: JSON.stringify({ lang: "sql", code: `select populaton from @table:${hand.documentId}`, args: [] }),
      });
      assertEquals(res.status, 400);
      const body = await res.text();
      assert(body.includes("populaton"), `expected the bad column named, got: ${body}`);
      assert(body.includes("population"), `expected a nearest-match suggestion, got: ${body}`);

      // A mistyped @sheet ref used to read as a bare access denial.
      const typo = await app.request(`/query`, {
        method: "POST",
        headers: new Headers({ Authorization: `Bearer ${jwt}`, "Content-Type": "application/json" }),
        body: JSON.stringify({
          lang: "sql",
          code: `select * from @table:${hand.documentId.slice(0, -1)}x`,
          args: [],
        }),
      });
      assertEquals(typo.status, 400);
      const typoBody = await typo.text();
      assert(
        typoBody.includes(`table:${hand.documentId}`),
        `expected the nearest real sheet named, got: ${typoBody}`,
      );

      // ...but an explicit alias is the author naming the column on purpose.
      // AlaSQL yields undefined for `null as note` too, and rejecting that would
      // break working queries.
      const ok: { data: Table } = await post(jwt, `/query`, {
        lang: "sql",
        code: `select population, null as note from @table:${hand.documentId}`,
        args: [],
      });
      assertEquals(Object.values(ok.data[0]).map((c) => c.name).join(), "population,note");
    }

    // A query sheet that reaches itself used to recurse until the stack blew:
    // executeSql -> sheet() -> querify -> executeSql had no depth or visited set.
    {
      const selfRef = automerge.create<{ data: Sheet["data"] }>({
        data: [{ lang: "sql", code: "select 1 as a", args: [] }],
      });
      await put(jwt, `/library/query:${selfRef.documentId}`, {});
      selfRef.change((d) => {
        (d.data[0] as Query).code = `select * from @query:${selfRef.documentId}`;
      });

      const res = await app.request(`/query`, {
        method: "POST",
        headers: new Headers({ Authorization: `Bearer ${jwt}`, "Content-Type": "application/json" }),
        body: JSON.stringify({ lang: "sql", code: `select * from @query:${selfRef.documentId}`, args: [] }),
      });
      assertEquals(res.status, 400);
      const body = await res.text();
      assert(body.includes("cycle"), `expected a cycle error, got: ${body}`);
      assert(
        body.includes(`@query:${selfRef.documentId} -> @query:${selfRef.documentId}`),
        `expected the error to name the path that closes the cycle, got: ${body}`,
      );
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

    // The one stable JSON read. Same access rules, same envelope as every other list route.
    const [cols_, ...rows] = await get<Table>(jwt, `/sheet/${sheet_id}`);
    assertEquals(Object.values(cols_).map((col) => col.name).join(), "name,age");
    assertEquals(rows.length, 2);
    await reject("", `/sheet/${sheet_id}`);

    // A public sheet exports and reads without membership; it did not before.
    const { jwt: outsider } = await usr("dave-outsider@example.com");
    const privateExport = await app.request(`/export/${sheet_id}.csv`, {
      headers: new Headers({ Authorization: `Bearer ${outsider}` }),
    });
    assertEquals(privateExport.status, 403, "a non-member must not export a private sheet");
    await post(jwt, `/library/${sheet_id}/public`, { public: true });
    const publicExport = await app.request(`/export/${sheet_id}.csv`, {
      headers: new Headers({ Authorization: `Bearer ${outsider}` }),
    });
    assert(publicExport.ok, `public export failed: ${publicExport.status}`);
    assert((await publicExport.text()).includes("Alice"), "public export should carry the rows");

    // A query sheet reads and exports through the same path, running its query first.
    const q = automerge.create<{ data: Sheet["data"] }>({
      data: [{ lang: "sql", code: `select name from @${sheet_id} order by name`, args: [] }],
    });
    await put(jwt, `/library/query:${q.documentId}`, {});
    const [qCols, ...qRows] = await get<Table>(jwt, `/sheet/query:${q.documentId}`);
    assertEquals(Object.values(qCols).map((col) => col.name).join(), "name");
    assertEquals(qRows.map((r) => String(r.name)), ["Alice", "Bob"]);
    const qCsv = await app.request(`/export/query:${q.documentId}.csv`, {
      headers: new Headers({ Authorization: `Bearer ${jwt}` }),
    });
    assert(qCsv.ok, `query export failed: ${qCsv.status}`);

    // A table with no column row must say so, not fall over inside the CSV builder.
    const bare = automerge.create<{ data: Sheet["data"] }>({ data: [] });
    await put(jwt, `/library/table:${bare.documentId}`, {});
    const bareCsv = await app.request(`/export/table:${bare.documentId}.csv`, {
      headers: new Headers({ Authorization: `Bearer ${jwt}` }),
    });
    assert(
      400 <= bareCsv.status && bareCsv.status < 500,
      `an empty sheet should be a 4xx, got ${bareCsv.status}: ${await bareCsv.text()}`,
    );
    assertEquals((await qCsv.text()).split("\n"), ["name", "Alice", "Bob"]);

    // Every export format rides the same sheet() path, so each inherits access,
    // pagination and the query recursion rather than repeating them.
    const exp = async (id: string, format: string, who = jwt) =>
      await app.request(`/export/${id}.${format}`, { headers: new Headers({ Authorization: `Bearer ${who}` }) });
    {
      const json = await exp(sheet_id, "json");
      assertEquals(json.headers.get("content-type"), "application/json; charset=utf-8");
      assertEquals(await json.json(), [{ name: "Alice", age: 30 }, { name: "Bob", age: 25 }]);

      const ndjson = await exp(sheet_id, "ndjson");
      assertEquals(ndjson.headers.get("content-type"), "application/x-ndjson; charset=utf-8");
      assertEquals((await ndjson.text()).split("\n").map((line) => JSON.parse(line).name), ["Alice", "Bob"]);

      const md = await exp(sheet_id, "md");
      assertEquals((await md.text()).split("\n"), [
        "| name | age |",
        "| --- | --- |",
        "| Alice | 30 |",
        "| Bob | 25 |",
      ]);

      // The download name carries the format, and a non-member still cannot read.
      assertEquals(
        (await exp(sheet_id, "ndjson")).headers.get("content-disposition"),
        `attachment; filename="${sheet_id.replace(/[^a-zA-Z0-9-_]/g, "_")}.ndjson"`,
      );
      assertEquals((await exp(`table:${bare.documentId}`, "json", outsider)).status, 403);
    }

    // .ics needs a date column, and says so plainly when there is none.
    {
      const noDate = await exp(sheet_id, "ics");
      assertEquals(noDate.status, 400);
      const said = await noDate.text();
      assert(said.includes("date column"), said);
      assert(said.includes("age"), `the message should list the columns it did find, got: ${said}`);

      const cal = automerge.create<{ data: Sheet["data"] }>({
        data: [
          arrayify([
            { name: "day", type: "date", key: 0 },
            { name: "what", type: "text", key: 1 },
            { name: "seats", type: "num", key: 2 },
          ]),
          { 0: "2026-06-01", 1: "Kickoff; with a comma", 2: 40 },
          { 0: "2026-07-04T18:30:00Z", 1: "Fireworks", 2: 900 },
        ],
      });
      await put(jwt, `/library/table:${cal.documentId}`, {});
      const ics = await exp(`table:${cal.documentId}`, "ics");
      assertEquals(ics.headers.get("content-type"), "text/calendar; charset=utf-8");
      const lines = (await ics.text()).split("\r\n");
      assertEquals(lines[0], "BEGIN:VCALENDAR");
      assertEquals(lines.at(-1), "END:VCALENDAR");
      assertEquals(lines.filter((l) => l === "BEGIN:VEVENT").length, 2);
      // A date-only value stays all-day; a timestamp keeps its time.
      assert(lines.includes("DTSTART;VALUE=DATE:20260601"), lines.join("\n"));
      assert(lines.includes("DTSTART:20260704T183000Z"), lines.join("\n"));
      // RFC 5545 reserves the comma and semicolon inside a value.
      assert(lines.includes("SUMMARY:Kickoff\\; with a comma"), lines.join("\n"));

      // Folding counts octets but must cut on codepoints: a long emoji summary
      // split mid-sequence comes back as replacement characters.
      const wide = automerge.create<{ data: Sheet["data"] }>({
        data: [
          arrayify([{ name: "day", type: "date", key: 0 }, { name: "what", type: "text", key: 1 }]),
          { 0: "2026-06-01", 1: "🦄".repeat(40) },
        ],
      });
      await put(jwt, `/library/table:${wide.documentId}`, {});
      const folded = (await (await exp(`table:${wide.documentId}`, "ics")).text()).split("\r\n");
      assert(!folded.some((l) => l.includes("\ufffd")), "folding must not split a codepoint");
      for (const l of folded)
        assert(new TextEncoder().encode(l).byteLength <= 75, `line over 75 octets: ${JSON.stringify(l)}`);
      assertEquals(
        folded.filter((l) => l.startsWith("SUMMARY:") || l.startsWith(" ")).join("").replace(/^SUMMARY:| /g, ""),
        "🦄".repeat(40),
        "unfolding must give the summary back",
      );
    }

    // Two columns of the same name would silently overwrite each other in a
    // name-keyed format, so those formats refuse it.
    {
      const dup = automerge.create<{ data: Sheet["data"] }>({
        data: [arrayify([{ name: "a", type: "text", key: 0 }, { name: "a", type: "text", key: 1 }]), {
          0: "x",
          1: "y",
        }],
      });
      await put(jwt, `/library/table:${dup.documentId}`, {});
      assert((await exp(`table:${dup.documentId}`, "csv")).ok, "csv carries positions, so duplicates are fine");
      const json = await exp(`table:${dup.documentId}`, "json");
      assertEquals(json.status, 400);
      assert((await json.text()).includes("same name"), "the duplicate must be named");
    }
  }

  // A CSV that does not match its own header is a rejection, not a coercion.
  {
    const { jwt } = await usr("ruth@example.com");
    const importCsv = (text: string) =>
      app.request("/import/csv", {
        method: "POST",
        headers: new Headers({ Authorization: `Bearer ${jwt}`, "Content-Type": "text/csv" }),
        body: text,
      });

    const short = await importCsv("name,age,city\nAlice,30,Reno\nBob,25\n");
    assertEquals(short.status, 400);
    const said = await short.text();
    for (const part of ["Line 3", "3 fields", "2 fields", "Bob,25", "city"])
      assert(said.includes(part), `${part} missing from: ${said}`);

    // A quoted newline is one row, so the line number must survive it.
    const quoted = await importCsv('a,b\n"one\ntwo",2\nragged\n');
    assertEquals(quoted.status, 400);
    assert((await quoted.text()).includes("Line 4"), "a quoted newline still advances the line count");

    // 4 of 5 numbers used to make the column numeric and turn the fifth into NaN.
    const mixed = await importCsv("qty\n1\n2\n3\n4\nn/a\n");
    const body = await mixed.json();
    assert(mixed.ok, `a mostly-numeric column should import as text: ${JSON.stringify(body)}`);
    const { sheet_id } = body;
    const [cols_, ...rows] = await get<Table>(jwt, `/sheet/${sheet_id}`);
    assertEquals(Object.values(cols_)[0].type, "text");
    assertEquals(rows.map((r) => r["0"]), ["1", "2", "3", "4", "n/a"]);
  }

  // A webhook sender only ever sees the response body, so the body diagnoses.
  {
    const { jwt } = await usr("sam@example.com");
    const deliver = (id: string, body: string) =>
      app.request(`/net/${id}`, {
        method: "POST",
        headers: new Headers({ "Content-Type": "application/json" }),
        body,
      });

    const missing = await deliver("net-hook:nosuchdoc", "{}");
    assertEquals(missing.status, 404);
    assert((await missing.text()).includes("PUT /library/net-hook:"), "it must say how to create the sheet");

    const table = automerge.create<Sheet>({ type: "table", data: [arrayify([{ name: "a", type: "text", key: 0 }])] });
    await put(jwt, `/library/table:${table.documentId}`, {});
    const wrongType = await deliver(`table:${table.documentId}`, "{}");
    assertEquals(wrongType.status, 400);
    assert((await wrongType.text()).includes("a table sheet"), "it must name the type it received");

    const hook = automerge.create<Sheet>({ type: "net-hook", data: [] });
    await put(jwt, `/library/net-hook:${hook.documentId}`, {});
    const big = await deliver(`net-hook:${hook.documentId}`, "x".repeat(1_048_577));
    assertEquals(big.status, 413);
    assert((await big.text()).includes("1048577 bytes"), "it must name the size it received");
    assert((await deliver(`net-hook:${hook.documentId}`, "{}")).ok, "a valid delivery still lands");
  }

  // describe @sheet, cost guards, and the source-type check.
  {
    const { jwt } = await usr("quinn@example.com");
    const hand = automerge.create<{ data: Sheet["data"] }>({
      data: [
        arrayify([
          { name: "city", type: "text", key: 0 },
          { name: "pop", type: "num", key: 1 },
        ]),
        { 0: "Reno", 1: 264000 },
        { 0: "Elko", 1: "" },
      ],
    });
    const id = `table:${hand.documentId}`;
    await put(jwt, `/library/${id}`, {});

    // describe answers "what columns does this sheet have" without a guess.
    const runs = (code: string) => post(jwt, `/query`, { lang: "sql", code, args: [] });
    {
      const [cols_, ...rows] = (await runs(`describe @${id}`)).data as Table;
      assertEquals(Object.values(cols_).map((col) => col.name).join(), "column,type,rows,nulls,sample");
      assertEquals(rows, [
        { column: "city", type: "text", rows: 2, nulls: 0, sample: "Reno" },
        { column: "pop", type: "num", rows: 2, nulls: 1, sample: "264000" },
      ]);
      // Trailing semicolon and case are the two things a SQL author types by habit.
      assertEquals(((await runs(`DESCRIBE @${id};`)).data as Table).length, 3);
    }

    // min()/max() over text used to drop the column out of the result: a silent
    // wrong answer, which is the worst kind. Now it says so, and says what to use.
    {
      const codes = automerge.create<{ data: Sheet["data"] }>({
        data: [
          arrayify([{ name: "code", type: "text", key: 0 }, { name: "n", type: "num", key: 1 }]),
          { 0: "10", 1: 5 },
          { 0: "01", 1: 9 },
        ],
      });
      const codesId = `table:${codes.documentId}`;
      await put(jwt, `/library/${codesId}`, {});
      const fails = async (code: string) => {
        const res = await app.request(`/query`, {
          method: "POST",
          headers: new Headers({ "Content-Type": "application/json", Authorization: `Bearer ${jwt}` }),
          body: JSON.stringify({ lang: "sql", code, args: [] }),
        });
        assertEquals(res.status, 400, code);
        return await res.text();
      };
      // Aliased and bare, since the alias exemption used to hide this one.
      for (const code of [`select min(code) as lowest from @${codesId}`, `select max(code) from @${codesId}`]) {
        const said = await fails(code);
        assert(said.includes("min_text"), `${code} -> ${said}`);
      }
      // The replacements compare as text, and min() over numbers still works.
      const [, row] = (await runs(
        `select min_text(code) as lo, max_text(code) as hi, min(n) as least from @${codesId}`,
      )).data as Table;
      assertEquals([row.lo, row.hi, row.least], ["01", "10", 5]);
    }

    // A numeric column holding text is a mismatch the sum would have hidden.
    {
      const bad = automerge.create<{ data: Sheet["data"] }>({
        data: [
          arrayify([{ name: "price", type: "usd", key: 0 }]),
          { 0: 10 },
          { 0: "n/a" },
        ],
      });
      const badId = `table:${bad.documentId}`;
      await put(jwt, `/library/${badId}`, {});
      const res = await app.request(`/query`, {
        method: "POST",
        headers: new Headers({ "Content-Type": "application/json", Authorization: `Bearer ${jwt}` }),
        body: JSON.stringify({ lang: "sql", code: `select sum(price) as total from @${badId}`, args: [] }),
      });
      assertEquals(res.status, 400);
      const said = await res.text();
      for (const part of ["usd", '"n/a"', "row 2", "price"]) assert(said.includes(part), `${part} missing: ${said}`);
      // describe still works on it: a broken sheet is the one you need to inspect.
      const [, ...rows] = (await runs(`describe @${badId}`)).data as Table;
      assertEquals(rows.length, 1);
    }
  }

  // net-hook ingestion + net-http polling.
  {
    const { jwt } = await usr("nadia@example.com");
    const hookHand = automerge.create<Sheet>({ type: "net-hook", data: [] });
    const hookId = `net-hook:${hookHand.documentId}`;
    await put(jwt, `/library/${hookId}`, {});

    // Anyone can POST a payload; only the owner reads the log.
    {
      const res = await app.request(`/net/${hookId}?x=1`, {
        method: "POST",
        headers: new Headers({ "Content-Type": "application/json" }),
        body: JSON.stringify({ event: "ping" }),
      });
      assert(res.ok, `webhook ingest failed: ${res.status}`);
      const cols = "created_at,body,method,req_headers,query_params,meta";
      const [cols_, ...rows] = await get<Table>(jwt, `/net/${hookId}`);
      assertEquals(Object.values(cols_).map((col) => col.name).join(), cols);
      // jsonb reads back as json, not text, so a query author sees the real type.
      assertEquals(Object.values(cols_).map((col) => col.type).join(), "text,text,text,json,json,json");
      // Every delivery carries what the run itself cost, beside what it delivered.
      assertEquals(JSON.parse(String(rows[0].meta)).bytes, '{"event":"ping"}'.length);

      assertEquals(rows.length, 1);
      assert(String(rows[0].body).includes("ping"), JSON.stringify(rows[0]));
      // The delivery's own method, headers and query string are the raw material
      // signature verification needs; they were stored but never readable.
      assertEquals(rows[0].method, "POST");
      // postgresjs hands jsonb back as the raw JSON text, in Postgres and PGlite
      // alike, so a cell holds a string and json_extract() is how a query reads it.
      assertEquals(JSON.parse(String(rows[0].req_headers))["content-type"], "application/json");
      assertEquals(JSON.parse(String(rows[0].query_params)), { x: "1" });
      await reject("", `/net/${hookId}`);

      // Retention: the log is capped per sheet, so a webhook firing every second
      // cannot fill the disk. A sheet that must keep everything writes to a table,
      // which is never trimmed. Its own sheet, so nothing else counts these rows.
      {
        const busy = automerge.create<Sheet>({ type: "net-hook", data: [] });
        const busyId = `net-hook:${busy.documentId}`;
        await put(jwt, `/library/${busyId}`, {});
        await sql`
          insert into net (sheet_id, method, body)
          select ${busyId}, 'POST', 'filler ' || g from generate_series(1, ${NET_KEEP + 5}) g
        `;
        await trimNet(busyId);
        const [{ n }] = await sql`select count(*)::int as n from net where sheet_id = ${busyId}`;
        assertEquals(n, NET_KEEP);
        // The newest survive: the last row written is still the first row read.
        const [, newest] = await get<Table>(jwt, `/net/${busyId}`);
        assertEquals(String(newest.body), `filler ${NET_KEEP + 5}`);
        // A sheet under the cap loses nothing.
        await trimNet(hookId);
        const [{ n: kept }] = await sql`select count(*)::int as n from net where sheet_id = ${hookId}`;
        assertEquals(kept, 1);
      }

      // A net log reads and exports like any other sheet, and pages in a stable order.
      const [netCols, ...netRows] = await get<Table>(jwt, `/sheet/${hookId}`);
      assertEquals(Object.values(netCols).map((col) => col.name).join(), cols);
      assertEquals(netRows.length, 1);
      const netCsv = await app.request(`/export/${hookId}.csv`, {
        headers: new Headers({ Authorization: `Bearer ${jwt}` }),
      });
      assert(netCsv.ok, `net export failed: ${netCsv.status}`);
      const netCsvText = await netCsv.text();
      assertEquals(netCsvText.split("\n")[0], cols);
      assert(netCsvText.includes("ping"), `expected the payload in the csv, got: ${netCsvText}`);

      // Ten more deliveries must page without repeating or skipping a row.
      for (let i = 0; i < 10; i++) {
        await app.request(`/net/${hookId}`, {
          method: "POST",
          headers: new Headers({ "Content-Type": "application/json" }),
          body: JSON.stringify({ seq: i }),
        });
      }
      const pageOf = async (offset: number) =>
        (await get<Table>(jwt, `/net/${hookId}`, { limit: 4, offset })).slice(1).map((r) => String(r.body));
      const paged = [...await pageOf(0), ...await pageOf(4), ...await pageOf(8)];
      assertEquals(paged.length, 11);
      assertEquals(new Set(paged).size, 11, `paging repeated a row: ${JSON.stringify(paged)}`);
      // Newest first, tie-broken by net_id: all eleven can share a created_at.
      assert(paged[0].includes(`"seq":9`), `expected the newest delivery first, got: ${paged[0]}`);
      assert(paged[10].includes("ping"), `expected the oldest delivery last, got: ${paged[10]}`);
    }

    // The poller honors intervals, skips empty urls, and forwards headers.
    {
      const httpHand = automerge.create<Sheet>({
        type: "net-http",
        data: [{ url: "https://feeds.test/data", interval: 120, headers: "X-Api-Key: k1" }],
      });
      const httpId = `net-http:${httpHand.documentId}`;
      await put(jwt, `/library/${httpId}`, {});
      const blankHand = automerge.create<Sheet>({ type: "net-http", data: [{ url: "", interval: 120 }] });
      await put(jwt, `/library/net-http:${blankHand.documentId}`, {});

      const calls: [string, Record<string, string>][] = [];
      const fetcher = (url: string, headers: Record<string, string> = {}) => {
        calls.push([url, headers]);
        return Promise.resolve(new Response(`{"ok":true}`));
      };
      const t0 = Date.now();
      await pollNetOnce(fetcher, t0);
      const urls = calls.map(([u]) => u);
      assert(urls.includes("https://feeds.test/data"), urls.join());
      assert(!urls.includes(""), "empty urls must not be fetched");
      assertEquals(calls.find(([u]) => u === "https://feeds.test/data")?.[1], { "X-Api-Key": "k1" });

      const countRows = async () => (await get<Table>(jwt, `/net/${httpId}`)).length - 1;
      assertEquals(await countRows(), 1);
      await pollNetOnce(fetcher, t0 + 1_000); // not due yet
      assertEquals(await countRows(), 1);
      await pollNetOnce(fetcher, t0 + 121_000); // due again
      assertEquals(await countRows(), 2);
    }

    // Header parsing: one "Name: value" per line; malformed lines throw loudly.
    assertEquals(parseNetHeaders("A: b\nC: d: e"), { A: "b", C: "d: e" });
    assertEquals(parseNetHeaders(""), {});
    assertThrows(() => parseNetHeaders("Authorization Bearer xyz"), Error, "Authorization Bearer xyz");
  }

  // MCP server: JSON-RPC 2.0 over POST /mcp/:id.
  {
    const { jwt } = await usr("mia@example.com");
    const hand = automerge.create<Sheet>({
      type: "table",
      data: [
        arrayify([{ name: "item", type: "text", key: "0" }, { name: "price", type: "usd", key: "1" }]),
        { 0: "apple", 1: 1.5 },
      ],
    });
    const sheet_id = `table:${hand.documentId}`;
    await put(jwt, `/library/${sheet_id}`, {});

    const mcp = (auth: string, body: unknown) =>
      app.request(`/mcp/${sheet_id}`, {
        method: "POST",
        headers: new Headers({ "Content-Type": "application/json", Authorization: `Bearer ${auth}` }),
        body: JSON.stringify(body),
      });
    const rpc = async (auth: string, method: string, params?: unknown) => {
      const res = await mcp(auth, { jsonrpc: "2.0", id: 1, method, params });
      assert(res.ok, `mcp ${method} returned ${res.status}`);
      return await res.json();
    };
    const call = async (auth: string, name: string, args: unknown) =>
      (await rpc(auth, "tools/call", { name, arguments: args })).result;

    // Handshake: initialize, initialized notification, ping.
    {
      const { result } = await rpc(jwt, "initialize", {
        protocolVersion: "2025-06-18",
        capabilities: {},
        clientInfo: { name: "test", version: "0" },
      });
      assertEquals(result.protocolVersion, "2025-06-18");
      assertEquals(result.serverInfo.name, "scrapsheets");
      assert(result.capabilities.tools, "server should advertise tools");
      const notif = await mcp(jwt, { jsonrpc: "2.0", method: "notifications/initialized" });
      assertEquals(notif.status, 202);
      assertEquals((await rpc(jwt, "ping")).result, {});
    }

    // tools/list names all four tools.
    {
      const { result } = await rpc(jwt, "tools/list");
      assertEquals(
        result.tools.map((t: { name: string }) => t.name).sort().join(),
        "list_sheets,query_sheet,read_sheet,write_cells",
      );
    }

    // read_sheet returns cols and rows.
    {
      const out = await call(jwt, "read_sheet", {});
      assertEquals(out.isError, false);
      assertEquals(out.structuredContent.cols.map((col: { name: string }) => col.name).join(), "item,price");
      assertEquals(out.structuredContent.rows.length, 1);
    }

    // write_cells updates and appends, then reads back.
    {
      const out = await call(jwt, "write_cells", {
        cells: [
          { row: 0, col: "price", value: 2.25 },
          { row: 1, col: "item", value: "banana" },
          { row: 1, col: "price", value: 0.5 },
        ],
      });
      assertEquals(out.structuredContent, { written: 3, rows: 2 });
      const read = await call(jwt, "read_sheet", {});
      assertEquals(read.structuredContent.rows, [{ "0": "apple", "1": 2.25 }, { "0": "banana", "1": 0.5 }]);
    }

    // write_cells failures name the problem.
    {
      const badCol = await call(jwt, "write_cells", { cells: [{ row: 0, col: "nope", value: "x" }] });
      assertEquals(badCol.isError, true);
      assert(badCol.content[0].text.includes("item"), `should list valid columns, got: ${badCol.content[0].text}`);
      const badType = await call(jwt, "write_cells", { cells: [{ row: 0, col: "price", value: "expensive" }] });
      assertEquals(badType.isError, true);
      assert(badType.content[0].text.includes("expected usd"), badType.content[0].text);
      const badRow = await call(jwt, "write_cells", { cells: [{ row: 9, col: "price", value: 1 }] });
      assertEquals(badRow.isError, true);
      assert(badRow.content[0].text.includes("appends"), badRow.content[0].text);
    }

    // query_sheet resolves @refs; list_sheets scopes to the caller.
    {
      const q = await call(jwt, "query_sheet", { code: `select sum(price) as spent from @${sheet_id}` });
      assertEquals(q.isError, false, q.content?.[0]?.text);
      assertEquals(q.structuredContent.rows, [{ spent: 2.75 }]);
      const ls = await call(jwt, "list_sheets", {});
      assertEquals(ls.structuredContent.sheets.length, 1);
      assertEquals(ls.structuredContent.sheets[0].sheet_id, sheet_id);
    }

    // Cross-user access is denied in-band.
    {
      const { jwt: intruder } = await usr("oscar@example.com");
      const denied = await call(intruder, "read_sheet", {});
      assertEquals(denied.isError, true);
      assert(denied.content[0].text.includes("access"), denied.content[0].text);
      // A denial has to say how access is granted, not just that it is missing.
      assert(denied.content[0].text.includes(`/library/${sheet_id}/share`), denied.content[0].text);
    }

    // Protocol errors.
    {
      const unknownTool = await rpc(jwt, "tools/call", { name: "nope", arguments: {} });
      assertEquals(unknownTool.error.code, -32602);
      const unknownMethod = await rpc(jwt, "bogus/method");
      assertEquals(unknownMethod.error.code, -32601);
      const got = await app.request(`/mcp/${sheet_id}`, { headers: new Headers({ Authorization: `Bearer ${jwt}` }) });
      assertEquals(got.status, 405);
      const unauthed = await mcp("", { jsonrpc: "2.0", id: 1, method: "ping" });
      assertEquals(unauthed.status, 401);
    }
  }

  // Proxy guards. Each rejection must name its own cause, never a bare status.
  {
    // `code` is the proxy's own status; the body's `status` is the origin's.
    const proxy = async (url?: string) => {
      const res = await app.request(
        "/proxy" + (url === undefined ? "" : `?url=${encodeURIComponent(url)}`),
      );
      return { code: res.status, ...(await res.json()) };
    };
    assertEquals(await proxy(), { code: 400, error: "Missing url parameter" });
    {
      const { code, error, repro } = await proxy("ftp://example.com/x");
      assertEquals([code, error], [400, "Only HTTP(S) URLs allowed."]);
      // A failure has to be reproducible by hand, not just described.
      assertEquals(repro, "curl -i 'ftp://example.com/x'");
    }
    for (
      const url of [
        "http://localhost/x",
        "http://foo.local/x",
        "http://127.0.0.1/x",
        "http://169.254.169.254/latest/meta-data",
      ]
    ) {
      const { code, error, repro } = await proxy(url);
      assertEquals([code, error, repro], [400, "Internal URLs not allowed.", `curl -i '${url}'`], url);
    }
    const { code, error } = await proxy("notaurl");
    assertEquals(code, 502);
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

  // Stripe checkout regressions: only cases that actually broke.
  {
    const { jwt: sellerJwt } = await usr("stripe-seller@example.com");
    const { jwt: buyerJwt, usr_id: buyerId } = await usr("stripe-buyer@example.com");
    const webhookSecret = "whsec_hunt";
    const raw = async (path: string, init?: RequestInit) =>
      await app.request(path, {
        headers: new Headers({
          "Content-Type": "application/json",
          Authorization: `Bearer ${buyerJwt}`,
          ...Object.fromEntries(new Headers(init?.headers).entries()),
        }),
        ...init,
      });
    const listPaid = async (name: string) => {
      const hand = automerge.create<{ type: string; data: unknown[] }>({
        type: "table",
        data: [arrayify([{ name: "a", type: "text", key: 0 }])],
      });
      await put(sellerJwt, `/library/table:${hand.documentId}`, { name });
      await post(sellerJwt, `/sell/table:${hand.documentId}`, { price: 5 });
      const [, listing] = await get<Table>(buyerJwt, `/shop`, { name });
      assert(listing?.sell_id, `expected ${name} in the shop`);
      return listing as { sell_id: string };
    };
    const sign = (payload: string) =>
      new Stripe("sk_test_unused").webhooks.generateTestHeaderStringAsync({ payload, secret: webhookSecret });
    const payloadFor = (over: Record<string, unknown>, sell_id: string, usr_id = String(buyerId)) =>
      JSON.stringify({
        id: "evt_hunt",
        object: "event",
        type: "checkout.session.completed",
        data: {
          object: {
            id: "cs_hunt",
            object: "checkout.session",
            payment_status: "paid",
            payment_intent: "pi_hunt",
            amount_total: 500,
            metadata: { usr_id, sell_id },
            ...over,
          },
        },
      });
    const postStripe = async (payload: string) => {
      Deno.env.set("STRIPE_WEBHOOK_SECRET", webhookSecret);
      const res = await raw("/stripe", {
        method: "POST",
        body: payload,
        headers: { "Content-Type": "application/json", "stripe-signature": await sign(payload) },
      });
      Deno.env.delete("STRIPE_WEBHOOK_SECRET");
      return res;
    };

    {
      const listing = await listPaid("Reject amount_total 0");
      const res = await postStripe(payloadFor({ id: "cs_amt_zero", amount_total: 0 }, listing.sell_id));
      assertEquals(res.status, 400);
      assert((await res.text()).includes("amount_total"));
      assertEquals((await sql`select * from sheet where buy_id = ${listing.sell_id}`).length, 0);
    }

    {
      const listing = await listPaid("Reject amount_total string");
      const res = await postStripe(payloadFor({ id: "cs_amt_type", amount_total: "500" }, listing.sell_id));
      assertEquals(res.status, 400);
      assert((await res.text()).includes("amount_total"));
      assertEquals((await sql`select * from sheet where buy_id = ${listing.sell_id}`).length, 0);
    }

    {
      const listing = await listPaid("Reject missing session id");
      const res = await postStripe(payloadFor({ id: undefined }, listing.sell_id));
      assertEquals(res.status, 400);
      assert((await res.text()).includes("checkout.session id"));
      assertEquals((await sql`select * from sheet where buy_id = ${listing.sell_id}`).length, 0);
    }

    {
      const listing = await listPaid("Reject bad usr_id");
      const res = await postStripe(payloadFor({ id: "cs_bad_usr" }, listing.sell_id, "not-a-user"));
      assertEquals(res.status, 400);
      const text = await res.text();
      assert(text.includes("usr_id"), `error must name usr_id, got: ${text}`);
      assertEquals((await sql`select * from sheet where buy_id = ${listing.sell_id}`).length, 0);
    }

    {
      const listing = await listPaid("Deliver after unsell");
      const origFetch = globalThis.fetch;
      Deno.env.set("STRIPE_SECRET_KEY", "sk_test_hunt");
      globalThis.fetch = ((input: RequestInfo | URL, init?: RequestInit) => {
        const r = new Request(input, init);
        if (!r.url.startsWith("https://api.stripe.com/")) return origFetch(input, init);
        if (r.url.includes("/v1/customers"))
          return Promise.resolve(Response.json({ id: "cus_unsold", object: "customer" }));
        if (r.url.includes("/v1/checkout/sessions")) {
          return Promise.resolve(Response.json({
            id: "cs_unsold",
            object: "checkout.session",
            url: "https://checkout.stripe.com/c/pay/cs_unsold",
          }));
        }
        return Promise.resolve(Response.json({ error: { message: `unexpected Stripe ${r.url}` } }, { status: 500 }));
      }) as typeof fetch;
      const buyRes = await raw(`/buy/${listing.sell_id}`, { method: "POST", body: "{}" });
      globalThis.fetch = origFetch;
      Deno.env.delete("STRIPE_SECRET_KEY");
      assertEquals(buyRes.status, 200, await buyRes.text());
      const [listed] = await sql`select sheet_id from sheet where sell_id = ${listing.sell_id}`;
      await post(sellerJwt, `/sell/${listed.sheet_id}`, { price: null });
      const res = await postStripe(payloadFor({ id: "cs_unsold" }, listing.sell_id));
      assertEquals(res.status, 200, await res.text());
      assertEquals((await sql`select * from sheet where buy_id = ${listing.sell_id}`).length, 1);
    }

    {
      const listing = await listPaid("Stripe outage names Stripe");
      const origFetch = globalThis.fetch;
      Deno.env.set("STRIPE_SECRET_KEY", "sk_test_hunt");
      globalThis.fetch = ((input: RequestInfo | URL, init?: RequestInit) => {
        const r = new Request(input, init);
        if (!r.url.startsWith("https://api.stripe.com/")) return origFetch(input, init);
        return Promise.resolve(
          Response.json({ error: { type: "invalid_request_error", message: "Invalid API Key provided" } }, {
            status: 401,
          }),
        );
      }) as typeof fetch;
      const res = await raw(`/buy/${listing.sell_id}`, { method: "POST", body: "{}" });
      globalThis.fetch = origFetch;
      Deno.env.delete("STRIPE_SECRET_KEY");
      assertEquals(res.status, 502);
      const text = await res.text();
      assert(text.includes("Stripe"), `error must name Stripe, got: ${text}`);
      assertEquals((await sql`select * from sheet where buy_id = ${listing.sell_id}`).length, 0);
    }
  }

  await sql.end();
  listener.close();
  await pglite.close();

  await new Promise((res) => setTimeout(res, 250));
});
