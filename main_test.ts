import { assert, assertEquals, assertThrows } from "@std/assert";
import { PGlite } from "@electric-sql/pglite";
import { PostgresConnection } from "pg-gateway";
import { citext } from "@electric-sql/pglite/contrib/citext";
import * as AM from "@automerge/automerge-repo";
import { decodeSyncMessage } from "@automerge/automerge";
import { WebSocketClientAdapter } from "@automerge/automerge-repo-network-websocket";
import Stripe from "stripe";
import {
  app,
  arrayify,
  automerge,
  BODY_CAP,
  callerIp,
  createJwt,
  createToken,
  flushFolds,
  HOOK_BYTES_PER_WINDOW,
  HOOK_ROWS_PER_WINDOW,
  HOOK_WINDOW_S,
  hookBucket,
  hookBuckets,
  hookSecret,
  hookSign,
  errorLogged,
  flushWebhooks,
  HOST_GAP_MS,
  WEBHOOK_FAILS_MAX,
  webhookTimer,
  WEBHOOKS_PER_SHEET_MAX,
  accountBuckets,
  APPEND_ROWS_MAX,
  USER_EMAILS_PER_DAY,
  USER_SHEETS_MAX,
  LICENSES,
  safeFetch,
  USER_AGENT,
  hostDue,
  LOG_EVERY_MS,
  NET_KEEP,
  netDue,
  parseNetHeaders,
  pollAlertOnce,
  pollNetOnce,
  RATE_LIMIT_KEYS_MAX,
  requireSecret,
  seed,
  sendDigestOnce,
  sql,
  status,
  trimNet,
} from "./main.ts";
import type { Col, Query, Sheet, Table, Template } from "./main.ts";
import { DATASETS } from "./src/examples.mjs";
import { MAX_QUERY_ROWS } from "./src/sql.mjs";
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

// Every delivery is signed, so the tests sign it the way a sender does: through
// the server's own hookSign, rather than a second implementation that could
// agree with a bug in the first. The target is signed alongside the body, so it
// is passed in rather than assumed.
const deliver = async (sheet_id: string, body: string, query = "") =>
  await app.request(`/net/${sheet_id}${query}`, {
    method: "POST",
    headers: new Headers({
      "Content-Type": "application/json",
      "scrapsheets-signature": await hookSign(await hookSecret(sheet_id), `/net/${sheet_id}${query}`, body),
    }),
    body,
  });

const usr = async (email: string) => {
  const [{ usr_id }] = await sql`
    insert into usr (email) values (${email})
    on conflict (email) do update set email = excluded.email
    returning *
  `;
  return { usr_id, jwt: await createJwt(usr_id) };
};

Deno.test(async function allTests(t) {
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
  // seed() grants the error log to this address. It is set before seed() runs
  // because that is the only moment the grant is written.
  Deno.env.set("OPERATOR_EMAIL", "erin@example.com");
  await seed();

  // That this suite loaded main.ts at all is the other half of the proof -- `deno task test` is the one place the
  // three are set.
  await t.step("A secret-less boot is a crash, not a warning", async () => {
    assertThrows(
      () => requireSecret("SCRAPSHEETS_SECRET_THAT_IS_NEVER_SET"),
      Error,
      "SCRAPSHEETS_SECRET_THAT_IS_NEVER_SET",
    );
    assertEquals(requireSecret("JWT_SECRET"), Deno.env.get("JWT_SECRET"));
  });

  await t.step("Signup completion + login round-trip (app-level password hashing, no pgcrypto)", async () => {
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
  });

  await t.step("POST /signup must send the verification email through resend with the api key", async () => {
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
  });

  await t.step("Alice creates every sheet type, shares them, and syncs over the real socket", async () => {
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
        await post(jwt, `/sell/${sheet_id}`, { price: 0, license: "own" });
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
  });

  await t.step("Automerge sync over the real websocket route (CBOR join/peer protocol)", async () => {
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
      // The first frame `pick` takes. The server announces every document
      // this account may read as soon as the peer is in, so the answer to a
      // question is not the next frame, only the next one about it.
      const next = (what: string, pick: (frame: Record<string, unknown>) => boolean = () => true) =>
        new Promise<Record<string, unknown>>((resolve, reject) => {
          const timer = setTimeout(() => reject(new Error(`No ${what} within 2s.`)), 2000);
          ws.onmessage = (e) => {
            const frame = AM.cbor.decode(new Uint8Array(e.data as ArrayBuffer)) as Record<string, unknown>;
            if (!pick(frame)) return;
            clearTimeout(timer);
            resolve(frame);
          };
          ws.onerror = () => {
            clearTimeout(timer);
            reject(new Error(`WebSocket errored before the ${what}.`));
          };
        });
      const peer = next("peer reply");
      await new Promise<void>((resolve) => {
        ws.onopen = () => resolve();
      });
      ws.send(AM.cbor.encode({
        type: "join",
        senderId: "raw-test-peer",
        peerMetadata: {},
        supportedProtocolVersions: ["1"],
      }));
      const reply = await peer;
      assertEquals(reply.type, "peer");
      assertEquals(reply.selectedProtocolVersion, "1");

      // A document id no document can have. Postgres text cannot hold a NUL,
      // so the role lookup used to throw and the socket closed on an
      // unexplained error; a document that cannot exist is unavailable.
      const unavailable = next("doc-unavailable reply", (frame) => frame.documentId === "no\u0000such");
      ws.send(AM.cbor.encode({
        type: "request",
        senderId: "raw-test-peer",
        targetId: reply.senderId,
        documentId: "no\u0000such",
        data: new Uint8Array(),
      }));
      assertEquals((await unavailable).type, "doc-unavailable", "a NUL in a document id is a document nobody has");
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

      // ...but the server must not persist the viewer's edit. It answers the
      // edit with an error frame, which the adapter logs at a debug namespace
      // and emits nothing for; hearing it is how this knows the server has
      // decided, rather than sleeping and hoping it has.
      const before = JSON.stringify((await automerge.find<Sheet>(hand.documentId)).doc());
      const vHandle = await vRepo.find<Sheet>(hand.documentId);
      const refused = new Promise<string>((resolve) => {
        const receive = vAdapter.receiveMessage.bind(vAdapter);
        vAdapter.receiveMessage = (bytes: Uint8Array) => {
          const frame = AM.cbor.decode(new Uint8Array(bytes)) as { type: string; message?: string };
          if (frame.type === "error") resolve(frame.message ?? "");
          return receive(bytes);
        };
      });
      vHandle.change((d: Sheet) => {
        (d.data as unknown as Record<string, unknown>[])[1] = { 0: 666 };
      });
      const said = await Promise.race([
        refused,
        new Promise<string>((resolve) => setTimeout(() => resolve("timed out"), 2000)),
      ]);
      assert(said !== "timed out", "the server should answer a viewer's edit with an error frame");
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

      // An editor's write does land, and the server says so: its next sync
      // frame for the document carries the editor's new head. That frame is
      // how src/index.html learns a write it once held in this browser has
      // landed, so the protocol is pinned here, where the server's own repo
      // is in the loop. The handle's heads are base58 and the wire's are hex,
      // which is why the page decodes them before comparing.
      const eAdapter = new WebSocketClientAdapter(wsUrl(editor.jwt), 100);
      const eRepo = new AM.Repo({ network: [eAdapter], peerId: "client-editor" as AM.PeerId });
      const eHandle = await eRepo.find<Sheet>(hand.documentId);
      const heard: string[][] = [];
      const receive = eAdapter.receiveMessage.bind(eAdapter);
      eAdapter.receiveMessage = (bytes: Uint8Array) => {
        const frame = AM.cbor.decode(new Uint8Array(bytes)) as { type: string; documentId?: string; data?: Uint8Array };
        if (frame.type === "sync" && frame.documentId === hand.documentId) heard.push(decodeSyncMessage(frame.data!).heads);
        return receive(bytes);
      };
      eHandle.change((d: Sheet) => {
        (d.data as unknown as Record<string, unknown>[])[1] = { 0: 4242 };
      });
      const written = AM.decodeHeads(eHandle.heads()!);
      for (let i = 0; i < 500; i++) {
        if (heard.some((heads) => written.every((h) => heads.includes(h)))) break;
        await new Promise((r) => setTimeout(r, 10));
      }
      assert(
        JSON.stringify((await automerge.find<Sheet>(hand.documentId)).doc()).includes("4242"),
        "an editor's write should reach the server copy",
      );
      assert(
        heard.some((heads) => written.every((h) => heads.includes(h))),
        `the server's sync reply must carry the editor's head ${written}, heard: ${JSON.stringify(heard)}`,
      );
      eAdapter.disconnect();

      // The socket is where a sheet is opened and edited in the app, so it is
      // where the audit log hears it: one open per peer per document, one edit
      // the first time a peer's frame carries a change, and nothing for a
      // change the server refused.
      const trail: { email: string; action: string }[] = await sql`
        select u.email, a.action from audit a left join usr u using (usr_id)
        where a.sheet_id = ${sheet_id} order by a.audit_id
      `;
      const seen: string[] = trail.map((r) => `${r.email}:${r.action}`);
      assert(seen.includes("viewer@example.com:open"), `a viewer's open is audited: ${seen}`);
      assert(seen.includes("editor@example.com:open"), `an editor's open is audited: ${seen}`);
      assert(seen.includes("editor@example.com:edit"), `an editor's edit is audited: ${seen}`);
      assert(!seen.includes("viewer@example.com:edit"), `a refused edit is not an edit: ${seen}`);
      assertEquals(seen.filter((x) => x === "editor@example.com:edit").length, 1, "once per connection, not per frame");

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
      const [anonymous]: { via: string }[] = await sql`
        select via from audit where sheet_id = ${sheet_id} and usr_id is null order by audit_id desc limit 1
      `;
      assertEquals(anonymous?.via, "public", "an anonymous reader of a public sheet is not a share link");
      await post(jwt, `/library/${sheet_id}/public`, { public: false });
    }

    await server.shutdown();
  });

  await t.step("The shop is publicly viewable", async () => {
    const [cols_, ...rows] = await get<Table>("", `/shop`);
    const cols = Object.values(cols_);
    assert(cols.length);
    assert(rows.length);
  });

  await t.step("Bob buys from the shop, then queries across the sheets he owns", async () => {
    const { jwt } = await usr("bob@example.com");

    // Bob purchases items from the shop.
    {
      const [cols_, ...rows] = await get<Table>(jwt, `/shop`);
      const cols = Object.values(cols_);
      assert(cols.length);
      assertEquals(cols.map((col) => col.name).join(), "name,price,license,");
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
        body: JSON.stringify({ price: 5, license: "own" }),
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

      // A join walks the product of its inputs, and the engine cannot be
      // stopped once it starts: a five-way self-join of this sheet used to
      // kill the process with a native out-of-memory, every request with it.
      // Refused before the engine, naming the product, and the server is
      // still here to answer the next one.
      const five = ["a", "b", "c", "d", "e"].map((alias) => `@${sheet_id} ${alias}`).join(", ");
      const walked = await app.request(`/query`, {
        method: "POST",
        headers: new Headers({ "Content-Type": "application/json", Authorization: `Bearer ${jwt}` }),
        body: JSON.stringify({ lang: "sql", code: `select count(*) as n from ${five}`, args: [] }),
      });
      assertEquals(walked.status, 400);
      const said = await walked.text();
      const n = countries.doc.data.length - 1;
      assert(said.includes(`${n ** 5} pairs`), said);
      assert(said.includes(`@${sheet_id} (${n})`), said);
      assert(said.includes("filter each large sheet"), said);
      const { data: [, again] }: { data: Table } = await post(jwt, `/query`, {
        lang: "sql",
        code: `select count(*) as n from @${sheet_id}`,
        args: [],
      });
      assertEquals(again.n, n, "the server answers the next request");
    }

    // examples_test.ts replays every bundled sheet through both engines and
    // compares them row for row. What is checked here is only that this server
    // serves them: the seeded datasets are the shop's inventory.

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

      // The same answer again is the same alert, so nothing is sent -- but the
      // run is still recorded. A quiet alert and a dead poller write the same
      // empty log otherwise, and nothing outside this log can tell them apart.
      const settled = (await history()).length;
      clock += 120_000;
      await pollAlertOnce(send, clock);
      const quiet = await history();
      assertEquals(quiet.length, settled + 1, "every run is recorded, changed or not");
      // The sheet's own interval rides the run, which is what lets the status
      // check grade liveness in SQL rather than by opening every document.
      const [{ recorded }] = await sql`
        select meta->>'interval' as recorded from net where sheet_id = ${alert_id} order by net_id desc limit 1
      `;
      assertEquals(recorded, "60");
      assertEquals(quiet[0].status, "unchanged");
      assertEquals(quiet[0].fingerprint, quiet[1].fingerprint, "the de-dupe still reads the last row");
      assertEquals(sent.length, 0, "an unchanged answer should not email anyone");

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

      // Still breaching, same rows: no second email, and the unchanged run
      // carries the matched rows forward so the next diff is still honest.
      clock += 120_000;
      await pollAlertOnce(send, clock);
      assertEquals(sent.length, 1, "the same breach should not email twice");
      assertEquals((await history())[0].status, "unchanged");

      // A second quiet tick moves that row's timestamp rather than adding
      // another. Otherwise a minute-interval alert writes 1440 rows a day and
      // pushes the held run the daily digest still has to find past NET_KEEP.
      const held = (await history()).length;
      clock += 120_000;
      await pollAlertOnce(send, clock);
      assertEquals((await history()).length, held, "a repeated quiet tick is one row, not a row per tick");

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

      // A send that failed must not be de-duped away: the same rows next interval
      // would then never be sent, so one Resend outage silences the alert for
      // good. The refusal is recorded, and the run after it tries again.
      {
        const refusals: number[] = [];
        const refuse = () => {
          refusals.push(1);
          return Promise.resolve("resend refused it with 500: down");
        };
        watched.change((d: { data: Record<number, unknown>[] }) => {
          d.data[1][1] = 2.5;
        });
        clock += 120_000;
        await pollAlertOnce(refuse, clock);
        assertEquals(refusals.length, 1);
        assert(String((await history())[0].delivery).includes("resend refused"));

        clock += 120_000;
        await pollAlertOnce(refuse, clock);
        assertEquals(refusals.length, 2, "the same rows must be sent again after a failed delivery");
        assertEquals((await history())[0].status, "firing", "a retry is a firing run, not an unchanged one");
      }

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
      await reject(jwt, `/sell/${alert_id}`, { method: "POST", body: JSON.stringify({ price: 0, license: "own" }) });

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

      // `when` asks a different question of the same answer: not "is there a
      // row" but "is there a row that was not there the run before", or one
      // that has left. The verdict stays `status`, so nothing downstream learns
      // a new word.
      const setWhen = (when: string, digest = false) =>
        alert.change((d: { data: [{ code: string; when?: string; digest?: boolean }] }) => {
          d.data[0].when = when;
          d.data[0].digest = digest;
        });
      const burn = (row: number, value: number) =>
        watched.change((d: { data: Record<number, unknown>[] }) => {
          d.data[row][1] = value;
        });
      const to = (address: string) => sent.filter((mail) => mail.to === address);

      // Switching to `added` over the same answer is still the same answer.
      {
        setWhen("added");
        clock += 120_000;
        await pollAlertOnce(send, clock);
        assertEquals((await history())[0].status, "unchanged");
        assertEquals(sent.length, 2);
      }

      // A row leaving is not news to an `added` alert, and the run says why.
      {
        burn(2, 0);
        clock += 120_000;
        await pollAlertOnce(send, clock);
        const left = (await history())[0];
        assertEquals([left.status, left.rows, left.added, left.removed, left.when], ["clear", 1, 0, 1, "added"]);
        assertEquals(left.delivery, "no rows added since the run before, so nothing was sent");
        assertEquals(sent.length, 2, "a row leaving is not a row added");
      }

      // A row arriving is.
      {
        burn(2, 1.1);
        clock += 120_000;
        await pollAlertOnce(send, clock);
        assertEquals((await history())[0].status, "firing");
        assertEquals(sent.length, 3, "a new row is what an added alert exists for");
        assertEquals(sent[2].added, 1);
      }

      // A change condition folds into the digest like any other: the verdict
      // is the diff's, and holding the email is the delivery's.
      {
        setWhen("added", true);
        burn(2, 1.3);
        clock += 120_000;
        await pollAlertOnce(send, clock);
        const held_ = (await history())[0];
        assertEquals([held_.status, held_.added, held_.delivery], ["firing", 1, "held for the daily digest"]);
        assertEquals(sent.length, 3, "held, not mailed");
      }

      // A `removed` alert fires when a row leaves -- and when the last one
      // leaves, which is the transition `rows` can never say.
      {
        setWhen("removed");
        burn(2, 0);
        clock += 120_000;
        await pollAlertOnce(send, clock);
        assertEquals((await history())[0].status, "firing");
        assertEquals(sent.length, 4);
        assertEquals(sent[3].removed, 1);

        burn(1, 0);
        clock += 120_000;
        await pollAlertOnce(send, clock);
        const emptied = (await history())[0];
        assertEquals([emptied.status, emptied.rows, emptied.removed], ["firing", 0, 1]);
        assertEquals(sent.length, 5, "the last row leaving is a change worth sending");
        assertEquals(sent[4].rows.length, 0);
      }

      // A failed send under a change condition is retried, although the run it
      // diffs against already holds the rows it never delivered.
      {
        setWhen("added");
        burn(1, 1.2);
        const refusals: number[] = [];
        const refuse = () => {
          refusals.push(1);
          return Promise.resolve("resend refused it with 500: down");
        };
        clock += 120_000;
        await pollAlertOnce(refuse, clock);
        assertEquals(refusals.length, 1);
        clock += 120_000;
        await pollAlertOnce(refuse, clock);
        assertEquals(refusals.length, 2, "the rows that never arrived are sent again");
        assertEquals((await history())[0].status, "firing");
      }

      // An unknown `when` is an error run that names the choices, not a quiet
      // fall back to rows.
      {
        setWhen("bogus");
        clock += 120_000;
        await pollAlertOnce(send, clock);
        const unknown = (await history())[0];
        assertEquals(unknown.status, "error");
        assert(String(unknown.error).includes("bogus"), String(unknown.error));
        assert(String(unknown.error).includes("rows, added, removed"), String(unknown.error));
        setWhen("rows");
      }

      // A profile is a different answer every run, so an alert watching one
      // would mail every interval. Refused by name.
      {
        const watching = `select region, burn from @table:${watched.documentId} where burn > 0`;
        alert.change((d: { data: [{ code: string }] }) => {
          d.data[0].code = `explain ${watching}`;
        });
        clock += 120_000;
        await pollAlertOnce(send, clock);
        const profiled = (await history())[0];
        assertEquals(profiled.status, "error");
        assert(String(profiled.error).includes("watches a profile"), String(profiled.error));
        alert.change((d: { data: [{ code: string }] }) => {
          d.data[0].code = watching;
        });
      }

      // A change condition's first run is its baseline, a run past ALERT_ROWS is
      // a refusal rather than a silent "nothing new", and the run after an error
      // is not a diff against nothing -- which is what used to call every row new.
      {
        const big = automerge.create<{ data: Sheet["data"] }>({
          data: [
            arrayify([{ name: "n", type: "num", key: 0 }]),
            { 0: 1 },
            { 0: 2 },
            { 0: 3 },
          ],
        });
        await put(jwt, `/library/table:${big.documentId}`, {});
        const alert2 = automerge.create<{ data: [{ code: string; to: string; interval: number; when: string }] }>({
          data: [{ code: `select n from @table:${big.documentId}`, to: "two@example.com", interval: 60, when: "added" }],
        });
        await put(jwt, `/library/alert:${alert2.documentId}`, { name: "big watch" });
        const history2 = async () => {
          const [, ...rows] = await get<Table>(jwt, `/sheet/alert:${alert2.documentId}`, {});
          return rows.map((row) => JSON.parse(String(row.body)) as Record<string, unknown>);
        };

        clock += 120_000;
        await pollAlertOnce(send, clock);
        const first = (await history2())[0];
        assertEquals(first.status, "clear");
        assert(String(first.diff_skipped).includes("first run"), String(first.diff_skipped));
        assertEquals(first.delivery, "nothing to compare with, so nothing was sent");
        assertEquals(to("two@example.com").length, 0, "a baseline is not a backlog to mail");

        big.change((d: { data: Record<number, unknown>[] }) => {
          for (let i = 0; i < 198; i++) d.data.push({ 0: 10 + i });
        });
        clock += 120_000;
        await pollAlertOnce(send, clock);
        const over = (await history2())[0];
        assertEquals(over.status, "error");
        assert(String(over.error).includes("more than the 200 rows"), String(over.error));
        assert(String(over.error).includes("narrow the query"), String(over.error));

        big.change((d: { data: Record<number, unknown>[] }) => {
          d.data.splice(4, 198);
        });
        clock += 120_000;
        await pollAlertOnce(send, clock);
        const after = (await history2())[0];
        assertEquals([after.status, after.added], ["clear", null]);
        assert(String(after.diff_skipped).includes("the run before failed"), String(after.diff_skipped));
        assertEquals(to("two@example.com").length, 0, "the run after an error must not call every row new");
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
      await post(aliceJwt, `/sell/table:${hand.documentId}`, { price: 5, license: "own" });
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

      // Every value below is a published one, so these check the arithmetic
      // rather than checking it against itself.
      const fails = async (code: string) => {
        const res = await app.request(`/query`, {
          method: "POST",
          headers: new Headers({ "Content-Type": "application/json", Authorization: `Bearer ${jwt}` }),
          body: JSON.stringify({ lang: "sql", code, args: [] }),
        });
        assertEquals(res.status, 400, code);
        return await res.text();
      };
      const five = `(select 1 as x, 3 as y union all select 2, 5 union all select 3, 7
                     union all select 4, 9 union all select 5, 11)`;

      // Regression: the line through y = 2x + 1 predicts 21 at x = 10 and has no
      // spread around it, because every point is on it.
      const line = await run(
        `select regr_predict(array(x),array(y),10) at10, regr_stderr(array(x),array(y)) se from ${five}`,
      );
      assertEquals([round(line.at10), round(line.se)], [21, 0]);

      // Curve fitting: 100 * exp(-0.1x) read back at x = 6, and y = 2x^3 at x = 4.
      const curves = await run(
        `select fit_exponential(array(x),array(y),6) e from
           (select 0 as x, 100 as y union all select 1, 90.4837418 union all select 2, 81.8730753
            union all select 3, 74.0818221 union all select 4, 67.0320046)`,
      );
      assertEquals(round(curves.e, 3), 54.881);
      const power = await run(
        `select fit_power(array(x),array(y),4) p from
           (select 1 as x, 2 as y union all select 2, 16 union all select 3, 54)`,
      );
      assertEquals(round(power.p, 6), 128);

      // A robust score: one value at 100 among small ones cannot widen the ruler
      // it is measured against, which is what a plain z-score lets it do.
      const outlier = await run(
        `select mad(array(x)) m, robust_z(100, array(x)) z from
           (select 1 as x union all select 2 union all select 3 union all select 4 union all select 100)`,
      );
      assertEquals([outlier.m, round(outlier.z, 2)], [1, 65.43]);

      // Welch's t-test and the mean's interval, against the textbook numbers.
      const ab = `(select 'a' as g, 1 as v union all select 'a', 2 union all select 'a', 3 union all select 'a', 4
                   union all select 'a', 5 union all select 'b', 6 union all select 'b', 7 union all select 'b', 8
                   union all select 'b', 9 union all select 'b', 10)`;
      const test = await run(
        `select t_test(a.vs, b.vs) p from (select array(v) vs from ${ab} where g = 'a') a,
                (select array(v) vs from ${ab} where g = 'b') b`,
      );
      assertEquals(round(test.p, 6), 0.001053);
      const band = await run(`select ci_low(array(x), 0.95) lo, ci_high(array(x), 0.95) hi from ${five}`);
      assertEquals([round(band.lo, 4), round(band.hi, 4)], [1.0368, 4.9632]);

      // Histogram bins. Below the range is bucket 0 and above it is n+1, so the
      // tails stay visible instead of being folded into the end bars.
      const bins = await run(
        `select width_bucket(5,0,30,6) mid, width_bucket(-1,0,30,6) below, width_bucket(30,0,30,6) above`,
      );
      assertEquals([bins.mid, bins.below, bins.above], [2, 0, 7]);

      // Geospatial. The geohash is the canonical vector, and the area is a one
      // degree square on the equator, which is 12363.7 km2 on a sphere.
      const geo = await run(
        `select geohash(57.64911, 10.40744, 11) h, bearing_deg(0,0,0,1) east,
                point_in_polygon(0.5, 0.5, '[[0,0],[0,1],[1,1],[1,0]]') inside,
                point_in_polygon(2, 0.5, '[[0,0],[0,1],[1,1],[1,0]]') outside,
                polygon_area_km2('[[0,0],[0,1],[1,1],[1,0]]') km2`,
      );
      assertEquals([geo.h, geo.east, geo.inside, geo.outside], ["u4pruydqqvj", 90, true, false]);
      assertEquals(round(geo.km2, 1), 12363.7);

      // Each refusal has to name the argument and the value, not return NaN.
      for (
        const [code, said] of [
          [`select width_bucket(5, 0, 30, 0) b`, "1 or more"],
          [`select geohash(1, 2, 99) h`, "between 1 and 12"],
          [`select geohash(100, 2, 5) h`, "between -90 and 90"],
          [`select point_in_polygon(1, 2, 'not json') p`, "JSON array of [lat, lon] pairs"],
          [`select polygon_area_km2('[[0,-179],[0,179],[1,179]]') a`, "antimeridian"],
        ]
      ) {
        assert((await fails(code)).includes(said), `${code} should say ${said}`);
      }
      // AlaSQL cannot run any aggregate over a one-row derived table, so the
      // too-small-sample guard is checked against the function rather than
      // through a query that the engine refuses before it is reached.
      assertThrows(() => ala.fn.t_test([1], [2, 3]), Error, "at least 2 values");
      assertThrows(() => ala.fn.ci_low([1, 2, 3], 95), Error, "between 0 and 1");
      assertThrows(() => ala.fn.fit_exponential([1, 2], [1, 0], 4), Error, "above zero");
      // JSON.stringify() turns Infinity and NaN into "null", so these used to
      // read "received number null", which names neither the value nor the bug.
      assertThrows(() => ala.fn.width_bucket(Infinity, 0, 30, 6), Error, "received number Infinity");
      assertThrows(() => ala.fn.robust_z(NaN, [1, 2, 3]), Error, "received number NaN");

      // Reading a sheet, a function's own message comes through. Reading a
      // subquery in the from clause, AlaSQL drops it and reports "Cannot read
      // properties of null" from somewhere else entirely -- the worst error in
      // the system, so it is replaced by one that says what happened.
      const dropped = await fails(`select ci_low(array(x), 95) lo from ${five}`);
      assert(dropped.includes("dropped its message"), dropped);
      assert(dropped.includes("read the subquery's rows into a sheet"), dropped);

      // AlaSQL evaluates a group by expression against an empty row, so every UDF
      // named there receives nothing. The message has to say so: hunting a typo
      // in a column name that is spelled correctly is the worse bug.
      const grouped = await fails(
        `select date_trunc('month', missing) m, count(*) n from series('2026-01-01','2026-01-03') group by 1`,
      );
      assert(grouped.includes("subquery"), grouped);
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

      // A mistyped @sheet ref used to read as a bare access denial. The last
      // character is swapped for one it is not: an id that already ended in the
      // one hard-coded here made the typo the real id, and that run asserted
      // nothing while still passing.
      const mistyped = hand.documentId.slice(0, -1) + (hand.documentId.endsWith("x") ? "y" : "x");
      assert(mistyped !== hand.documentId, "the typo must not be the id it is a typo of");
      const typo = await app.request(`/query`, {
        method: "POST",
        headers: new Headers({ Authorization: `Bearer ${jwt}`, "Content-Type": "application/json" }),
        body: JSON.stringify({
          lang: "sql",
          code: `select * from @table:${mistyped}`,
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
  });

  await t.step("CSV import -> export round-trip", async () => {
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

    // Deleting every column leaves the values in the document with nothing left
    // to name them. Every read is name-keyed, so each of those rows projects to
    // one empty object -- N rows of nothing, handed back as a healthy answer.
    // Refused by name instead, and pointed at the sheet, which is where the
    // values are still reachable and the deletion still undoable.
    {
      const orphan = automerge.create<{ data: Sheet["data"] }>({ data: [arrayify([]), { 0: "x" }] });
      await put(jwt, `/library/table:${orphan.documentId}`, {});
      const res = await app.request(`/sheet/table:${orphan.documentId}`, {
        headers: new Headers({ Authorization: `Bearer ${jwt}` }),
      });
      assertEquals(res.status, 400);
      const said = await res.text();
      assert(said.includes("under no columns"), said);
      assert(said.includes("1 rows"), said);

      // An empty sheet is empty, not broken: there is nothing to lose.
      const blank = automerge.create<{ data: Sheet["data"] }>({ data: [arrayify([])] });
      await put(jwt, `/library/table:${blank.documentId}`, {});
      assertEquals((await get<Table>(jwt, `/sheet/table:${blank.documentId}`)).length, 1);
    }

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

    // Two columns of the same name collapse into one in a name-keyed row, so
    // the read refuses the sheet rather than answering with one of the two.
    // csv and md used to carry such a sheet positionally; they cannot any more,
    // because every read is name-keyed now and there are no positions left to
    // carry. That is the price, and it is the right way round: toRecords has
    // always collapsed them, so a query over this sheet was already wrong --
    // silently, which is the half worth fixing.
    {
      const dup = automerge.create<{ data: Sheet["data"] }>({
        data: [arrayify([{ name: "a", type: "text", key: 0 }, { name: "a", type: "text", key: 1 }]), {
          0: "x",
          1: "y",
        }],
      });
      await put(jwt, `/library/table:${dup.documentId}`, {});
      for (const format of ["csv", "json", "ndjson", "md"]) {
        const res = await exp(`table:${dup.documentId}`, format);
        assertEquals(res.status, 400, `${format} must refuse a sheet it cannot key by name`);
        assert((await res.text()).includes(`"a" appears more than once`), "and the duplicate must be named");
      }
      const read = await app.request(`/sheet/table:${dup.documentId}`, {
        headers: new Headers({ Authorization: `Bearer ${jwt}` }),
      });
      assertEquals(read.status, 400, "and so must the read the exports go through");

      // The commonest pair is two columns nobody has named yet, and a message
      // about `` appearing twice names nothing.
      const blank = automerge.create<{ data: Sheet["data"] }>({
        data: [arrayify([{ name: "", type: "text", key: 0 }, { name: "", type: "text", key: 1 }])],
      });
      await put(jwt, `/library/table:${blank.documentId}`, {});
      const said = await (await exp(`table:${blank.documentId}`, "csv")).text();
      assert(said.includes(`"" appears more than once`), said);
      assert(said.includes("rename one of them"), said);
    }
  });

  await t.step("A CSV that does not match its own header is a rejection, not a coercion", async () => {
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

    // Two columns of one name would import happily and then be unreadable, so
    // the file is refused where the header that has to change is on screen.
    // Blanks collide too, since a blank header is named for its position.
    const twice = await importCsv("a,b,a\n1,2,3\n");
    assertEquals(twice.status, 400);
    const twiceSaid = await twice.text();
    assert(twiceSaid.includes(`"a" appears more than once`), twiceSaid);
    assert(twiceSaid.includes("line 1"), twiceSaid);

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
    // By name, which is the header the file carried and the shape the write
    // takes back. The read used to answer r["0"] here and r.qty from a query
    // over the same sheet.
    assertEquals(rows.map((r) => r.qty), ["1", "2", "3", "4", "n/a"]);

    // A blank is not a zero and it is not a false. The importer stored "" in a
    // num column and an invented false in a bool one, so a file of two readings
    // and one gap became three readings, and the false was a value the file
    // never contained. checkColumnTypes() is the one place a blank turns into a
    // null; an importer that disagrees with it makes the stored document and
    // the query answer two different questions about the same file.
    const gaps = await importCsv("qty,ok\n1,true\n,\n3,false\n");
    const gapBody = await gaps.text();
    assert(gaps.ok, `blanks are not a rejection: ${gapBody}`);
    const { sheet_id: gapId } = JSON.parse(gapBody);
    const [gapCols, ...gapRows] = await get<Table>(jwt, `/sheet/${gapId}`);
    assertEquals(Object.values(gapCols).map((col) => col.type), ["num", "bool"]);
    assertEquals(gapRows.map((r) => r.qty), [1, null, 3]);
    assertEquals(gapRows.map((r) => r.ok), [true, null, false]);

    // And the arithmetic over it is the arithmetic over a sheet built by hand.
    // sum() and avg() are where a blank read as a zero actually shows: avg over
    // [1, "", 3] answered 1.333, a reading nobody took.
    const byHand = automerge.create<{ data: Sheet["data"] }>({
      data: [
        arrayify([{ name: "qty", type: "num", key: 0 }, { name: "ok", type: "bool", key: 1 }]),
        { 0: 1, 1: true },
        { 0: null, 1: null },
        { 0: 3, 1: false },
      ],
    });
    await put(jwt, `/library/table:${byHand.documentId}`, {});
    const arithmetic = async (id: string) => {
      const res = await app.request("/query", {
        method: "POST",
        headers: new Headers({ "Content-Type": "application/json", Authorization: `Bearer ${jwt}` }),
        body: JSON.stringify({
          lang: "sql",
          code: `select sum(qty) as sum_qty, avg(qty) as avg_qty, count(qty) as n_qty from @${id}`,
        }),
      });
      const said = await res.text();
      assert(res.ok, `arithmetic over @${id}: ${res.status} ${said}`);
      return (JSON.parse(said).data as Table)[1];
    };
    assertEquals(await arithmetic(gapId), await arithmetic(`table:${byHand.documentId}`));
    assertEquals(Number((await arithmetic(gapId)).avg_qty), 2, "the gap is not a reading of zero");
  });

  await t.step("A webhook sender only ever sees the response body, so the body diagnoses", async () => {
    const { jwt } = await usr("sam@example.com");

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
    const big = await deliver(`net-hook:${hook.documentId}`, "x".repeat(BODY_CAP + 1));
    assertEquals(big.status, 413);
    assert((await big.text()).includes(`${BODY_CAP} bytes`), "it must name the limit");
    assert((await deliver(`net-hook:${hook.documentId}`, "{}")).ok, "a valid delivery still lands");
  });

  await t.step("describe @sheet, cost guards, and the source-type check", async () => {
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

    // explain runs the query and answers with its profile: one row per stage,
    // and the query's own checks still stand under it.
    {
      const [cols_, ...rows] = (await runs(`explain select city from @${id} where pop > 0`)).data as Table;
      assertEquals(
        Object.values(cols_).map((col) => `${col.name}:${col.type}`).join(),
        "stage:text,rows_in:int,rows_out:int,ms:num",
      );
      assertEquals(
        rows.map((r) => [r.stage, r.rows_in, r.rows_out]),
        [[`load @${id}`, null, 2], ["plan", 2, 2], ["engine", 2, 1], ["total", 2, 1]],
      );
      for (const r of rows) assert(typeof r.ms === "number" && r.ms >= 0, JSON.stringify(r));
      const [, ...win] = (await runs(
        `explain select city, row_number() over (order by pop) as rn from @${id} limit 1`,
      )).data as Table;
      assertEquals(win.map((r) => r.stage), [`load @${id}`, "plan", "engine", "windows", "total"]);
      assertEquals(win.at(-1)!.rows_out, 1, "the profile counts what the windows pass kept");
      const refusal = async (code: string) => {
        const res = await app.request(`/query`, {
          method: "POST",
          headers: new Headers({ "Content-Type": "application/json", Authorization: `Bearer ${jwt}` }),
          body: JSON.stringify({ lang: "sql", code, args: [] }),
        });
        assertEquals(res.status, 400, code);
        return await res.text();
      };
      assert((await refusal(`explain describe @${id}`)).includes("profiles a query"));
      assert((await refusal(`explain select nope from @${id}`)).includes("nope"), "a profile of a refused query is refused");
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

    // A sheet somebody else owns is refused with the status the refusal earned.
    // The query path wraps its own checks as 400, and re-wrapping this one would
    // turn "you cannot see this sheet" into "your SQL is wrong".
    {
      const mine = automerge.create<{ data: Sheet["data"] }>({
        data: [arrayify([{ name: "secret", type: "text", key: 0 }]), { 0: "shh" }],
      });
      const mineId = `table:${mine.documentId}`;
      await put(jwt, `/library/${mineId}`, {});
      const { jwt: stranger } = await usr("mallory-outsider@example.com");
      const res = await app.request(`/query`, {
        method: "POST",
        headers: new Headers({ "Content-Type": "application/json", Authorization: `Bearer ${stranger}` }),
        body: JSON.stringify({ lang: "sql", code: `select * from @${mineId}`, args: [] }),
      });
      assertEquals(res.status, 403, `a stranger's query should be refused as access, got: ${await res.text()}`);
    }
  });

  // A result column used to be typed by its name alone -- whatever column of that name some loaded sheet declared, or
  // text -- so `cast(price as string) as price` still read usd, `count(*) as n` read text, and every sheet downstream
  // of that query inherited the lie. The type is a property of the select item, so it is read off the select item, in
  // src/sql.mjs where both engines read it.
  await t.step("A query result carries its types forward", async () => {
    const { jwt } = await usr("tessa@example.com");
    const hand = automerge.create<{ data: Sheet["data"] }>({
      data: [
        arrayify([
          { name: "label", type: "text", key: 0 },
          { name: "qty", type: "int", key: 1 },
          { name: "price", type: "usd", key: 2 },
        ]),
        { 0: "widget", 1: 2, 2: 10 },
        { 0: "gasket", 1: 3, 2: 4 },
      ],
    });
    const id = `table:${hand.documentId}`;
    await put(jwt, `/library/${id}`, {});
    const typesOf = async (code: string) => {
      const { data: [cols_] }: { data: Table } = await post(jwt, `/query`, { lang: "sql", code, args: [] });
      return Object.fromEntries(Object.values(cols_).map((col) => [col.name, col.type]));
    };

    // A bare column, a qualified one and an alias each keep the source type. A
    // cast is the one expression whose type is stated rather than inferred.
    assertEquals(
      await typesOf(
        `select label, t.price, qty as units, cast(price as string) as price_text, 'each' as sold_by, 7 as seven from @${id} t`,
      ),
      { label: "text", price: "usd", units: "int", price_text: "text", sold_by: "text", seven: "num" },
    );
    // ...and the value really is text, which is the half a type alone cannot say.
    {
      const { data: [, row] }: { data: Table } = await post(jwt, `/query`, {
        lang: "sql",
        code: `select cast(price as string) as price_text from @${id} order by price_text`,
        args: [],
      });
      assertEquals(row.price_text, "10");
    }

    // An aggregate is typed by what it aggregates: count is a count, sum and
    // min keep their argument's type, min_text compares as text, and an average
    // of whole numbers is not a whole number. The sum is `takings` because
    // `total` is an AlaSQL keyword and will not parse as an alias, the way
    // `store` and `class` will not parse as column names.
    assertEquals(
      await typesOf(
        `select count(*) as n, sum(price) as takings, avg(qty) as mean, avg(price) as spend, min_text(label) as lo, round(avg(price), 2) as typical from @${id}`,
      ),
      { n: "int", takings: "usd", mean: "num", spend: "usd", lo: "text", typical: "usd" },
    );

    // An expression the table cannot type falls back to what typing did before
    // this existed -- the source column of that name, or text -- because a type
    // is a hint about an answer the engine already computed, and a hint must
    // never be the reason a query that ran stops answering.
    assertEquals(
      await typesOf(`select qty * price as gross, case when qty > 2 then price else 0 end as price from @${id}`),
      { gross: "text", price: "usd" },
    );

    // The point of all of it: the sheet downstream reads the corrected type.
    {
      const q = automerge.create<Sheet>({
        type: "query",
        data: [{
          lang: "sql",
          code: `select count(*) as n, cast(price as string) as price_text from @${id} group by price`,
          args: [],
        }],
      });
      await put(jwt, `/library/query:${q.documentId}`, {});
      assertEquals(await typesOf(`select n, price_text from @query:${q.documentId}`), {
        n: "int",
        price_text: "text",
      });
    }
  });

  // Number("") is 0, so a blank cell in a numeric column summed, averaged and plotted as a reading of zero rather
  // than as a reading nobody took: AlaSQL skips a null and counts a "", and its avg() over ["1","2","3"] answers 41
  // because "+" concatenated them first. checkColumnTypes() is now the one place a cell becomes what its column says
  // it is, and it is the only coercion there is.
  await t.step("A column spelled the old way is read, queried and still checked", async () => {
    // The bug this whole list exists because of: the page wrote "pct" for a
    // percentage, nothing else knew the word, and NUMERIC_TYPES therefore
    // skipped the column -- its blanks stayed blanks and avg() counted each as a
    // reading of zero. An alias is a type the engine knows now, so the documents
    // that actually hold one are checked rather than condemned.
    const { jwt } = await usr("nadia@example.com");
    const hand = automerge.create<{ data: Sheet["data"] }>({
      data: [
        arrayify([{ name: "site", type: "string", key: 0 }, { name: "share", type: "pct", key: 1 }]),
        { 0: "a", 1: 0.5 },
        { 0: "b", 1: "" },
        { 0: "c", 1: "0.25" },
      ],
    });
    const id = `table:${hand.documentId}`;
    await put(jwt, `/library/${id}`, {});
    const { data: [, row] }: { data: Table } = await post(jwt, `/query`, {
      lang: "sql",
      code: `select avg(share) as mean, count(share) as n from @${id}`,
      args: [],
    });
    assertEquals([row.mean, Number(row.n)], [0.375, 2], "the blank is absent, not a zero, and the string is a number");

    // And a spelling nobody knows is refused by name rather than skipped, which
    // is the same failure wearing a typo.
    const typo = automerge.create<{ data: Sheet["data"] }>({
      // Cast, because the Type union refuses this spelling at compile time --
      // which is the guard working. A document written by an older page is how
      // one reaches the engine at runtime.
      data: [arrayify([{ name: "share", type: "percentag", key: 0 } as unknown as Col]), { 0: "" }],
    });
    await put(jwt, `/library/table:${typo.documentId}`, {});
    const refused = await app.request("/query", {
      method: "POST",
      headers: new Headers({ "Content-Type": "application/json", Authorization: `Bearer ${jwt}` }),
      body: JSON.stringify({ lang: "sql", code: `select * from @table:${typo.documentId}`, args: [] }),
    });
    assertEquals(refused.status, 400);
    const said = await refused.text();
    assert(said.includes("percentage"), `the refusal should name the nearest type it knows: ${said}`);

    // Matched against every spelling and answered with what it means. Searching
    // only the writable half sent PCT to `int` -- a suggestion that turns a
    // percentage column into a whole number.
    const shouted = automerge.create<{ data: Sheet["data"] }>({
      data: [arrayify([{ name: "share", type: "PCT", key: 0 } as unknown as Col]), { 0: "" }],
    });
    await put(jwt, `/library/table:${shouted.documentId}`, {});
    const yelled = await app.request("/query", {
      method: "POST",
      headers: new Headers({ "Content-Type": "application/json", Authorization: `Bearer ${jwt}` }),
      body: JSON.stringify({ lang: "sql", code: `select * from @table:${shouted.documentId}`, args: [] }),
    });
    const about = await yelled.text();
    assert(
      about.includes("set that column's type to percentage"),
      `PCT should be pointed at percentage, not at whatever is nearest among the writable names: ${about}`,
    );
    // describe is the way back in: a sheet this refuses is the one it exists to
    // let somebody look at.
    const seen = await app.request("/query", {
      method: "POST",
      headers: new Headers({ "Content-Type": "application/json", Authorization: `Bearer ${jwt}` }),
      body: JSON.stringify({ lang: "sql", code: `describe @table:${typo.documentId}`, args: [] }),
    });
    assertEquals(seen.status, 200, "describe still answers on a sheet whose types are wrong");
  });

  await t.step("Null, empty and zero stay different", async () => {
    const { jwt } = await usr("nils@example.com");
    const hand = automerge.create<{ data: Sheet["data"] }>({
      data: [
        arrayify([{ name: "site", type: "text", key: 0 }, { name: "reading", type: "num", key: 1 }]),
        { 0: "a", 1: 2 },
        { 0: "b", 1: "" },
        { 0: "c", 1: 4 },
        { 0: "d", 1: null },
        { 0: "e", 1: 6 },
        { 0: "f", 1: "  " },
        { 0: "", 1: 8 },
        { 0: "h", 1: "10" },
      ],
    });
    const id = `table:${hand.documentId}`;
    await put(jwt, `/library/${id}`, {});
    const one = async (code: string) => {
      const { data: [, row] }: { data: Table } = await post(jwt, `/query`, { lang: "sql", code, args: [] });
      return row;
    };

    // Five readings and three blanks: the sum adds five values and the average
    // divides by five. It used to divide by eight.
    assertEquals(
      await one(
        `select sum(reading) as takings, avg(reading) as mean, count(reading) as taken, count(*) as rows_ from @${id}`,
      ),
      { takings: 30, mean: 6, taken: 5, rows_: 8 },
    );
    // A numeric string is that number, not the string that concatenates: avg()
    // over ["1","2","3"] answers 41 without this.
    assertEquals((await one(`select max(reading) as high from @${id}`)).high, 10);

    // A blank is a legitimate null, not the rejection a "n/a" earns...
    assertEquals(
      (await one(`select count(*) as blanks from @${id} where reading is null`)).blanks,
      3,
    );
    // ...and an empty string in a text column is still an empty string. Only a
    // numeric column is touched, which is what keeps the three apart.
    assertEquals((await one(`select count(*) as unnamed from @${id} where site = ''`)).unnamed, 1);

    // A missing reading must not plot as zero. The chart reads the same rows
    // through the same load, so a blank arrives as null and the page drops it.
    {
      const chart = automerge.create<{ data: [{ source: string; kind: string; x: string; y: string }] }>({
        data: [{ source: `@${id}`, kind: "line", x: "site", y: "reading" }],
      });
      await put(jwt, `/library/chart:${chart.documentId}`, { name: "readings" });
      const [, ...points] = await get<Table>(jwt, `/sheet/chart:${chart.documentId}`);
      assertEquals(points.map((row) => row.y), [8, 2, null, 4, null, 6, null, 10]);
    }

    // A blank that reaches a function is refused by name rather than read as a
    // zero, which is what silently pulled a median and a mad toward it.
    {
      const res = await app.request(`/query`, {
        method: "POST",
        headers: new Headers({ "Content-Type": "application/json", Authorization: `Bearer ${jwt}` }),
        body: JSON.stringify({ lang: "sql", code: `select mad(array(reading)) as m from @${id}`, args: [] }),
      });
      assertEquals(res.status, 400);
      const said = await res.text();
      assert(said.includes("filter the blanks out"), said);
    }
  });

  await t.step("net-hook ingestion + net-http polling", async () => {
    const { jwt } = await usr("nadia@example.com");
    const hookHand = automerge.create<Sheet>({ type: "net-hook", data: [] });
    const hookId = `net-hook:${hookHand.documentId}`;
    await put(jwt, `/library/${hookId}`, {});

    // Anyone can POST a payload; only the owner reads the log.
    {
      const res = await deliver(hookId, JSON.stringify({ event: "ping" }), "?x=1");
      assert(res.ok, `webhook ingest failed: ${res.status}`);
      const cols = "created_at,body,method,req_headers,query_params,meta";
      const [cols_, ...rows] = await get<Table>(jwt, `/net/${hookId}`);
      assertEquals(Object.values(cols_).map((col) => col.name).join(), cols);
      // The three jsonb columns are cast to text on the way out, so a cell holds
      // the JSON rather than a parsed object json_extract() cannot read. The
      // column says text because that is what the cell is.
      assertEquals(Object.values(cols_).map((col) => col.type).join(), "text,text,text,text,text,text");
      // Stored as a real jsonb object, not a jsonb string holding JSON. Written
      // the other way, `meta->>'status'` was null on every row -- which is how
      // the 5xx and the refusals conditions came to grade a dead check as fine.
      const [{ shape }] = await sql`
        select jsonb_typeof(meta) as shape from net where sheet_id = ${hookId} limit 1
      `;
      assertEquals(shape, "object");
      // Every delivery carries what the run itself cost, beside what it delivered.
      assertEquals(JSON.parse(String(rows[0].meta)).bytes, '{"event":"ping"}'.length);

      assertEquals(rows.length, 1);
      assert(String(rows[0].body).includes("ping"), JSON.stringify(rows[0]));
      // The delivery's own method, headers and query string are the raw material
      // signature verification needs; they were stored but never readable.
      assertEquals(rows[0].method, "POST");
      // The cell holds the raw JSON text, so json_extract() is how a query reads
      // it -- the same in Postgres and in PGlite.
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
      for (let i = 0; i < 10; i++) await deliver(hookId, JSON.stringify({ seq: i }));
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
      // A body that differs per call: the same body twice is one row moved to
      // now, and this test counts rows to see the polls happen.
      const fetcher = (url: string, headers: Record<string, string> = {}) => {
        calls.push([url, headers]);
        return Promise.resolve(new Response(`{"ok":true,"poll":${calls.length}}`));
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

    // A header that names a secret instead of carrying one. The value lives in
    // the secret table; the document keeps only the reference, because sync
    // hands that document to every viewer and every share-link holder.
    {
      const keyed = automerge.create<Sheet>({
        type: "net-http",
        data: [{
          url: "https://keyed.test/keyed",
          interval: 120,
          headers: "X-Api-Key: {{secret:weather}}\nAuthorization: Bearer {{secret:weather}}",
        }],
      });
      const keyedId = `net-http:${keyed.documentId}`;
      await put(jwt, `/library/${keyedId}`, {});

      // Every due net-http sheet is polled, so only this one's calls count.
      const seen: Record<string, string>[] = [];
      const fetcher = (url: string, headers: Record<string, string> = {}) => {
        if (url === "https://keyed.test/keyed") seen.push(headers);
        return Promise.resolve(new Response(`{"ok":true}`));
      };
      const rowsOf = async () => (await get<Table>(jwt, `/net/${keyedId}`)).slice(1);
      const t1 = Date.now() + 500_000;

      // Before the secret exists: a failure row naming what is missing, and no
      // request at all -- sending it without the header would come back as
      // somebody else's 401 and read as the API's fault.
      await pollNetOnce(fetcher, t1);
      assertEquals(seen.length, 0, "a header that cannot be built must not be sent without it");
      const [failed] = await rowsOf();
      const failure = JSON.parse(String(failed.body));
      assert(failure.error.includes("{{secret:weather}}"), `it must name the reference: ${failure.error}`);
      assert(failure.error.includes("does not hold"), failure.error);
      assert(failure.repro.includes("X-Api-Key: <value>"), "the repro still names keys and not values");

      await post(jwt, `/library/${keyedId}/secret`, { name: "weather", value: "sk-live-1" });
      await pollNetOnce(fetcher, t1 + 200_000);
      assertEquals(seen.length, 1, "with the secret stored, the request goes");
      assertEquals(seen[0], { "X-Api-Key": "sk-live-1", Authorization: "Bearer sk-live-1" });

      // Writing the secret again rotates it, and the feed picks the newest up
      // on its next poll rather than needing the sheet edited.
      await post(jwt, `/library/${keyedId}/secret`, { name: "weather", value: "sk-live-2" });
      await pollNetOnce(fetcher, t1 + 400_000);
      assertEquals(seen[1]["X-Api-Key"], "sk-live-2", "a rotated secret reaches the next poll");

      // The value must be nowhere a viewer can read: not in the log, and not in
      // the sheet row the shop copies from.
      const log = JSON.stringify(await rowsOf());
      assert(!log.includes("sk-live-1") && !log.includes("sk-live-2"), "a resolved value must not reach the log");
      const [row] = await sql`select row_0::text as r from sheet where sheet_id = ${keyedId}`;
      assert(String(row.r).includes("{{secret:weather}}"), "the document keeps the reference");
      assert(!String(row.r).includes("sk-live-"), "and never the value");
    }

    // Header parsing: one "Name: value" per line; malformed lines throw loudly.
    assertEquals(parseNetHeaders("A: b\nC: d: e"), { A: "b", C: "d: e" });
    assertEquals(parseNetHeaders(""), {});
    assertThrows(() => parseNetHeaders("Authorization Bearer xyz"), Error, "Authorization Bearer xyz");
  });

  // The validators ride the last good row, so what this sheet already holds is answered by the log itself. A 304 is a
  // healthy poll that appends nothing: graded as the 304 it is on the wire, a daily file polled hourly would read to
  // /status and to library:freshness as a feed that has been failing for 23 hours.
  await t.step("A feed that has not changed is not downloaded again", async () => {
    const { jwt } = await usr("ivy@example.com");
    const hand = automerge.create<Sheet>({
      type: "net-http",
      data: [{ url: "https://etag.test/feed", interval: 120 }],
    });
    const id = `net-http:${hand.documentId}`;
    await put(jwt, `/library/${id}`, {});
    const stamp = "Wed, 21 Oct 2026 07:28:00 GMT";
    const calls: Record<string, string>[] = [];
    let answer = () => new Response(`{"n":1}`, { headers: { etag: `"v1"`, "last-modified": stamp } });
    const fetcher = (url: string, headers: Record<string, string> = {}) => {
      if (url !== "https://etag.test/feed") return Promise.resolve(new Response(`{"ok":true}`));
      calls.push(headers);
      return Promise.resolve(answer());
    };
    const rows = async () => (await get<Table>(jwt, `/net/${id}`)).slice(1);
    const t = Date.now() + 10_000_000;

    await pollNetOnce(fetcher, t);
    assertEquals(calls.length, 1);
    assert(!("If-None-Match" in calls[0]), "the first poll has nothing to ask about");
    assertEquals((await rows()).length, 1);

    answer = () => new Response(null, { status: 304 });
    await pollNetOnce(fetcher, t + 121_000);
    assertEquals(calls[1]["If-None-Match"], `"v1"`, "the second poll sends back what the first was given");
    assertEquals(calls[1]["If-Modified-Since"], stamp);
    assertEquals((await rows()).length, 1, "a 304 appends nothing: the row already here is the answer");
    const [{ meta }]: { meta: { status: number; not_modified: boolean; etag: string } }[] = await sql`
      select meta from net where sheet_id = ${id}
    `;
    assertEquals(meta.status, 200, "and grades as the healthy poll it is, because POLL_OK reads 2xx");
    assertEquals(meta.not_modified, true, "with the status off the wire kept beside it");
    assertEquals(meta.etag, `"v1"`, "and the validator still there for the poll after that");

    // A 304 to a request that carried no validator is a host answering a
    // question nobody asked. Folding it onto a row would be inventing one.
    const odd = automerge.create<Sheet>({
      type: "net-http",
      data: [{ url: "https://odd.test/odd", interval: 120 }],
    });
    const oddId = `net-http:${odd.documentId}`;
    await put(jwt, `/library/${oddId}`, {});
    await pollNetOnce(
      (url: string) =>
        Promise.resolve(url === "https://odd.test/odd" ? new Response(null, { status: 304 }) : new Response(`{}`)),
      t + 300_000,
    );
    const [oddRow] = (await get<Table>(jwt, `/net/${oddId}`)).slice(1);
    assert(JSON.parse(String(oddRow.body)).error.includes("carried no validator"), String(oddRow.body));
  });

  // A 5xx is the host saying "later" and is retried on a backoff; a 404 is a "no", and retrying it is noise on top of
  // the failure row that already answered. Every retry is scheduled and never slept, which is what keeps one cycle
  // short enough that it cannot still be running when the next tick starts.
  await t.step("A flaky source recovers by itself and a broken one says so", async () => {
    const { jwt } = await usr("ivy@example.com");
    const flaky = automerge.create<Sheet>({
      type: "net-http",
      data: [{ url: "https://flaky.test/feed", interval: 600 }],
    });
    const gone = automerge.create<Sheet>({
      type: "net-http",
      data: [{ url: "https://gone.test/feed", interval: 600 }],
    });
    const dead = automerge.create<Sheet>({
      type: "net-http",
      data: [{ url: "https://dead.test/feed", interval: 600 }],
    });
    const [flakyId, goneId, deadId] = [flaky, gone, dead].map((h) => `net-http:${h.documentId}`);
    for (const id of [flakyId, goneId, deadId]) await put(jwt, `/library/${id}`, {});
    const hits: string[] = [];
    let broken = true;
    const fetcher = (url: string) => {
      hits.push(url);
      if (url === "https://flaky.test/feed")
        return Promise.resolve(broken ? new Response("upstream is down", { status: 503 }) : new Response(`{"n":2}`));
      if (url === "https://gone.test/feed") return Promise.resolve(new Response("no such feed", { status: 404 }));
      if (url === "https://dead.test/feed") return Promise.resolve(new Response("still down", { status: 500 }));
      return Promise.resolve(new Response(`{"ok":true}`));
    };
    const bodies = async (id: string) =>
      (await get<Table>(jwt, `/net/${id}`)).slice(1).map((r) => JSON.parse(String(r.body)));
    const times = (host: string) => hits.filter((u) => u.includes(host)).length;
    const t = Date.now() + 20_000_000;

    await pollNetOnce(fetcher, t);
    assertEquals((await bodies(flakyId))[0].attempt, 1, "the first transient failure counts itself");
    await pollNetOnce(fetcher, t + 10_000);
    assertEquals(times("flaky"), 1, "and the backoff has not elapsed, so nothing is refetched");
    broken = false;
    await pollNetOnce(fetcher, t + 31_000);
    assertEquals((await bodies(flakyId))[0].n, 2, "the poll after the backoff just works");

    // A 404 answers the same to every retry, and the sheet's own interval is
    // 600s, so a poll 31 seconds later must not touch it again.
    assertEquals(times("gone"), 1, "a permanent failure is not retried");
    assertEquals((await bodies(goneId)).length, 1, "and lands as exactly one failure row");
    assert((await bodies(goneId))[0].error.includes("404"), "naming what it received");

    // The bound. Three failures in a row is a crash carrying the counter, not a
    // fourth attempt: an unbounded retry on a host that is down all day is the
    // poller spending every tick on one sheet.
    assertEquals((await bodies(deadId)).map((b) => b.attempt), [2, 1], "two attempts so far, newest first");
    await pollNetOnce(fetcher, t + 92_000);
    const gaveUp = (await bodies(deadId))[0];
    assert(gaveUp.error.includes("failed 3 polls in a row"), gaveUp.error);
    assert(gaveUp.error.includes("within 3 attempts"), gaveUp.error);
    assertEquals(gaveUp.attempt, undefined, "giving up ends the count rather than continuing it");
    await pollNetOnce(fetcher, t + 150_000);
    assertEquals(times("dead"), 3, "and the sheet waits its own interval rather than backing off again");
  });

  // Without that, two sheets pointed at one API take turns stampeding the host that just asked them to stop.
  await t.step("Retry-After is the host's answer and not one sheet's", async () => {
    const { jwt } = await usr("ivy@example.com");
    const [one, two] = ["a", "b"].map((path) =>
      automerge.create<Sheet>({
        type: "net-http",
        data: [{ url: `https://stampede.test/${path}`, interval: 600 }],
      })
    );
    const [oneId, twoId] = [one, two].map((h) => `net-http:${h.documentId}`);
    await put(jwt, `/library/${oneId}`, {});
    const hits: string[] = [];
    let busy = true;
    const fetcher = (url: string) => {
      if (!url.includes("stampede.test")) return Promise.resolve(new Response(`{"ok":true}`));
      hits.push(url);
      return Promise.resolve(
        busy ? new Response("slow down", { status: 429, headers: { "retry-after": "120" } }) : new Response(`{}`),
      );
    };
    const t = Date.now() + 30_000_000;
    await pollNetOnce(fetcher, t);
    assertEquals(hits.length, 1);

    // Registered after that answer, so the hold is already in place the first
    // time this sheet comes due -- which is what makes the assertion about the
    // host and not about which sheet the poller happened to reach first.
    await put(jwt, `/library/${twoId}`, {});
    await pollNetOnce(fetcher, t + 1_000);
    assertEquals(hits.length, 1, "the second sheet waits out what the first was told");
    assertEquals((await get<Table>(jwt, `/net/${twoId}`)).slice(1).length, 0, "and logs nothing: it never ran");
    busy = false;
    await pollNetOnce(fetcher, t + 121_000);
    assertEquals(hits.length, 2, "and one goes once the host's own wait is over");
    await pollNetOnce(fetcher, t + 121_000 + HOST_GAP_MS);
    assertEquals(hits.length, 3, "and the other a gap later: one host, one request per cycle");

    // A Retry-After neither side can read is this sheet's failure row naming
    // what it received. The poller keeps every other sheet.
    const cranky = automerge.create<Sheet>({
      type: "net-http",
      data: [{ url: "https://cranky.test/feed", interval: 600 }],
    });
    const crankyId = `net-http:${cranky.documentId}`;
    await put(jwt, `/library/${crankyId}`, {});
    await pollNetOnce(
      (url: string) =>
        Promise.resolve(
          url === "https://cranky.test/feed"
            ? new Response("later", { status: 503, headers: { "retry-after": "soon" } })
            : new Response(`{}`),
        ),
      t + 200_000,
    );
    const [row] = (await get<Table>(jwt, `/net/${crankyId}`)).slice(1);
    const failure = JSON.parse(String(row.body));
    assert(failure.error.includes("cannot be read"), failure.error);
    assert(failure.error.includes("soon"), "naming what it received");
    assertEquals(failure.attempt, undefined, "a header nobody can read is not a retry");
  });

  // A truncated success is the same bug one layer down: a parse error blamed on the data.
  await t.step("A body over the cap is a failure row naming both numbers", async () => {
    const { jwt } = await usr("ivy@example.com");
    const hand = automerge.create<Sheet>({
      type: "net-http",
      data: [{ url: "https://huge.test/feed", interval: 600 }],
    });
    const id = `net-http:${hand.documentId}`;
    await put(jwt, `/library/${id}`, {});
    await pollNetOnce(
      (url: string) =>
        Promise.resolve(url === "https://huge.test/feed" ? new Response("x".repeat(2_000_000)) : new Response(`{}`)),
      Date.now() + 40_000_000,
    );
    const [row] = (await get<Table>(jwt, `/net/${id}`)).slice(1);
    const failure = JSON.parse(String(row.body));
    assert(failure.error.includes("too large to store"), failure.error);
    // Both numbers, and the size is what it actually read past the cap: no
    // content-length declared one, and reading the rest of a runaway response
    // is the cost the cap exists to refuse.
    const size = Number(failure.error.match(/at least (\d+) bytes/)?.[1]);
    const cap = Number(failure.error.match(/(\d+) bytes per response/)?.[1]);
    assert(size > cap, `the size must name what it read past the cap it names: ${failure.error}`);
    assert(!String(row.body).startsWith("xxx"), "and the payload is refused, not truncated into the log");
  });

  // The watermark is when the last good poll started rather than when it finished, so a row written while that
  // request was in flight is asked for twice rather than missed once. It lives in net.meta beside the run that set
  // it: the automerge document is what sync hands every viewer and what the user edits, and a poller writing to it
  // every tick would fight those edits and mint a change for every open browser.
  await t.step("A since-last-run cursor", async () => {
    const { jwt } = await usr("ivy@example.com");
    const hand = automerge.create<Sheet>({
      type: "net-http",
      data: [{ url: "https://cursor.test/feed?kind=orders", interval: 120, cursor: "since" }],
    });
    const id = `net-http:${hand.documentId}`;
    await put(jwt, `/library/${id}`, {});
    const seen: string[] = [];
    const fetcher = (url: string) => {
      if (url.includes("cursor.test")) seen.push(url);
      return Promise.resolve(new Response(`{"n":1}`));
    };
    const t = Date.now() + 50_000_000;
    await pollNetOnce(fetcher, t);
    assertEquals(seen[0], "https://cursor.test/feed?kind=orders", "the first poll asks for everything");
    await pollNetOnce(fetcher, t + 121_000);
    assertEquals(
      new URL(seen[1]).searchParams.get("since"),
      new Date(t).toISOString(),
      "the next asks for what came after the last good poll",
    );
    assertEquals(new URL(seen[1]).searchParams.get("kind"), "orders", "without losing what the URL already carried");

    const bad = automerge.create<Sheet>({
      type: "net-http",
      data: [{ url: "https://badcursor.test/bad", interval: 600, cursor: "since=1&sneaky" }],
    });
    const badId = `net-http:${bad.documentId}`;
    await put(jwt, `/library/${badId}`, {});
    await pollNetOnce(fetcher, t + 300_000);
    const [row] = (await get<Table>(jwt, `/net/${badId}`)).slice(1);
    assert(JSON.parse(String(row.body)).error.includes("query parameter"), String(row.body));
  });

  await t.step("The poller's two maps grow with traffic and nothing ever took anything out of them", async () => {
    const { jwt } = await usr("ivy@example.com");
    const hand = automerge.create<Sheet>({
      type: "net-http",
      data: [{ url: "https://crowded.test/feed", interval: 600 }],
    });
    const id = `net-http:${hand.documentId}`;
    await put(jwt, `/library/${id}`, {});
    // Further ahead than any real due time, so the ghosts are never polled and
    // the broom would not sweep them either: only the cap can take them out.
    const far = Number.MAX_SAFE_INTEGER;
    for (let i = 0; i < RATE_LIMIT_KEYS_MAX + 5; i++) {
      netDue.set(`net-http:ghost-${i}`, far);
      hostDue.set(`ghost-${i}.test`, far);
    }
    const t = Date.now() + 60_000_000;
    await pollNetOnce(
      (url: string) =>
        Promise.resolve(
          url === "https://crowded.test/feed"
            ? new Response("slow down", { status: 429, headers: { "retry-after": "120" } })
            : new Response(`{}`),
        ),
      t,
    );
    assertEquals(netDue.size, RATE_LIMIT_KEYS_MAX, "the due-time map settles at its bound");
    assertEquals(hostDue.size, RATE_LIMIT_KEYS_MAX, "and so does the per-host holdoff");
    assert(netDue.has(id), "the sheet that just polled keeps its due time");
    assert(hostDue.has("crowded.test"), "and the host that just asked to wait keeps its holdoff");
    assert(!netDue.has("net-http:ghost-0"), "what goes is the oldest key, not the newest");
    assert(!hostDue.has("ghost-0.test"), "on both maps, by the one rule");
    // Nothing after this polls, and 10,000 ghosts are not this suite's state.
    netDue.clear();
    hostDue.clear();
  });

  await t.step("MCP server: JSON-RPC 2.0 over POST /mcp/:id", async () => {
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
      assertEquals(read.structuredContent.rows, [{ item: "apple", price: 2.25 }, { item: "banana", price: 0.5 }]);
    }

    // The two read tools spell a row the same way. They did not: read_sheet
    // answered {"0":"apple"} off the stored document while query_sheet answered
    // {item:"apple"} off the engine, so a model reading one sheet through both
    // had to guess which convention it was holding.
    {
      const read = await call(jwt, "read_sheet", {});
      const queried = await call(jwt, "query_sheet", { code: `select item, price from @${sheet_id} order by item` });
      assertEquals(read.structuredContent.rows, queried.structuredContent.rows);
    }

    // write_cells failures name the problem.
    {
      const badCol = await call(jwt, "write_cells", { cells: [{ row: 0, col: "nope", value: "x" }] });
      assertEquals(badCol.isError, true);
      assert(badCol.content[0].text.includes("item"), `should list valid columns, got: ${badCol.content[0].text}`);
      const badType = await call(jwt, "write_cells", { cells: [{ row: 0, col: "price", value: "expensive" }] });
      assertEquals(badType.isError, true);
      assert(
        badType.content[0].text.includes("usd") && badType.content[0].text.includes("price"),
        badType.content[0].text,
      );
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
  });

  // The boundary is the whole feature: a row that does not match the columns, or a value that does not parse at its
  // declared type, has to be refused naming what arrived -- and refused with the sheet exactly as it was, because a
  // half-written batch under a 201 is the silent failure this endpoint exists without.
  await t.step("A sheet takes rows over its own API", async () => {
    const { jwt } = await usr("nina@example.com");
    const hand = automerge.create<Sheet>({
      type: "table",
      data: [
        arrayify([{ name: "item", type: "text", key: "0" }, { name: "qty", type: "int", key: "1" }]),
        { 0: "apple", 1: 3 },
      ],
    });
    const sheet_id = `table:${hand.documentId}`;
    await put(jwt, `/library/${sheet_id}`, {});

    // Appended rows are the same rows the stable read and the query engine see.
    {
      const { data } = await post(jwt, `/sheet/${sheet_id}`, {
        rows: [{ item: "banana", qty: 2 }, { item: "cherry", qty: 5 }],
      });
      assertEquals(data, { appended: 2, rows: 3 });
      // The same rows in the same spelling they were sent in. They were not:
      // the write took {item, qty} and the read answered {0, 1}, so one
      // endpoint pair had two spellings of one row and the generated spec had
      // to say so.
      const read = await get<Table>(jwt, `/sheet/${sheet_id}`);
      assertEquals(read.slice(1), [
        { item: "apple", qty: 3 },
        { item: "banana", qty: 2 },
        { item: "cherry", qty: 5 },
      ]);
      const { data: [, ...rows] }: { data: Table } = await post(jwt, `/query`, {
        lang: "sql",
        code: `select sum(qty) as n from @${sheet_id}`,
        args: [],
      });
      assertEquals(rows, [{ n: 10 }]);
    }

    const refused = async (body: unknown, id = sheet_id, auth = jwt) => {
      const res = await app.request(`/sheet/${id}`, {
        method: "POST",
        headers: new Headers({ "Content-Type": "application/json", Authorization: `Bearer ${auth}` }),
        body: JSON.stringify(body),
      });
      assert(400 <= res.status && res.status < 500, `Expected a 4xx, received ${res.status}`);
      return { status: res.status, text: await res.text() };
    };

    // A sheet with no column row of its own is refused by name, not with a 500:
    // a query computes its rows, so appending to one is writing into an answer.
    {
      const q = automerge.create<Sheet>({
        type: "query",
        data: [{ lang: "sql", code: `select 1 as n`, args: [] }],
      });
      await put(jwt, `/library/query:${q.documentId}`, {});
      const { text } = await refused({ rows: [{ n: 1 }] }, `query:${q.documentId}`);
      assert(text.includes("a query sheet") && text.includes("table sheet"), text);
    }

    // Wrong field count, both directions, naming both counts and the row as sent.
    {
      const short = await refused({ rows: [{ item: "date" }] });
      assert(short.text.includes("2 fields: item, qty"), short.text);
      assert(short.text.includes(`nothing for "qty"`), short.text);
      assert(short.text.includes(`{"item":"date"}`), short.text);
      const long = await refused({ rows: [{ item: "date", qty: 1, colour: "brown" }] });
      assert(long.text.includes(`an extra field "colour"`), long.text);
    }

    // Wrong value type, named by row, by declared type and by value -- the same
    // check every table sheet meets on the way into a query, so nothing this
    // route accepts can be something a query then refuses.
    {
      const { text } = await refused({ rows: [{ item: "date", qty: "lots" }] });
      assert(text.includes("qty") && text.includes("int") && text.includes(`"lots"`), text);
      // A cell holds a scalar, so an object is refused rather than stored as one.
      const nested = await refused({ rows: [{ item: { en: "date" }, qty: 1 }] });
      assert(nested.text.includes("no cell can hold"), nested.text);
      const empty = await refused({ rows: [] });
      assert(empty.text.includes("empty rows array"), empty.text);
    }

    // No access at all, and read access that is not write access.
    {
      const stranger = await usr("pablo@example.com");
      const denied = await refused({ rows: [{ item: "date", qty: 1 }] }, sheet_id, stranger.jwt);
      assertEquals(denied.status, 403);
      assert(denied.text.includes("no role on this sheet"), denied.text);
      const looker = await usr("ulla@example.com");
      await post(jwt, `/library/${sheet_id}/share`, { email: "ulla@example.com", role: "viewer" });
      const viewer = await refused({ rows: [{ item: "date", qty: 1 }] }, sheet_id, looker.jwt);
      assert(viewer.text.includes("your role on this sheet is viewer"), viewer.text);
    }

    // Every refusal above left the sheet where it was.
    assertEquals((await get<Table>(jwt, `/sheet/${sheet_id}`)).length, 4);
  });

  // Minting one is rotating it, so the key before it still opens the sheet and the one before that is retired; and a
  // key that could reach a second sheet, or mint itself another, would be a JWT with extra steps -- which is the
  // whole reason this exists.
  await t.step("A script carries a key for one sheet rather than a person's JWT", async () => {
    const { jwt } = await usr("rosa@example.com");
    const sheetOf = async (name: string) => {
      const hand = automerge.create<Sheet>({
        type: "table",
        data: [arrayify([{ name: "n", type: "int", key: "0" }]), { 0: 1 }],
      });
      const sheet_id = `table:${hand.documentId}`;
      await put(jwt, `/library/${sheet_id}`, { name });
      return sheet_id;
    };
    const a = await sheetOf("a");
    const b = await sheetOf("b");

    const withKey = (key: string, path: string, options?: object) =>
      app.request(path, {
        headers: new Headers({ "Content-Type": "application/json", "scrapsheets-key": key }),
        ...options,
      });
    const mint = async (id: string) => {
      const res = await app.request(`/library/${id}/secret`, {
        method: "POST",
        headers: new Headers({ "Content-Type": "application/json", Authorization: `Bearer ${jwt}` }),
        body: JSON.stringify({ name: "api" }),
      });
      assertEquals(res.status, 201);
      // A key is answered once, so it may not sit in a shared cache on the way.
      assertEquals(res.headers.get("cache-control"), "no-store");
      const { data } = await res.json();
      assert(data.key.startsWith(`${id}.`), data.key);
      assert(data.repro.includes("scrapsheets-key"), data.repro);
      return data.key as string;
    };

    const first = await mint(a);

    // The key reads and writes its own sheet, as its owner.
    {
      assertEquals((await withKey(first, `/sheet/${a}`)).status, 200);
      const wrote = await withKey(first, `/sheet/${a}`, { method: "POST", body: JSON.stringify({ rows: [{ n: 2 }] }) });
      assertEquals(wrote.status, 201);
      assertEquals((await get<Table>(jwt, `/sheet/${a}`)).slice(1), [{ n: 1 }, { n: 2 }]);
    }

    // And nothing else. Not another sheet, and not the route that mints keys --
    // a key that could mint would carry the owner's whole authority after all.
    {
      const other = await withKey(first, `/sheet/${b}`);
      assertEquals(other.status, 403);
      const text = await other.text();
      assert(text.includes(`opens ${a}`) && text.includes(`GET /sheet/${b}`), text);
      assertEquals((await withKey(first, `/library/${a}/secret`)).status, 403);
      assertEquals(
        (await withKey(first, `/library/${b}/secret`, { method: "POST", body: JSON.stringify({ name: "api" }) }))
          .status,
        403,
      );
      assertEquals((await withKey(`${a}.deadbeef`, `/sheet/${a}`)).status, 401);
      assertEquals((await withKey("not-a-key-at-all", `/sheet/${a}`)).status, 401);
    }

    // Current and previous both open the sheet; the one before that is retired.
    {
      const second = await mint(a);
      assertEquals((await withKey(first, `/sheet/${a}`)).status, 200);
      assertEquals((await withKey(second, `/sheet/${a}`)).status, 200);
      const third = await mint(a);
      assertEquals((await withKey(first, `/sheet/${a}`)).status, 401);
      assertEquals((await withKey(second, `/sheet/${a}`)).status, 200);
      assertEquals((await withKey(third, `/sheet/${a}`)).status, 200);

      // The names are readable and the values are not, the same rule every other
      // sheet secret follows: a value that can be read back is a value a share
      // link can eventually be pointed at.
      const { secrets } = await get<{ secrets: { name: string }[] }>(jwt, `/library/${a}/secret`);
      assertEquals(secrets.map((s) => s.name), ["api"]);
      const listed = JSON.stringify(secrets);
      for (const key of [first, second, third]) assert(!listed.includes(key.split(".")[1]), listed);

      // Revoked is refused, and revoking takes the previous one with it.
      await request(jwt, `/library/${a}/secret`, { method: "DELETE", body: JSON.stringify({ name: "api" }) });
      assertEquals((await withKey(third, `/sheet/${a}`)).status, 401);
      assertEquals((await withKey(second, `/sheet/${a}`)).status, 401);
    }

    // The key is minted here, never supplied, and nothing else in its namespace
    // is a name the middleware knows.
    {
      await reject(jwt, `/library/${a}/secret`, {
        method: "POST",
        body: JSON.stringify({ name: "api", value: "sk-i-picked-this" }),
      });
      await reject(jwt, `/library/${a}/secret`, {
        method: "POST",
        body: JSON.stringify({ name: "api:readonly", value: "x" }),
      });
    }
  });

  // The spec is derived from the sheet's own columns at request time and never stored, so it cannot claim a column
  // the sheet does not have. It is what the keys point at: a key with nothing to read from is a key nobody can use.
  await t.step("The OpenAPI spec is derived from the sheet's own columns and never stored", async () => {
    const { jwt } = await usr("sven@example.com");
    const hand = automerge.create<Sheet>({
      type: "table",
      data: [
        arrayify([
          { name: "item", type: "text", key: "0" },
          { name: "qty", type: "int", key: "1" },
          { name: "price", type: "usd", key: "2" },
          { name: "due", type: "date", key: "3" },
          { name: "paid", type: "bool", key: "4" },
        ]),
      ],
    });
    const sheet_id = `table:${hand.documentId}`;
    await put(jwt, `/library/${sheet_id}`, {});
    const doc = await request(jwt, `/openapi/${sheet_id}`);

    assertEquals(doc.openapi, "3.1.0");
    assertEquals(doc.components.schemas.Row.required, ["item", "qty", "price", "due", "paid"]);
    assertEquals(doc.components.schemas.Row.properties, {
      item: { type: "string" },
      qty: { type: "integer" },
      price: { type: "number" },
      due: { type: "string", format: "date" },
      paid: { type: "boolean" },
    });
    // The header a key is presented in is part of the document, or the spec
    // describes a sheet nobody can call.
    assertEquals(doc.components.securitySchemes.sheetKey.name, "scrapsheets-key");
    const path = doc.paths[`/sheet/${sheet_id}`];
    assert(path.get, "the read is described");
    // And described by the same Row the write takes, which is the whole of the
    // read/write agreement: the document used to say the read was keyed by
    // column key and hand back an untyped object, so a generated client had a
    // type for what it sent and none for what came back.
    assertEquals(
      path.get.responses["200"].content["application/json"].schema.properties.data.items,
      { $ref: "#/components/schemas/Row" },
    );
    assertEquals(path.post.requestBody.content["application/json"].schema.properties.rows.items, {
      $ref: "#/components/schemas/Row",
    });

    // A query sheet has a read and no write, because appending to one is a 400.
    {
      const q = automerge.create<Sheet>({
        type: "query",
        data: [{ lang: "sql", code: `select 1 as n, 'x' as t`, args: [] }],
      });
      await put(jwt, `/library/query:${q.documentId}`, {});
      const spec = await request(jwt, `/openapi/query:${q.documentId}`);
      assertEquals(Object.keys(spec.components.schemas.Row.properties), ["n", "t"]);
      assert(!spec.paths[`/sheet/query:${q.documentId}`].post, "a computed sheet is not writable");
    }

    // The spec inherits the sheet's own access rule, because it is read through it.
    {
      const stranger = await usr("tomas@example.com");
      await reject(stranger.jwt, `/openapi/${sheet_id}`);
    }
  });

  // Proxy guards. Each rejection must name its own cause, never a bare status.
  await t.step("Every proxy rejection names its own cause, never a bare status", async () => {
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
      // The scheme it refused, not just that it refused one.
      assertEquals(code, 400);
      assert(error.includes("ftp:") && error.includes("https://"), error);
      // A failure has to be reproducible by hand, not just described.
      assertEquals(repro, "curl -i 'ftp://example.com/x'");
    }
    for (
      const [url, host] of [
        ["http://localhost/x", "localhost"],
        ["http://foo.local/x", "foo.local"],
        ["http://127.0.0.1/x", "127.0.0.1"],
        ["http://169.254.169.254/latest/meta-data", "169.254.169.254"],
      ]
    ) {
      const { code, error, repro } = await proxy(url);
      assertEquals([code, repro], [400, `curl -i '${url}'`], url);
      assert(error.includes(host), `${url} should be refused by name, got: ${error}`);
    }
    const { code, error } = await proxy("notaurl");
    assertEquals(code, 502);
    assert(
      error.includes("Invalid URL") && error.includes("notaurl"),
      `A malformed url should say which url is malformed, got: ${error}`,
    );
  });

  await t.step("Every seeded example must survive a visit with no ?q= param", async () => {
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
  });

  await t.step("Stripe checkout regressions", async () => {
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
      await post(sellerJwt, `/sell/table:${hand.documentId}`, { price: 5, license: "own" });
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
  });

  // Signing. Without it, anyone who learns a net sheet's id can write rows to it, so every rejection has to say which
  // half of the handshake was wrong -- and none of them may print the secret, which would make the message an oracle.
  await t.step("Every delivery must be signed, and each rejection names its own check", async () => {
    const { jwt } = await usr("wes@example.com");
    const hand = automerge.create<Sheet>({ type: "net-hook", data: [] });
    const id = `net-hook:${hand.documentId}`;
    await put(jwt, `/library/${id}`, {});
    const secret = await hookSecret(id);
    const body = JSON.stringify({ event: "signed" });
    const send = (signature: string | null) =>
      app.request(`/net/${id}`, {
        method: "POST",
        headers: new Headers({
          "Content-Type": "application/json",
          ...(signature === null ? {} : { "scrapsheets-signature": signature }),
        }),
        body,
      });

    const target = `/net/${id}`;
    const first = await hookSign(secret, target, body);
    assert((await send(first)).ok, "a correctly signed delivery must land");

    // A captured delivery, sent again unchanged. The skew window bounds a replay
    // to five minutes, which is not the same as never -- so the same signature
    // is refused rather than stored twice. 409 and not 401: the four checks
    // above are about signing, and the status check reads a 401 on /net/ as a
    // sender that cannot sign.
    {
      const again = await send(first);
      assertEquals(again.status, 409);
      const text = await again.text();
      assert(text.includes("already been stored"), `the rejection must name the replay, got: ${text}`);
      assert(!text.includes(secret), "a rejection that prints the secret is a signing oracle");
      const [{ n }] = await sql`select count(*) as n from net where sheet_id = ${id}`;
      assertEquals(Number(n), 1, "a replayed delivery must not be stored twice");
    }

    // Ten copies at once. Selecting first and inserting after let every one of
    // them see no prior row and land, so the control was bypassed by the least
    // sophisticated version of the attack it exists for. The unique index is
    // what decides it; the insert is what asks.
    {
      const burst = JSON.stringify({ event: "burst" });
      const signature = await hookSign(secret, target, burst);
      const answers = await Promise.all(
        Array.from({ length: 10 }, () =>
          app.request(`/net/${id}`, {
            method: "POST",
            headers: new Headers({ "Content-Type": "application/json", "scrapsheets-signature": signature }),
            body: burst,
          })),
      );
      assertEquals(answers.filter((res) => res.ok).length, 1, "exactly one copy of a burst may land");
      assertEquals(answers.filter((res) => res.status === 409).length, 9);
      const [{ n }] = await sql`select count(*) as n from net where sheet_id = ${id} and body = ${burst}`;
      assertEquals(Number(n), 1, "a concurrent replay must not be stored twice");
    }

    const refused = async (signature: string | null, says: string) => {
      const res = await send(signature);
      assertEquals(res.status, 401, `expected 401 for ${says}`);
      const text = await res.text();
      assert(text.includes(says), `the rejection must name its own check, got: ${text}`);
      assert(!text.includes(secret), "a rejection that prints the secret is a signing oracle");
      return text;
    };
    await refused(null, "is not signed");
    await refused("v1=nope", "is malformed");
    // An upper-cased v1 verifies against the same secret -- parseInt does not
    // care about case -- while reading as a different string, so every case
    // variant of one captured signature used to be a free replay. A hex digest
    // has one spelling here.
    const upper = (await hookSign(secret, target, body)).replace(/v2=(.*)$/, (_, hex) => `v2=${hex.toUpperCase()}`);
    assert(/^t=\d+,v2=[0-9A-F]{64}$/.test(upper), `only the digest may be upper-cased, got: ${upper}`);
    await refused(upper, "is malformed");
    // Signed correctly, but for a moment outside the replay window.
    const stale = Math.floor(Date.now() / 1000) - 3600;
    const staleText = await refused(await hookSign(secret, target, body, stale), "outside the replay window");
    assert(staleText.includes("3600 seconds old"), `it must name the skew, got: ${staleText}`);
    // The right shape over the wrong bytes: the one failure a sender misreads as
    // a wrong secret.
    await refused(await hookSign(secret, target, body + " "), "does not match its body");
    // The right shape under another sheet's secret.
    const other = automerge.create<Sheet>({ type: "net-hook", data: [] });
    await refused(
      await hookSign(await hookSecret(`net-hook:${other.documentId}`), target, body),
      "does not match its body",
    );

    // The owner reads the secret; nobody else does, and a non-net sheet has none.
    const hook = await get<{ url: string; secret: string; repro: string }>(jwt, `/library/${id}/hook`);
    assertEquals(hook.secret, secret);
    assert(hook.url.endsWith(`/net/${id}`), `the hook url must be the delivery url, got: ${hook.url}`);
    assert(hook.repro.includes(secret) && hook.repro.includes("openssl"), "the repro must be runnable");
    assert(hook.repro.includes("v2=$sig"), `the repro must sign the way the server verifies, got: ${hook.repro}`);
    const { jwt: strangerJwt } = await usr("wanda@example.com");
    await reject(strangerJwt, `/library/${id}/hook`);
    const table = automerge.create<Sheet>({ type: "table", data: [arrayify([{ name: "a", type: "text", key: 0 }])] });
    await put(jwt, `/library/table:${table.documentId}`, {});
    await reject(jwt, `/library/table:${table.documentId}/hook`);
  });

  // The four share routes read a JSON body, and all four handed a missing or unparseable one straight to
  // c.req.json(). That throw was an unexplained 500: a row in net-hook:errors and a point off the 5xx grade, for a
  // mistake the caller could have fixed from the message. A share link is a bearer credential that travels in a URL,
  // so it may not be immortal and it may not be openable by everyone who sees the url. The lock is an HMAC of the
  // password under the server's own root secret, not the password and not a hash of it: nothing new is stored, and
  // holding the link buys nobody an offline guess. Every refusal here has to name its own check without printing the
  // password or the digest -- the same oracle rule the delivery refusals follow.
  await t.step("A share route with a missing or unparseable body is the caller's error", async () => {
    const { jwt } = await usr("zara@example.com");
    const hand = automerge.create<Sheet>({
      type: "table",
      data: [arrayify([{ name: "a", type: "num", key: 0 }]), { 0: 7 }],
    });
    const sheet_id = `table:${hand.documentId}`;
    await put(jwt, `/library/${sheet_id}`, {});
    const raw = (route: string, options?: object) =>
      app.request(route, {
        headers: new Headers({ "Content-Type": "application/json", Authorization: `Bearer ${jwt}` }),
        ...options,
      });

    for (
      const [method, route] of [
        ["POST", `/library/${sheet_id}/share`],
        ["DELETE", `/library/${sheet_id}/share`],
        ["POST", `/library/${sheet_id}/public`],
      ] as const
    ) {
      for (const body of [undefined, "not json"]) {
        const res = await raw(route, { method, ...(body === undefined ? {} : { body }) });
        assertEquals(
          res.status,
          400,
          `${method} ${route} with ${body ?? "no body"} is the caller's error: ${await res.text()}`,
        );
      }
    }
    // DELETE validated nothing at all, so a missing email reached the delete and
    // came back as "undefined is not a non-owner member of this sheet" -- a
    // sentence about the sheet for a mistake in the request.
    const noEmail = await raw(`/library/${sheet_id}/share`, { method: "DELETE", body: JSON.stringify({}) });
    assertEquals(noEmail.status, 400);
    const noEmailText = await noEmail.text();
    assert(
      /Received: {4}\S/.test(noEmailText) && !noEmailText.includes("member of this sheet"),
      `the refusal has to name what arrived, not what the sheet holds: ${noEmailText}`,
    );

    // The link route's body is entirely optional, so no body is not an error
    // there: that is the page's own call, and it must keep minting the
    // thirty-day unlocked link it always did.
    const day = 60 * 60 * 24;
    const expOf = (token: string) => {
      const b64 = token.split(".")[1].replace(/-/g, "+").replace(/_/g, "/");
      return JSON.parse(atob(b64 + "=".repeat((4 - b64.length % 4) % 4))) as { exp: number; lock?: string };
    };
    const now = Math.floor(Date.now() / 1000);
    {
      const res = await raw(`/library/${sheet_id}/link`, { method: "POST" });
      const body = await res.text();
      assert(res.ok, `an empty body still mints a link: ${res.status} ${body}`);
      const { data: { token } } = JSON.parse(body);
      assert(Math.abs(expOf(token).exp - (now + 30 * day)) < 120, "and it still lives thirty days");
      assert(!expOf(token).lock, "and it is still openable by anyone holding it");
    }
    // A body that is not JSON is not "no options": it is a request the caller
    // meant to send and got wrong, and defaulting it minted a link nobody asked
    // for in that shape.
    await reject(jwt, `/library/${sheet_id}/link`, { method: "POST", body: "not json" });

    const { data: { token: plain } }: { data: { token: string } } = await post(jwt, `/library/${sheet_id}/link`, {});
    const { data: { token: short } }: { data: { token: string } } = await post(
      jwt,
      `/library/${sheet_id}/link`,
      { days: 1 },
    );
    assert(
      Math.abs(expOf(short).exp - (now + day)) < 120,
      `a chosen expiry has to reach the token: ${expOf(short).exp - now} seconds`,
    );
    // Zero, negative, over the bound, the right number as a string, and null.
    for (const days of [0, -1, 366, "7", null])
      await reject(jwt, `/library/${sheet_id}/link`, { method: "POST", body: JSON.stringify({ days }) });
    for (const password of ["", 7, null, "x".repeat(129)])
      await reject(jwt, `/library/${sheet_id}/link`, { method: "POST", body: JSON.stringify({ password }) });

    const password = "correct horse battery staple";
    const { data: { token: locked } }: { data: { token: string } } = await post(
      jwt,
      `/library/${sheet_id}/link`,
      { password, days: 7 },
    );
    const digest = expOf(locked).lock;
    assert(digest, "a locked link carries the lock claim");
    assert(!locked.includes(btoa(password)), "and never the password itself");

    // The socket refuses in the handshake, before any frame exists to carry a
    // message, so the refusal is an ordinary HTTP answer on the sync route --
    // which is why it is asked for here as one. A real client sees the
    // connection fail; only this shape can read what it says.
    const opens = (pass?: string) =>
      app.request(
        `/library/sync?auth=Bearer%20${encodeURIComponent(locked)}` +
          (pass === undefined ? "" : `&pass=${encodeURIComponent(pass)}`),
      );
    const bare = await opens();
    const wrong = await opens("wrong horse");
    assertEquals(bare.status, 401);
    assertEquals(wrong.status, 401);
    const bareText = await bare.text();
    const wrongText = await wrong.text();
    assert(bareText.includes("is locked"), bareText);
    assert(wrongText.includes("does not open"), wrongText);
    for (const text of [bareText, wrongText]) {
      assert(!text.includes(password), "a refusal must not print the password it wanted");
      assert(!text.includes("wrong horse"), "nor the one that was tried");
      assert(!text.includes(digest!), "nor the digest an offline guesser would grind against");
    }
    // The right password clears the lock: what answers now is the route past
    // it, refusing a plain GET that asked for no upgrade.
    const right = await opens(password);
    const rightText = await right.text();
    assert(
      !rightText.includes("is locked") && !rightText.includes("does not open"),
      `the right password clears the lock: ${right.status} ${rightText}`,
    );

    // And end to end over a real socket, which is the only path the share claim
    // has ever had. A link gated on the socket and open anywhere else would be
    // worse than no gate, so the claim is read in exactly one place.
    const server = Deno.serve({ hostname: "127.0.0.1", port: 0, onListen() {} }, app.fetch);
    const reads = async (peer: string, token: string, pass?: string) => {
      const adapter = new WebSocketClientAdapter(
        `ws://127.0.0.1:${server.addr.port}/library/sync?auth=Bearer%20${encodeURIComponent(token)}` +
          (pass === undefined ? "" : `&pass=${encodeURIComponent(pass)}`),
        100,
      );
      const repo = new AM.Repo({ network: [adapter], peerId: peer as AM.PeerId });
      const outcome = await Promise.race([
        repo.find<Sheet>(hand.documentId).then(() => "shared", () => "denied"),
        new Promise<string>((resolve) => setTimeout(() => resolve("refused"), 2000)),
      ]);
      adapter.disconnect();
      return outcome;
    };
    assertEquals(await reads("client-locked-ok", locked, password), "shared", "the right password opens the link");
    assertEquals(await reads("client-plain", plain), "shared", "an unlocked link is untouched by any of this");
    // There is deliberately no `reads(..., locked)` with no password here: the
    // refusal lands in the handshake, and a client that meets it throws out of
    // the ws library rather than resolving to anything a test can read. That is
    // the fact itself, not a gap -- a browser cannot read that 401 either,
    // which is why the page reads the lock claim off the token and asks for the
    // password before it opens the socket. The HTTP shape above is the only
    // place the message can be asserted.
    await server.shutdown();

    // A portal honours no share claim, so a locked token buys nothing there --
    // but it must not be refused there either. The call validates the token and
    // throws the role away; without the password beside it, the lock check
    // fired on a socket that was never going to grant anything.
    const portalOpen = (pass?: string) =>
      app.request(
        `/portal/time/sync?auth=Bearer%20${encodeURIComponent(locked)}` +
          (pass === undefined ? "" : `&pass=${encodeURIComponent(pass)}`),
      );
    assertEquals((await portalOpen()).status, 401, "a locked link with no password is still refused");
    const portalPassed = await portalOpen(password);
    assert(
      !(await portalPassed.text()).includes("is locked"),
      "and the password clears it there too, rather than there being no way to give one",
    );

    // Over plain HTTP the share claim buys nothing, locked or not: verifyWsAuth
    // is the only place it is read, and no HTTP route reads it. What this pins
    // is the next HTTP read path that honours the claim without going through
    // that one reader, which would be a link gated on the socket and open
    // everywhere else -- worse than no gate.
    //
    // Refused, and refused as the caller's mistake. A share token names a sheet
    // and carries no `sub`, so usr_id was undefined and postgres.js refused to
    // interpolate it: the holder of a link this server minted got 500 "Sorry,
    // something went wrong", a row in the operator's log, and a point off the
    // 5xx grade -- for using the credential we handed them. Nothing about that
    // request is the server's failure, so nothing about it may be logged as one.
    const crashes = async (path: string) => {
      const [{ n }] = await sql`
        select count(*) as n from net
        where sheet_id = 'net-hook:errors' and meta->>'path' = ${path}
          and coalesce(substring(meta->>'status' from '^[0-9]{1,9}$')::int, 0) >= 500
      `;
      return Number(n);
    };
    for (const token of [plain, locked]) {
      const res = await app.request(`/sheet/${sheet_id}`, {
        headers: new Headers({ Authorization: `Bearer ${token}` }),
      });
      const said = await res.text();
      assertEquals(res.status, 403, `a share token must be refused, not crash: ${res.status} ${said}`);
      assert(said.includes("sync socket"), `and the refusal must say where the token does work: ${said}`);
    }
    await errorLogged();
    assertEquals(await crashes(`/sheet/${sheet_id}`), 0, "a refusal the caller can fix is not the operator's failure");
  });

  await t.step("A sheet's own secrets", async () => {
    const { jwt } = await usr("xena@example.com");
    const hand = automerge.create<Sheet>({ type: "net-hook", data: [] });
    const id = `net-hook:${hand.documentId}`;
    await put(jwt, `/library/${id}`, {});
    const target = `/net/${id}`;
    const derived = await hookSecret(id);
    const send = (sheet: string, headers: Record<string, string>, body: string, query = "") =>
      app.request(`/net/${sheet}${query}`, {
        method: "POST",
        headers: new Headers({ "Content-Type": "application/json", ...headers }),
        body,
      });
    const signed = (signature: string, body: string, query = "") =>
      send(id, { "scrapsheets-signature": signature }, body, query);
    const hmac = async (secret: string, message: string) => {
      const key = await crypto.subtle.importKey(
        "raw",
        new TextEncoder().encode(secret),
        { name: "HMAC", hash: "SHA-256" },
        false,
        ["sign"],
      );
      return new Uint8Array(await crypto.subtle.sign("HMAC", key, new TextEncoder().encode(message)));
    };
    const hexOf = (buf: Uint8Array) => Array.from(buf).map((b) => b.toString(16).padStart(2, "0")).join("");
    // A v1 sender, written the way the old readme told it to: the timestamp, a
    // dot, and the body. Hand-rolled on purpose -- this is the one scheme where
    // an independent implementation is the point, because it is what every
    // sender already in the field has.
    const signV1 = async (secret: string, body: string, t = Math.floor(Date.now() / 1000)) =>
      `t=${t},v1=${hexOf(await hmac(secret, `${t}.${body}`))}`;

    // What identifies a delivery is what the sender varies. Two of them, the
    // same body in the same second, discriminated by query string: under v1
    // they were one delivery and the second was refused as a replay.
    const reading = JSON.stringify({ reading: 21.5 });
    const second = Math.floor(Date.now() / 1000);
    const one = await signed(await hookSign(derived, `${target}?sensor=1`, reading, second), reading, "?sensor=1");
    const two = await signed(await hookSign(derived, `${target}?sensor=2`, reading, second), reading, "?sensor=2");
    assert(one.ok && two.ok, "two deliveries differing only by query string are two deliveries");
    // Signing the target is the whole of the fix, so a v2 signature over one
    // path must not verify against another.
    const moved = await signed(await hookSign(derived, `${target}?sensor=1`, reading, second), reading, "?sensor=9");
    assertEquals(moved.status, 401, "a signature over one path must not carry to another");

    // Verified bytes that Postgres text cannot hold. The signature is right, so
    // this reaches the insert -- and used to come back as an unexplained 500.
    const nulBody = "{\u0000}";
    const nulRes = await signed(await hookSign(derived, target, nulBody), nulBody);
    assertEquals(nulRes.status, 400, "a byte that cannot be stored is a rejection, not a 500");
    const nulText = await nulRes.text();
    assert(nulText.includes("NUL byte at offset 1"), `it must name the byte and where: ${nulText}`);
    // Every other byte is kept as sent, valid UTF-8 or not.
    const oddBody = new TextDecoder().decode(new Uint8Array([0x22, 0xff, 0xfe, 0x22]));
    assert((await signed(await hookSign(derived, target, oddBody), oddBody)).ok, "invalid UTF-8 still lands");

    // v1 is still accepted, because refusing it would be the missed delivery
    // this whole section exists to avoid -- and meta.scheme is how you find out
    // who is still sending it.
    const legacy = JSON.stringify({ event: "legacy" });
    assert((await signed(await signV1(derived, legacy), legacy)).ok, "an old sender must keep working");
    const scheme = async (body: string) => {
      const [row] = await sql`select meta->>'scheme' as s, meta->>'secret_at' as at from net
                              where sheet_id = ${id} and body = ${body} limit 1`;
      return row as { s: string; at: string };
    };
    assertEquals((await scheme(legacy)).s, "v1", "the row records which scheme verified it");
    assertEquals((await scheme(reading)).s, "v2");
    assertEquals((await scheme(legacy)).at, "derived", "and which key, so a rollover has a visible end");

    // Rotation. Writing a secret is rotating it: the new one signs, the one
    // before it still verifies, and the derived key is the implicit previous
    // until a second rotation retires it -- without which storing a sheet's
    // first secret would drop every sender still using the derived one.
    const say = async (secret: string, note: string) => {
      const body = JSON.stringify({ note });
      return { res: await signed(await hookSign(secret, target, body), body), body };
    };
    await post(jwt, `/library/${id}/secret`, { name: "hook", value: "rolled-one" });
    assert(
      (await say(derived, "derived-after-first")).res.ok,
      "the first stored secret does not retire the derived one",
    );
    const first = await say("rolled-one", "first");
    assert(first.res.ok, "the stored secret signs");
    const firstAt = (await scheme(first.body)).at;
    assert(firstAt !== "derived", `the row must name the stored key, got: ${firstAt}`);

    await post(jwt, `/library/${id}/secret`, { name: "hook", value: "rolled-two" });
    assert((await say("rolled-one", "still-previous")).res.ok, "the previous secret still verifies");
    assert((await say("rolled-two", "current")).res.ok, "and so does the current one");
    assertEquals(
      (await say(derived, "derived-retired")).res.status,
      401,
      "a second rotation retires the derived key",
    );

    // Names and timestamps come back. A value never does: one that can be read
    // back is one a share link can eventually be pointed at.
    const listed = await get<{ secrets: { name: string; created_at: string; previous_at: string }[] }>(
      jwt,
      `/library/${id}/secret`,
    );
    assertEquals(listed.secrets.map((x) => x.name), ["hook"]);
    assert(listed.secrets[0].previous_at, "a rollover in progress says so");
    assert(!JSON.stringify(listed).includes("rolled-two"), "a secret value must never be readable");
    const { jwt: strangerJwt } = await usr("yuri@example.com");
    await reject(strangerJwt, `/library/${id}/secret`);
    await reject(jwt, `/library/${id}/secret`, { method: "POST", body: JSON.stringify({ name: "Hook!", value: "x" }) });
    await reject(jwt, `/library/${id}/secret`, { method: "POST", body: JSON.stringify({ name: "hook", value: "" }) });
    // A body that is missing or is not JSON must be answered, not thrown at.
    for (const options of [{ method: "POST" }, { method: "DELETE" }, { method: "POST", body: "not json" }]) {
      const res = await app.request(`/library/${id}/secret`, {
        headers: new Headers({ "Content-Type": "application/json", Authorization: `Bearer ${jwt}` }),
        ...options,
      });
      assert(res.status >= 400 && res.status < 500, `a malformed body is the caller's error: ${res.status}`);
    }
    // `hook:*` is the space verifyDelivery reads, so a name in it that no
    // verifier knows would be written happily and then fail every delivery.
    await reject(jwt, `/library/${id}/secret`, {
      method: "POST",
      body: JSON.stringify({ name: "hook:acme", value: "x" }),
    });
    // A general name is fine, and stays out of the verifier's way.
    await post(jwt, `/library/${id}/secret`, { name: "weather-api-key", value: "k" });
    assert((await say("rolled-two", "still-ours")).res.ok, "an unrelated secret does not change the scheme");

    // The hook panel shows the current key, not the derived one it replaced.
    const hook = await get<{ secret: string; repro: string }>(jwt, `/library/${id}/hook`);
    assertEquals(hook.secret, "rolled-two", "the panel shows the key that is signing now");
    // A secret is pasted by hand. One apostrophe in it ended the shell quoting,
    // so the documented line ran and signed something else.
    const quotey = automerge.create<Sheet>({ type: "net-hook", data: [] });
    const quoteyId = `net-hook:${quotey.documentId}`;
    await put(jwt, `/library/${quoteyId}`, {});
    await post(jwt, `/library/${quoteyId}/secret`, { name: "hook", value: "a'b" });
    const quoted = await get<{ repro: string }>(jwt, `/library/${quoteyId}/hook`);
    assert(quoted.repro.includes(`-hmac 'a'\\''b'`), `the secret must stay inside its quotes: ${quoted.repro}`);

    // A provider signs its own way, and which verifier runs is read off the
    // sheet's stored secrets. A spoofed header must not be able to pick it.
    const gh = automerge.create<Sheet>({ type: "net-hook", data: [] });
    const ghId = `net-hook:${gh.documentId}`;
    await put(jwt, `/library/${ghId}`, {});
    const spoof = JSON.stringify({ action: "opened" });
    const spoofed = await app.request(`/net/${ghId}`, {
      method: "POST",
      headers: new Headers({
        "Content-Type": "application/json",
        "x-hub-signature-256": `sha256=${hexOf(await hmac("gh-secret", spoof))}`,
      }),
      body: spoof,
    });
    assertEquals(spoofed.status, 401, "a provider header on a sheet with no provider secret signs nothing");
    assert(
      (await spoofed.text()).includes("is not signed"),
      "and it is refused as unsigned, not verified against a secret nobody set",
    );

    await post(jwt, `/library/${ghId}/secret`, { name: "hook:github", value: "gh-secret" });
    const ghSign = async (body: string, secret = "gh-secret") => ({
      "x-hub-signature-256": `sha256=${hexOf(await hmac(secret, body))}`,
    });
    const ghBody = JSON.stringify({ action: "opened", number: 1 });
    const ghFirst = await send(ghId, await ghSign(ghBody), ghBody);
    assert(ghFirst.ok, `a GitHub delivery lands on its own scheme: ${ghFirst.status} ${await ghFirst.text()}`);
    // The replay index keys on the signature that verified. Keyed on a header
    // name instead, a captured provider delivery replayed with a junk
    // scrapsheets-signature beside it takes a new key and lands every time.
    assertEquals((await send(ghId, await ghSign(ghBody), ghBody)).status, 409, "a GitHub replay is refused");
    const decoy = await send(ghId, {
      ...(await ghSign(ghBody)),
      "scrapsheets-signature": `t=${Math.floor(Date.now() / 1000)},v2=${"0".repeat(64)}`,
    }, ghBody);
    assertEquals(decoy.status, 409, "a header the verifier never read must not buy a replay a new key");

    // The key is the digest that verified, canonically spelled -- not the header
    // as sent. Stripe's header is a tolerant field list, so a captured delivery
    // with one junk field appended verifies against the same secret over the
    // same message, and keyed on the raw header each junk suffix was a fresh
    // replay for the whole skew window.
    const st = automerge.create<Sheet>({ type: "net-hook", data: [] });
    const stId = `net-hook:${st.documentId}`;
    await put(jwt, `/library/${stId}`, {});
    await post(jwt, `/library/${stId}/secret`, { name: "hook:stripe", value: "whsec_test" });
    const stBody = JSON.stringify({ type: "checkout.session.completed" });
    const stamp = Math.floor(Date.now() / 1000);
    const stDigest = hexOf(await hmac("whsec_test", `${stamp}.${stBody}`));
    const stripeSig = (extra: string) => ({ "stripe-signature": `t=${stamp},v1=${stDigest}${extra}` });
    assert((await send(stId, stripeSig(""), stBody)).ok, "a Stripe delivery lands on its own scheme");
    assertEquals((await send(stId, stripeSig(""), stBody)).status, 409, "and its replay is refused");
    assertEquals((await send(stId, stripeSig(",z=1"), stBody)).status, 409, "a junk field must not buy a new key");
    assertEquals(
      (await send(stId, stripeSig(`,v1=${stDigest}`), stBody)).status,
      409,
      "nor may repeating the digest Stripe already sent",
    );

    // Shopify's digest is base64, and the last character of a 32-byte digest
    // carries two bits nothing reads -- four spellings, one digest. Shopify
    // signs no timestamp, so each spelling would be a replay that never stales.
    const sh = automerge.create<Sheet>({ type: "net-hook", data: [] });
    const shId = `net-hook:${sh.documentId}`;
    await put(jwt, `/library/${shId}`, {});
    await post(jwt, `/library/${shId}/secret`, { name: "hook:shopify", value: "shpss_test" });
    const shBody = JSON.stringify({ id: 1, total: "9.99" });
    const raw = await hmac("shpss_test", shBody);
    const b64of = (buf: Uint8Array) => btoa(Array.from(buf).map((b) => String.fromCharCode(b)).join(""));
    const canonical = b64of(raw);
    const ALPHABET = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
    const variants = [
      ...new Set(
        ALPHABET.split("").map((ch) => canonical.slice(0, 42) + ch + "=").filter((v) =>
          b64of(Uint8Array.from(atob(v), (ch) => ch.charCodeAt(0))) === canonical
        ),
      ),
    ];
    assert(variants.length > 1, `base64 must actually have slack here, got ${variants.length}`);
    assert((await send(shId, { "x-shopify-hmac-sha256": canonical }, shBody)).ok, "a Shopify delivery lands");
    for (const spelling of variants) {
      assertEquals(
        (await send(shId, { "x-shopify-hmac-sha256": spelling }, shBody)).status,
        409,
        `a re-spelled digest is the same delivery: ${spelling}`,
      );
    }
    const wrong = await send(ghId, await ghSign(ghBody, "not-it"), ghBody);
    assertEquals(wrong.status, 401);
    assert((await wrong.text()).includes("does not match its body"));
    // Our own header is no longer what this sheet is checked against.
    const ours = await send(ghId, {
      "scrapsheets-signature": await hookSign(await hookSecret(ghId), `/net/${ghId}`, ghBody),
    }, ghBody);
    assertEquals(ours.status, 401, "a sheet configured for GitHub is not also on our scheme");

    // One scheme per sheet: with two, the header a sender chose would decide
    // which check it faced. The insert is what asks, so two at once cannot both
    // land and leave the sheet refusing every delivery.
    await reject(jwt, `/library/${ghId}/secret`, {
      method: "POST",
      body: JSON.stringify({ name: "hook:stripe", value: "whsec_x" }),
    });
    const race = automerge.create<Sheet>({ type: "net-hook", data: [] });
    const raceId = `net-hook:${race.documentId}`;
    await put(jwt, `/library/${raceId}`, {});
    const both = await Promise.all(
      ["hook:stripe", "hook:shopify"].map((name) =>
        app.request(`/library/${raceId}/secret`, {
          method: "POST",
          headers: new Headers({ "Content-Type": "application/json", Authorization: `Bearer ${jwt}` }),
          body: JSON.stringify({ name, value: "v" }),
        })
      ),
    );
    assertEquals(both.filter((res) => res.ok).length, 1, "exactly one of two concurrent schemes may land");
    // Both landing leaves the sheet unable to answer at all: verifyDelivery
    // refuses to guess which scheme was meant, so every delivery becomes a 500.
    const judged = await send(raceId, { "x-shopify-hmac-sha256": `${"A".repeat(43)}=` }, "{}");
    assertEquals(judged.status, 401, `a delivery must still be judged, not crash: ${await judged.text()}`);
    await request(jwt, `/library/${ghId}/secret`, { method: "DELETE", body: JSON.stringify({ name: "hook:github" }) });
    const after = JSON.stringify({ action: "after" });
    assertEquals((await send(ghId, await ghSign(after), after)).status, 401, "deleting the secret ends the scheme");
    await reject(jwt, `/library/${ghId}/secret`, { method: "DELETE", body: JSON.stringify({ name: "hook:github" }) });
  });

  // It must carry the path and the status, and it must never carry a header value: an Authorization in a log outlives
  // the request that sent it.
  await t.step('Every failure lands on one sheet, so "what is breaking, and where" is a query', async () => {
    // No psql prompt: seed() granted OPERATOR_EMAIL the viewer row, and erin
    // signed up afterwards onto the account row seed() had already made.
    const { jwt } = await usr("erin@example.com");
    const [grant] = await sql`
      select role from sheet_usr su inner join usr u using (usr_id)
      where su.sheet_id = 'net-hook:errors' and u.email = 'erin@example.com'
    `;
    assertEquals(grant?.role, "viewer");
    const marker = `/no-such-route-${crypto.randomUUID()}?auth=Bearer%20super-secret-token`;
    // An unknown path with a bad bearer token: the jwt middleware answers before
    // the not-found handler does, which is exactly the sort of thing the log is
    // for -- the path alone does not explain the status.
    await app.request(marker, { headers: new Headers({ Authorization: "Bearer super-secret-token" }) });

    const [, newest] = await get<Table>(jwt, `/sheet/net-hook:errors`);
    const meta = JSON.parse(String(newest.meta));
    assertEquals(meta.path, marker.split("?")[0]);
    assertEquals(meta.status, 401);
    const row = JSON.stringify(newest);
    assert(row.includes("authorization"), "the header names are what makes a failed request reproducible");
    assert(row.includes("auth"), "a query parameter's name is worth keeping");
    // The sync socket takes ?auth=<jwt> and a share link rides the same
    // parameter, so a query value in this log is a live token sitting in a
    // sheet that can be shared and exported. One rule for both, not two.
    assert(!row.includes("super-secret-token"), "neither a header nor a query value may reach the log");

    // One row per (status, path) per minute. Every 4xx used to cost an insert
    // and a trimNet behind it, so refusing a request was dearer than serving
    // it -- the wrong way round for the path whose whole job is shedding load.
    const settle = errorLogged;
    const counted = async (path: string) => {
      const [{ n }] = await sql`
        select count(*) as n from net where sheet_id = 'net-hook:errors' and meta->>'path' = ${path}
      `;
      return Number(n);
    };
    const flood = `/no-such-route-${crypto.randomUUID()}`;
    for (let i = 0; i < 3; i++) await app.request(flood);
    await settle();
    assertEquals(await counted(flood), 1, "three identical rejections are one row, not three");

    const elsewhere = `/no-such-route-${crypto.randomUUID()}`;
    await app.request(elsewhere);
    await settle();
    assertEquals(await counted(elsewhere), 1, "suppression is per status and path, never global");

    // A burst that stops must not read as one failure. What a suppressed minute
    // hid sits in memory until that key writes again, which never comes once
    // the burst ends -- so the broom flushes it. Counted the way the two
    // conditions out of this sheet count it: each row is itself plus its folds.
    const occurrences = async (path: string) => {
      const [{ n }] = await sql`
        select coalesce(sum(1 + coalesce(substring(meta->>'folded' from '^[0-9]{1,9}$')::int, 0)), 0) as n
        from net where sheet_id = 'net-hook:errors' and meta->>'path' = ${path}
      `;
      return Number(n);
    };
    const burst = `/no-such-route-${crypto.randomUUID()}`;
    for (let i = 0; i < 6; i++) await app.request(burst);
    await settle();
    assertEquals(await counted(burst), 1, "six rejections inside one minute are one row");
    assertEquals(await occurrences(burst), 1, "and the other five are still only in memory");
    await flushFolds(Date.now() + LOG_EVERY_MS + 1);
    assertEquals(await counted(burst), 2, "the flush is a second row, not a rewrite of the first");
    assertEquals(await occurrences(burst), 6, "every one of the six is counted");
    await flushFolds(Date.now() + LOG_EVERY_MS + 1);
    assertEquals(await occurrences(burst), 6, "a burst already flushed is not counted a second time");

    // Your own failures, without a share on the operator's sheet. Safe to widen
    // because the row keeps header and query names and never their values, so
    // this read leaks nothing the operator's read does not.
    const frank = await usr("frank@example.com");
    const gina = await usr("gina@example.com");
    const franks = `/no-such-route-${crypto.randomUUID()}`;
    const ginas = `/no-such-route-${crypto.randomUUID()}`;
    await app.request(franks, { headers: new Headers({ Authorization: `Bearer ${frank.jwt}` }) });
    await app.request(ginas, { headers: new Headers({ Authorization: `Bearer ${gina.jwt}` }) });
    await settle();
    const paths = async (token: string) => {
      const [, ...rows] = await get<Table>(token, "/sheet/net-hook:errors");
      return rows.map((r) => JSON.parse(String(r.meta)).path);
    };
    assert((await paths(frank.jwt)).includes(franks), "you read back the failure your own request caused");
    // A 5xx body is the stack the 500 response deliberately withheld. Reading
    // your own failures must not hand it back.
    await sql`insert into net ${
      sql({
        sheet_id: "net-hook:errors",
        body: "Error: boom\n    at /Users/somebody/main.ts:1:1",
        method: "GET",
        req_headers: sql.json({ names: "(none)" }),
        query_params: sql.json({ names: "(none)" }),
        meta: sql.json({ status: 500, path: "/boom", usr_id: frank.usr_id }),
      })
    }`;
    const [, ...franksRows] = await get<Table>(frank.jwt, "/sheet/net-hook:errors");
    const franksLog = JSON.stringify(franksRows);
    assert(franksLog.includes("/boom"), "you still see that it failed, and where");
    assert(!franksLog.includes("main.ts:1:1"), "but never the stack the 500 response withheld");
    const [, ...operatorRows] = await get<Table>(jwt, "/sheet/net-hook:errors");
    assert(JSON.stringify(operatorRows).includes("main.ts:1:1"), "the operator's log still keeps it");
    assert(!(await paths(frank.jwt)).includes(ginas), "and never the one somebody else's did");
    const operator = await paths(jwt);
    assert(operator.includes(franks) && operator.includes(ginas), "the operator still reads every row");

    // A suppressed write must not clear `logWriteFailures`. Cleared by a
    // suppression, a database refusing every insert reads as a service with no
    // failures -- and the two conditions counted out of this sheet read an
    // empty sheet as a clean hour, so /status answers 200 while nothing works.
    // The sheet row is what `net` references, so removing it breaks the write.
    const reaching = "Every failure is reaching the error log.";
    await sql`delete from net where sheet_id = 'net-hook:errors'`;
    await sql`delete from sheet_usr where sheet_id = 'net-hook:errors'`;
    await sql`delete from sheet where sheet_id = 'net-hook:errors'`;
    const broken = `/no-such-route-${crypto.randomUUID()}`;
    await app.request(broken);
    await settle();
    assert((await status())[reaching]["0"] < 1, "a log that cannot be written must say so");

    // The same failure again, now that the first one's rejection has landed.
    // It is suppressed, so no write is attempted -- and a suppression that
    // reported itself as a write would clear the counter here.
    await app.request(broken);
    await settle();
    assert((await status())[reaching]["0"] < 1, "a suppressed write must not clear the counter");

    await seed();
    const healed = `/no-such-route-${crypto.randomUUID()}`;
    await app.request(healed);
    await settle();
    assertEquals((await status())[reaching]["0"], 1, "one real write clears it again");
  });

  // /status grades them in aggregate and names none of them, so this is the read that says which one is rotten.
  await t.step("Which feed stopped refreshing", async () => {
    const { jwt } = await usr("zoe@example.com");
    const fresh = automerge.create<Sheet>({ type: "net-http", data: [{ url: "https://example.com/a", interval: 60 }] });
    const rotten = automerge.create<Sheet>({
      type: "net-http",
      data: [{ url: "https://example.com/b", interval: 60 }],
    });
    const silent = automerge.create<Sheet>({
      type: "net-http",
      data: [{ url: "https://example.com/c", interval: 60 }],
    });
    const ids = [fresh, rotten, silent].map((h) => `net-http:${h.documentId}`);
    for (const [i, hand] of [fresh, rotten, silent].entries())
      await put(jwt, `/library/net-http:${hand.documentId}`, { name: `feed-${i}` });
    const run = (sheet_id: string, status: number, ago: string) =>
      sql`insert into net (sheet_id, method, body, meta, created_at)
          values (${sheet_id}, 'GET', '{}', ${sql.json({ status, ms: 5, bytes: 2 })}, now() - ${ago}::interval)`;
    await run(ids[0], 200, "1 minute");
    // Two failures since the last good one, which is the number the alarm
    // cannot give you today.
    await run(ids[1], 200, "3 hours");
    await run(ids[1], 500, "2 hours");
    await run(ids[1], 0, "1 hour");
    // ids[2] has never run at all -- a poller that never fired once, which an
    // inner join would drop and so report as healthy.

    const rows = async (token: string) => {
      const [cols, ...body] = await get<Table>(token, "/sheet/library:freshness");
      return { names: Object.values(cols).map((c) => (c as Col).name), body };
    };
    const { names, body } = await rows(jwt);
    assertEquals(
      names.join(),
      "sheet_id,name,type,last_run,last_ok,failures_since_ok,last_meta",
      "the freshness sheet has a stable shape, because a query sheet selects from it",
    );
    const byId = Object.fromEntries(body.map((r) => [String(r.sheet_id), r]));
    assertEquals(Object.keys(byId).sort(), [...ids].sort(), "one row per feed the caller can read");
    assertEquals(Number(byId[ids[0]].failures_since_ok), 0);
    assertEquals(Number(byId[ids[1]].failures_since_ok), 2, "the failures since the last good run are counted");
    assertEquals(byId[ids[2]].last_run, null, "a feed that never ran is the failure this read is for");
    assertEquals(String(body[0].sheet_id), ids[2], "stalest first: never-run sorts above everything");
    // The meta of that run rides along, cast to text, so a query sheet can read
    // it with json_extract the way it reads a net sheet's headers.
    assertEquals(JSON.parse(String(byId[ids[1]].last_meta)).status, 0);

    // Two runs sharing a timestamp. Compared on created_at alone, last_ok equals
    // last_run and the count is zero, so the failing sheet reads healthy --
    // which is the one thing this read exists to prevent.
    const tied = automerge.create<Sheet>({ type: "net-http", data: [{ url: "https://example.com/t", interval: 60 }] });
    const tiedId = `net-http:${tied.documentId}`;
    await put(jwt, `/library/${tiedId}`, { name: "feed-tied" });
    await sql`
      insert into net (sheet_id, method, body, meta) values
        (${tiedId}, 'GET', 'ok', ${sql.json({ status: 200 })}),
        (${tiedId}, 'GET', 'bad', ${sql.json({ status: 500 })})
    `;
    const tiedRow = (await rows(jwt)).body.find((r) => String(r.sheet_id) === tiedId);
    assertEquals(Number(tiedRow?.failures_since_ok), 1, "a tie on the timestamp is broken by net_id, not ignored");

    // Shape is not magnitude: '99999999999999999999' is all digits and still
    // out of range for an int, and it used to take both this read and the
    // status check -- the alarm itself -- to 500.
    await sql`
      insert into net (sheet_id, method, body, meta)
      values (${ids[0]}, 'GET', 'x', ${sql.json({ status: "99999999999999999999" })})
    `;
    assertEquals((await rows(jwt)).body.length > 0, true, "one unreadable status must not empty the answer");
    assertEquals(Object.keys(await status()).length, 16, "nor take the alarm down with it");
    await sql`delete from net where sheet_id = ${ids[0]} and body = 'x'`;

    // A poll is not the only kind of run. A net-hook sheet's run is the
    // delivery it was sent, and a webhook that has gone quiet is exactly as
    // stale as a feed that has stopped answering -- so the read covers every
    // type whose runs land in `net`, which is every type that table's own check
    // constraint admits. A delivery that reached the table landed, so every one
    // of its rows is a good run and last_ok tracks last_run.
    {
      const talked = automerge.create<Sheet>({ type: "net-hook", data: [] });
      const quiet = automerge.create<Sheet>({ type: "net-hook", data: [] });
      const talkedId = `net-hook:${talked.documentId}`;
      const quietId = `net-hook:${quiet.documentId}`;
      for (const [i, id] of [talkedId, quietId].entries()) await put(jwt, `/library/${id}`, { name: `hook-${i}` });
      assertEquals((await deliver(talkedId, `{"n":1}`)).status, 200);

      const hooks = Object.fromEntries((await rows(jwt)).body.map((r) => [String(r.sheet_id), r]));
      assert(hooks[talkedId], "a net-hook sheet that has been delivered to has a freshness");
      assertEquals(
        hooks[talkedId].last_ok,
        hooks[talkedId].last_run,
        "every delivery stored is a delivery that landed",
      );
      assertEquals(Number(hooks[talkedId].failures_since_ok), 0);
      // The one this widening is for: not "has rows in net", which would drop
      // the silent webhook, which is the failure worth naming.
      assert(hooks[quietId], "a net-hook sheet nobody has delivered to is the failure this read is for");
      assertEquals(hooks[quietId].last_run, null, "and it says so rather than being absent");

      // A net-socket sheet is opened by the browser against the user's own url,
      // so no run of it is ever recorded here. It is left out rather than
      // reported as never run, which is what it would say forever about a sheet
      // that works.
      const socket = automerge.create<Sheet>({ type: "net-socket", data: [{ url: "ws://127.0.0.1:5051/x" }] });
      await put(jwt, `/library/net-socket:${socket.documentId}`, { name: "live" });
      assert(
        !(await rows(jwt)).body.some((r) => String(r.sheet_id) === `net-socket:${socket.documentId}`),
        "a sheet whose liveness is not recorded here is left out, not reported as never run",
      );

      // A codex sheet's run is the connection GET /codex/:id opened. There is
      // no poller for one, so the only moment anybody learns whether the far
      // database still answers is when somebody opens the sheet -- which is
      // why a connection nobody has opened reads "never run" rather than being
      // left out: it has never been verified, and that is the fact.
      const conn = automerge.create<Sheet>({ type: "codex-db", data: [] });
      const cold = automerge.create<Sheet>({ type: "codex-db", data: [] });
      const own = automerge.create<Sheet>({ type: "codex-scrapsheets", data: [] });
      const connId = `codex-db:${conn.documentId}`;
      const coldId = `codex-db:${cold.documentId}`;
      const ownId = `codex-scrapsheets:${own.documentId}`;
      for (const [i, id] of [connId, coldId, ownId].entries()) await put(jwt, `/library/${id}`, { name: `codex-${i}` });

      // No DSN saved yet, so the connection cannot be opened. The refusal is
      // the run: this is what a rotated credential looks like from here.
      await reject(jwt, `/codex/${connId}`);
      await get<Table>(jwt, `/codex/${ownId}`);

      const conns = Object.fromEntries((await rows(jwt)).body.map((r) => [String(r.sheet_id), r]));
      assert(conns[connId], "a codex sheet has a freshness, because a connection is a run");
      assertEquals(conns[connId].last_ok, null, "a refused connection is not a good run");
      assert(conns[connId].last_run, "but it is a run, and it is the one worth seeing");
      assertEquals(Number(conns[connId].failures_since_ok), 1);
      assertEquals(
        JSON.parse(String(conns[connId].last_meta)).status,
        400,
        "graded by POLL_OK off the same meta.status a poll writes, not a second spelling of it",
      );
      assertEquals(conns[ownId].last_ok, conns[ownId].last_run, "a connection that answered is a good run");
      assertEquals(Number(conns[ownId].failures_since_ok), 0);
      assertEquals(conns[coldId].last_run, null, "a connection nobody has opened has never been verified");

      // Somebody else holding the doc_id must not be able to write failures
      // into this sheet's freshness, which is why the access check runs before
      // the clock starts.
      const { jwt: stranger } = await usr("zeb@example.com");
      await reject(stranger, `/codex/${connId}`);
      assertEquals(
        Number((await rows(jwt)).body.find((r) => String(r.sheet_id) === connId)?.failures_since_ok),
        1,
        "a refused reader is not a connection that failed",
      );

      // A sheet with no runs to be stale has no row at all. It is not a zero.
      const table = automerge.create<Sheet>({
        type: "table",
        data: [arrayify([{ name: "a", type: "text", key: "0" }])],
      });
      await put(jwt, `/library/table:${table.documentId}`, { name: "not-a-feed" });
      assert(
        !(await rows(jwt)).body.some((r) => String(r.sheet_id) === `table:${table.documentId}`),
        "a table sheet has no runs, so it has no freshness",
      );
    }

    // Somebody else's feeds are not in it, and the route is the same answer.
    const { jwt: outsider } = await usr("zane@example.com");
    assertEquals((await rows(outsider)).body.length, 0, "you see the feeds you can read and no others");
    const viaRoute = await get<Table>(jwt, "/library/freshness");
    assertEquals(viaRoute.length, (await rows(jwt)).body.length + 1, "GET /library/freshness is the same sheet");

    // It exports and a query sheet selects from it, because it goes through
    // sheet() like everything else.
    const csv = await app.request("/export/library:freshness.csv", {
      headers: new Headers({ Authorization: `Bearer ${jwt}` }),
    });
    assert(csv.ok, `the freshness sheet must export: ${csv.status}`);
    assert((await csv.text()).includes(ids[1]), "and the export must carry the rotten feed");
    const answer = await post(jwt, "/query", {
      lang: "sql",
      code: `select count(*) as n from @library:freshness where failures_since_ok > 0`,
    }).then((res) => res.data);
    // The rotten feed, the tied one, and the codex connection with no DSN. The
    // never-run feed and the never-opened connection have failed nothing.
    assertEquals(Number((answer as Table)[1].n), 3, "a query sheet can select from it");
  });

  await t.step("A live socket is only as fresh as the last browser that watched it", async () => {
    const { jwt } = await usr("sasha@example.com");
    const hand = automerge.create<Sheet>({ type: "net-socket", data: [{ url: "wss://example.com/feed" }] });
    const sheet_id = `net-socket:${hand.documentId}`;
    await put(jwt, `/library/${sheet_id}`, { name: "a live feed" });

    const fresh = async () =>
      Object.fromEntries(
        (await get<Table>(jwt, "/sheet/library:freshness")).slice(1).map((r) => [String(r.sheet_id), r]),
      );

    // Before a browser has said anything, the read says nothing about it.
    assert(!(await fresh())[sheet_id], "a socket nobody has watched is absent, not never-run");

    await post(jwt, `/library/${sheet_id}/socket`, { status: "connected" });
    const seen = (await fresh())[sheet_id];
    assert(seen, "a socket a browser has seen open has a freshness");
    assertEquals(seen.last_ok, seen.last_run, "seeing it open is a good run");
    assertEquals(Number(seen.failures_since_ok), 0);

    await post(jwt, `/library/${sheet_id}/socket`, { status: "error" });
    const broken = (await fresh())[sheet_id];
    assertEquals(Number(broken.failures_since_ok), 1, "a failed connection is a failure since the last good one");
    assertEquals(
      JSON.parse(String(broken.last_meta)).status,
      502,
      "graded by POLL_OK off the same meta.status a poll writes, not a second spelling of it",
    );

    // Two states and no more, and `__proto__` is not one of them: `in` walked the
    // prototype, so every Object.prototype key answered 200 and wrote a run
    // POLL_OK grades as a failure forever, on a socket that never failed.
    for (const status of ["disconnected", "__proto__", "toString", "constructor", 7, null, ["connected"], undefined]) {
      await reject(jwt, `/library/${sheet_id}/socket`, { method: "POST", body: JSON.stringify({ status }) });
    }
    // A body that is not an object at all. JSON `null` parses, so the `.catch`
    // does not see it and destructuring it used to be a 500 anyone could send.
    for (const body of ["null", "[]", "7", '"connected"', "{", ""]) {
      await reject(jwt, `/library/${sheet_id}/socket`, { method: "POST", body });
    }
    assertEquals(
      Number((await fresh())[sheet_id].failures_since_ok),
      1,
      "a refused status is not a socket that failed",
    );

    // syncRole answers on doc_id alone, so owning any sheet gets you past the
    // access check for that doc_id under another type prefix, and a second colon
    // leaves the tail out of the check entirely. The row this writes has to
    // exist, or the insert violates net's foreign key and answers an unexplained
    // 500 -- and the budget below it gets keyed on an id the caller minted.
    const other = automerge.create<Sheet>({
      type: "table",
      data: [arrayify([{ name: "a", type: "text", key: "0" }])],
    });
    await put(jwt, `/library/table:${other.documentId}`, { name: "not a socket" });
    const minted = [
      `net-socket:${other.documentId}`,
      `table:${other.documentId}`,
      `net-socket:${hand.documentId}:x`,
      "net-socket:",
      "net-socket",
    ];
    for (const id of minted) {
      await reject(jwt, `/library/${id}/socket`, { method: "POST", body: JSON.stringify({ status: "connected" }) });
    }
    assertEquals(
      minted.filter((id) => hookBuckets.has(id)),
      [],
      "a minted id must not mint a rate-limit key, which evicts the budgets of sheets that exist",
    );

    // A viewer watches the same socket and must not be able to write its
    // health, and a stranger holding the doc_id must not be able to write
    // failures into somebody else's freshness.
    await usr("sam@example.com");
    const { jwt: viewerJwt } = await usr("sonia@example.com");
    await post(jwt, `/library/${sheet_id}/share`, { email: "sonia@example.com", role: "viewer" });
    const { jwt: strangerJwt } = await usr("sven@example.com");
    for (const token of [viewerJwt, strangerJwt]) {
      await reject(token, `/library/${sheet_id}/socket`, {
        method: "POST",
        body: JSON.stringify({ status: "error" }),
      });
    }
    assertEquals(
      Number((await fresh())[sheet_id].failures_since_ok),
      1,
      "a refused reporter is not a socket that failed",
    );

    // A page stuck in a reconnect ladder is a sender that will not stop, and it
    // spends the same budget a webhook sender does.
    hookBuckets.delete(sheet_id);
    let refused = 0;
    for (let i = 0; i < HOOK_ROWS_PER_WINDOW + 5; i++) {
      const res = await app.request(`/library/${sheet_id}/socket`, {
        method: "POST",
        headers: new Headers({ "Content-Type": "application/json", Authorization: `Bearer ${jwt}` }),
        body: JSON.stringify({ status: "connected" }),
      });
      if (res.status === 429) refused++;
    }
    assert(refused > 0, "a flood of reports must run out of budget and say so");

    // Thirty reports at once against a budget of one, for the reason
    // POST /net/:id is raced the same way: the check and the charge are one
    // step, or every report passes.
    hookBuckets.delete(sheet_id);
    hookBucket(sheet_id).rows = 1;
    const [{ n: before }] = await sql`select count(*)::int as n from net where sheet_id = ${sheet_id}`;
    const raced = await Promise.all(
      Array.from({ length: 30 }, () =>
        app.request(`/library/${sheet_id}/socket`, {
          method: "POST",
          headers: new Headers({ "Content-Type": "application/json", Authorization: `Bearer ${jwt}` }),
          body: JSON.stringify({ status: "connected" }),
        })),
    );
    assertEquals(raced.filter((res) => res.ok).length, 1, "one report may land on a budget of one");
    assertEquals(raced.filter((res) => res.status === 429).length, 29, "and the rest are refused");
    const [{ n: after }] = await sql`select count(*)::int as n from net where sheet_id = ${sheet_id}`;
    assertEquals(after - before, 1, "concurrent reports must not outrun the budget");
    hookBuckets.delete(sheet_id);
  });

  // Three ways a caller holding a real credential reached an unexplained 500.
  // Each is one guard at the boundary the input crosses, so no handler has to
  // remember it.
  await t.step("A refusal is a refusal, not a 500", async () => {
    const { jwt } = await usr("nul@example.com");
    const hand = automerge.create<Sheet>({ type: "table", data: [arrayify([{ name: "a", type: "text", key: 0 }])] });
    const sheet_id = `table:${hand.documentId}`;
    await put(jwt, `/library/${sheet_id}`, {});

    // A percent-encoded NUL in an id. Postgres text cannot hold one, so every
    // :id that reached a SQL comparison threw -- and a query-string id is the
    // same id by another door.
    for (
      const [method, route] of [
        ["POST", `/library/${sheet_id}%00/public`],
        ["GET", `/sheet/${sheet_id}%00`],
        ["GET", `/library?doc_id=${hand.documentId}%00`],
      ] as const
    ) {
      const res = await app.request(route, {
        method,
        headers: new Headers({ "Content-Type": "application/json", Authorization: `Bearer ${jwt}` }),
        body: method === "POST" ? JSON.stringify({ public: true }) : undefined,
      });
      assertEquals(res.status, 400, `${method} ${route} must be refused, not fail`);
      const text = await res.text();
      assert(text.includes("NUL"), `${method} ${route} must name the byte, got: ${text}`);
    }

    // A body whose size the caller picks, on a route that parses JSON. One cap
    // for every body, or the one on POST /net/:id was theatre.
    const big = await app.request(`/library/${sheet_id}`, {
      method: "PUT",
      headers: new Headers({ "Content-Type": "application/json", Authorization: `Bearer ${jwt}` }),
      body: JSON.stringify({ name: "x".repeat(BODY_CAP) }),
    });
    assertEquals(big.status, 413, "a body over the cap is refused by size");
    const text = await big.text();
    assert(text.includes(`${BODY_CAP} bytes`), `it must name the limit, got: ${text}`);
    await put(jwt, `/library/${sheet_id}`, { name: "small" });

    // The rest of the hunt: every await whose failure was not already a
    // refusal. Each row is the request that reached a SyntaxError, a
    // TypeError or a PostgresError, and the word its refusal now carries.
    const bare = automerge.create({ type: "table" } as unknown as Sheet);
    await put(jwt, `/library/table:${bare.documentId}`, {});
    const conn = automerge.create<Sheet>({ type: "codex-db", data: [] });
    await put(jwt, `/library/codex-db:${conn.documentId}`, {});
    const form = new FormData();
    form.set("file", "a,b\n1,2");
    const json = (body: unknown) => ({ body: JSON.stringify(body), type: "application/json" });
    for (
      const [method, route, init, status, needle] of [
        ["POST", "/login", { body: "x", type: "application/json" }, 400, "not JSON"],
        ["POST", "/login", { body: "null", type: "application/json" }, 400, "not a JSON object"],
        ["POST", "/login", json({ email: true, password: "x" }), 400, "email"],
        ["GET", "/library?limit=abc", {}, 400, "limit"],
        ["GET", "/library?offset=-1", {}, 400, "offset"],
        ["GET", "/shop?sell_price=5", {}, 400, "sell_price"],
        ["POST", `/sell/${sheet_id}`, json({ price: "abc" }), 400, "price"],
        ["PUT", `/library/${sheet_id}`, json({ tags: "abc" }), 400, "tags"],
        ["POST", "/query", json({ lang: "sql" }), 400, "needs SQL"],
        ["POST", `/library/${sheet_id}/share`, { body: "null", type: "application/json" }, 400, "not a JSON object"],
        ["POST", "/import/csv", { body: "x", type: "multipart/form-data" }, 400, "multipart"],
        ["POST", "/import/csv", { body: form }, 400, "text field"],
        ["GET", `/sheet/table:${bare.documentId}`, {}, 400, "no rows in it"],
        ["POST", `/codex-db/${conn.documentId}`, json({ a: 1 }), 400, "dsn"],
        ["POST", `/codex-db/${conn.documentId}`, json({ dsn: "postgresql://u:p@nope.invalid:5432/db" }), 200, ""],
        ["GET", `/codex/codex-db:${conn.documentId}`, {}, 502, "did not answer"],
      ] as const
    ) {
      const headers = new Headers({ Authorization: `Bearer ${jwt}` });
      if ("type" in init) headers.set("Content-Type", init.type);
      const res = await app.request(route, { method, headers, body: "body" in init ? init.body : undefined });
      const answer = await res.text();
      assertEquals(res.status, status, `${method} ${route} must answer ${status}, got ${res.status}: ${answer}`);
      assert(answer.includes(needle), `${method} ${route} must name ${JSON.stringify(needle)}, got: ${answer}`);
    }
  });

  // safeFetch guarded our network and not theirs. Three things a polite
  // scraper does: it says who it is, it does not stampede one host, and the
  // shop it feeds records whether what it sells may be sold, and can be told
  // when it should not have been.
  await t.step("We are a polite scraper", async () => {
    const { jwt } = await usr("polite@example.com");

    // Who we are, on every outbound request, with the address that explains it.
    {
      const seen: Record<string, string>[] = [];
      const origFetch = globalThis.fetch;
      globalThis.fetch = ((_url: string, init?: RequestInit) => {
        seen.push(Object.fromEntries(new Headers(init?.headers).entries()));
        return Promise.resolve(new Response("ok"));
      }) as typeof fetch;
      try {
        await safeFetch("http://93.184.216.34/feed");
        assertEquals(seen[0]["user-agent"], USER_AGENT, "every fetch says who it is");
        assert(USER_AGENT.includes("https://"), `the user agent points at where it is explained: ${USER_AGENT}`);
      } finally {
        globalThis.fetch = origFetch;
      }
    }

    // The real door, not a stub: the poller through safeFetch. Every other poll
    // test hands in a fetcher, which is how a poller that refused its own
    // fetch passed the suite.
    {
      const hand = automerge.create<Sheet>({
        type: "net-http",
        data: [{ url: "http://93.184.216.34/poll", interval: 3600 }],
      });
      const id = `net-http:${hand.documentId}`;
      await put(jwt, `/library/${id}`, {});
      const origFetch = globalThis.fetch;
      globalThis.fetch = (() => Promise.resolve(new Response(`{"polled":true}`))) as typeof fetch;
      try {
        await pollNetOnce(undefined, Date.now() + 70_000_000);
      } finally {
        globalThis.fetch = origFetch;
      }
      const [{ meta }]: { meta: { status: number } }[] = await sql`select meta from net where sheet_id = ${id}`;
      assertEquals(meta.status, 200, `a poll through safeFetch itself lands: ${JSON.stringify(meta)}`);
    }

    // Two sheets on one host take turns across cycles rather than one cycle.
    {
      const feeds = ["https://polite.test/a", "https://polite.test/b", "https://elsewhere.test/c"];
      for (const url of feeds) {
        const hand = automerge.create<Sheet>({ type: "net-http", data: [{ url, interval: 3600 }] });
        await put(jwt, `/library/net-http:${hand.documentId}`, {});
      }
      const calls: string[] = [];
      const fetcher = (url: string) => {
        calls.push(url);
        return Promise.resolve(new Response(`{"ok":true}`));
      };
      hostDue.delete("polite.test");
      hostDue.delete("elsewhere.test");
      const t0 = Date.now() + 10_000_000;
      await pollNetOnce(fetcher, t0);
      assertEquals(calls.filter((u) => u.includes("polite.test")).length, 1, "one request to one host per cycle");
      assertEquals(calls.filter((u) => u.includes("elsewhere.test")).length, 1, "and another host is not held for it");
      await pollNetOnce(fetcher, t0 + HOST_GAP_MS / 2);
      assertEquals(calls.filter((u) => u.includes("polite.test")).length, 1, "inside the gap the other sheet waits");
      await pollNetOnce(fetcher, t0 + HOST_GAP_MS + 1);
      assertEquals(calls.filter((u) => u.includes("polite.test")).length, 2, "and past it the other sheet is fetched");
      assertEquals(new Set(calls.filter((u) => u.includes("polite.test"))).size, 2, "each sheet once");
      hostDue.delete("polite.test");
      hostDue.delete("elsewhere.test");
    }

    // A listing records whether what it sells may be sold. Without a license a
    // listing does not go live; with one, the shop shows it beside the price.
    const hand = automerge.create<Sheet>({ type: "table", data: [arrayify([{ name: "a", type: "text", key: 0 }])] });
    const sheet_id = `table:${hand.documentId}`;
    await put(jwt, `/library/${sheet_id}`, { name: "polite listing" });
    const unlicensed = await app.request(`/sell/${sheet_id}`, {
      method: "POST",
      headers: new Headers({ "Content-Type": "application/json", Authorization: `Bearer ${jwt}` }),
      body: JSON.stringify({ price: 1 }),
    });
    assertEquals(unlicensed.status, 400, "a listing without a license does not go live");
    const said = await unlicensed.text();
    assert(said.includes("license"), `and the refusal names the field: ${said}`);
    await reject(jwt, `/sell/${sheet_id}`, { method: "POST", body: JSON.stringify({ price: 1, license: "mine" }) });
    await post(jwt, `/sell/${sheet_id}`, { price: 1, license: LICENSES[0] });
    const [, ...listed] = await get<Table>(jwt, `/shop`, { name: "polite listing" });
    assertEquals(listed.length, 1);
    assertEquals(listed[0].license, LICENSES[0], "the shop shows the license beside the price");
    const sell_id = String(listed[0].sell_id);

    // Anyone signed in may report a listing, once, with a reason. The reports
    // are a sheet: the reporter reads their own, the operator reads them all.
    const { jwt: buyer, usr_id: buyerId } = await usr("reporter@example.com");
    const report = (body: unknown, who = buyer) =>
      app.request(`/shop/${sell_id}/report`, {
        method: "POST",
        headers: new Headers({ "Content-Type": "application/json", Authorization: `Bearer ${who}` }),
        body: JSON.stringify(body),
      });
    assertEquals((await report({})).status, 400, "a report needs a reason");
    assertEquals((await report({ reason: "this is the census bureau's table, resold" })).status, 200);
    assertEquals((await report({ reason: "again" })).status, 409, "one report per account per listing");
    assertEquals((await report({ reason: "x" }, jwt)).status, 400, "a seller cannot report their own listing");
    const missing = await app.request(`/shop/${"0".repeat(32)}/report`, {
      method: "POST",
      headers: new Headers({ "Content-Type": "application/json", Authorization: `Bearer ${buyer}` }),
      body: JSON.stringify({ reason: "x" }),
    });
    assertEquals(missing.status, 404, "a report on nothing is refused by name");

    const [, ...mine] = await get<Table>(buyer, `/sheet/net-hook:reports`);
    assertEquals(mine.length, 1, "a reporter reads their own reports");
    assert(String(mine[0].body).includes("census"), `and the reason is the row: ${JSON.stringify(mine[0])}`);
    assertEquals(String(mine[0].sell_id), sell_id);
    const [, ...others] = await get<Table>(jwt, `/sheet/net-hook:reports`);
    assertEquals(others.length, 0, "and nobody else's");

    // The operator is whoever reads the error log. Review closes the reports;
    // a takedown pulls the listing too, and the status check grades the queue.
    const { jwt: operator, usr_id: operatorId } = await usr("moderator@example.com");
    const review = (action: unknown, who = operator) =>
      app.request(`/shop/${sell_id}/review`, {
        method: "POST",
        headers: new Headers({ "Content-Type": "application/json", Authorization: `Bearer ${who}` }),
        body: JSON.stringify({ action }),
      });
    assertEquals((await review("takedown", buyer)).status, 403, "a review is the operator's");
    const waiting = "No shop report is waiting for review.";
    assert((await status())[waiting]["0"] < 1, "an open report is a failing grade");
    await sql`insert into sheet_usr (sheet_id, usr_id, role) values ('net-hook:errors', ${operatorId}, 'viewer')`;
    const [, ...queue] = await get<Table>(operator, `/sheet/net-hook:reports`);
    assert(queue.some((r) => String(r.sell_id) === sell_id), "the operator reads every report");
    assertEquals((await review("shrug")).status, 400, "a review is keep or takedown");
    assertEquals((await review("takedown")).status, 200);
    const [, ...gone] = await get<Table>(jwt, `/shop`, { name: "polite listing" });
    assertEquals(gone.length, 0, "a takedown pulls the listing");
    assertEquals((await status())[waiting]["0"], 1, "and clears the queue");
    assertEquals((await review("takedown")).status, 404, "a listing that is down has nothing left to pull");

    // A seller who pulls a reported listing does not take the report with it:
    // the operator still closes it, or the alarm would ring until somebody
    // muted it.
    await post(jwt, `/sell/${sheet_id}`, { price: 2, license: "own" });
    assertEquals((await report({ reason: "relisted, still theirs" }, buyer)).status, 409, "one report per account, still");
    const { jwt: second } = await usr("second-reporter@example.com");
    assertEquals((await report({ reason: "still not theirs" }, second)).status, 200);
    await post(jwt, `/sell/${sheet_id}`, { price: null });
    assert((await status())[waiting]["0"] < 1, "a report on a pulled listing is still open");
    assertEquals((await review("keep")).status, 200, "and the operator closes it");
    assertEquals((await status())[waiting]["0"], 1);
    void buyerId;
  });

  // Every read and write of a sheet is a row in one log, and the log is a
  // sheet: an owner reads who touched what they own, everybody reads what they
  // did themselves, and a refused request did nothing and so is not in it.
  await t.step("You can read who opened a sheet and what they changed", async () => {
    const { jwt: owner } = await usr("audit-owner@example.com");
    const { jwt: editor } = await usr("audit-editor@example.com");
    const { jwt: viewer } = await usr("audit-viewer@example.com");
    const { jwt: stranger } = await usr("audit-stranger@example.com");
    const hand = automerge.create<Sheet>({
      type: "table",
      data: [arrayify([{ name: "a", type: "text", key: "0" }]), { 0: "x" }],
    });
    const sheet_id = `table:${hand.documentId}`;
    await put(owner, `/library/${sheet_id}`, { name: "audited" });
    await post(owner, `/library/${sheet_id}/share`, { email: "audit-editor@example.com", role: "editor" });
    await post(owner, `/library/${sheet_id}/share`, { email: "audit-viewer@example.com", role: "viewer" });

    await get(viewer, `/sheet/${sheet_id}`);
    await post(editor, `/sheet/${sheet_id}`, { rows: [{ a: "y" }] });
    await reject(viewer, `/sheet/${sheet_id}`, { method: "POST", body: JSON.stringify({ rows: [{ a: "z" }] }) });
    await reject(stranger, `/sheet/${sheet_id}`);
    const raw = (jwt: string, route: string) =>
      app.request(route, { headers: new Headers({ Authorization: `Bearer ${jwt}` }) });
    assert((await raw(owner, `/export/${sheet_id}.csv`)).ok);

    // The owner reads everything that happened to their sheet, by who and how.
    const trail = async (jwt: string) =>
      (await get<Table>(jwt, `/sheet/library:audit`)).slice(1)
        .filter((r) => r.sheet_id === sheet_id)
        .map((r) => `${r.who} ${r.action} ${r.via}`);
    const owned = await trail(owner);
    assert(owned.includes("audit-viewer@example.com GET /sheet/:id jwt"), `a read is a row: ${owned}`);
    assert(owned.includes("audit-editor@example.com POST /sheet/:id jwt"), `a write is a row: ${owned}`);
    assert(owned.includes("audit-owner@example.com GET /export/:id jwt"), `an export is a row: ${owned}`);
    assert(owned.includes("audit-owner@example.com POST /library/:id/share jwt"), `a share is a row: ${owned}`);
    assert(!owned.some((x) => x.startsWith("audit-viewer@example.com POST")), `a refused write is not: ${owned}`);
    assert(!owned.some((x) => x.startsWith("audit-stranger")), `a refused read is not: ${owned}`);

    // Everybody else reads what they did themselves and nothing more.
    const viewed = await trail(viewer);
    assertEquals(viewed, ["audit-viewer@example.com GET /sheet/:id jwt"], "a viewer sees their own read only");
    assertEquals(await trail(stranger), [], "and a stranger sees nothing about a sheet they never reached");

    // An agent's write and a key's read are rows in the same log, marked so.
    const rpc = await app.request(`/mcp/${sheet_id}`, {
      method: "POST",
      headers: new Headers({ "Content-Type": "application/json", Authorization: `Bearer ${editor}` }),
      body: JSON.stringify({
        jsonrpc: "2.0",
        id: 1,
        method: "tools/call",
        params: { name: "write_cells", arguments: { cells: [{ row: 0, col: "a", value: "agent" }] } },
      }),
    });
    assert(rpc.ok, `mcp answered ${rpc.status}`);
    const { data: { key } }: { data: { key: string } } = await post(owner, `/library/${sheet_id}/secret`, {
      name: "api",
    });
    assert((await app.request(`/sheet/${sheet_id}`, { headers: new Headers({ "scrapsheets-key": key }) })).ok);
    const again = await trail(owner);
    assert(again.includes("audit-editor@example.com mcp write_cells mcp"), `an agent's write is a row: ${again}`);
    assert(again.includes("audit-owner@example.com GET /sheet/:id key"), `a key's read is a row: ${again}`);

    // A query is a read of every sheet it selects from, whoever spelled the
    // request, and a route whose :id is not a sheet id is still keyed by the
    // sheet, or its owner would never see the row.
    await post(viewer, `/query`, { lang: "sql", code: `select * from @${sheet_id}`, args: [] });
    const conn = automerge.create<Sheet>({ type: "codex-db", data: [] });
    await put(owner, `/library/codex-db:${conn.documentId}`, {});
    await post(owner, `/codex-db/${conn.documentId}`, { dsn: "postgresql://u:p@nope.invalid:5432/db" });
    const hook = automerge.create<Sheet>({ type: "net-hook", data: [] });
    await put(owner, `/library/net-hook:${hook.documentId}`, {});
    await get(owner, `/net/${hook.documentId}`);
    const spelled = (await get<Table>(owner, `/sheet/library:audit`)).slice(1).map((r) => `${r.sheet_id} ${r.action}`);
    assert(spelled.includes(`${sheet_id} query`), `a query's read of a sheet is a row on that sheet: ${spelled}`);
    assert(spelled.includes(`codex-db:${conn.documentId} POST /codex-db/:id`), `a codex write is keyed by its sheet: ${spelled}`);
    assert(spelled.includes(`net-hook:${hook.documentId} GET /net/:id`), `a bare doc id is keyed by its sheet: ${spelled}`);

    // A sheet, so it exports and queries like one.
    const csv = await raw(owner, `/export/library:audit.csv`);
    assert(csv.ok, `export answered ${csv.status}`);
    assertEquals((await csv.text()).split("\n")[0], "created_at,sheet_id,who,action,via,detail");
    const { data: [, ...queried] }: { data: Table } = await post(owner, `/query`, {
      lang: "sql",
      code: `select action, count(*) as n from @library:audit where sheet_id = '${sheet_id}' group by action order by action`,
      args: [],
    });
    assert(queried.some((r) => r.action === "GET /sheet/:id"), `the log is a query source: ${JSON.stringify(queried)}`);
  });

  // A sheet's own API spends the budget a net sheet's deliveries spend, keyed
  // on the sheet: a read is one row of it, an append is the rows it carries.
  // The refusal names the limit and the window, and it is a 429, which is the
  // one status app.onError does not log.
  await t.step("A public sheet API has its own limit", async () => {
    const { jwt } = await usr("limited@example.com");
    const { jwt: stranger } = await usr("unlimited@example.com");
    const hand = automerge.create<Sheet>({
      type: "table",
      data: [arrayify([{ name: "a", type: "text", key: "0" }]), { 0: "x" }],
    });
    const sheet_id = `table:${hand.documentId}`;
    await put(jwt, `/library/${sheet_id}`, {});
    const read = (who = jwt) =>
      app.request(`/sheet/${sheet_id}`, { headers: new Headers({ Authorization: `Bearer ${who}` }) });
    const append = (n: number) =>
      app.request(`/sheet/${sheet_id}`, {
        method: "POST",
        headers: new Headers({ "Content-Type": "application/json", Authorization: `Bearer ${jwt}` }),
        body: JSON.stringify({ rows: Array.from({ length: n }, (_, i) => ({ a: `r${i}` })) }),
      });

    // Reads, one row of budget each.
    hookBuckets.delete(sheet_id);
    hookBucket(sheet_id).rows = 2;
    assert((await read()).ok);
    assert((await read()).ok);
    const third = await read();
    assertEquals(third.status, 429, "the third read on a budget of two is refused");
    const said = await third.text();
    assert(said.includes(`${HOOK_ROWS_PER_WINDOW} reads`), `it names the limit in reads: ${said}`);
    assert(said.includes(`${HOOK_WINDOW_S} seconds`), `and the window: ${said}`);
    const [{ n: logged }] = await sql`
      select count(*)::int as n from net where sheet_id = 'net-hook:errors'
        and meta->>'path' = ${`/sheet/${sheet_id}`} and meta->>'status' = '429'
    `;
    assertEquals(logged, 0, "a 429 is shed, not logged");

    // Writes: one append is one unit whatever it carries, so a full batch
    // lands on a fresh budget, and the second append on a budget of one does not.
    hookBuckets.delete(sheet_id);
    assertEquals((await append(APPEND_ROWS_MAX)).status, 201, "a full batch lands on a fresh budget");
    hookBucket(sheet_id).rows = 1;
    assertEquals((await append(2)).status, 201, "an append of two lands");
    const spent = await append(1);
    assertEquals(spent.status, 429, "and the next append is refused");
    assert((await spent.text()).includes("appends"), "counting in appends");

    // A refused read spends nothing of the owner's budget, and an export is a read.
    hookBucket(sheet_id).rows = 1;
    assertEquals((await read(stranger)).status, 403);
    assert((await read()).ok, "a stranger's 403 did not spend the row");
    hookBucket(sheet_id).rows = 1;
    const csv = await app.request(`/export/${sheet_id}.csv`, { headers: new Headers({ Authorization: `Bearer ${jwt}` }) });
    assert(csv.ok, `an export is a read: ${csv.status}`);
    assertEquals((await read()).status, 429, "and it spent the row");

    // Thirty readers at once against a budget of one.
    hookBucket(sheet_id).rows = 1;
    const raced = await Promise.all(Array.from({ length: 30 }, () => read()));
    assertEquals(raced.filter((res) => res.ok).length, 1, "one read lands on a budget of one");
    assertEquals(raced.filter((res) => res.status === 429).length, 29);
    hookBuckets.delete(sheet_id);
  });

  // rateLimit() bounded an address and the sheet budget bounds one sheet;
  // nothing bounded an account across its addresses and its sheets. Each
  // quota is a count the refusal names beside the limit, and every refusal
  // says "quota", which is the word the status check counts.
  await t.step("One account cannot exhaust the service", async () => {
    const { jwt, usr_id } = await usr("greedy@example.com");
    const raw = (route: string, init: { method?: string; body?: string; headers?: Record<string, string> } = {}) =>
      app.request(route, {
        method: init.method ?? "GET",
        body: init.body,
        headers: new Headers({ "Content-Type": "application/json", Authorization: `Bearer ${jwt}`, ...init.headers }),
      });

    // Requests, whatever address they come from.
    accountBuckets.set(usr_id, { tokens: 1, lastRefill: Date.now() });
    assert((await raw(`/library`)).ok);
    const flooded = await raw(`/library`, { headers: { "x-forwarded-for": "203.0.113.9" } });
    assertEquals(flooded.status, 429, "the account's bucket, not the address's, refuses");
    assert((await flooded.text()).includes("account"), "and says so");
    accountBuckets.delete(usr_id);

    // Sheets an account may own.
    await sql`
      insert into sheet (created_by, type, doc_id)
      select ${usr_id}, 'table', 'quota-' || g from generate_series(1, ${USER_SHEETS_MAX}) g
    `;
    const hand = automerge.create<Sheet>({ type: "table", data: [arrayify([{ name: "a", type: "text", key: 0 }])] });
    const capped = await raw(`/library/table:${hand.documentId}`, { method: "PUT", body: "{}" });
    assertEquals(capped.status, 413, "a sheet past the account's cap is refused");
    assert((await capped.text()).includes(`${USER_SHEETS_MAX} sheets`), "naming the limit");
    await sql`delete from sheet where created_by = ${usr_id} and doc_id like 'quota-%'`;
    await put(jwt, `/library/table:${hand.documentId}`, {});

    // Rows one sheet may hold: the engine's own cap, so a sheet past it could
    // not be queried anyway.
    const big = await raw(`/import/csv`, {
      method: "POST",
      headers: { "Content-Type": "text/csv" },
      body: "a\n" + "1\n".repeat(MAX_QUERY_ROWS + 1),
    });
    assertEquals(big.status, 413, "a file past the row cap is refused before a document is made");
    assert((await big.text()).includes(`${MAX_QUERY_ROWS} rows`), "naming the cap");

    // Fetches need no quota of their own: a feed polls at most once a minute,
    // so an account's fetches are bounded by the sheets it may own, and there
    // is nothing to test.
    const later = Date.now() + 90_000_000;

    // Alert emails a day, across every alert the account owns: a sheet used
    // as a cannon stops at the quota and says so in its own run log.
    const watched = automerge.create<Sheet>({
      type: "table",
      data: [arrayify([{ name: "n", type: "num", key: "0" }]), { 0: 1 }],
    });
    await put(jwt, `/library/table:${watched.documentId}`, {});
    const alert = automerge.create<{ data: [{ code: string; to: string; interval: number }] }>({
      data: [{ code: `select n from @table:${watched.documentId}`, to: "victim@example.com", interval: 60 }],
    });
    const alertId = `alert:${alert.documentId}`;
    await put(jwt, `/library/${alertId}`, { name: "cannon" });
    await sql`
      insert into net (sheet_id, method, body, meta)
      select ${alertId}, 'ALERT', ${JSON.stringify({ status: "firing", delivery: "sent" })}, '{}'
      from generate_series(1, ${USER_EMAILS_PER_DAY}) g
    `;
    let sent = 0;
    await pollAlertOnce(() => {
      sent++;
      return Promise.resolve("sent");
    }, later);
    assertEquals(sent, 0, "nothing is sent past the account's daily quota");
    const [run] = (await get<Table>(jwt, `/sheet/${alertId}`)).slice(1);
    const delivery = String(JSON.parse(String(run.body)).delivery);
    assert(delivery.includes("quota") && delivery.includes(`${USER_EMAILS_PER_DAY}`), `the run names it: ${delivery}`);

    // The operator hears about it.
    assert((await status())["No account hit a quota in the past day."]["0"] < 1, "a quota hit is a failing grade");
  });

  // Change flowed in and never out. A sheet's owner names a url, and every
  // change to the sheet -- a cell over the socket, an append, a delivery on a
  // net sheet -- is posted there, signed with the same secret an inbound
  // delivery to the sheet is verified with, so one secret serves both ways.
  await t.step("Something is told when a sheet changes", async () => {
    // The production flush timer would race the flushes below; this step
    // drives every flush itself.
    clearInterval(webhookTimer);
    const { jwt } = await usr("teller@example.com");
    const { jwt: viewer } = await usr("listener@example.com");
    const hand = automerge.create<Sheet>({
      type: "table",
      data: [arrayify([{ name: "a", type: "text", key: "0" }]), { 0: "x" }],
    });
    const sheet_id = `table:${hand.documentId}`;
    await put(jwt, `/library/${sheet_id}`, {});
    await post(jwt, `/library/${sheet_id}/share`, { email: "listener@example.com", role: "viewer" });
    const hooks = () => get<Table>(jwt, `/library/${sheet_id}/webhook`).then((rows) => rows.slice(1));

    // Every receiver in this step is this stub. A literal address, because
    // safeFetch resolves a hostname before it posts and a made-up name can
    // stall this machine's resolver for the length of a test.
    const heard: { url: string; headers: Record<string, string>; body: string }[] = [];
    let answer = 200;
    let inboxDoc = hand;
    const origFetch = globalThis.fetch;
    globalThis.fetch = (async (url: string, init?: RequestInit) => {
      heard.push({ url, headers: Object.fromEntries(new Headers(init?.headers).entries()), body: String(init?.body) });
      return new Response("", { status: answer });
    }) as typeof fetch;
    try {
      // Registering: owner or editor, an http(s) url, a receiver that answers
      // a signed ping, and a bounded number.
      const register = (url: string, who = jwt) =>
        app.request(`/library/${sheet_id}/webhook`, {
          method: "POST",
          headers: new Headers({ "Content-Type": "application/json", Authorization: `Bearer ${who}` }),
          body: JSON.stringify({ url }),
        });
      assertEquals((await register("http://93.184.216.34/", viewer)).status, 403, "a viewer may not name a receiver");
      assertEquals((await register("ftp://x.test/")).status, 400, "a receiver is http or https");
      const inside = await register("http://127.0.0.1:9/hook");
      assertEquals(inside.status, 400, "a url inside our own network is refused at registration");
      assert((await inside.text()).includes("own network"), "by name");
      answer = 404;
      const unwilling = await register("http://93.184.216.34/nope");
      assertEquals(unwilling.status, 400, "a receiver that does not take the ping is not registered");
      assert((await unwilling.text()).includes("ping"), "and the refusal says so");
      answer = 200;
      assertEquals((await register("http://93.184.216.34/hook?via=sheets")).status, 201);
      assertEquals(JSON.parse(heard.at(-1)!.body).event, "ping", "registration is a signed ping");
      assertEquals((await register("http://93.184.216.34/hook?via=sheets")).status, 201);
      assertEquals((await hooks()).map((h) => h.url), ["http://93.184.216.34/hook?via=sheets"], "one row per url");
      assertEquals(
        (await app.request(`/library/${sheet_id}/webhook`, { headers: new Headers({ Authorization: `Bearer ${viewer}` }) }))
          .status,
        403,
        "a receiver's url is a credential, so a viewer may not read it",
      );
      for (let i = 1; i < WEBHOOKS_PER_SHEET_MAX; i++) assertEquals((await register(`http://93.184.216.34/${i}`)).status, 201);
      const past = await register("http://93.184.216.34/one-too-many");
      assertEquals(past.status, 413);
      assert((await past.text()).includes(`${WEBHOOKS_PER_SHEET_MAX} per sheet`), "naming the cap");
      for (let i = 1; i < WEBHOOKS_PER_SHEET_MAX; i++) {
        await request(jwt, `/library/${sheet_id}/webhook`, {
          method: "DELETE",
          body: JSON.stringify({ url: `http://93.184.216.34/${i}` }),
        });
      }
      assertEquals((await hooks()).length, 1);
      heard.length = 0;

      // Delivery: the receiver hears one signed POST per flush however many
      // changes landed, verifiable with the secret GET /library/:id/hook answers.
      // A change is heard as the save the repo makes of it, which is debounced.
      // `flush()` makes that save now, and a save with nothing new emits
      // nothing, so the debounced one still queued behind it stays silent. The
      // creation save is drained before anything is counted.
      const flushed = async () => {
        await automerge.flush();
        await flushWebhooks();
      };
      await flushed();
      heard.length = 0;
      await flushed();
      assertEquals(heard.length, 0, "nothing changed, nothing is sent");
      await post(jwt, `/sheet/${sheet_id}`, { rows: [{ a: "y" }] });
      await post(jwt, `/sheet/${sheet_id}`, { rows: [{ a: "z" }] });
      await flushed();
      assertEquals(heard.length, 1, "two changes before a flush are one delivery");
      assertEquals(heard[0].url, "http://93.184.216.34/hook?via=sheets");
      const body = JSON.parse(heard[0].body);
      assertEquals(body.sheet_id, sheet_id, "the delivery names the sheet");
      const { secret }: { secret: string } = await get(jwt, `/library/${sheet_id}/hook`);
      const t = heard[0].headers["scrapsheets-signature"].match(/^t=(\d+),v2=/)?.[1];
      assert(t, `the signature is the inbound scheme: ${heard[0].headers["scrapsheets-signature"]}`);
      assertEquals(
        heard[0].headers["scrapsheets-signature"],
        await hookSign(secret, "/hook?via=sheets", heard[0].body, Number(t)),
        "and verifies with the sheet's own secret over the receiver's path and the body",
      );
      const [{ status: outcome, failures }] = await hooks();
      assertEquals([Number(outcome), Number(failures)], [200, 0], "the outcome lands on the hook");

      // A change over the sync socket is a change too.
      (await automerge.find<Sheet>(hand.documentId)).change((d) => {
        (d.data as unknown as Record<string, unknown>[])[1] = { 0: "synced" };
      });
      await flushed();
      assertEquals(heard.length, 2, "a document change over sync is delivered");

      // A receiver that keeps failing is left alone after WEBHOOK_FAILS_MAX,
      // and the row says so.
      answer = 500;
      for (let i = 0; i < WEBHOOK_FAILS_MAX; i++) {
        await post(jwt, `/sheet/${sheet_id}`, { rows: [{ a: `fail ${i}` }] });
        await flushed();
      }
      assertEquals(heard.length, 2 + WEBHOOK_FAILS_MAX, "each failure was tried");
      const [dead] = await hooks();
      assertEquals([Number(dead.status), Number(dead.failures)], [500, WEBHOOK_FAILS_MAX]);
      await post(jwt, `/sheet/${sheet_id}`, { rows: [{ a: "after" }] });
      await flushed();
      assertEquals(heard.length, 2 + WEBHOOK_FAILS_MAX, "and a dead hook is not tried again");
      assert((await status())["Every webhook's last delivery in the past day was accepted."]["0"] < 1);

      // Setting it again is the owner saying try again.
      answer = 200;
      await post(jwt, `/library/${sheet_id}/webhook`, { url: "http://93.184.216.34/hook?via=sheets" });
      assertEquals(JSON.parse(heard.at(-1)!.body).event, "ping", "and setting it again pinged it again");
      await post(jwt, `/sheet/${sheet_id}`, { rows: [{ a: "again" }] });
      await flushed();
      assertEquals(heard.length, 4 + WEBHOOK_FAILS_MAX, "a re-set hook is tried again");
      assertEquals(JSON.parse(heard.at(-1)!.body).event, "change");

      // A delivery to a net sheet is a change to it.
      inboxDoc = automerge.create<Sheet>({ type: "net-hook", data: [] });
      const inboxId = `net-hook:${inboxDoc.documentId}`;
      await put(jwt, `/library/${inboxId}`, {});
      await post(jwt, `/library/${inboxId}/webhook`, { url: "http://93.184.216.34/inbox" });
      const pinged = heard.length;
      assert((await deliver(inboxId, JSON.stringify({ event: "in" }))).ok);
      await flushed();
      assertEquals(heard.length, pinged + 1, "a delivery in is a delivery out");
      assertEquals(heard.at(-1)?.url, "http://93.184.216.34/inbox");
      assertEquals(JSON.parse(heard.at(-1)!.body).event, "change");
    } finally {
      globalThis.fetch = origFetch;
      // Nothing after this step may post to the address above for real.
      await sql`delete from webhook where sheet_id in (${sheet_id}, ${`net-hook:${inboxDoc.documentId}`})`;
    }
  });

  // The first thing a new feed did was wait for the poller to fail. A
  // pre-flight is the poller's request, once, now, and nothing is written.
  await t.step("You test the request before you save it", async () => {
    const { jwt } = await usr("pilot@example.com");
    const { jwt: viewer } = await usr("copilot@example.com");
    const hand = automerge.create<Sheet>({
      type: "net-http",
      data: [{ url: "http://93.184.216.34/feed", interval: 3600, headers: "X-Api-Key: {{secret:weather}}" }],
    });
    const sheet_id = `net-http:${hand.documentId}`;
    await put(jwt, `/library/${sheet_id}`, {});
    await post(jwt, `/library/${sheet_id}/share`, { email: "copilot@example.com", role: "viewer" });
    const test = (body: unknown, who = jwt) =>
      app.request(`/library/${sheet_id}/preflight`, {
        method: "POST",
        headers: new Headers({ "Content-Type": "application/json", Authorization: `Bearer ${who}` }),
        body: JSON.stringify(body),
      });
    const asked: { url: string; headers: Record<string, string> }[] = [];
    let answer: () => Response | Promise<Response> = () =>
      new Response(`{"ok":true}`, { status: 200, headers: { "content-type": "application/json" } });
    const origFetch = globalThis.fetch;
    globalThis.fetch = ((url: string, init?: RequestInit) => {
      asked.push({ url, headers: Object.fromEntries(new Headers(init?.headers).entries()) });
      return Promise.resolve(answer());
    }) as typeof fetch;
    try {
      const request = { url: "http://93.184.216.34/feed", headers: "X-Api-Key: {{secret:weather}}" };
      assertEquals((await test(request, viewer)).status, 403, "a viewer may not run the sheet's request");
      assertEquals((await test({})).status, 400, "a pre-flight needs a url");
      const inside = await test({ url: "http://127.0.0.1:9/feed" });
      assertEquals(inside.status, 400, "a url inside our network is refused by name");
      assert((await inside.text()).includes("own network"));
      // A header naming a secret the sheet does not hold is refused now, where
      // the poller would have written a failure row an hour from now.
      const missing = await test(request);
      assertEquals(missing.status, 400);
      assert((await missing.text()).includes("{{secret:weather}}"), "naming the reference");
      assertEquals(asked.length, 0, "and nothing was sent");

      await post(jwt, `/library/${sheet_id}/secret`, { name: "weather", value: "sk-test" });
      const { data: got }: { data: { status: number; ms: number; bytes: number; content_type: string; body: string } } =
        await (await test(request)).json();
      assertEquals([got.status, got.bytes, got.content_type, got.body], [200, 11, "application/json", `{"ok":true}`]);
      assert(Number.isInteger(got.ms) && got.ms >= 0, `ms is a number: ${got.ms}`);
      assertEquals(asked.at(-1)?.headers["x-api-key"], "sk-test", "with the secret resolved");
      assertEquals(asked.at(-1)?.url, "http://93.184.216.34/feed");

      // What the host answered, whatever it answered, and a wire failure as
      // the poller's own failure record.
      answer = () => new Response("gone", { status: 404 });
      const { data: gone }: { data: { status: number; body: string } } = await (await test(request)).json();
      assertEquals([gone.status, gone.body], [404, "gone"]);
      answer = () => Promise.reject(new Error("connection reset"));
      const { data: failed }: { data: { error: string; status: number | null } } = await (await test(request)).json();
      assertEquals(failed.status, null);
      assert(failed.error.includes("connection reset"), `the failure is the poller's record: ${failed.error}`);

      const [{ n }] = await sql`select count(*)::int as n from net where sheet_id = ${sheet_id}`;
      assertEquals(n, 0, "a pre-flight writes nothing");
    } finally {
      globalThis.fetch = origFetch;
      hostDue.delete("93.184.216.34");
    }
  });

  // Import inferred silently and there was no way back. Now the guess is
  // shown first, the user settles the types, and the sheet is made with those.
  await t.step("You see the type guess and correct it", async () => {
    const { jwt } = await usr("guesser@example.com");
    const csv = "n,flag,label\n1,true,a\n2,false,b\n";
    const send = (path: string, body = csv) =>
      app.request(path, {
        method: "POST",
        headers: new Headers({ "Content-Type": "text/csv", Authorization: `Bearer ${jwt}` }),
        body,
      });
    const sheets = async () => (await sql`select count(*)::int as n from sheet where created_by = (select usr_id from usr where email = 'guesser@example.com')`)[0].n;

    // The preview: every column with the type it was guessed to carry, the
    // first rows under them, and no sheet.
    const preview = await send("/import/preview");
    assertEquals(preview.status, 200);
    const { data: seen } = await preview.json();
    assertEquals(seen.cols.map((c: { name: string; type: string }) => `${c.name}:${c.type}`), ["n:num", "flag:bool", "label:text"]);
    assertEquals(seen.rows, [{ "0": 1, "1": true, "2": "a" }, { "0": 2, "1": false, "2": "b" }]);
    assertEquals(seen.count, 2);
    assertEquals(await sheets(), 0, "a preview makes no sheet");

    // A settled type overrides the guess, and the sheet is made with it.
    const types = (t: unknown) => `?types=${encodeURIComponent(JSON.stringify(t))}`;
    const made = await send("/import/csv" + types({ n: "text" }));
    const madeText = await made.text();
    assertEquals(made.status, 201, madeText);
    const { sheet_id } = JSON.parse(madeText);
    const [cols, first] = await get<Table>(jwt, `/sheet/${sheet_id}`);
    assertEquals((cols as Record<string, { type: string }>)["0"]?.type ?? Object.values(cols)[0], "text");
    assertEquals(first.n, "1", "the value is kept as the text the file had");

    // A settled type the values do not fit is refused on the line that does
    // not, and the types themselves are checked before the file is read.
    const unfit = await send("/import/csv" + types({ label: "num" }));
    assertEquals(unfit.status, 400);
    const said = await unfit.text();
    assert(said.includes("Line 2") && said.includes('"label"'), `the refusal names the line and the column: ${said}`);
    assertEquals((await send("/import/csv" + types({ n: "nope" }))).status, 400, "an unknown type is refused");
    assertEquals((await send("/import/csv" + types({ ghost: "num" }))).status, 400, "a column the file lacks is refused");
    assertEquals((await send("/import/csv?types=not-json")).status, 400, "types that are not JSON are refused");
    assertEquals(await sheets(), 1, "and none of those made a sheet");
  });

  // A feed that drops a column read as a sheet of blanks and graded as
  // healthy. The columns a run answered with are recorded beside it, and a run
  // whose columns differ from the run before is a failed run naming the
  // difference -- through POLL_OK, so freshness and the status alarm hear it
  // the way they hear every other failure.
  await t.step("You are told when an upstream feed changes shape", async () => {
    const { jwt } = await usr("shapes@example.com");
    const feed = automerge.create<Sheet>({ type: "net-http", data: [{ url: "https://shaped.test/rows", interval: 60 }] });
    const feedId = `net-http:${feed.documentId}`;
    await put(jwt, `/library/${feedId}`, {});
    let answer = `[{"id":1,"name":"a"}]`;
    const fetcher = (url: string) =>
      Promise.resolve(url === "https://shaped.test/rows" ? new Response(answer) : new Response(`{}`));
    const newest = async () => {
      const [row]: { body: string; meta: Record<string, unknown> }[] = await sql`
        select body, meta from net where sheet_id = ${feedId} order by net_id desc limit 1
      `;
      return row;
    };
    const failures = async () =>
      Number((await get<Table>(jwt, `/sheet/library:freshness`)).slice(1).find((r) => r.sheet_id === feedId)?.failures_since_ok);
    const at = Date.now() + 110_000_000;

    await pollNetOnce(fetcher, at);
    assertEquals((await newest()).meta.shape, { id: "number", name: "string" }, "the columns a run answered with ride its row");
    assertEquals(await failures(), 0);

    // A column added, one retyped: the data lands, and the run is a failure
    // naming what changed.
    answer = `[{"id":"1","name":"a","extra":true},{"id":"2","name":null}]`;
    await pollNetOnce(fetcher, at + 61_000);
    const changed = await newest();
    assertEquals(changed.body, answer, "the rows that arrived are kept");
    assertEquals(changed.meta.shape, { extra: "boolean", id: "string", name: "string" });
    assertEquals(changed.meta.shape_change, { added: ["extra"], dropped: [], retyped: ["id"] }, "and the row names the change");
    assertEquals(await failures(), 1, "which freshness counts as a failed run");
    assert((await status())["Every net-http poll in the past hour returned 2xx in the shape the feed had before."]["0"] < 1);

    // The same shape again is the new normal.
    await pollNetOnce(fetcher, at + 122_000);
    assertEquals((await newest()).meta.shape_change, undefined, "the run after is not a change");
    assertEquals(await failures(), 0);

    // A feed that stops answering rows at all dropped every column, once.
    answer = "<html>maintenance</html>";
    await pollNetOnce(fetcher, at + 183_000);
    const gone = await newest();
    assertEquals(gone.meta.shape, null, "a body that is not rows has no shape");
    assertEquals(gone.meta.shape_change, { added: [], dropped: ["extra", "id", "name"], retyped: [] });
    await pollNetOnce(fetcher, at + 244_000);
    assertEquals((await newest()).meta.shape_change, undefined, "and nothing is compared against no shape");
  });

  // A host with no validators answered the same body every poll and every
  // poll appended it. A run's body is its idempotency key, in the slot the
  // delivery index already refuses a replay on; and a failed run is in the
  // sheet's log and not in a query over it, so one bad poll cannot empty what
  // is built downstream.
  await t.step("A re-run does not double-append", async () => {
    const { jwt } = await usr("rerun@example.com");
    const feed = automerge.create<Sheet>({ type: "net-http", data: [{ url: "https://rerun.test/rows", interval: 60 }] });
    const feedId = `net-http:${feed.documentId}`;
    await put(jwt, `/library/${feedId}`, {});
    let answer = () => new Response(`[{"n":1}]`);
    const fetcher = (url: string) => Promise.resolve(url === "https://rerun.test/rows" ? answer() : new Response(`{}`));
    const log = async () => (await get<Table>(jwt, `/sheet/${feedId}`)).slice(1);
    const queried = async () => {
      const { data: [, ...rows] }: { data: Table } = await post(jwt, `/query`, {
        lang: "sql",
        code: `select body from @${feedId}`,
        args: [],
      });
      return rows.map((r) => String(r.body));
    };
    const at = Date.now() + 130_000_000;

    await pollNetOnce(fetcher, at);
    await pollNetOnce(fetcher, at + 61_000);
    const [same] = await log();
    assertEquals((await log()).length, 1, "the same body twice is one row");
    const meta = JSON.parse(String(same.meta));
    assert(meta.sig, "keyed by its digest, in the slot a delivery's signature takes");
    assertEquals(meta.repeated, true, "and the row says it came again");

    answer = () => new Response(`[{"n":2}]`);
    await pollNetOnce(fetcher, at + 122_000);
    assertEquals((await log()).length, 2, "a different body is a second row");
    answer = () => new Response(`[{"n":1}]`);
    await pollNetOnce(fetcher, at + 183_000);
    const back = await log();
    assertEquals(back.length, 2, "a body this sheet already holds is not a third");
    assertEquals(String(back[0].body), `[{"n":1}]`, "the row it matches is the newest again");

    // One failed poll: the log shows it, a query over the feed does not, so a
    // sheet built downstream keeps what it had while freshness says why.
    assertEquals(await queried(), [`[{"n":1}]`, `[{"n":2}]`]);
    answer = () => new Response("boom", { status: 500 });
    await pollNetOnce(fetcher, at + 244_000);
    assertEquals((await log()).length, 3, "the failed run is in the log");
    assertEquals(await queried(), [`[{"n":1}]`, `[{"n":2}]`], "and not in a query over the feed");
    const mine = (await get<Table>(jwt, `/sheet/library:freshness`)).slice(1).find((r) => r.sheet_id === feedId);
    assertEquals(Number(mine?.failures_since_ok), 1, "which is where the failure is told");
  });

  await t.step("The bucket a flood is counted against must be one the flood cannot choose", async () => {
    const ip = (xff: string | undefined, remote?: string) =>
      callerIp(
        {
          req: { header: (name: string) => (name === "x-forwarded-for" ? xff : undefined) },
          env: remote ? { remoteAddr: { hostname: remote } } : undefined,
        } as unknown as Parameters<typeof callerIp>[0],
      );
    // The rightmost entry is the one our proxy appended; the leftmost is
    // whatever the caller typed, so keying on it rotated the bucket for free.
    assertEquals(ip("9.9.9.9, 1.2.3.4"), "1.2.3.4");
    assertEquals(ip("1.2.3.4"), "1.2.3.4");
    // Nothing proxied us, so the socket's own peer is the caller.
    assertEquals(ip(undefined, "5.6.7.8"), "5.6.7.8");
    assertEquals(ip(undefined), "127.0.0.1");
  });

  // BODY_CAP bounds a delivery and trimNet bounds the log, so a sender posting flat out loses nothing of its own
  // -- it evicts everything the sheet held before it. The budget is keyed on the sheet, not on callerIp(), because a
  // webhook sender is one machine that will not rotate its address. So the ways this breaks are: a bound that leaks
  // across sheets, a bound a refused delivery pays for, and a count bound alone, which one 1 MB body a second walks
  // straight through.
  await t.step("One noisy sender may not fill one sheet", async () => {
    const { jwt } = await usr("nyx-flood@example.com");
    const netSheet = async () => {
      const hand = automerge.create<Sheet>({ type: "net-hook", data: [] });
      const id = `net-hook:${hand.documentId}`;
      await put(jwt, `/library/${id}`, {});
      return id;
    };
    // Bounded: a flood that never earns a 429 is the failure, so the loop
    // crashes rather than running forever. Every body differs, or the unique
    // signature index refuses the second one and nothing reaches the budget.
    const flood = async (id: string, fill: string, cap: number) => {
      for (let n = 1; n <= cap; n++) {
        const res = await deliver(id, JSON.stringify({ n, fill }));
        if (res.status === 429) return { n, text: await res.text() };
        assert(res.ok, `delivery ${n} to ${id} was refused with ${res.status}: ${await res.text()}`);
      }
      throw new Error(`Expected a 429 within ${cap} deliveries to ${id}, received none.`);
    };

    // The count bound. Everything before the refusal landed, which is the honest
    // sender: the budget only bites once the sender is past it.
    const busy = await netSheet();
    const many = await flood(busy, "", HOOK_ROWS_PER_WINDOW + 3);
    assert(many.n > HOOK_ROWS_PER_WINDOW, `a sender inside the bound must not be refused, refused at ${many.n}`);
    assert(many.text.includes(`${HOOK_ROWS_PER_WINDOW} deliveries`), `it must name the limit, got: ${many.text}`);
    assert(many.text.includes(`${HOOK_WINDOW_S} seconds`), `it must name the window, got: ${many.text}`);
    // A refused delivery writes no row, so the budget and the log agree.
    const [{ n: kept }] = await sql`select count(*)::int as n from net where sheet_id = ${busy}`;
    assertEquals(kept, many.n - 1, "a refused delivery must not be stored");

    // The byte bound, which a count bound cannot see: few deliveries, each just
    // under the per-delivery cap.
    const fill = "x".repeat(900_000);
    const each = Math.ceil(HOOK_BYTES_PER_WINDOW / fill.length);
    const wide = await netSheet();
    const big = await flood(wide, fill, each + 3);
    assert(big.n <= each + 1, `the byte bound must refuse by delivery ${each + 1}, refused at ${big.n}`);
    assert(big.n < HOOK_ROWS_PER_WINDOW, "and it must refuse long before the count bound does");
    assert(big.text.includes(`${HOOK_BYTES_PER_WINDOW} bytes`), `it must name the byte limit, got: ${big.text}`);
    assert(big.text.includes(`${HOOK_WINDOW_S} seconds`), `it must name the window, got: ${big.text}`);

    // Thirty senders at once against a budget of one. The check and the charge
    // are one step with no await between them, or every sender reads the same
    // budget, every one passes, and thirty rows land on a budget of one.
    const race = await netSheet();
    hookBucket(race).rows = 1;
    const raced = await Promise.all(
      Array.from({ length: 30 }, (_, n) => deliver(race, JSON.stringify({ n, race: true }))),
    );
    assertEquals(raced.filter((res) => res.ok).length, 1, "one delivery may land on a budget of one");
    assertEquals(raced.filter((res) => res.status === 429).length, 29, "and the rest are refused, not lost");
    const [{ n: landed }] = await sql`select count(*)::int as n from net where sheet_id = ${race}`;
    assertEquals(landed, 1, "concurrent senders must not outrun the budget");

    // A replay is charged nothing: the budget protects the sender, and one
    // captured delivery must not be able to spend it. Signed once, sent twice,
    // then a fresh delivery on the one row of budget the replay must have
    // given back.
    hookBucket(race).rows = 2;
    const once = JSON.stringify({ event: "once" });
    const signature = await hookSign(await hookSecret(race), `/net/${race}`, once);
    const replay = (body: string, sig: string) =>
      app.request(`/net/${race}`, {
        method: "POST",
        headers: new Headers({ "Content-Type": "application/json", "scrapsheets-signature": sig }),
        body,
      });
    assert((await replay(once, signature)).ok, "the first copy lands");
    assertEquals((await replay(once, signature)).status, 409, "the second is a replay");
    const after = await deliver(race, JSON.stringify({ event: "after the replay" }));
    assert(after.ok, `a replay must refund what it charged, got: ${after.status} ${await after.text()}`);

    // The bound belongs to the sheet, so two sheets over budget refuse nothing
    // on a third.
    const quiet = await netSheet();
    const alone = await deliver(quiet, JSON.stringify({ event: "quiet" }));
    assert(alone.ok, `a sheet nobody flooded must still take deliveries, got: ${alone.status}`);

    // Bounded like rateLimitBuckets and for the same reason: sweeping by idle
    // time alone loses to a caller minting keys faster than the broom runs.
    for (let i = 0; i <= RATE_LIMIT_KEYS_MAX; i++) hookBucket(`net-hook:minted-${i}`);
    assertEquals(hookBuckets.size, RATE_LIMIT_KEYS_MAX, "the delivery budget map must stay bounded");
    hookBuckets.clear();
  });

  // The status check. Every condition is graded so that 1.0 is the minimum pass, which is what lets an uptime check
  // read the whole thing without knowing what any of it means.
  await t.step("Every status condition is graded so that 1.0 is the minimum pass", async () => {
    const grades = await status();
    const conditions = Object.keys(grades);
    assertEquals(conditions.length, 16);
    for (const [condition, series] of Object.entries(grades)) {
      assert(condition.endsWith("."), `a condition is a sentence: ${condition}`);
      assert("0" in series, `${condition} must be graded now`);
      for (const value of Object.values(series))
        assert(Number.isFinite(value), `${condition} graded ${value}, which says nothing`);
    }
    // A historical condition carries the trend; a live one carries only now.
    assertEquals(Object.keys(grades["No request failed with a 5xx in the past hour."]), ["0", "3600", "86400"]);
    assertEquals(Object.keys(grades["The database is under 4 GB."]), ["0"]);

    // A row this code did not write must degrade one grade, never take the
    // endpoint down: an uptime checker cannot tell a 500 here from a dead
    // server. Both of these used to raise out of a cast.
    await sql`
      insert into net (sheet_id, method, body, meta)
      values ('net-hook:errors', 'GET', 'x', '{"status":"oops"}'::jsonb),
             ('net-hook:errors', 'ALERT', 'not json at all', '{}'::jsonb)
    `;
    const survived = await app.request("/status");
    assertEquals(
      Object.keys(await survived.json()).length,
      16,
      "a malformed row must degrade a grade, not replace the whole answer with an error",
    );

    // A clear run delivered nothing, which is the healthy outcome and not a
    // failure. Graded the other way, an alert going quiet held /status at 503
    // for a day -- and an unchanged run, recorded every interval now, would
    // have held it there for good.
    const [quietAlert] = await sql`select sheet_id from sheet where type = 'alert' limit 1`;
    assert(quietAlert, "the alert block above should have left a sheet to grade");
    await sql`delete from net`;
    await sql`
      insert into net (sheet_id, method, body, meta) values
        (${quietAlert.sheet_id}, 'ALERT',
         '{"status":"clear","rows":0,"delivery":"cleared, so nothing was sent"}', '{"interval":60}'::jsonb),
        (${quietAlert.sheet_id}, 'ALERT',
         '{"status":"unchanged","rows":1,"delivery":"the same answer as the run before, so nothing was sent"}',
         '{"interval":60}'::jsonb)
    `;
    assertEquals(
      (await status())["Every alert run in the past day either delivered or had nothing to deliver."]["0"],
      1,
      "an alert with nothing to say is not an undelivered alert",
    );
    assertEquals((await app.request("/status")).status, 200, "a quiet alert is not an outage");

    // A poller that stopped. Every run is recorded now, so the newest one being
    // older than twice its own interval is what separates a dead setInterval
    // from an alert that simply had nothing to say.
    await sql`update net set created_at = now() - interval '10 minutes' where sheet_id = ${quietAlert.sheet_id}`;
    assert(
      (await status())["Every alert sheet ran within twice its own interval."]["0"] < 1,
      "an alert that stopped running must grade as stopped",
    );
    // An alert that has never run at all -- a poller that never fired once, on
    // a cold isolate. An inner join would drop it, and an empty set grades as a
    // pass, so the condition would report healthy on exactly the failure it is
    // for. Graded from the sheet's own creation instead.
    await sql`delete from net`;
    await sql`update sheet set created_at = now() - interval '3 hours' where type = 'alert'`;
    assert(
      (await status())["Every alert sheet ran within twice its own interval."]["0"] < 1,
      "an alert that has never run must grade as never run",
    );

    // Done with the alert sheets. Every condition below is about an idle
    // product, and an alert sheet with no runs at all is a dead poller, not an
    // idle one -- which is what the liveness condition above just proved.
    await sql`delete from net`;
    await sql`delete from sheet_usr where sheet_id like 'alert:%'`;
    await sql`delete from sheet where type = 'alert'`;

    // A condition that is reported but does not page: a service with no users is
    // not a service that is down. It still has to be graded and still has to be
    // in the answer.
    assert(
      (await status())["Somebody created a sheet in the past 24 hours."]["0"] >= 0,
      "the usage condition is still graded",
    );
    await sql`update sheet set created_at = now() - interval '3 days'
              where created_by <> (select usr_id from usr where email = '')`;
    const idle = await app.request("/status");
    assertEquals(idle.status, 200, "a product nobody used today is not an outage");
    assertEquals((await idle.json())["Somebody created a sheet in the past 24 hours."]["0"], 0);

    // 200 is reachable. The suite has spent the run filling the error log and
    // failing net-http polls on purpose, so a clean log is what proves the
    // healthy path exists at all -- without this the 503 below passes for
    // whatever happens to be true, which is no test.
    await sql`delete from net`;
    const clean = await app.request("/status");
    assertEquals(
      clean.status,
      200,
      `every condition must pass on a healthy database: ${JSON.stringify(await clean.json())}`,
    );

    // The refusals condition reads `meta->>'status'`, which answers null on a
    // jsonb string -- so a log written as one made the check count nothing and
    // grade every hour as clean. Count a row this app actually wrote, with the
    // condition's own predicate, rather than one the test hand-wrote.
    await sql`
      insert into sheet (created_by, type, doc_id, name)
      values ((select usr_id from usr where email = ''), 'net-hook', 'refusal-probe', 'refusal probe')
      on conflict (doc_id) do nothing
    `;
    await app.request("/net/net-hook:refusal-probe", { method: "POST", body: "{}" });
    await errorLogged();
    const [{ refused: refusedRows }] = await sql`
      select count(*) as refused from net
      where sheet_id = 'net-hook:errors'
        and substring(meta->>'status' from '^[0-9]{1,9}$')::int = 401
        and meta->>'path' like '/net/%'
    `;
    assertEquals(Number(refusedRows), 1, "the refusals condition must count rows this app wrote");
    await sql`delete from net`;

    // A 500 in the log is what pulls that condition under 1.0, and one failing
    // condition is what turns the route 503 -- so a checker that reads only the
    // status line still learns the answer.
    await sql`
      insert into net (sheet_id, method, body, meta)
      values ('net-hook:errors', 'GET', 'boom', '{"status":500,"path":"/boom"}'::jsonb)
    `;
    assert((await status())["No request failed with a 5xx in the past hour."]["0"] < 1);
    const res = await app.request("/status");
    assertEquals(res.status, 503);
    // Public, because an uptime check carries no bearer token, and grades only.
    const body = await res.json();
    for (const series of Object.values(body as Record<string, Record<string, number>>))
      for (const value of Object.values(series)) assertEquals(typeof value, "number");
    await sql`delete from net where sheet_id = 'net-hook:errors' and body = 'boom'`;
  });

  await sql.end();
  listener.close();
  await pglite.close();
});
