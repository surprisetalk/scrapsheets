import { assert, assertEquals, assertStringIncludes, assertThrows } from "@std/assert";
import * as AM from "@automerge/automerge-repo";
import { decodeSyncMessage } from "@automerge/automerge";
import { WebSocketClientAdapter } from "@automerge/automerge-repo-network-websocket";
import Stripe from "stripe";
import * as XLSX from "xlsx";
import { parquetReadObjects } from "hyparquet";
import { parquetWriteBuffer } from "hyparquet-writer";
import {
  ACCOUNT_FRAME_BURST,
  ACCOUNT_FRAME_RATE,
  app,
  arrayify,
  automerge,
  BODY_CAP,
  createToken,
  FRAME_BURST,
  FRAME_RATE,
  hookBuckets,
  hookSecret,
  hookSign,
  HOST_GAP_MS,
  hostDue,
  type Method,
  NET_KEEP,
  netDue,
  PAGE_MAX,
  parseNetHeaders,
  pollNetOnce,
  RATE_LIMIT_KEYS_MAX,
  requireSecret,
  seed,
  sendDigestOnce,
  sql,
  syncAccessCache,
  trimNet,
  zipData,
  zipMembers,
} from "./main.ts";
import type { Col, NetHttp, Query, Sheet, Table, Template } from "./main.ts";
import { DATASETS } from "./src/examples.mjs";
import ala from "alasql";
import { boot, deliver, FLOOD, get, pollAlert, pollFeed, post, put, reject, request, usr } from "./main_harness.ts";

Deno.test(async function allTests(t) {
  const stack = await boot();

  await t.step("A secret-less boot is a crash, not a warning", () => {
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

    // A peer cannot flood the document's other peers with ephemeral frames: an
    // oversize frame and a burst stop at the server, the sender is told once,
    // and its sync frames keep flowing.
    {
      const adapter = new WebSocketClientAdapter(wsUrl(jwt), 100);
      const reader = new AM.Repo({ network: [adapter], peerId: "client-ephemeral-reader" as AM.PeerId });
      const handle = await reader.find<Sheet>(hand.documentId);
      let heard = 0;
      handle.on("ephemeral-message", () => heard++);

      const until = async (what: string, done: () => boolean) => {
        for (let i = 0; i < 400 && !done(); i++) await new Promise((r) => setTimeout(r, 5));
        if (!done()) throw new Error(`No ${what} within 2s.`);
      };
      // Each connection keeps its own bucket and its own notice clock, so the
      // oversize case gets a second connection instead of a wait past the notice.
      const flooder = async (senderId: string) => {
        const ws = new WebSocket(wsUrl(jwt));
        ws.binaryType = "arraybuffer";
        const frames: Record<string, unknown>[] = [];
        ws.onmessage = (e) =>
          frames.push(AM.cbor.decode(new Uint8Array(e.data as ArrayBuffer)) as Record<string, unknown>);
        await new Promise<void>((resolve) => {
          ws.onopen = () => resolve();
        });
        ws.send(AM.cbor.encode({ type: "join", senderId, peerMetadata: {}, supportedProtocolVersions: ["1"] }));
        await until("peer reply", () => frames.some((f) => f.type === "peer"));
        let count = 0;
        const self = {
          ws,
          frames,
          errors: () => frames.filter((f) => f.type === "error" && String(f.message).startsWith("Ephemeral")),
          sendData: (data: Uint8Array) =>
            ws.send(AM.cbor.encode({
              type: "ephemeral",
              senderId,
              targetId: automerge.peerId,
              documentId: hand.documentId,
              count: ++count,
              sessionId: `${senderId}-session`,
              data,
            })),
          send: (bytes: number) => self.sendData(AM.cbor.encode({ pad: new Uint8Array(bytes) })),
        };
        return self;
      };

      // The relay is live: a small frame reaches the reader.
      const burst = await flooder("raw-flooder");
      burst.send(10);
      await until("relayed ephemeral frame", () => heard === 1);

      // A burst of 30: the bucket holds 20 and has spent one, and the refill
      // during the burst is a few tokens at most. An oversize frame inside the
      // same second is dropped with no second notice.
      for (let i = 0; i < 30; i++) burst.send(10);
      burst.send(5000);
      await until("rate error", () => burst.errors().length === 1);
      assertStringIncludes(String(burst.errors()[0].message), "a second");
      assertEquals(burst.frames.some((f) => f.type === "disconnect"), false);

      // A sync frame on the same connection still reaches the server, and its
      // reply comes after every frame queued before it.
      const before = burst.frames.length;
      burst.ws.send(AM.cbor.encode({
        type: "request",
        senderId: "raw-flooder",
        targetId: automerge.peerId,
        documentId: "no\u0000such",
        data: new Uint8Array(),
      }));
      await until("sync reply", () => burst.frames.slice(before).some((f) => f.type === "doc-unavailable"));
      assertEquals(burst.errors().length, 1, "one error frame per second per connection");
      assert(heard <= 1 + 20 + 5, `the reader heard ${heard} frames, past the burst`);

      const big = await flooder("raw-oversize");
      big.send(5000);
      await until("oversize error", () => big.errors().length === 1);
      assertStringIncludes(String(big.errors()[0].message), "at most");
      // Presence is stamped by the server with who sent it: a client that claims
      // another name or colour is overwritten, and a cell rides through.
      await sql`update usr set name = 'Erin' where email = 'erin@example.com'`;
      const heardPresence: Record<
        string,
        { type: string; state?: Record<string, unknown>; channel?: string; value?: unknown }
      >[] = [];
      handle.on("ephemeral-message", (e) => heardPresence.push(e.message as (typeof heardPresence)[0]));
      const named = await flooder("raw-presence");
      const presence = (message: unknown) => named.sendData(AM.cbor.encode({ __presence: message }));
      presence({ type: "snapshot", state: { colour: "#ff0000", name: "Mallory", cell: { x: 1, y: 2 } } });
      presence({ type: "update", channel: "name", value: "Mallory" });
      presence({ type: "update", channel: "colour", value: "#ff0000" });
      presence({ type: "update", channel: "cell", value: { x: 3, y: 4 } });
      await until("four presence frames", () => heardPresence.length === 4);
      const [snap, rename, recolour, cell] = heardPresence.map((m) => m.__presence);
      assertEquals(snap.state?.name, "Erin", "the snapshot's name is the account's, not the claimed one");
      assertEquals(snap.state?.cell, { x: 1, y: 2 }, "the cell is the sender's");
      assert(
        /^#[0-9a-f]{6}$/.test(String(snap.state?.colour)) && snap.state?.colour !== "#ff0000",
        "the colour is derived",
      );
      assertEquals(rename.value, "Erin", "a name update is overwritten");
      assertEquals(recolour.value, snap.state?.colour, "a colour update is overwritten with the same derived colour");
      assertEquals(cell.value, { x: 3, y: 4 }, "a cell update passes through");
      // A channel named like an Object.prototype member is not a stamped channel:
      // it must neither throw in the relay nor get a function as its value.
      heardPresence.length = 0;
      for (const channel of ["constructor", "toString", "__proto__", "hasOwnProperty"])
        presence({ type: "update", channel, value: "x" });
      presence({ type: "update", channel: "cell", value: { x: 5, y: 6 } });
      await until("prototype-named channels relayed", () => heardPresence.length === 5);
      assertEquals(heardPresence.map((m) => m.__presence.value), ["x", "x", "x", "x", { x: 5, y: 6 }]);
      named.ws.close();
      burst.ws.close();
      big.ws.close();
      adapter.disconnect();
    }

    // Frames that name a document stop at one bucket before the access check, so
    // a flood of random document ids costs a bounded number of database lookups.
    // A real repo opening ten documents is nowhere near it.
    {
      const docs = Array.from(
        { length: 10 },
        (_, i) =>
          automerge.create<Sheet>({ type: "table", data: [arrayify([{ name: "a", type: "num", key: 0 }]), { 0: i }] }),
      );
      for (const d of docs) await put(jwt, `/library/table:${d.documentId}`, {});
      const adapter = new WebSocketClientAdapter(wsUrl(jwt), 100);
      const repo = new AM.Repo({ network: [adapter], peerId: "client-opens-ten" as AM.PeerId });
      await repo.networkSubsystem.whenReady();
      let sent = 0;
      const sock = (adapter as unknown as { socket: WebSocket }).socket;
      const send = sock.send.bind(sock);
      sock.send = (data) => {
        sent++;
        return send(data);
      };
      const found = await Promise.all(docs.map((d) => repo.find<Sheet>(d.documentId)));
      found.forEach((h, i) => assertEquals(JSON.parse(JSON.stringify(h.doc().data[1])), { 0: i }));
      assert(sent < FRAME_BURST / 5, `a real client sent ${sent} frames to open ten documents`);
      adapter.disconnect();

      const ws = new WebSocket(wsUrl(jwt));
      ws.binaryType = "arraybuffer";
      const replies: Record<string, unknown>[] = [];
      ws.onmessage = (e) =>
        replies.push(AM.cbor.decode(new Uint8Array(e.data as ArrayBuffer)) as Record<string, unknown>);
      await new Promise<void>((resolve) => {
        ws.onopen = () => resolve();
      });
      ws.send(
        AM.cbor.encode({
          type: "join",
          senderId: "raw-id-flooder",
          peerMetadata: {},
          supportedProtocolVersions: ["1"],
        }),
      );
      for (let i = 0; i < 400 && !replies.some((f) => f.type === "peer"); i++)
        await new Promise((r) => setTimeout(r, 5));
      // A document id is a cache key held 30 s: a 100 kB id must not be kept.
      ws.send(AM.cbor.encode({
        type: "request",
        senderId: "raw-id-flooder",
        targetId: automerge.peerId,
        documentId: "x".repeat(100_000),
        data: new Uint8Array(),
      }));
      for (let i = 0; i < 400 && !replies.some((f) => f.type === "doc-unavailable"); i++)
        await new Promise((r) => setTimeout(r, 5));
      assert(replies.some((f) => f.type === "doc-unavailable"), "an oversize document id is a document nobody has");
      for (const key of syncAccessCache.keys()) assert(key.length < 1000, `cache holds a ${key.length}-char key`);
      // A document id that is not text is a document nobody has, and the frames after it still get answers.
      for (const odd of [7, { a: 1 }, ["x"], true]) {
        const seenBefore = replies.length;
        ws.send(AM.cbor.encode({
          type: "request",
          senderId: "raw-id-flooder",
          targetId: automerge.peerId,
          documentId: odd,
          data: new Uint8Array(),
        }));
        ws.send(AM.cbor.encode({
          type: "request",
          senderId: "raw-id-flooder",
          targetId: automerge.peerId,
          documentId: `after-${JSON.stringify(odd)}`,
          data: new Uint8Array(),
        }));
        for (
          let i = 0;
          i < 400 && !replies.slice(seenBefore).some((f) => f.documentId === `after-${JSON.stringify(odd)}`);
          i++
        ) { await new Promise((r) => setTimeout(r, 5)); }
        assert(
          replies.slice(seenBefore).some((f) => f.documentId === `after-${JSON.stringify(odd)}`),
          `no answer after a document id of ${JSON.stringify(odd)}`,
        );
      }
      // Past the bound the socket closes, so a flood of new ids costs a bounded
      // number of lookups and a real client reconnects with fresh state.
      let shut = false;
      ws.onclose = () => shut = true;
      const cached = syncAccessCache.size;
      const started = performance.now();
      for (let i = 0; i < FLOOD; i++) {
        ws.send(AM.cbor.encode({
          type: "request",
          senderId: "raw-id-flooder",
          targetId: automerge.peerId,
          documentId: crypto.randomUUID(),
          data: new Uint8Array(),
        }));
      }
      // The server reads the flood one frame at a time, a role lookup each until
      // the bucket runs dry, so under load the close lands seconds later.
      for (let i = 0; i < 2000 && !shut; i++) await new Promise((r) => setTimeout(r, 5));
      assert(shut, "Expected the server to close a flooding socket within 10s; it stayed open.");
      assert(
        replies.some((f) => f.type === "error" && String(f.message).includes("this connection was closed")),
        "the flooder is told why before the close",
      );
      const lookups = syncAccessCache.size - cached;
      // The bucket refills while the flood runs; without it every one of the frames is a lookup.
      const seconds = Math.ceil((performance.now() - started) / 1000);
      assert(
        lookups > 0 && lookups <= FRAME_BURST + FRAME_RATE * seconds,
        `${lookups} lookups in ${seconds}s for ${FLOOD} frames`,
      );
    }

    // Connections of one account share a bucket, so N sockets do not multiply
    // the lookups; and a full cache evicts its oldest roles, never everyone's.
    {
      const { jwt: flooderJwt } = await usr("account-flooder@example.com");
      const sockets = await Promise.all([0, 1, 2].map(async (n) => {
        const ws = new WebSocket(wsUrl(flooderJwt));
        ws.binaryType = "arraybuffer";
        const replies: Record<string, unknown>[] = [];
        ws.onmessage = (e) =>
          replies.push(AM.cbor.decode(new Uint8Array(e.data as ArrayBuffer)) as Record<string, unknown>);
        const state = { ws, replies, shut: false, senderId: `account-flooder-${n}` };
        ws.onclose = () => state.shut = true;
        await new Promise<void>((resolve) => {
          ws.onopen = () => resolve();
        });
        ws.send(AM.cbor.encode({
          type: "join",
          senderId: state.senderId,
          peerMetadata: {},
          supportedProtocolVersions: ["1"],
        }));
        for (let i = 0; i < 400 && !replies.some((f) => f.type === "peer"); i++)
          await new Promise((r) => setTimeout(r, 5));
        return state;
      }));
      const cached = syncAccessCache.size;
      const flooded = performance.now();
      for (let i = 0; i < FLOOD; i++) {
        for (const s of sockets) {
          if (s.shut) continue;
          s.ws.send(AM.cbor.encode({
            type: "request",
            senderId: s.senderId,
            targetId: automerge.peerId,
            documentId: crypto.randomUUID(),
            data: new Uint8Array(),
          }));
        }
      }
      for (let i = 0; i < 2000 && !sockets.every((s) => s.shut); i++) await new Promise((r) => setTimeout(r, 5));
      assert(sockets.every((s) => s.shut), "Expected every connection of a flooding account to close within 10s.");
      const accountLookups = syncAccessCache.size - cached;
      // The bucket refills while the flood runs, and a loaded machine stretches the flood past a second. Three
      // unshared buckets would allow 3 * (FRAME_BURST + FRAME_RATE * s), always above this.
      const seconds = Math.ceil((performance.now() - flooded) / 1000);
      assert(
        accountLookups > 0 && accountLookups <= ACCOUNT_FRAME_BURST + ACCOUNT_FRAME_RATE * seconds,
        `${accountLookups} lookups in ${seconds}s for three connections of one account`,
      );
      assert(
        sockets.some((s) => s.replies.some((f) => f.type === "error" && String(f.message).includes("across"))),
        "a connection is told the account bound, not its own, was the one it crossed",
      );

      // Fill an empty cache to the bound: 2,000 fakes, one real peer's role, then fakes. The flood's k lookups must
      // evict exactly the k oldest fakes. A whole-cache clear, or an eviction of the newest, fails below. The
      // entries of earlier steps go first, or they would be the oldest and the flood would never reach a fake.
      syncAccessCache.clear();
      const fakes: string[] = [];
      const fake = () => {
        const key = `fake-${fakes.length}/doc`;
        fakes.push(key);
        syncAccessCache.set(key, { role: null, expires: Date.now() + 30_000 });
      };
      for (let i = 0; i < 2000; i++) fake();
      const peerKey = "peer-with-a-role:-/doc";
      syncAccessCache.set(peerKey, { role: "editor", expires: Date.now() + 30_000 });
      while (syncAccessCache.size < 10_000) fake();
      const { jwt: otherJwt } = await usr("account-flooder-two@example.com");
      const ws = new WebSocket(wsUrl(otherJwt));
      ws.binaryType = "arraybuffer";
      let shut = false;
      let joined = false;
      ws.onclose = () => shut = true;
      ws.onmessage = (e) => {
        if ((AM.cbor.decode(new Uint8Array(e.data as ArrayBuffer)) as { type: string }).type === "peer") joined = true;
      };
      await new Promise<void>((resolve) => {
        ws.onopen = () => resolve();
      });
      ws.send(AM.cbor.encode({
        type: "join",
        senderId: "cache-flooder",
        peerMetadata: {},
        supportedProtocolVersions: ["1"],
      }));
      for (let i = 0; i < 400 && !joined; i++) await new Promise((r) => setTimeout(r, 5));
      assert(joined, "Expected the cache flooder to join within 2s.");
      for (let i = 0; i < FLOOD; i++) {
        ws.send(AM.cbor.encode({
          type: "request",
          senderId: "cache-flooder",
          targetId: automerge.peerId,
          documentId: crypto.randomUUID(),
          data: new Uint8Array(),
        }));
      }
      for (let i = 0; i < 2000 && !shut; i++) await new Promise((r) => setTimeout(r, 5));
      assert(shut, "Expected the cache flooder to close within 10s; it stayed open.");
      assertEquals(syncAccessCache.size, 10_000, "a flood fills the cache to its bound and no further");
      const k = fakes.filter((key) => !syncAccessCache.has(key)).length;
      assert(k > 0 && k < 2000, `the flood evicted ${k} fakes, expected between 1 and the 2,000 ahead of the peer`);
      assert(
        fakes.slice(0, k).every((key) => !syncAccessCache.has(key)) &&
          fakes.slice(k).every((key) => syncAccessCache.has(key)),
        `the ${k} evicted fakes are the ${k} oldest`,
      );
      assert(syncAccessCache.has(peerKey), "a flood evicted another peer's cached role");
      for (const key of [...fakes, peerKey]) syncAccessCache.delete(key);
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
      // `create` is local and returns at once, so the document exists here
      // before the socket carrying it does. The block above synchronises by
      // awaiting a `find`; this one has nothing to find, so it waits for the
      // network itself -- without it the PUT asks the server to fetch a document
      // no peer has announced yet and is answered 404, which is the right answer
      // to the wrong question and made this step fail about one run in five.
      await clientRepo.networkSubsystem.whenReady();
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
        if (frame.type === "sync" && frame.documentId === hand.documentId)
          heard.push(decodeSyncMessage(frame.data!).heads);
        return receive(bytes);
      };
      eHandle.change((d: Sheet) => {
        (d.data as unknown as Record<string, unknown>[])[1] = { 0: 4242 };
      });
      const written = AM.decodeHeads(eHandle.heads()!);
      for (let i = 0; i < 1000; i++) {
        if (heard.some((heads) => written.every((h) => heads.includes(h)))) break;
        await new Promise((r) => setTimeout(r, 5));
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

      // Signed-out readers share an address in an office or a classroom, so each
      // socket spends only its own bucket: three sockets that together pass one
      // account's burst, each under its own, all stay open.
      const readers = await Promise.all([0, 1, 2].map(async (n) => {
        const ws = new WebSocket(wsUrl(undefined));
        ws.binaryType = "arraybuffer";
        const replies: Record<string, unknown>[] = [];
        ws.onmessage = (e) =>
          replies.push(AM.cbor.decode(new Uint8Array(e.data as ArrayBuffer)) as Record<string, unknown>);
        const reader = { ws, replies, shut: false, senderId: `public-reader-${n}` };
        ws.onclose = () => reader.shut = true;
        await new Promise<void>((resolve) => {
          ws.onopen = () => resolve();
        });
        ws.send(
          AM.cbor.encode({
            type: "join",
            senderId: reader.senderId,
            peerMetadata: {},
            supportedProtocolVersions: ["1"],
          }),
        );
        for (let i = 0; i < 400 && !replies.some((f) => f.type === "peer"); i++)
          await new Promise((r) => setTimeout(r, 5));
        return reader;
      }));
      const each = Math.ceil(ACCOUNT_FRAME_BURST / readers.length) + 50;
      assert(each < FRAME_BURST, `each reader sends ${each}, under its own burst of ${FRAME_BURST}`);
      for (const reader of readers) {
        for (let i = 0; i < each; i++) {
          reader.ws.send(AM.cbor.encode({
            type: "ephemeral",
            senderId: reader.senderId,
            targetId: automerge.peerId,
            documentId: hand.documentId,
            count: i,
            sessionId: reader.senderId,
            data: AM.cbor.encode({}),
          }));
        }
        // Answered only after every frame queued before it, so it proves the socket read them all.
        reader.ws.send(AM.cbor.encode({
          type: "request",
          senderId: reader.senderId,
          targetId: automerge.peerId,
          documentId: crypto.randomUUID(),
          data: new Uint8Array(),
        }));
      }
      const answered = (r: (typeof readers)[number]) => r.shut || r.replies.some((f) => f.type === "doc-unavailable");
      for (let i = 0; i < 2000 && !readers.every(answered); i++) await new Promise((r) => setTimeout(r, 5));
      assert(readers.every(answered), "Expected every public reader's barrier frame answered within 10s.");
      assertEquals(
        readers.map((r) => r.shut),
        [false, false, false],
        "no signed-out reader is closed by the others' frames",
      );
      for (const reader of readers) reader.ws.close();
      const [anonymous]: { via: string }[] = await sql`
        select via from audit where sheet_id = ${sheet_id} and usr_id is null order by audit_id desc limit 1
      `;
      assertEquals(anonymous?.via, "public", "an anonymous reader of a public sheet is not a share link");
      await post(jwt, `/library/${sheet_id}/public`, { public: false });
    }

    await server.shutdown();
  });

  await t.step("The shop is publicly viewable, and browsed by type and tag", async () => {
    const [cols_, ...rows] = await get<Table>("", `/shop`);
    const cols = Object.values(cols_);
    assert(cols.length);
    assert(rows.length);
    // The two facets are columns of the answer, so the column panel is what
    // browses them, over the whole catalogue the page asks for.
    assertEquals(cols.map((col) => col.name).join(), "name,price,license,type,tags,");
    const [, ...datasets] = await get<Table>("", `/shop`, { tags: "dataset" });
    assert(datasets.length, "the seeded datasets are listed under their own tag");
    assert(
      datasets.every((row) => (row.tags as string[]).includes("dataset")),
      "?tags= answers only the listings carrying it",
    );
    assert(datasets.length < rows.length, "and not the whole catalogue");

    // The seeded catalogue -- every bundled dataset -- is already bigger than
    // cselect's own default page, which is the bug the page's hardcoded
    // ?limit=500 fixes: a bare /shop only ever sees the first page, and the
    // page's own fetch is what asks for the rest.
    const bare = await app.request("/shop");
    const total = Number(bare.headers.get("Content-Range")?.split("/")[1]);
    assertEquals((await bare.json()).data.length - 1, 50, "a bare /shop still answers only the default page");
    assert(total > 50, "the seeded catalogue is bigger than one default page");
    const [, ...whole] = await get<Table>("", `/shop`, { limit: 500 });
    assertEquals(whole.length, total, "?limit=500 is what makes the whole catalogue visible, not just the first page");

    // Adversarial ?tags= values. A NUL is refused before routing, by the guard
    // every path and query string already goes through; a quote is a value
    // bound through postgres.js, never spliced text, so it answers no rows
    // rather than a 500 or a leaked row; a repeated key is Hono's own to
    // resolve, and whichever single string it hands `qs.tags` is still bound
    // the same parameterized way -- never a second where clause and never a
    // crash over an array.
    await reject("", `/shop?tags=%00`);
    const [, ...quoted] = await get<Table>("", `/shop`, { tags: "'; drop table sheet; --" });
    assertEquals(quoted.length, 0, "a quote in ?tags= matches no listing rather than breaking the query");
    const stillThere = await get<Table>("", `/shop`);
    assert(stillThere.length > 1, "the quote did not actually drop the table");
    const repeated = await app.request("/shop?tags=a&tags=b");
    assertEquals(repeated.status, 200, "a repeated ?tags= is answered rather than crashing on an array");
  });

  await t.step("Bob buys from the shop, then queries across the sheets he owns", async () => {
    const { jwt } = await usr("bob@example.com");

    // Bob purchases items from the shop.
    {
      const [cols_, ...rows] = await get<Table>(jwt, `/shop`);
      const cols = Object.values(cols_);
      assert(cols.length);
      assertEquals(cols.map((col) => col.name).join(), "name,price,license,type,tags,");
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
      await pollAlert(alert_id, send, clock);
      assertEquals(sent.length, 0, "a clear alert should not email anyone");
      assertEquals((await history())[0].status, "clear");

      // The same answer again is the same alert, so nothing is sent -- but the
      // run is still recorded. A quiet alert and a dead poller write the same
      // empty log otherwise, and nothing outside this log can tell them apart.
      const settled = (await history()).length;
      clock += 120_000;
      await pollAlert(alert_id, send, clock);
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
      await pollAlert(alert_id, send, clock);
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
      await pollAlert(alert_id, send, clock);
      assertEquals(sent.length, 1, "the same breach should not email twice");
      assertEquals((await history())[0].status, "unchanged");

      // A second quiet tick moves that row's timestamp rather than adding
      // another. Otherwise a minute-interval alert writes 1440 rows a day and
      // pushes the held run the daily digest still has to find past NET_KEEP.
      const held = (await history()).length;
      clock += 120_000;
      await pollAlert(alert_id, send, clock);
      assertEquals((await history()).length, held, "a repeated quiet tick is one row, not a row per tick");

      // A second region breaches and the first recovers: one row in, one out.
      watched.change((d: { data: Record<number, unknown>[] }) => {
        d.data[1][1] = 0.7;
        d.data[2][1] = 1.9;
      });
      clock += 120_000;
      await pollAlert(alert_id, send, clock);
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
        await pollAlert(alert_id, refuse, clock);
        assertEquals(refusals.length, 1);
        assert(String((await history())[0].delivery).includes("resend refused"));

        clock += 120_000;
        await pollAlert(alert_id, refuse, clock);
        assertEquals(refusals.length, 2, "the same rows must be sent again after a failed delivery");
        assertEquals((await history())[0].status, "firing", "a retry is a firing run, not an unchanged one");
      }

      // A broken query is recorded against the sheet rather than thrown away.
      alert.change((d: { data: [{ code: string }] }) => {
        d.data[0].code = "select * fromm nowhere";
      });
      clock += 120_000;
      await pollAlert(alert_id, send, clock);
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
        await pollAlert(alert_id, send, clock);
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
        await pollAlert(alert_id, send, clock);
        assertEquals((await history())[0].status, "unchanged");
        assertEquals(sent.length, 2);
      }

      // A row leaving is not news to an `added` alert, and the run says why.
      {
        burn(2, 0);
        clock += 120_000;
        await pollAlert(alert_id, send, clock);
        const left = (await history())[0];
        assertEquals([left.status, left.rows, left.added, left.removed, left.when], ["clear", 1, 0, 1, "added"]);
        assertEquals(left.delivery, "no rows added since the run before, so nothing was sent");
        assertEquals(sent.length, 2, "a row leaving is not a row added");
      }

      // A row arriving is.
      {
        burn(2, 1.1);
        clock += 120_000;
        await pollAlert(alert_id, send, clock);
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
        await pollAlert(alert_id, send, clock);
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
        await pollAlert(alert_id, send, clock);
        assertEquals((await history())[0].status, "firing");
        assertEquals(sent.length, 4);
        assertEquals(sent[3].removed, 1);

        burn(1, 0);
        clock += 120_000;
        await pollAlert(alert_id, send, clock);
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
        await pollAlert(alert_id, refuse, clock);
        assertEquals(refusals.length, 1);
        clock += 120_000;
        await pollAlert(alert_id, refuse, clock);
        assertEquals(refusals.length, 2, "the rows that never arrived are sent again");
        assertEquals((await history())[0].status, "firing");
      }

      // An unknown `when` is an error run that names the choices, not a quiet
      // fall back to rows.
      {
        setWhen("bogus");
        clock += 120_000;
        await pollAlert(alert_id, send, clock);
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
        await pollAlert(alert_id, send, clock);
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
          data: [{
            code: `select n from @table:${big.documentId}`,
            to: "two@example.com",
            interval: 60,
            when: "added",
          }],
        });
        await put(jwt, `/library/alert:${alert2.documentId}`, { name: "big watch" });
        const history2 = async () => {
          const [, ...rows] = await get<Table>(jwt, `/sheet/alert:${alert2.documentId}`, {});
          return rows.map((row) => JSON.parse(String(row.body)) as Record<string, unknown>);
        };

        clock += 120_000;
        await pollAlert(`alert:${alert2.documentId}`, send, clock);
        const first = (await history2())[0];
        assertEquals(first.status, "clear");
        assert(String(first.diff_skipped).includes("first run"), String(first.diff_skipped));
        assertEquals(first.delivery, "nothing to compare with, so nothing was sent");
        assertEquals(to("two@example.com").length, 0, "a baseline is not a backlog to mail");

        big.change((d: { data: Record<number, unknown>[] }) => {
          for (let i = 0; i < 198; i++) d.data.push({ 0: 10 + i });
        });
        clock += 120_000;
        await pollAlert(`alert:${alert2.documentId}`, send, clock);
        const over = (await history2())[0];
        assertEquals(over.status, "error");
        assert(String(over.error).includes("more than the 200 rows"), String(over.error));
        assert(String(over.error).includes("narrow the query"), String(over.error));

        big.change((d: { data: Record<number, unknown>[] }) => {
          d.data.splice(4, 198);
        });
        clock += 120_000;
        await pollAlert(`alert:${alert2.documentId}`, send, clock);
        const after = (await history2())[0];
        assertEquals([after.status, after.added], ["clear", null]);
        assert(String(after.diff_skipped).includes("the run before failed"), String(after.diff_skipped));
        assertEquals(to("two@example.com").length, 0, "the run after an error must not call every row new");
      }

      // A destination that is a url is posted to rather than mailed. Slack,
      // Discord and Teams each read a shape they document; anything else gets
      // the alert.
      {
        const posts: { url: string; body: Record<string, unknown> }[] = [];
        const fetcher = (url: string, _headers?: Record<string, string>, _method?: string, body?: string) => {
          posts.push({ url, body: JSON.parse(String(body)) as Record<string, unknown> });
          return Promise.resolve(new Response("ok"));
        };
        const target = (address: string) =>
          alert.change((d: { data: [{ to: string }] }) => {
            d.data[0].to = address;
          });
        const mailed = sent.length;

        target("https://hooks.slack.com/services/T0/B0/zzz");
        burn(2, 1.4);
        clock += 120_000;
        await pollAlert(alert_id, send, clock, fetcher);
        assertEquals(posts.length, 1, "a url destination is posted to");
        assertEquals(posts[0].url, "https://hooks.slack.com/services/T0/B0/zzz");
        assertEquals(Object.keys(posts[0].body), ["text"], "slack reads one field and ignores the rest");
        assert(String(posts[0].body.text).includes("burn watch"), String(posts[0].body.text));
        assertEquals((await history())[0].delivery, "sent");
        assertEquals(sent.length, mailed, "a url destination is not also mailed");
        // The run row is read by everyone the sheet is shared with, and a
        // webhook url's path is the whole of its authorization: the log says
        // which service was posted to and the document keeps the rest.
        assertEquals((await history())[0].to, "hooks.slack.com");
        assert(
          !JSON.stringify(await get<Table>(jwt, `/sheet/${alert_id}`)).includes("T0/B0/zzz"),
          "the run log holds the webhook's path",
        );

        // Teams is Slack's deal exactly: the customer makes an Incoming Webhook
        // in their own channel and pastes the url in, and the connector reads a
        // MessageCard.
        target("https://contoso.webhook.office.com/webhookb2/00/IncomingWebhook/11/22");
        burn(2, 1.5);
        clock += 120_000;
        await pollAlert(alert_id, send, clock, fetcher);
        assertEquals(posts.length, 2, "a Teams url is posted to");
        assertEquals(Object.keys(posts[1].body), ["@type", "@context", "text"]);
        assertEquals(posts[1].body["@type"], "MessageCard");
        assert(String(posts[1].body.text).includes("burn watch"), String(posts[1].body.text));
        assertEquals((await history())[0].delivery, "sent");
        assertEquals((await history())[0].to, "contoso.webhook.office.com");
        assert(
          !JSON.stringify(await get<Table>(jwt, `/sheet/${alert_id}`)).includes("IncomingWebhook"),
          "the run log holds the webhook's path",
        );

        // The bare host, with no tenant subdomain at all, is still Teams.
        target("https://webhook.office.com/webhookb2/00/IncomingWebhook/11/22");
        burn(2, 1.52);
        clock += 120_000;
        await pollAlert(alert_id, send, clock, fetcher);
        assertEquals(posts.length, 3);
        assertEquals(posts[2].body["@type"], "MessageCard", "a bare webhook.office.com is Teams too");

        // A host that merely ends in the same letters is not a subdomain of
        // it: the dot right before "webhook" has to be there, so a domain an
        // attacker registered after office.com's own does not borrow its shape.
        target("https://evil-webhook.office.com.attacker.test/webhookb2/00");
        burn(2, 1.53);
        clock += 120_000;
        await pollAlert(alert_id, send, clock, fetcher);
        assertEquals(posts.length, 4);
        assertEquals(Object.keys(posts[3].body), ["sheet", "name", "rows"], "a look-alike host gets the generic body");

        // Every other url gets the alert itself: which sheet, its name, its rows.
        target("https://example.com/hooks/alerts");
        burn(2, 1.6);
        clock += 120_000;
        await pollAlert(alert_id, send, clock, fetcher);
        assertEquals(posts.length, 5);
        assertEquals([posts[4].body.sheet, posts[4].body.name], [alert_id, "burn watch"]);
        assertEquals((posts[4].body.rows as unknown[]).length, 2);

        // A url inside this server's own network is refused by safeFetch, and a
        // refused post is recorded and sent again next interval, which is what a
        // refused email already does.
        target("http://localhost:9/hooks/alerts");
        burn(2, 1.8);
        clock += 120_000;
        await pollAlert(alert_id, send, clock);
        const refused = (await history())[0];
        assertEquals(refused.status, "firing");
        assert(String(refused.delivery).includes("own network"), String(refused.delivery));
        clock += 120_000;
        await pollAlert(alert_id, send, clock);
        const retried = (await history())[0];
        assertEquals(retried.status, "firing", "a refused post is sent again next interval");
        assert(String(retried.delivery).includes("own network"), String(retried.delivery));

        // A digest is one email a day over many alerts, and there is no summary
        // to post: the pair is refused by name rather than quietly dropped.
        alert.change((d: { data: [{ to: string; digest?: boolean }] }) => {
          d.data[0].digest = true;
        });
        clock += 120_000;
        await pollAlert(alert_id, send, clock);
        const both = (await history())[0];
        assertEquals(both.status, "error");
        assert(String(both.error).includes("digest"), String(both.error));

        // Back to an address, and left quiet: the steps after this one share
        // one database, and an alert that is still firing mails from theirs.
        alert.change((d: { data: [{ digest?: boolean; to: string }] }) => {
          d.data[0].digest = false;
          d.data[0].to = "ops@example.com";
        });
        clock += 120_000;
        await pollAlert(alert_id, send, clock);
        clock += 120_000;
        await pollAlert(alert_id, send, clock);
        assertEquals((await history())[0].status, "unchanged");
      }

      // An alert reads as a sheet, so its history is queryable like any log.
      const { data: [, row] }: { data: Table } = await post(jwt, `/query`, {
        lang: "sql",
        code: `select count(*) as n from @${alert_id}`,
        args: [],
      });
      assertEquals(Number(row.n), (await history()).length);
      assert(Number(row.n) >= 3, `expected the clear, the firing and the error, got ${row.n}`);

      // Silence without deletion. A run inside the snooze is decided and
      // recorded exactly as it would be, and delivered to nobody.
      {
        const snooze = (value: string) =>
          alert.change((d: { data: [{ code: string; snoozed_until?: string }] }) => {
            d.data[0].snoozed_until = value;
          });
        const before = sent.length;

        snooze(new Date(clock + 86_400_000).toISOString());
        burn(1, 2.5);
        clock += 120_000;
        await pollAlert(alert_id, send, clock);
        const quieted = (await history())[0];
        assertEquals([quieted.status, quieted.delivery], ["firing", "snoozed"]);
        assertEquals(sent.length, before, "a snoozed alert mails nobody");
        // The verdict is still the verdict: the rows it matched are what the
        // next run diffs against, so a snooze does not cost the baseline.
        assertEquals((quieted.matched as unknown[]).length, quieted.rows);

        // A snooze in the past is over, and nothing had to clear it.
        snooze(new Date(clock - 86_400_000).toISOString());
        burn(1, 2.6);
        clock += 120_000;
        await pollAlert(alert_id, send, clock);
        const woken = (await history())[0];
        assertEquals([woken.status, woken.delivery], ["firing", "sent"]);
        assertEquals(sent.length, before + 1, "the snooze is over, so the alert sends");

        // A cell nobody can read is not the same as no snooze: it is an error
        // run naming the field, rather than an alert that goes on sending
        // while its owner believes it silenced.
        snooze("next tuesday");
        clock += 120_000;
        await pollAlert(alert_id, send, clock);
        const unreadable = (await history())[0];
        assertEquals(unreadable.status, "error");
        assert(String(unreadable.error).includes("snoozed_until"), String(unreadable.error));
        assert(String(unreadable.error).includes("next tuesday"), String(unreadable.error));
        assertEquals(sent.length, before + 1, "a refused snooze sends nothing either");

        // A run with nowhere to send was never going to send regardless of the
        // snooze, so its delivery must still name the real reason: calling it
        // "snoozed" would tell library:freshness and GET /status a config with
        // no destination is a deliberate silence rather than an alert stuck
        // firing into the void.
        snooze(new Date(clock + 86_400_000).toISOString());
        alert.change((d: { data: [{ to: string }] }) => {
          d.data[0].to = "";
        });
        burn(1, 3.0);
        clock += 120_000;
        await pollAlert(alert_id, send, clock);
        const noDestination = (await history())[0];
        assertEquals(noDestination.status, "firing");
        assertEquals(
          noDestination.delivery,
          "no destination, so nothing was sent",
          "a snooze on an alert with nowhere to send must not be reported as the reason nothing sent",
        );
        assertEquals(sent.length, before + 1, "still nobody to mail");
        alert.change((d: { data: [{ to: string }] }) => {
          d.data[0].to = "ops@example.com";
        });

        // Cleared, and left quiet: the steps after this one share one database.
        snooze("");
        clock += 120_000;
        await pollAlert(alert_id, send, clock);
        clock += 120_000;
        await pollAlert(alert_id, send, clock);
        assertEquals((await history())[0].status, "unchanged");
      }
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

      // A second scale and a box read, page and export through the same one
      // path: `chartSql` is the only thing either changes, so what the picture
      // draws and what the API answers cannot come apart.
      const second = automerge.create<{ data: [{ source: string; kind: string; x: string; y: string; y2: string }] }>({
        data: [{ source: `@table:${source.documentId}`, kind: "line", x: "month", y: "spent", y2: "spent" }],
      });
      await put(jwt, `/library/chart:${second.documentId}`, { name: "two scales" });
      const [twoCols] = await get<Table>(jwt, `/sheet/chart:${second.documentId}`);
      assertEquals(Object.values(twoCols).map((col) => col.name).join(), "x,y,y2");

      // Every month is its own group here, so each box is one measurement: the
      // five numbers are equal, which is exactly what a one-row group means.
      const box = automerge.create<{ data: [{ source: string; kind: string; x: string; y: string }] }>({
        data: [{ source: `@table:${source.documentId}`, kind: "box", x: "month", y: "spent" }],
      });
      await put(jwt, `/library/chart:${box.documentId}`, { name: "spread" });
      const [boxCols, ...boxes] = await get<Table>(jwt, `/sheet/chart:${box.documentId}`);
      assertEquals(Object.values(boxCols).map((col) => col.name).join(), "x,lo,q1,med,q3,hi");
      assertEquals(boxes.length, 3, "a box chart answers one row per group, not one per source row");
      assertEquals(boxes.map((row) => row.med).join(), "120,150,90");

      // A blank cell is the normal state of a spreadsheet. Every other kind of
      // chart drops the row and draws the rest; a box aggregates, and one null
      // reaching `percentile` used to refuse the whole chart -- every group of
      // it, including the groups with nothing wrong with them.
      const gappy = automerge.create<{ data: Sheet["data"] }>({
        data: [
          arrayify([{ name: "month", type: "date", key: 0 }, { name: "spent", type: "usd", key: 1 }]),
          { 0: "2026-01-01", 1: 120 },
          { 0: "2026-01-01", 1: null },
          { 0: "2026-01-01", 1: 160 },
          { 0: "2026-02-01", 1: 90 },
        ],
      });
      await put(jwt, `/library/table:${gappy.documentId}`, {});
      const overGaps = automerge.create<{ data: [{ source: string; kind: string; x: string; y: string }] }>({
        data: [{ source: `@table:${gappy.documentId}`, kind: "box", x: "month", y: "spent" }],
      });
      await put(jwt, `/library/chart:${overGaps.documentId}`, { name: "spread over gaps" });
      const [, ...gapBoxes] = await get<Table>(jwt, `/sheet/chart:${overGaps.documentId}`);
      assertEquals(gapBoxes.length, 2, "a blank cell drops its row, and both groups are still drawn");
      assertEquals(gapBoxes.map((row) => `${row.lo}-${row.hi}`).join(), "120-160,90-90");

      // A column name is the only thing that goes into the SQL, so anything else
      // is refused by name rather than concatenated in. A kind is refused for a
      // different reason: it changes no query, but one nobody draws used to
      // render as a line and say nothing about the typo that made it.
      for (
        const [cfg, said] of [
          [{ source: "budget", kind: "line", x: "month", y: "spent" }, "one table or query sheet"],
          [{ source: "@chart:abc", kind: "line", x: "month", y: "spent" }, "one table or query sheet"],
          [{ source: `@table:${source.documentId}`, kind: "line", x: "month", y: "spent; drop table" }, "column name"],
          [{ source: `@table:${source.documentId}`, kind: "scater", x: "month", y: "spent" }, "not a kind of chart"],
          // A second scale and a box are the two settings that mean nothing on
          // some kinds, and a setting a chart holds but nobody draws is a
          // document that lies about the picture.
          [
            { source: `@table:${source.documentId}`, kind: "kpi", x: "month", y: "spent", y2: "budget" },
            "no second scale",
          ],
          [
            { source: `@table:${source.documentId}`, kind: "box", x: "month", y: "spent", series: "team" },
            "takes no series column",
          ],
          [
            { source: `@table:${source.documentId}`, kind: "box", x: "month", y: "spent", y2: "budget" },
            "takes no second y column",
          ],
          [
            { source: `@table:${source.documentId}`, kind: "line", x: "month", y: "spent", y2: "budget; drop table" },
            "second y column has to be a column name",
          ],
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

      // The same pairs `similarity` and `soundex` in src/Main.elm are asserted
      // against, because the near-duplicate verb in the column's panel is a copy
      // of these two rules on the other side of the language boundary: the panel
      // and a query have to call the same two rows the same distance apart.
      // tests/MainTest.elm holds the other half.
      const near = await run(
        `select round(similarity('Acme Corp','Acme Corp.'), 4) a, round(similarity('Acme','Zebra'), 4) b,
                round(similarity('', ''), 4) c, soundex('Rupert') r, soundex('Tymczak') t, soundex('') e`,
      );
      assertEquals([near.a, near.b, near.c], [0.75, 0, 1]);
      assertEquals([near.r, near.t, near.e], ["R163", "T522", ""]);

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

      // Arps hyperbolic decline, qi = 1000, Di = 0.1, b = 0.5, which is
      // q(t) = 1000 / (1 + 0.05t)^2. Two years of it, read back a year past the
      // end, where the closed form is 1000 / 2.8^2.
      const months = [...Array(25).keys()];
      const rows = (q: (t: number) => number) =>
        `(${months.map((t) => `select ${t} as x, ${q(t).toFixed(4)} as y`).join(" union all ")})`;
      const decline = (t: number) => 1000 / (1 + 0.05 * t) ** 2;
      const hyp = await run(`select fit_hyperbolic(array(x),array(y),36) h from ${rows(decline)}`);
      assertEquals(round(hyp.h, 3), 127.551);
      // The same curve with five percent of wobble on it. A least-squares fit of
      // three parameters to noisy points does not land on the parameters that drew
      // them, so what is checked is that it still lands near the answer.
      const rough = await run(
        `select fit_hyperbolic(array(x),array(y),36) h from ${
          rows((t) => decline(t) * (1 + 0.05 * Math.sin(t * 2.4)))
        }`,
      );
      assert(Math.abs(Number(rough.h) / 127.551 - 1) < 0.02, `noisy decline answered ${rough.h}`);
      // A rate that declines straight is the b = 0 member of the same family, so
      // the hyperbolic fit answers what the exponential one does rather than
      // chasing a b the points do not have.
      const both = await run(
        `select fit_hyperbolic(array(x),array(y),36) h, fit_exponential(array(x),array(y),36) e
         from ${rows((t) => 1000 * Math.exp(-0.1 * t))}`,
      );
      assertEquals(round(both.h, 6), round(both.e, 6));
      // The same decline in different units is the same decline. The fit read
      // its step tolerance in the units of the columns, so a rate in the billions
      // answered a quarter low and a month column spelled as unix seconds never
      // moved b off the exponential seed it starts from.
      const big = await run(`select fit_hyperbolic(array(x),array(y),36) h from ${rows((t) => decline(t) * 1e9)}`);
      assertEquals(round(Number(big.h) / 1e9, 3), 127.551, `qi = 1e9 answered ${big.h}`);
      const month = 2629800, epoch = months.map((t) => 1735689600 + t * month);
      assertEquals(
        round(
          ala.fn.fit_hyperbolic(epoch, epoch.map((t) => decline((t - epoch[0]) / month)), epoch[0] + 36 * month),
          3,
        ),
        127.551,
      );
      // b = 2 is the top of the range the fit holds b in, so an exact b = 2
      // decline is an answer and not a refusal: the guard used to fire on a trial
      // step that overshot the bound rather than on the fit that came back.
      const steep = (t: number) => 1000 / (1 + 0.2 * t) ** 0.5;
      assertEquals(round(ala.fn.fit_hyperbolic(months, months.map(steep), 36), 3), round(steep(36), 3));
      // A straight exponential decline fits b = 0, the member of the family with
      // no asymptote, so a refusal past the end of it cannot name where the curve
      // begins: -1 / (0 * Di) is -Infinity, a bound every x already clears.
      const overflow = assertThrows(
        () => ala.fn.fit_hyperbolic(months, months.map((t) => 1000 * Math.exp(-0.1 * t)), -10000),
        Error,
      ).message;
      assert(!overflow.includes("Infinity"), overflow);

      // A distribution on an input rather than one number for it. Every draw is
      // seeded off a hash of the whole call, never Math.random, so the same call
      // answers the same number on every run and in both engines -- which is
      // what examples_test compares draw for draw.
      const drawn = await run(
        `select sample_uniform(7, 0, 10) u, sample_uniform(7, 0, 10) again, sample_uniform(8, 0, 10) other,
                sample_normal(7, 0, 10) n, sample_triangular(7, 0, 5, 10) t`,
      );
      assertEquals(drawn.u, drawn.again);
      assert(drawn.u !== drawn.other, `seed 8 drew ${drawn.other}, the same value seed 7 drew`);
      // The function's own name is hashed in beside the arguments, so one seed
      // column draws an independent value per distribution rather than the same
      // quantile out of each.
      assert(drawn.u !== drawn.n && drawn.u !== drawn.t, `one seed drew ${drawn.u} from every distribution`);
      assert(Number(drawn.u) >= 0 && Number(drawn.u) < 10, `sample_uniform left its range at ${drawn.u}`);
      assert(Number(drawn.t) >= 0 && Number(drawn.t) <= 10, `sample_triangular left its range at ${drawn.t}`);
      // A thousand draws average out to the mean they were drawn around. The
      // standard error of a thousand draws at sd = 15 is about 0.47, so a mean
      // this far out is the sampler and not the sample.
      const drift = await run(
        `select count(*) n, avg(sample_normal(date_diff('day','2026-01-01',date), 100, 15)) m
         from series('2026-01-01','2028-09-26')`,
      );
      assertEquals(drift.n, 1000);
      assert(Math.abs(Number(drift.m) - 100) < 1.5, `a thousand draws around 100 averaged ${drift.m}`);
      // Each refusal names the argument and the value. A sampler that answered
      // NaN would put it in a percentile and in every number built on it.
      // Both bounds finite and correctly ordered still overflows a double once
      // they are 1e308 apart -- (hi - lo) alone clears Number.MAX_VALUE -- and a
      // fraction of exactly zero on that Infinity draws NaN rather than a bound,
      // so the check is on the draw itself and not on the bounds that produced it.
      for (
        const [code, said] of [
          [`select sample_uniform('x', 0, 10) u`, "a finite number"],
          [`select sample_uniform(1, 10, 0) u`, "at or below the high bound 0"],
          [`select sample_normal(1, 0, -1) n`, "at or above zero"],
          [`select sample_triangular(1, 0, 20, 10) t`, "a mode between 0 and 10"],
          [`select sample_triangular(1, 10, 5, 0) t`, "at or below the high bound 0"],
          // AlaSQL's parser reads a bare exponent as a name, so "1e308" -- and
          // "1e3" with it -- throws out of its own compiler before the query
          // runs. The decimal point in each literal here is what it parses.
          [`select sample_uniform(0, -1.0e308, 1.0e308) u`, "a draw that fits in a finite number"],
          [`select sample_normal(2, 1.0e308, 1.0e308) n`, "a draw that fits in a finite number"],
          [`select sample_triangular(1, -1.0e300, 0, 1.0e300) t`, "a draw that fits in a finite number"],
        ]
      ) {
        assert((await fails(code)).includes(said), `${code} should say ${said}`);
      }

      // Multiple regression. These points lie on the plane z = 2 + 3x - y
      // exactly, so the coefficients are the plane's own and the prediction is
      // arithmetic: 2 + 30 - 3.
      const plane = await run(
        `select g.c c, round(ols_predict(g.c, 10, 3), 6) p from
           (select ols(array(z), array(x), array(y)) c from
             (select 0 as x, 1 as y, 1 as z union all select 1, 0, 5 union all select 2, 3, 5
              union all select 3, 2, 9 union all select 4, 5, 9)) g`,
      );
      assertEquals((plane.c as number[]).map((b) => round(b)), [2, 3, -1]);
      assertEquals(plane.p, 29);
      // Hours studied against a pass, with one student who passed early and one
      // who failed late: the two outcomes overlap, so the likelihood has a
      // maximum and the fit is a number rather than a runaway. The sign is the
      // claim -- more hours, better odds -- and the ends of the range read as
      // probabilities on either side of the boundary.
      const hours = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
      const odds = ala.fn.logit([0, 0, 0, 1, 0, 1, 1, 1, 1, 1], hours) as number[];
      assert(odds[1] > 0, `more hours should raise the odds, fit ${JSON.stringify(odds)}`);
      const [low, high] = [ala.fn.logit_predict(odds, 1), ala.fn.logit_predict(odds, 10)] as number[];
      assert(low < 0.05 && high > 0.95, `probabilities at the ends of the range were ${low} and ${high}`);

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
      assertThrows(() => ala.fn.fit_hyperbolic([1, 2, 3], [1, 2, 0], 4), Error, "above zero");
      // Past b = 2 the curve integrates to infinite production. These points are
      // drawn with b = 4, so the fit runs out of the range and says so.
      assertThrows(
        () => ala.fn.fit_hyperbolic(months, months.map((t) => 1000 / (1 + 0.4 * t) ** 0.25), 36),
        Error,
        "between 0 and 2",
      );
      // The same refusal reaches an ordinary noisy well whose tail flattens
      // faster than b = 2 allows. It used to be told its points do not decline;
      // these fall from 2.83 to 1.53, so that sentence sent whoever read it
      // looking at a rate column that was fine.
      const flat = assertThrows(
        () =>
          ala.fn.fit_hyperbolic(
            [1, 2, 3, 4, 5, 6, 7, 8, 9],
            [2.83, 2.27, 2.15, 2.16, 1.85, 1.86, 1.65, 1.56, 1.53],
            12,
          ),
        Error,
        "between 0 and 2",
      );
      assert(!flat.message.includes("do not decline"), flat.message);
      // Steps were bounded and points were not, so the cost of one call was the
      // rows a sheet holds: a million pairs was seconds of blocked event loop.
      const crowd = Array.from({ length: 5001 }, (_, i) => i + 1);
      assertThrows(
        () => ala.fn.fit_hyperbolic(crowd, crowd.map((t) => 1000 / (1 + 0.05 * t) ** 2), 36),
        Error,
        "at most 5000 pairs",
      );
      // A predictor the columns before it already draw leaves the normal
      // equations singular, and the refusal names which column it stopped on
      // rather than answering a coefficient the pivoting happened to pick.
      assertThrows(() => ala.fn.ols([1, 2, 4, 8], [1, 2, 3, 4], [2, 4, 6, 8]), Error, "argument 3");
      assertThrows(() => ala.fn.ols([1, 2, 3], [1, 2]), Error, "the same length");
      assertThrows(() => ala.fn.ols([1, 2, 3], [1, 2, "x"]), Error, "only finite numbers");
      assertThrows(() => ala.fn.ols([1, 2, 3], [1, 2, 3], [3, 2, 1], [1, 1, 2]), Error, "one point per coefficient");
      const wide = Array.from({ length: 14 }, (_, i) => i);
      assertThrows(
        () => ala.fn.ols(wide, ...Array.from({ length: 13 }, (_, j) => wide.map((i) => (i * (j + 2)) % 7))),
        Error,
        "at most 12 predictor columns",
      );
      assertThrows(() => ala.fn.ols(crowd, crowd), Error, "at most 5000 points");
      assertThrows(() => ala.fn.ols_predict([1, 2], 1, 2), Error, "one more coefficient");
      assertThrows(() => ala.fn.logit([0, 1, 2], [1, 2, 3]), Error, "only 0 and 1");
      // A repeated column names itself here too, and says so rather than reading
      // as the runaway below: the first reweighted step weighs every point the
      // same, so the matrix it goes singular on is the one ols() solves.
      assertThrows(() => ala.fn.logit([0, 1, 0, 1], [1, 2, 3, 4], [2, 4, 6, 8]), Error, "argument 3");
      // Nothing overlaps here, so every step fits the points better than the one
      // before it and the coefficients run off rather than settling.
      assertThrows(() => ala.fn.logit([0, 0, 1, 1], [1, 2, 3, 4]), Error, "separates them exactly");
      // y = x is the same line whichever unit x is read in. Building a column's
      // scale squared the raw values, which overflows past 1e154 and underflows
      // past 1e-162, so both ends reached the solve as a column of zeros -- a
      // well-conditioned fit refused as a column the others already span.
      for (const unit of [1e200, 1e-200]) {
        const [b0, b1] = ala.fn.ols([1, 2, 3, 4], [1, 2, 3, 4].map((x) => x * unit)) as number[];
        assert(Math.abs(b0) < 1e-6 && Math.abs(b1 * unit - 1) < 1e-6, `x in units of ${unit} fit ${b0} + ${b1}x`);
      }
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

      // A seasonal series is a trend plus a pattern that repeats every period
      // rows. This one is 100 + 10t plus the four-step pattern -6, 2, 8, -4,
      // which already sums to zero over one cycle, so the decomposition has to
      // hand that same pattern back.
      const S = [104, 122, 138, 136, 144, 162, 178, 176, 184, 202, 218, 216]
        .map((y, t) => `select ${t + 1} as t, ${y} as y`).join(" union all ");
      const parts = await rows(
        `select t, y, trend(y, 4) over (order by t) as trend,
                seasonal(y, 4) over (order by t) as seasonal,
                deseasonalized(y, 4) over (order by t) as clean,
                trend(y, 3) over (order by t) as trend3
         from (${S}) order by t`,
      );
      // An even period has no middle row, so the trend is the 2 x period
      // average: the third row's is (104/2 + 122 + 138 + 136 + 144/2) / 4, and
      // the two rows at each end have no window to sit in the middle of.
      assertEquals(col(parts, "trend"), ",,130,140,150,160,170,180,190,200,,");
      assertEquals(col(parts, "seasonal"), "-6,2,8,-4,-6,2,8,-4,-6,2,8,-4");
      assertEquals(col(parts, "clean"), "110,120,130,140,150,160,170,180,190,200,210,220");
      assertEquals(parts.slice(0, 4).reduce((n, r) => n + Number(r.seasonal), 0), 0);
      // An odd period has a middle row, so the trend is the plain average of
      // the period rows around it -- (122 + 138 + 136) / 3 on the third row --
      // and one row at each end has no window.
      assertEquals([parts[0].trend3, parts[2].trend3, parts[11].trend3].join(), ",132,");

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
          [`select t, trend(y, 1) over (order by t) as z from (${S})`, "whole number of steps in one cycle"],
          [`select t, trend(y, 8) over (order by t) as z from (${S})`, "two whole cycles"],
          [`select shop, seasonal(shop, 2) over (order by day) as z from ${T}`, "cannot add up"],
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

    // `using @table:` makes {{name}} a snippet: the same expansion as the page.
    {
      const mk = (cols: Col[], ...rows: Record<number, unknown>[]) => {
        const doc = automerge.create<{ data: Sheet["data"] }>({ data: [arrayify(cols), ...rows] });
        return `table:${doc.documentId}`;
      };
      const snips = mk(
        [{ name: "name", type: "text", key: 0 }, { name: "expr", type: "text", key: 1 }],
        { 0: "total", 1: "sum(n)" },
      );
      const nums = mk([{ name: "n", type: "int", key: 0 }], { 0: 2 }, { 0: 5 });
      for (const id of [snips, nums]) await put(jwt, `/library/${id}`, {});
      const ok: { data: Table } = await post(jwt, `/query`, {
        lang: "sql",
        code: `using @${snips} select {{total}} as t from @${nums}`,
        args: [],
      });
      assertEquals(ok.data.slice(1), [{ t: 7 }]);
      const res = await app.request(`/query`, {
        method: "POST",
        headers: new Headers({ Authorization: `Bearer ${jwt}`, "Content-Type": "application/json" }),
        body: JSON.stringify({ lang: "sql", code: `using @${snips} select {{totl}} as t`, args: [] }),
      });
      assertEquals(res.status, 400);
      const said = await res.text();
      for (const part of ["Did you mean", "{{total}}", "Source", "Fix"]) assert(said.includes(part), said);

      // A second source, and a query that answers name and expr: the same expansion, one door.
      const more = mk(
        [{ name: "name", type: "text", key: 0 }, { name: "expr", type: "text", key: 1 }],
        { 0: "rows", 1: "count(*)" },
      );
      const answers = automerge.create<{ data: Sheet["data"] }>({
        data: [{ lang: "sql", code: `select 'top' as name, 'max(n)' as expr`, args: [] }],
      });
      await put(jwt, `/library/${more}`, {});
      await put(jwt, `/library/query:${answers.documentId}`, {});
      const many: { data: Table } = await post(jwt, `/query`, {
        lang: "sql",
        code: `using @${snips}, @${more}, @query:${answers.documentId} ` +
          `select {{total}} as t, {{rows}} as r, {{top}} as m from @${nums}`,
        args: [],
      });
      assertEquals(many.data.slice(1), [{ t: 7, r: 2, m: 5 }]);
      const clash = await app.request(`/query`, {
        method: "POST",
        headers: new Headers({ Authorization: `Bearer ${jwt}`, "Content-Type": "application/json" }),
        body: JSON.stringify({ lang: "sql", code: `using @${snips}, @${snips} select 1 as x`, args: [] }),
      });
      assertEquals(clash.status, 400);
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
          // No zone at all -- the same shape `at2` holds in the xlsx fixture
          // above. `dateMs` reads it as UTC, so it must land on the exact same
          // instant, and therefore the exact same DTSTART line, as the row
          // above that spells the zone out.
          { 0: "2026-07-04T18:30:00", 1: "Afterparty", 2: 50 },
        ],
      });
      await put(jwt, `/library/table:${cal.documentId}`, {});
      const ics = await exp(`table:${cal.documentId}`, "ics");
      assertEquals(ics.headers.get("content-type"), "text/calendar; charset=utf-8");
      const lines = (await ics.text()).split("\r\n");
      assertEquals(lines[0], "BEGIN:VCALENDAR");
      assertEquals(lines.at(-1), "END:VCALENDAR");
      assertEquals(lines.filter((l) => l === "BEGIN:VEVENT").length, 3);
      // A date-only value stays all-day; a timestamp keeps its time.
      assert(lines.includes("DTSTART;VALUE=DATE:20260601"), lines.join("\n"));
      assertEquals(
        lines.filter((l) => l === "DTSTART:20260704T183000Z").length,
        2,
        "a zone-less timestamp and its zoned twin both stamp UTC, so both rows share this line",
      );
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
      for (const format of ["csv", "json", "ndjson", "md", "parquet"]) {
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

    // .xlsx is the one export that answers bytes rather than text. Parsed back
    // with the library that wrote it, because a workbook that does not open is
    // the failure worth catching and nothing else in the suite would see it.
    {
      const book = await exp(sheet_id, "xlsx");
      assertEquals(
        book.headers.get("content-type"),
        "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
      );
      // cellNF keeps each cell's number format; without it a read answers only
      // the text the format produced.
      const wb = XLSX.read(new Uint8Array(await book.arrayBuffer()), { type: "array", cellNF: true });
      // Excel refuses a colon in a sheet name and refuses one past 31 characters.
      assertEquals(wb.SheetNames, [sheet_id.replace(":", "").slice(0, 31)]);
      const ws = wb.Sheets[wb.SheetNames[0]];
      assertEquals([ws.A1.v, ws.B1.v], ["name", "age"], "the header is the first row");
      assertEquals([ws.A2.v, ws.B2.v], ["Alice", 30]);
      assertEquals(ws.B2.t, "n", "a number column opens as a number, not as text");

      // The column type is the number format, and a value the column cannot
      // hold stays the text it is rather than becoming a wrong number.
      const money = automerge.create<{ data: Sheet["data"] }>({
        data: [
          arrayify([
            { name: "paid", type: "usd", key: 0 },
            { name: "cut", type: "percentage", key: 1 },
            { name: "day", type: "date", key: 2 },
            { name: "at", type: "timestamp", key: 3 },
            { name: "qty", type: "num", key: 4 },
            { name: "at2", type: "timestamp", key: 5 },
            // A column's name comes straight off the document and is untyped in
            // memory -- a sync peer can write anything into data[0] -- and this
            // render is the one export that reads it as anything but a string.
            { name: null, type: "text", key: 6 } as unknown as Col,
            { name: "leverage", type: "ratio", key: 7 },
            { name: "spread", type: "bps", key: 8 },
          ]),
          {
            0: 1234.5,
            1: 0.155,
            2: "2026-06-01",
            3: "2026-07-04T18:30:00Z",
            4: "n/a",
            // No zone designator at all -- the shape a browser's
            // datetime-local input writes, and JS reads a bare date-time
            // string like this as the server's own local time rather than
            // UTC. A stored instant with no zone still means one thing.
            5: "2026-07-04T18:30:00",
            6: "kept",
            7: 1.5,
            8: 0.0125,
          },
        ],
      });
      await put(jwt, `/library/table:${money.documentId}`, {});
      const cells = XLSX.read(
        new Uint8Array(await (await exp(`table:${money.documentId}`, "xlsx")).arrayBuffer()),
        { type: "array", cellNF: true },
      );
      const m = cells.Sheets[cells.SheetNames[0]];
      assertEquals([m.A2.z, m.A2.v], ["$#,##0.00", 1234.5]);
      assertEquals([m.B2.z, m.B2.v], ["0.00%", 0.155]);
      assertEquals([m.H2.z, m.H2.v, m.I2.z, m.I2.v], ['General":1"', 1.5, "0.00%", 0.0125]);
      // A date is a number wearing a date format, so what it shows is the
      // assertion: the serial is computed off the UTC instant, and the day it
      // opens on must not follow the server's timezone.
      assertEquals([m.C2.z, m.C2.w], ["yyyy-mm-dd", "2026-06-01"]);
      assertEquals([m.D2.z, m.D2.w], ["yyyy-mm-dd hh:mm:ss", "2026-07-04 18:30:00"]);
      assertEquals([m.E2.t, m.E2.v], ["s", "n/a"], "a word in a num column is text, not NaN");
      assertEquals(
        [m.F2.z, m.F2.w],
        ["yyyy-mm-dd hh:mm:ss", "2026-07-04 18:30:00"],
        "a timestamp with no zone still reads as UTC, not the server's own zone",
      );
      assertEquals([m.G1.v, m.G2.v], ["", "kept"], "a null column name is a blank header, and its row survives");

      // A duration is seconds wherever it arrives from, and a workbook counts
      // it in days: 5400 written as-is under [h]:mm:ss opens as 5400 days.
      const timed = async (csv: string) => {
        const form = new FormData();
        form.append("file", new File([csv], "laps.csv", { type: "text/csv" }));
        return await app.request(`/import/csv?types=${encodeURIComponent(JSON.stringify({ took: "duration" }))}`, {
          method: "POST",
          headers: new Headers({ Authorization: `Bearer ${jwt}` }),
          body: form,
        });
      };
      const laps = await timed("lap,took\na,1:30\nb,5400");
      assertEquals(laps.status, 201, await laps.clone().text());
      const lapsId = (await laps.json()).sheet_id;
      const [, ...lapRows] = await get<Table>(jwt, `/sheet/${lapsId}`);
      assertEquals(lapRows.map((r) => r.took), [5400, 5400], "h:mm and seconds store the same number");
      const lapBook = XLSX.read(new Uint8Array(await (await exp(lapsId, "xlsx")).arrayBuffer()), {
        type: "array",
        cellNF: true,
      });
      const l = lapBook.Sheets[lapBook.SheetNames[0]];
      assertEquals([l.B2.z, l.B2.v, l.B3.v], ["[h]:mm:ss", 0.0625, 0.0625]);

      // A money type is chosen by the importer, never guessed, and a spelling outside the family is refused.
      const priced = async (type: string) => {
        const form = new FormData();
        form.append("file", new File(["item,fee\na,1250\nb,30"], "fees.csv", { type: "text/csv" }));
        return await app.request(`/import/csv?types=${encodeURIComponent(JSON.stringify({ fee: type }))}`, {
          method: "POST",
          headers: new Headers({ Authorization: `Bearer ${jwt}` }),
          body: form,
        });
      };
      const fees = await priced("money:JPY");
      assertEquals(fees.status, 201, await fees.clone().text());
      const feeId = (await fees.json()).sheet_id;
      const [, ...feeRows] = await get<Table>(jwt, `/sheet/${feeId}`);
      assertEquals(feeRows.map((r) => r.fee), [1250, 30]);
      const feeXlsx = XLSX.read(new Uint8Array(await (await exp(feeId, "xlsx")).arrayBuffer()), {
        type: "array",
        cellNF: true,
      });
      assertEquals(feeXlsx.Sheets[feeXlsx.SheetNames[0]].B2.z, '"JPY "#,##0');
      for (const type of ["money:jpy", "money:", "money:JPYY"]) {
        const refused = await priced(type);
        assertEquals(refused.status, 400, type);
        const said = await refused.text();
        assert(said.includes("money:EUR") && said.includes("num"), `${type}: ${said}`);
      }

      // A negative day fraction opens blank under [h]:mm:ss, so a negative
      // duration goes out as text.
      const negBook = XLSX.read(
        new Uint8Array(
          await (await exp((await (await timed("lap,took\na,-0:45")).json()).sheet_id, "xlsx")).arrayBuffer(),
        ),
        { type: "array", cellNF: true },
      );
      const n = negBook.Sheets[negBook.SheetNames[0]];
      assertEquals(
        XLSX.utils.format_cell(n.B2),
        "-2700",
        "a negative duration reads as its seconds, not as a blank cell",
      );

      const late = await timed("lap,took\na,1:30\nb,1:60");
      assertEquals(late.status, 400);
      const lateSaid = await late.text();
      assert(lateSaid.includes("Line 3") && lateSaid.includes('"took"'), `names the line and the column: ${lateSaid}`);
      assert(lateSaid.includes("h:mm"), `and says what a duration takes: ${lateSaid}`);

      // A cell past Excel's own 32,767-character limit throws out of
      // XLSX.write. It is refused by name here rather than cut to fit: a
      // workbook missing the tail of a cell is a wrong answer, and the formats
      // with no such limit still carry the whole value.
      const wide = automerge.create<{ data: Sheet["data"] }>({
        data: [arrayify([{ name: "note", type: "text", key: 0 }]), { 0: "x".repeat(40000) }],
      });
      await put(jwt, `/library/table:${wide.documentId}`, {});
      const wideRes = await exp(`table:${wide.documentId}`, "xlsx");
      assertEquals(wideRes.status, 400, "a cell over the limit is a refusal, not a cut and not a 500");
      const cut = await wideRes.text();
      assert(cut.includes("column note, row 1"), `the refusal says where it is: ${cut}`);
      assert(cut.includes("40000 characters"), `and how long it is: ${cut}`);
      assert(!cut.includes("xxxx"), `and never what it is: ${cut}`);
      const whole = await (await exp(`table:${wide.documentId}`, "csv")).text();
      assert(whole.includes("x".repeat(40000)), "and the .csv the refusal names carries the whole value");
    }

    // .parquet is the other export that answers bytes, read back with the
    // library that wrote it for the reason the workbook above is. A parquet
    // column holds one type, so what is under test is the per-column decision.
    {
      const read = async (res: Response) => {
        const bytes = new Uint8Array(await res.arrayBuffer());
        return await parquetReadObjects({
          file: { byteLength: bytes.byteLength, slice: (start, end) => bytes.slice(start, end).buffer },
        });
      };
      const file = await exp(sheet_id, "parquet");
      assertEquals(file.headers.get("content-type"), "application/vnd.apache.parquet");
      // The same rows .json answers with: the importer typed `age` num, which
      // is a DOUBLE, and the int column in the fixture below is the INT64.
      assertEquals(await read(file), [{ name: "Alice", age: 30 }, { name: "Bob", age: 25 }]);

      const typed = automerge.create<{ data: Sheet["data"] }>({
        data: [
          arrayify([
            { name: "n", type: "int", key: 0 },
            { name: "paid", type: "usd", key: 1 },
            // An alias: read through canonicalType it is a number, and read off
            // COLUMN_TYPES directly it would have been a string.
            { name: "cut", type: "pct", key: 2 },
            { name: "ok", type: "bool", key: 3 },
            { name: "day", type: "date", key: 4 },
            { name: "at", type: "timestamp", key: 5 },
            { name: "blob", type: "json", key: 6 },
            { name: "note", type: "text", key: 7 },
            // Every column below holds one value its own type cannot hold, so
            // each is written whole as the text it is rather than coerced.
            { name: "qty", type: "num", key: 8 },
            { name: "count", type: "int", key: 9 },
            { name: "when", type: "timestamp", key: 10 },
          ]),
          {
            0: 7,
            1: 1234.5,
            2: 0.155,
            3: true,
            4: "2026-06-01",
            // No zone designator, the shape a datetime-local input writes: it
            // must land on the same instant as the zoned spelling would.
            5: "2026-07-04T18:30:00",
            6: { a: 1 },
            7: "kept",
            8: "n/a",
            9: 1.5,
            10: "whenever",
          },
          { 0: null, 1: null, 2: null, 3: null, 4: null, 5: null, 6: null, 7: null, 8: 2, 9: 2, 10: null },
        ],
      });
      await put(jwt, `/library/table:${typed.documentId}`, {});
      const [first, second] = await read(await exp(`table:${typed.documentId}`, "parquet"));
      assertEquals(first.n, 7n, "an int column is an INT64");
      assertEquals([first.paid, first.cut], [1234.5, 0.155], "every other numeric type is a DOUBLE");
      assertEquals(first.ok, true);
      assertEquals((first.day as Date).toISOString(), "2026-06-01T00:00:00.000Z", "a date is UTC midnight");
      assertEquals(
        (first.at as Date).toISOString(),
        "2026-07-04T18:30:00.000Z",
        "and a zoneless timestamp is UTC, not the server's own zone",
      );
      assertEquals(first.blob, { a: 1 });
      assertEquals(first.note, "kept");
      assertEquals([first.qty, second.qty], ["n/a", "2"], "a word in a num column takes the whole column to text");
      assertEquals([first.count, second.count], ["1.5", "2"], "and so does a fraction in an int column");
      assertEquals(first.when, "whenever", "and a date that does not parse");
      // A blank is a parquet null in every column, whichever type it landed in.
      assertEquals(
        [second.n, second.paid, second.ok, second.day, second.blob, second.note, second.when],
        [null, null, null, null, null, null, null],
      );

      // A money column is a number in both files. The workbook draws the code before the digits.
      const euros = automerge.create<Sheet>({
        type: "table",
        data: [arrayify([{ name: "fee", type: "money:EUR", key: "0" }]), { 0: 12.5 }],
      });
      await put(jwt, `/library/table:${euros.documentId}`, {});
      const book = XLSX.read(
        new Uint8Array(await (await exp(`table:${euros.documentId}`, "xlsx")).arrayBuffer()),
        { type: "array", cellNF: true },
      );
      const cell = book.Sheets[book.SheetNames[0]].A2;
      assertEquals([cell.t, cell.v, cell.z], ["n", 12.5, '"EUR "#,##0.00']);
      const yen = automerge.create<Sheet>({
        type: "table",
        data: [arrayify([{ name: "fee", type: "money:JPY", key: "0" }]), { 0: 1250 }],
      });
      await put(jwt, `/library/table:${yen.documentId}`, {});
      const yenBook = XLSX.read(
        new Uint8Array(await (await exp(`table:${yen.documentId}`, "xlsx")).arrayBuffer()),
        { type: "array", cellNF: true },
      );
      assertEquals(yenBook.Sheets[yenBook.SheetNames[0]].A2.z, '"JPY "#,##0');
      assertEquals((await read(await exp(`table:${euros.documentId}`, "parquet")))[0].fee, 12.5);
    }
  });

  await t.step("GET /library.zip holds every sheet the caller owns", async () => {
    const { jwt } = await usr("zip@example.com");
    const { jwt: other } = await usr("zip-other@example.com");
    const auth = { headers: new Headers({ Authorization: `Bearer ${jwt}` }) };
    const table = automerge.create<{ data: Sheet["data"] }>({
      data: [
        arrayify([{ name: "name", type: "text", key: 0 }, { name: "n", type: "num", key: 1 }]),
        { 0: "Alice", 1: 30 },
        { 0: "Bob, Jr.", 1: 2 },
      ],
    });
    await put(jwt, `/library/table:${table.documentId}`, { name: "people", tags: ["zip"] });
    const settings: Query = { lang: "sql", code: `select name from @table:${table.documentId}`, args: [] };
    const query = automerge.create<{ data: Sheet["data"] }>({ data: [settings] });
    await put(jwt, `/library/query:${query.documentId}`, { name: "names" });
    // Shared in as an editor: the caller can read it, and it is not theirs to download.
    const theirs = automerge.create<{ data: Sheet["data"] }>({
      data: [arrayify([{ name: "x", type: "text", key: 0 }])],
    });
    await put(other, `/library/table:${theirs.documentId}`, {});
    await post(other, `/library/table:${theirs.documentId}/share`, { email: "zip@example.com", role: "editor" });

    const res = await app.request("/library.zip", auth);
    assertEquals(res.status, 200, await res.clone().text());
    assertEquals(res.headers.get("content-type"), "application/zip");
    const raw = new Uint8Array(await res.arrayBuffer());
    const members = zipMembers(raw, "GET /library.zip");
    const csvName = `table/${table.documentId}.csv`;
    const queryName = `query/${query.documentId}.json`;
    assertEquals(members.map((m) => m.name).sort(), [csvName, "manifest.json", queryName].sort());
    const read = async (name: string) =>
      new TextDecoder().decode(await zipData(raw, members.find((m) => m.name === name)!, "GET /library.zip"));
    const exported = await app.request(`/export/table:${table.documentId}.csv`, auth);
    assertEquals(await read(csvName), await exported.text());
    assertEquals(JSON.parse(await read(queryName)), settings);
    assertEquals(JSON.parse(await read("manifest.json")), [
      {
        sheet_id: `query:${query.documentId}`,
        name: "names",
        type: "query",
        tags: [],
        license: null,
      },
      {
        sheet_id: `table:${table.documentId}`,
        name: "people",
        type: "table",
        tags: ["zip"],
        license: null,
        columns: [{ name: "name", type: "text" }, { name: "n", type: "num" }],
      },
    ]);
    const [{ n }] = await sql`
      select count(*)::int as n from audit
      where action = 'export' and sheet_id in (${`table:${table.documentId}`}, ${`query:${query.documentId}`})
    `;
    assertEquals(n, 2);

    // One sheet that cannot export fails the whole download: nothing is skipped.
    const clash = automerge.create<{ data: Sheet["data"] }>({
      data: [arrayify([{ name: "a", type: "text", key: 0 }, { name: "a", type: "text", key: 1 }]), { 0: "x", 1: "y" }],
    });
    await put(jwt, `/library/table:${clash.documentId}`, {});
    const refused = await app.request("/library.zip", auth);
    assertEquals(refused.status, 400);
    const said = await refused.text();
    assert(said.includes(`table:${clash.documentId} has two columns with the same name`), said);
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
      assert(
        (await refusal(`explain select nope from @${id}`)).includes("nope"),
        "a profile of a refused query is refused",
      );
    }

    // min()/max() over text used to drop the column out of the result: a silent
    // wrong answer, which is the worst kind. rewriteExtremes() now aims the call
    // at min_text()/max_text() when the argument is a text column, and what it
    // cannot resolve still says so and says what to use.
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
      // Aliased and bare, and a number beside them, all in one statement: the
      // rewrite has to leave min(n) alone or it answers "5" as text.
      const [, answered] = (await runs(
        `select min(code) as lowest, max(code) as highest, min(n) as least from @${codesId}`,
      )).data as Table;
      assertEquals([answered.lowest, answered.highest, answered.least], ["01", "10", 5]);

      // An expression is not a column, so nothing can say what type it produces
      // and the call reaches the engine as the author wrote it. That is the case
      // the message is still for.
      const said = await fails(`select min(upper(code)) from @${codesId}`);
      assert(said.includes("min_text"), said);

      // KEYWORD was widened past the clause words this check needs so
      // namesIn() could read it too -- which cost this exact message: an
      // alias spelled as one of the added words (bracketed, because AlaSQL
      // itself refuses these bare, as its own keywords too) is a real name
      // and must never be read as an exempt alias and silently dropped.
      const saidEnd = await fails(`select min(upper(code)) as [end] from @${codesId}`);
      assert(saidEnd.includes("min_text"), saidEnd);
      const saidOrder = await fails(`select max(upper(code)) as [order] from @${codesId}`);
      assert(saidOrder.includes("min_text"), saidOrder);
      const saidLeft = await fails(`select min(upper(code)) as [left] from @${codesId}`);
      assert(saidLeft.includes("min_text"), saidLeft);

      // And the replacements are still there to be written by hand.
      const [, row] = (await runs(
        `select min_text(code) as lo, max_text(code) as hi from @${codesId}`,
      )).data as Table;
      assertEquals([row.lo, row.hi], ["01", "10"]);
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

    // A duration reads h:mm and h:mm:ss as seconds, and refuses anything else
    // by row and column, the way a num column refuses a word.
    const clock = automerge.create<{ data: Sheet["data"] }>({
      data: [arrayify([{ name: "took", type: "duration", key: 0 }]), { 0: "1:30" }, { 0: "-0:00:05" }, { 0: 60 }],
    });
    await put(jwt, `/library/table:${clock.documentId}`, {});
    const { data: [, sum] }: { data: Table } = await post(jwt, `/query`, {
      lang: "sql",
      code: `select sum(took) as secs from @table:${clock.documentId}`,
      args: [],
    });
    assertEquals(sum.secs, 5455);
    const stopped = automerge.create<{ data: Sheet["data"] }>({
      data: [arrayify([{ name: "took", type: "duration", key: 0 }]), { 0: "1:30" }, { 0: "1:5" }],
    });
    await put(jwt, `/library/table:${stopped.documentId}`, {});
    const halted = await app.request("/query", {
      method: "POST",
      headers: new Headers({ "Content-Type": "application/json", Authorization: `Bearer ${jwt}` }),
      body: JSON.stringify({ lang: "sql", code: `select * from @table:${stopped.documentId}`, args: [] }),
    });
    assertEquals(halted.status, 400);
    const stops = await halted.text();
    assert(stops.includes(`row 2 of @table:${stopped.documentId}, column "took"`), stops);
    assert(stops.includes("h:mm"), stops);

    // A money column is `money:` and a three-letter code. Its cells are numbers like a usd column's, and
    // adding two currencies is refused where the statement names both.
    const ledger = async (name: string, type: string, cells: unknown[]) => {
      const doc = automerge.create<{ data: Sheet["data"] }>({
        data: [arrayify([{ name, type, key: 0 } as unknown as Col]), ...cells.map((v) => ({ 0: v }))],
      });
      await put(jwt, `/library/table:${doc.documentId}`, {});
      return `table:${doc.documentId}`;
    };
    const ask = async (code: string) => {
      const res = await app.request("/query", {
        method: "POST",
        headers: new Headers({ "Content-Type": "application/json", Authorization: `Bearer ${jwt}` }),
        body: JSON.stringify({ lang: "sql", code, args: [] }),
      });
      return { status: res.status, text: await res.text() };
    };
    const eur = await ledger("eur", "money:EUR", ["12.5", 12.5, ""]);
    const gbp = await ledger("gbp", "money:GBP", [1]);
    const dollars = await ledger("usd_col", "usd", [2]);
    const usdCode = await ledger("usd_code", "money:USD", [3]);
    const eurAgain = await ledger("eur2", "money:EUR", [4]);
    const sums = await ask(`select sum(eur) as s, count(eur) as n from @${eur}`);
    assertEquals(sums.status, 200, sums.text);
    assertEquals(
      JSON.parse(sums.text).data[1],
      { s: 25, n: 2 },
      "a string and a number both store as 12.5, a blank is null",
    );
    const word = await ledger("eur", "money:EUR", ["abc"]);
    const wordRefused = await ask(`select * from @${word}`);
    assertEquals(wordRefused.status, 400);
    assert(wordRefused.text.includes("money:EUR, so a number or a blank"), wordRefused.text);
    for (const type of ["money:eur", "money:EURO", "money:"]) {
      const shapeless = await ask(`select * from @${await ledger("x", type, [1])}`);
      assertEquals(shapeless.status, 400, type);
      assert(shapeless.text.includes("money:EUR"), `${type}: ${shapeless.text}`);
    }
    const mixed = await ask(`select a.eur + b.gbp as t from @${eur} a, @${gbp} b`);
    assertEquals(mixed.status, 400, mixed.text);
    assert(mixed.text.includes("money:EUR") && mixed.text.includes("money:GBP"), mixed.text);
    for (
      const code of [
        `select sum(a.eur) - max(b.gbp) as t from @${eur} a, @${gbp} b`,
        `select a.eur + b.usd_col as t from @${eur} a, @${dollars} b`,
        `select b.usd_col - a.eur as t from @${eur} a, @${dollars} b`,
        `select a.eur + b.gbp total from @${eur} a, @${gbp} b`,
        `select (a.eur) + b.gbp "t" from @${eur} a, @${gbp} b`,
      ]
    ) { assertEquals((await ask(code)).status, 400, code); }
    for (
      const code of [
        `select a.eur + b.eur2 as t from @${eur} a, @${eurAgain} b`,
        `select sum(a.eur) as t from @${eur} a, @${gbp} b`,
        `select a.usd_col + b.usd_code as t from @${dollars} a, @${usdCode} b`,
      ]
    ) {
      const ok = await ask(code);
      assertEquals(ok.status, 200, `${code}: ${ok.text}`);
    }
    // The where clause, each case and each join's on read as comparisons: two currencies on the two sides
    // are refused and name the clause; a literal, a scalar multiple and one currency pass.
    for (
      const [code, clause] of [
        [`select a.eur from @${eur} a, @${gbp} b where a.eur > b.gbp`, "the where clause"],
        [`select a.eur from @${eur} a, @${gbp} b where a.eur > 1 and a.eur + b.gbp > 2`, "the where clause"],
        [`select case when a.eur > 0 then a.eur else b.gbp end as v from @${eur} a, @${gbp} b`, "a case"],
        [`select case when a.eur = b.gbp then 1 else 0 end as v from @${eur} a, @${gbp} b`, "a case"],
        [`select a.eur from @${eur} a join @${gbp} b on a.eur = b.gbp`, "a join's on"],
        [
          `select a.eur from @${eur} a, @${gbp} b where a.eur > 1 or (a.eur > 2 and (a.eur < b.gbp))`,
          "the where clause",
        ],
        [`select a.eur from @${eur} a, @${gbp} b where not a.eur = b.gbp`, "the where clause"],
        [`select a.eur from @${eur} a, @${gbp} b where a.eur between 1 and b.gbp`, "the where clause"],
        [`select a.eur from @${eur} a, @${gbp} b where a.eur in (2, b.gbp)`, "the where clause"],
        [
          `select case when 1 = 1 then case when 2 = 2 then a.eur else 0 end else b.gbp end from @${eur} a, @${gbp} b`,
          "a case",
        ],
        [`select case a.eur when b.gbp then 1 else 0 end as v from @${eur} a, @${gbp} b`, "a case"],
      ]
    ) {
      const refused = await ask(code);
      assertEquals(refused.status, 400, `${code}: ${refused.text}`);
      assert(refused.text.includes(clause), `${code}: ${refused.text}`);
      assert(refused.text.includes("money:EUR") && refused.text.includes("money:GBP"), refused.text);
    }
    for (
      const code of [
        `select a.eur from @${eur} a, @${gbp} b where a.eur > 1 and b.gbp * 2 < 5`,
        `select a.eur from @${eur} a, @${eurAgain} b where a.eur > b.eur2`,
        `select case when a.eur > 1 then a.eur else 0 end as v from @${eur} a, @${gbp} b`,
        `select a.eur from @${eur} a join @${eurAgain} b on a.eur = b.eur2`,
        `select a.eur from @${eur} a, @${gbp} b where a.eur > 'x ' || 'b.gbp'`,
        `select a.eur from @${eur} a, @${gbp} b where (a.eur > 1 and (b.gbp < 2)) or a.eur is null`,
        `select a.eur from @${eur} a, @${gbp} b where a.eur between 1 and 5 and b.gbp in (1, 2)`,
        `select a.eur from @${eur} a join @${eur} b on a.eur = b.eur`,
      ]
    ) {
      const ok = await ask(code);
      assertEquals(ok.status, 200, `${code}: ${ok.text}`);
    }
    // A name typed two currencies across the loaded sheets names both sheets.
    const other = await ledger("eur", "money:GBP", [5]);
    const twice = await ask(`select a.eur + 1 as t from @${eur} a, @${other} b`);
    assertEquals(twice.status, 400, twice.text);
    assert(twice.text.includes(eur) && twice.text.includes(other), twice.text);

    // convert() answers amount * rate and is typed by its literal `to`: the converted euros add to a euro
    // column, and a `from` that is not the column's currency is refused by the column's name.
    {
      const rows = (r: { text: string }) => JSON.parse(r.text).data.slice(1);
      const ok = await ask(
        `select sum(a.eur) + sum(convert(b.usd_col, 'USD', 'EUR', 0.5)) as t from @${eur} a, @${dollars} b`,
      );
      assertEquals(ok.status, 200, ok.text);
      assertEquals(rows(ok), [{ t: 28 }], "25 euros plus one dollar at 0.5 on each of three joined rows");
      const each = await ask(`select convert(eur, 'EUR', 'USD', 2) as v from @${eur}`);
      assertEquals(rows(each), [{ v: 25 }, { v: 25 }, { v: null }], "a blank amount answers null");
      const wrongFrom = await ask(
        `select a.eur + convert(b.usd_col, 'EUR', 'USD', 1.1) as t from @${eur} a, @${dollars} b`,
      );
      assertEquals(wrongFrom.status, 400, wrongFrom.text);
      assert(wrongFrom.text.includes("usd_col"), wrongFrom.text);
      const wrongTo = await ask(
        `select sum(a.eur) + sum(convert(b.usd_col, 'USD', 'GBP', 0.8)) as t from @${eur} a, @${dollars} b`,
      );
      assertEquals(wrongTo.status, 400, wrongTo.text);
      for (
        const call of [
          "'usd', 'EUR', 2",
          "'USD', 'EURO', 2",
          "'USD', 'EUR', 0",
          "'USD', 'EUR', -1",
          "'USD', 'EUR', 'x'",
        ]
      ) {
        const refused = await ask(`select convert(eur, ${call}) as v from @${eur}`);
        assertEquals(refused.status, 400, `${call}: ${refused.text}`);
      }
      const word = await ask(`select convert('abc', 'USD', 'EUR', 2) as v from @${eur}`);
      assertEquals(word.status, 400, word.text);
    }

    // A snippet body that adds two currencies is refused: planQuery reads the expanded text. A body that only
    // names a money column is an ordinary column in the statement around it.
    {
      const snips = automerge.create<{ data: Sheet["data"] }>({
        data: [
          arrayify([{ name: "name", type: "text", key: 0 }, { name: "expr", type: "text", key: 1 }]),
          { 0: "both", 1: "a.eur + b.gbp" },
          { 0: "fee", 1: "a.eur" },
        ],
      });
      const snipId = `table:${snips.documentId}`;
      await put(jwt, `/library/${snipId}`, {});
      for (
        const code of [
          `using @${snipId} select {{both}} as t from @${eur} a, @${gbp} b`,
          `using @${snipId} select {{fee}} + b.gbp as t from @${eur} a, @${gbp} b`,
          `select sum(a.eur + b.gbp) as t from @${eur} a, @${gbp} b`,
        ]
      ) {
        const res = await ask(code);
        assertEquals(res.status, 400, `${code}: ${res.text}`);
        assert(res.text.includes("money:EUR") && res.text.includes("money:GBP"), res.text);
      }
      const named = await ask(`using @${snipId} select sum({{fee}}) as t from @${eur} a`);
      assertEquals(named.status, 200, named.text);
    }

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
      const blankId = `net-http:${blankHand.documentId}`;
      await put(jwt, `/library/${blankId}`, {});

      const calls: [string, Record<string, string>][] = [];
      // A body that differs per call: the same body twice is one row moved to
      // now, and this test counts rows to see the polls happen.
      const fetcher = (url: string, headers: Record<string, string> = {}) => {
        calls.push([url, headers]);
        return Promise.resolve(new Response(`{"ok":true,"poll":${calls.length}}`));
      };
      const t0 = Date.now();
      // The tick itself: this block is about the due-map gating (an interval
      // honored, a blank url skipped), which only the tick enforces --
      // `pollNetSheet` always runs when called directly. Only Alice's one
      // net-http sheet and this one's blank sibling are due yet, so the pile
      // it walks is still small here.
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

      // The one sheet, so a call proves this feed's header and not some other
      // due sheet's.
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
      await pollFeed(keyedId, fetcher, t1);
      assertEquals(seen.length, 0, "a header that cannot be built must not be sent without it");
      const [failed] = await rowsOf();
      const failure = JSON.parse(String(failed.body));
      assert(failure.error.includes("{{secret:weather}}"), `it must name the reference: ${failure.error}`);
      assert(failure.error.includes("does not hold"), failure.error);
      assert(failure.repro.includes("X-Api-Key: <value>"), "the repro still names keys and not values");

      await post(jwt, `/library/${keyedId}/secret`, { name: "weather", value: "sk-live-1" });
      await pollFeed(keyedId, fetcher, t1 + 200_000);
      assertEquals(seen.length, 1, "with the secret stored, the request goes");
      assertEquals(seen[0], { "X-Api-Key": "sk-live-1", Authorization: "Bearer sk-live-1" });

      // Writing the secret again rotates it, and the feed picks the newest up
      // on its next poll rather than needing the sheet edited.
      await post(jwt, `/library/${keyedId}/secret`, { name: "weather", value: "sk-live-2" });
      await pollFeed(keyedId, fetcher, t1 + 400_000);
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
    await pollFeed(
      id,
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
    await pollFeed(id, fetcher, t);
    assertEquals(seen[0], "https://cursor.test/feed?kind=orders", "the first poll asks for everything");
    await pollFeed(id, fetcher, t + 121_000);
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
    await pollFeed(badId, fetcher, t + 300_000);
    const [row] = (await get<Table>(jwt, `/net/${badId}`)).slice(1);
    assert(JSON.parse(String(row.body)).error.includes("query parameter"), String(row.body));
  });

  // One GET per interval was the whole of a feed. A body is templated where a
  // header already was -- a secret out of the secret table, the watermark out
  // of the last good run -- so a search API that takes a POST is a sheet and
  // not a feature request.
  await t.step("A feed that posts a body", async () => {
    const { jwt } = await usr("ivy@example.com");
    const posted = automerge.create<Sheet>({
      type: "net-http",
      data: [{
        url: "https://post.verbs.test/search",
        interval: 120,
        method: "POST",
        headers: "Content-Type: application/json",
        body: '{"since":"{{cursor}}","key":"{{secret:searchkey}}"}',
      }],
    });
    const postedId = `net-http:${posted.documentId}`;
    await put(jwt, `/library/${postedId}`, {});
    await post(jwt, `/library/${postedId}/secret`, { name: "searchkey", value: "sk-post-1" });

    // An apostrophe is a valid HTTP token character, so a header name may hold
    // one, and the repro line is pasted into a shell.
    const quoting = automerge.create<Sheet>({
      type: "net-http",
      data: [{ url: "https://quote.verbs.test/feed", interval: 120, headers: "X'Y: abc", method: "POST", body: "b" }],
    });
    const quotingId = `net-http:${quoting.documentId}`;
    await put(jwt, `/library/${quotingId}`, {});

    // A GET carrying a body is refused while the sheet's own text is still in
    // hand. The refusal is written to this log, which every viewer and every
    // share-link holder reads, and a size measured on the resolved body is an
    // oracle on the secret behind it, refreshed every interval.
    const getBody = automerge.create<Sheet>({
      type: "net-http",
      data: [{ url: "https://getbody.verbs.test/feed", interval: 120, body: "{{secret:leaky}}" }],
    });
    const getBodyId = `net-http:${getBody.documentId}`;
    await put(jwt, `/library/${getBodyId}`, {});
    await post(jwt, `/library/${getBodyId}/secret`, { name: "leaky", value: "0123456789abcdefghij" });

    const sent: { url: string; method: string; body?: string }[] = [];
    let answer = () => new Response(`{"ok":true,"at":${sent.length}}`);
    const fetcher = (url: string, _headers: Record<string, string> = {}, method: Method = "GET", body?: string) => {
      if (url.includes(".verbs.test")) sent.push({ url, method, body });
      return Promise.resolve(answer());
    };
    const t = Date.now() + 70_000_000;
    // A cycle stops starting sheets past POLL_CYCLE_MS, and by here the suite
    // has made more net-http sheets than one cycle takes. The next tick takes
    // the rest, which is what a second cycle at the same instant is.
    const pollUntil = async (at: number, polled: () => boolean | Promise<boolean>) => {
      for (let cycle = 0; cycle < 10; cycle++) {
        await pollNetOnce(fetcher, at);
        if (await polled()) return;
      }
      throw new Error(`ten cycles at ${at} and this sheet still had not been polled`);
    };
    const from = (host: string) => sent.filter((s) => s.url.startsWith(`https://${host}.verbs.test`));

    await pollUntil(t, () => from("post").length === 1 && from("quote").length === 1);
    assertEquals(from("post")[0].method, "POST");
    assertEquals(
      from("post")[0].body,
      '{"since":"","key":"sk-post-1"}',
      "the secret is resolved and the first poll has no watermark to ask from",
    );

    await pollUntil(t + 121_000, () => from("post").length === 2);
    assertEquals(
      JSON.parse(String(from("post")[1].body)).since,
      new Date(t).toISOString(),
      "the next poll asks for what came after the last good one",
    );

    // A failure row is what the user runs by hand, so it carries the method and
    // the body the sheet spells -- never the value the secret resolved to.
    answer = () => new Response("no", { status: 404 });
    await pollUntil(t + 242_000, () => from("post").length === 3 && from("quote").length === 3);
    const rows = (await get<Table>(jwt, `/net/${postedId}`)).slice(1);
    const [failed] = rows;
    const failure = JSON.parse(String(failed.body));
    assert(failure.repro.includes("-X POST"), `the repro names the method: ${failure.repro}`);
    assert(failure.repro.includes("{{secret:searchkey}}"), `and the reference: ${failure.repro}`);
    assert(!JSON.stringify(await get<Table>(jwt, `/net/${postedId}`)).includes("sk-post-1"), "never the secret");
    // The log's own column, not just the repro line: `select method from
    // @net-http:x` reads it, and every export carries it.
    assertEquals(
      rows.map((r) => r.method),
      rows.map(() => "POST"),
      "and every run says which verb it sent",
    );

    // Every apostrophe in the line is either a quote of its own or escaped as
    // `'\''`. One that is neither ends the quoting, and the pasted line hangs
    // on an unterminated quote instead of running the request it is about.
    const [quoted] = (await get<Table>(jwt, `/net/${quotingId}`)).slice(1);
    const repro = String(JSON.parse(String(quoted.body)).repro);
    assertEquals(
      (repro.replaceAll(`'\\''`, "").match(/'/g) ?? []).length % 2,
      0,
      `an unbalanced curl line: ${repro}`,
    );

    // PUT is the other verb a feed may be polled with.
    const putting = automerge.create<Sheet>({
      type: "net-http",
      data: [{ url: "https://put.verbs.test/feed", interval: 120, method: "PUT", body: "ping" }],
    });
    await put(jwt, `/library/net-http:${putting.documentId}`, {});
    answer = () => new Response(`{"ok":true}`);
    await pollUntil(
      t + 363_000,
      () => from("post").length === 4 && sent.some((s) => s.url === "https://put.verbs.test/feed"),
    );
    const putted = sent.find((s) => s.url === "https://put.verbs.test/feed");
    assertEquals([putted?.method, putted?.body], ["PUT", "ping"]);

    // The watermark survives a failed poll. Losing it asks the feed for all of
    // history again -- the one thing the cursor exists to prevent.
    assertEquals(
      JSON.parse(String(from("post")[3].body)).since,
      new Date(t + 121_000).toISOString(),
      "the poll after a failure resumes from the last good one",
    );

    // A verb this server does not send is refused by name where the sheet is
    // read, rather than sent as a GET the feed answers with the wrong thing.
    const odd = automerge.create({
      type: "net-http",
      data: [{ url: "https://odd.verbs.test/feed", interval: 120, method: "DELETE" }],
    });
    const oddId = `net-http:${odd.documentId}`;
    await put(jwt, `/library/${oddId}`, {});
    await pollUntil(t + 484_000, async () => (await get<Table>(jwt, `/net/${oddId}`)).length > 1);
    assertEquals(sent.filter((s) => s.url.startsWith("https://odd.")).length, 0, "and nothing was sent");
    const [refused] = (await get<Table>(jwt, `/net/${oddId}`)).slice(1);
    assert(JSON.parse(String(refused.body)).error.includes("DELETE"), String(refused.body));

    // And the watermark survives a failure raised before the run log is read at
    // all. A verb this server will not send is refused off the document, above
    // where the last row used to be read, so the failure row it wrote carried
    // no cursor and the next good poll asked the feed for all of history.
    const posts = from("post").length;
    await pollUntil(t + 605_000, () => from("post").length === posts + 1);
    const verb = (sends: string) =>
      (posted as unknown as { change: (fn: (d: { data: [{ method: string }] }) => void) => void })
        .change((d) => {
          d.data[0].method = sends;
        });
    verb("DELETE");
    const newest = async () =>
      JSON.parse(String((await get<Table>(jwt, `/net/${postedId}`)).slice(1)[0].body)) as { error?: string };
    await pollUntil(t + 726_000, async () => String((await newest()).error ?? "").includes("DELETE"));
    assertEquals(from("post").length, posts + 1, "nothing was sent for a verb this server does not send");
    verb("POST");
    await pollUntil(t + 847_000, () => from("post").length === posts + 2);
    assertEquals(
      JSON.parse(String(from("post")[posts + 1].body)).since,
      new Date(t + 605_000).toISOString(),
      "the poll after a config the poller refused still resumes from the last good one",
    );

    // A run is a run whichever verb made it. RUN_OF used to say a net-http run
    // was a GET, so a feed polled with a POST wrote rows nothing matched: it
    // read as never-run in freshness while it was polling, and a dead one could
    // not fail GET /status.
    const posting = (await get<Table>(jwt, "/sheet/library:freshness")).slice(1)
      .find((r) => r.sheet_id === postedId);
    assert(posting, "a feed polled with a POST is in the freshness read");
    assert(posting.last_run, `and its runs are its runs: ${JSON.stringify(posting)}`);

    // A GET with a body is refused the same way, before the secret in it is
    // resolved, so the row it writes measures the sheet's own text and never
    // the value behind `{{secret:leaky}}`.
    const [leaked] = (await get<Table>(jwt, `/net/${getBodyId}`)).slice(1);
    const refusal = String(JSON.parse(String(leaked.body)).error);
    assert(refusal.includes("A GET carries no body"), refusal);
    assert(!refusal.includes("20"), `the row measured the secret: ${refusal}`);
    assertEquals(from("getbody").length, 0, "and nothing was sent");
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

  // Conditional requests and the since-last-run cursor shipped, and one poll was still one request: a feed that
  // answers in pages delivered its first page forever, and every sheet built on it read a slice as the whole. A poll
  // now walks the feed to its end and concatenates the pages into one array, so shapeOf, the digest, the cap and
  // every reader downstream see one body per run exactly as they do from a feed that answers in one page.
  await t.step("A feed that answers in pages is read to the end", async () => {
    const { jwt } = await usr("pia@example.com");
    const feed = async (data: NetHttp) => {
      const hand = automerge.create<Sheet>({ type: "net-http", data: [data] });
      const id = `net-http:${hand.documentId}`;
      await put(jwt, `/library/${id}`, {});
      return id;
    };
    const ids = {
      // Each on its own host: two sheets on one host take turns across cycles,
      // which is a fact about the gap and not about paging.
      numbered: await feed({
        url: "https://numbered.pages.test/feed",
        interval: 3600,
        page_by: "page",
        page_param: "page",
      }),
      offset: await feed({
        url: "https://offset.pages.test/feed",
        interval: 3600,
        page_by: "offset",
        page_param: "from",
      }),
      cursor: await feed({
        url: "https://cursor.pages.test/feed",
        interval: 3600,
        page_by: "cursor",
        page_param: "after",
        page_path: "meta.next",
      }),
      link: await feed({ url: "https://link.pages.test/p1", interval: 3600, page_by: "link" }),
      endless: await feed({
        url: "https://endless.pages.test/feed",
        interval: 3600,
        page_by: "page",
        page_param: "page",
      }),
      ragged: await feed({
        url: "https://ragged.pages.test/feed",
        interval: 3600,
        page_by: "page",
        page_param: "page",
      }),
      roaming: await feed({ url: "https://roaming.pages.test/p1", interval: 3600, page_by: "link" }),
      plain: await feed({ url: "https://plain.pages.test/feed", interval: 3600 }),
    };

    const calls: string[] = [];
    const rows = (n: number | string) => JSON.stringify([{ n }]);
    const answer = (raw: string): Response => {
      const url = new URL(raw);
      const page = Number(url.searchParams.get("page") ?? 0);
      switch (url.hostname) {
        case "numbered.pages.test":
          return new Response(page <= 3 ? rows(page) : "[]");
        case "offset.pages.test": {
          const from = Number(url.searchParams.get("from") ?? -1);
          return new Response(from < 3 ? rows(from) : "[]");
        }
        case "cursor.pages.test": {
          // The rows sit under an envelope, because the envelope is where the
          // next cursor sits: the first array-valued key is the page.
          const after = url.searchParams.get("after") ?? "a";
          return new Response(
            JSON.stringify({ rows: [{ n: after }], meta: { next: after === "a" ? "b" : after === "b" ? "c" : null } }),
          );
        }
        case "link.pages.test":
          // A relative next, an absolute one beside a link that is not next,
          // and a last page that names none.
          return url.pathname === "/p1"
            ? new Response(rows(1), { headers: { link: `</p2>; rel="next"` } })
            : url.pathname === "/p2"
            ? new Response(JSON.stringify({ items: [{ n: 2 }] }), {
              headers: {
                link: `<https://link.pages.test/p1>; rel="prev", <https://link.pages.test/p3>; rel="next"`,
              },
            })
            : new Response(rows(3));
        case "endless.pages.test":
          return new Response(rows(page));
        case "ragged.pages.test":
          return new Response(page <= 1 ? rows(page) : JSON.stringify({ items: [] }));
        case "roaming.pages.test":
          return new Response(rows(1), { headers: { link: `<https://elsewhere.test/p2>; rel="next"` } });
        default:
          // A next page named every way a paged feed names one, on a sheet
          // that asked for none.
          return new Response(JSON.stringify({ rows: [{ n: 1 }], meta: { next: "b" } }), {
            headers: { link: `<https://plain.pages.test/p2>; rel="next"` },
          });
      }
    };
    const fetcher = (url: string) => {
      if (!url.includes(".pages.test")) return Promise.resolve(new Response(`{"ok":true}`));
      calls.push(url);
      return Promise.resolve(answer(url));
    };

    const logOf = async (id: string) => (await get<Table>(jwt, `/net/${id}`)).slice(1);
    const at = Date.now() + 90_000_000;
    // A cycle stops starting sheets past POLL_CYCLE_MS, and by here the suite
    // holds more net-http sheets than one cycle takes.
    for (let cycle = 0;; cycle++) {
      if (cycle >= 10) throw new Error(`ten cycles at ${at} and a paged sheet still had not been polled`);
      await pollNetOnce(fetcher, at);
      let polled = true;
      for (const id of Object.values(ids)) polled = polled && (await logOf(id)).length > 0;
      if (polled) break;
    }
    const bodyOf = async (id: string) => JSON.parse(String((await logOf(id))[0].body));
    const sent = (host: string) => calls.filter((u) => new URL(u).hostname === host);

    // Page one, two, three, and a fourth answering an empty array, which is the
    // whole of a numbered feed's stop condition.
    assertEquals(sent("numbered.pages.test").map((u) => new URL(u).searchParams.get("page")), ["1", "2", "3", "4"]);
    assertEquals(await bodyOf(ids.numbered), [{ n: 1 }, { n: 2 }, { n: 3 }], "the pages are one body");
    assertEquals((await logOf(ids.numbered)).length, 1, "and three pages are one run");
    // The one body every reader downstream sees: shapeOf reads the
    // concatenation, and the run grades off the answer and not off the paging.
    const [{ meta }]: { meta: { status: number; shape: Record<string, string>; bytes: number } }[] = await sql`
      select meta from net where sheet_id = ${ids.numbered}
    `;
    assertEquals(meta.status, 200);
    assertEquals(meta.shape, { n: "number" }, `the shape is the concatenation's: ${JSON.stringify(meta)}`);

    // An offset feed is asked from the count of rows it has already handed over.
    assertEquals(sent("offset.pages.test").map((u) => new URL(u).searchParams.get("from")), ["0", "1", "2", "3"]);
    assertEquals(await bodyOf(ids.offset), [{ n: 0 }, { n: 1 }, { n: 2 }]);

    // A cursor feed is asked for nothing on the first request -- it has none
    // yet -- and stops when the path it names holds nothing.
    assertEquals(sent("cursor.pages.test").map((u) => new URL(u).searchParams.get("after")), [null, "b", "c"]);
    assertEquals(await bodyOf(ids.cursor), [{ n: "a" }, { n: "b" }, { n: "c" }]);

    // A link feed names the whole url, relative or absolute, beside however
    // many links that are not next, and stops by naming none.
    assertEquals(sent("link.pages.test").map((u) => new URL(u).pathname), ["/p1", "/p2", "/p3"]);
    assertEquals(await bodyOf(ids.link), [{ n: 1 }, { n: 2 }, { n: 3 }]);

    // A feed that never says "last" is a failure row carrying the counter, and
    // the sheet keeps nothing from that poll.
    assertEquals(sent("endless.pages.test").length, PAGE_MAX, "every page the cap allows, and the one past it refused");
    const endless = String((await bodyOf(ids.endless)).error);
    assert(endless.includes(`page ${PAGE_MAX + 1}`), endless);
    assert(endless.includes(`${PAGE_MAX} pages`), endless);
    assertEquals((await logOf(ids.endless)).length, 1, "and nothing of what those pages held is kept");

    // A page that is not an array is a failure row naming the page it arrived on.
    assertEquals(sent("ragged.pages.test").length, 2, "and the poll stops on it");
    const ragged = String((await bodyOf(ids.ragged)).error);
    assert(ragged.includes("Page 2"), ragged);
    assert(ragged.includes("not an array of rows"), ragged);

    // A feed must not walk the poller off the host its sheet names: safeFetch
    // guards every page whatever the Link header says, and this is the rule
    // that a next page is the same feed.
    assertEquals(sent("roaming.pages.test").length, 1);
    const roaming = String((await bodyOf(ids.roaming)).error);
    assert(roaming.includes("elsewhere.test"), roaming);
    assert(roaming.includes("not on the host this sheet names"), roaming);

    // And a sheet with no page_by is the one request it always was, whatever
    // the answer names.
    assertEquals(sent("plain.pages.test").length, 1);
    assertEquals(await bodyOf(ids.plain), { rows: [{ n: 1 }], meta: { next: "b" } }, "and stores what arrived");

    // These feeds have answered. Parked out of every later step's clock: a step
    // that re-polls a hundred-page sheet it does not care about spends the
    // suite's ten seconds walking it.
    for (const id of Object.values(ids)) netDue.set(id, Number.MAX_SAFE_INTEGER);
  });

  // Every way a paged sheet can be wrong, and the shapes a right one still takes: config the form should have
  // refused, a page that breaks after page one answered, a next-page cursor that is not a value that can be sent
  // back, a Link relation that merely contains "next", a next page on another origin, and the two edges PAGE_MAX
  // and BODY_CAP draw -- a feed that stops exactly at the cap, and one whose pages sum past it.
  await t.step("Paging refuses the sheets it cannot follow, and follows the odd ones it can", async () => {
    const { jwt } = await usr("bree@example.com");
    const feed = async (data: NetHttp) => {
      const hand = automerge.create<Sheet>({ type: "net-http", data: [data] });
      const id = `net-http:${hand.documentId}`;
      await put(jwt, `/library/${id}`, {});
      return id;
    };
    const ids = {
      noParam: await feed({ url: "https://noparam.paged.test/feed", interval: 3600, page_by: "page" }),
      noPath: await feed({
        url: "https://nopath.paged.test/feed",
        interval: 3600,
        page_by: "cursor",
        page_param: "after",
      }),
      badBy: await feed({ url: "https://badby.paged.test/feed", interval: 3600, page_by: "pages" as "page" }),
      paramLong: await feed({
        url: "https://paramlong.paged.test/feed",
        interval: 3600,
        page_by: "offset",
        page_param: "x".repeat(65),
      }),
      pathDeep: await feed({
        url: "https://pathdeep.paged.test/feed",
        interval: 3600,
        page_by: "cursor",
        page_param: "after",
        page_path: Array(9).fill("a").join("."),
      }),
      // page_param naming the parameter this feed already takes its
      // since-value in: paging overwrites it on every request.
      cursorCollide: await feed({
        url: "https://cursorcollide.paged.test/feed",
        interval: 3600,
        page_by: "page",
        page_param: "since",
        cursor: "since",
      }),
      cursorObject: await feed({
        url: "https://cursorobject.paged.test/feed",
        interval: 3600,
        page_by: "cursor",
        page_param: "after",
        page_path: "meta.next",
      }),
      jsonBad: await feed({
        url: "https://jsonbad.paged.test/feed",
        interval: 3600,
        page_by: "page",
        page_param: "page",
      }),
      status404: await feed({
        url: "https://status404.paged.test/feed",
        interval: 3600,
        page_by: "page",
        page_param: "page",
      }),
      retryMidRun: await feed({
        url: "https://retrymidrun.paged.test/feed",
        interval: 3600,
        page_by: "page",
        page_param: "page",
      }),
      atCap: await feed({ url: "https://atcap.paged.test/feed", interval: 3600, page_by: "page", page_param: "page" }),
      overCap: await feed({
        url: "https://overcap.paged.test/feed",
        interval: 3600,
        page_by: "page",
        page_param: "page",
      }),
      relAmbig: await feed({ url: "https://relambig.paged.test/p1", interval: 3600, page_by: "link" }),
      scheme: await feed({ url: "https://scheme.paged.test/p1", interval: 3600, page_by: "link" }),
      noArray: await feed({
        url: "https://noarray.paged.test/feed",
        interval: 3600,
        page_by: "cursor",
        page_param: "after",
        page_path: "meta.next",
      }),
      // page_path pointed at a field inside the rows array itself, rather than
      // beside it: a plausible slip rather than a made-up shape.
      arrayPath: await feed({
        url: "https://arraypath.paged.test/feed",
        interval: 3600,
        page_by: "cursor",
        page_param: "after",
        page_path: "items.next",
      }),
      encoded: await feed({
        url: "https://encoded.paged.test/feed",
        interval: 3600,
        page_by: "cursor",
        page_param: "after",
        page_path: "meta.next",
      }),
      numericCursor: await feed({
        url: "https://numericcursor.paged.test/feed",
        interval: 3600,
        page_by: "cursor",
        page_param: "after",
        page_path: "meta.next",
      }),
      bareValues: await feed({
        url: "https://barevalues.paged.test/feed",
        interval: 3600,
        page_by: "page",
        page_param: "page",
      }),
      paramInUrl: await feed({
        url: "https://paraminurl.paged.test/feed?from=999",
        interval: 3600,
        page_by: "offset",
        page_param: "from",
      }),
      shaped: await feed({
        url: "https://shapedpages.paged.test/feed",
        interval: 60,
        page_by: "page",
        page_param: "page",
      }),
      twoArrays: await feed({
        url: "https://twoarrays.paged.test/feed",
        interval: 3600,
        page_by: "cursor",
        page_param: "after",
        page_path: "meta.next",
      }),
      noMeta: await feed({
        url: "https://nometa.paged.test/feed",
        interval: 3600,
        page_by: "cursor",
        page_param: "after",
        page_path: "meta.next",
      }),
      metaDropped: await feed({
        url: "https://metadropped.paged.test/feed",
        interval: 3600,
        page_by: "cursor",
        page_param: "after",
        page_path: "meta.next",
      }),
    };

    const calls: string[] = [];
    const rows = (n: number | string) => JSON.stringify([{ n }]);
    // Round-trips through a real URL: set on one request, read back on the
    // next, exactly as the wire would carry it.
    const cursorValue = "b c&d=e/f+g #h 🎉";
    let shapedAnswer = [`[{"a":1}]`, `[{"a":1,"b":"x"}]`];
    const answer = (raw: string): Response => {
      const url = new URL(raw);
      const page = Number(url.searchParams.get("page") ?? 0);
      switch (url.hostname) {
        case "cursorobject.paged.test":
          return new Response(JSON.stringify({ rows: [{ n: 1 }], meta: { next: { weird: true } } }));
        case "jsonbad.paged.test":
          return new Response(page === 1 ? rows(1) : "{not json");
        case "status404.paged.test":
          return page === 1 ? new Response(rows(1)) : new Response("nope", { status: 404 });
        case "retrymidrun.paged.test":
          return page === 1
            ? new Response(rows(1))
            : new Response("slow down", { status: 429, headers: { "retry-after": "45" } });
        case "atcap.paged.test":
          // Every page but the last holds a row. The last one the cap allows is
          // empty, and must not be read as the first one past it.
          return new Response(page < PAGE_MAX ? rows(page) : "[]");
        case "overcap.paged.test":
          // Two pages under the per-response cap on their own, over it together.
          return new Response(JSON.stringify([{ n: "x".repeat(600_000) }]));
        case "relambig.paged.test":
          // A decoy relation holding "next" as a substring, and the real one
          // beside it: only the real one may be followed.
          return url.pathname === "/p1"
            ? new Response(rows(1), { headers: { link: `</decoy>; rel="x-next", </p2>; rel="next"` } })
            : url.pathname === "/p2"
            ? new Response(rows(2))
            : new Response(rows("decoy was fetched"));
        case "scheme.paged.test":
          // Same host, dropped to plain http. No next link past page one, so a
          // poller that (wrongly) followed it stops at two requests rather than
          // running to the cap.
          return url.pathname === "/p1"
            ? new Response(rows(1), { headers: { link: `<http://scheme.paged.test/p2>; rel="next"` } })
            : new Response(rows(2));
        case "noarray.paged.test":
          return new Response(JSON.stringify({ meta: { next: "b" }, count: 5 }));
        case "arraypath.paged.test":
          return new Response(JSON.stringify({ items: [{ n: 1 }], count: 1 }));
        case "encoded.paged.test": {
          const after = url.searchParams.get("after");
          return after === null
            ? new Response(JSON.stringify({ rows: [{ n: "a" }], meta: { next: cursorValue } }))
            : new Response(JSON.stringify({ rows: [{ n: after }], meta: { next: null } }));
        }
        case "numericcursor.paged.test": {
          const after = url.searchParams.get("after");
          return after === null
            ? new Response(JSON.stringify({ rows: [{ n: 1 }], meta: { next: 42 } }))
            : new Response(JSON.stringify({ rows: [{ n: after }], meta: { next: null } }));
        }
        case "barevalues.paged.test":
          return new Response(page <= 1 ? JSON.stringify([10, 20, 30]) : "[]");
        case "paraminurl.paged.test": {
          const from = Number(url.searchParams.get("from") ?? -1);
          return new Response(from < 3 ? JSON.stringify([{ n: from }]) : "[]");
        }
        case "shapedpages.paged.test":
          return new Response(shapedAnswer[page - 1] ?? "[]");
        case "twoarrays.paged.test":
          // An empty array beside the rows. The first array read as the page,
          // and this feed landed nothing, every poll, under a green run row.
          return new Response(JSON.stringify({ warnings: [], items: [{ n: 1 }], meta: { next: null } }));
        case "nometa.paged.test":
          return new Response(JSON.stringify({ items: [{ n: 1 }] }));
        case "metadropped.paged.test": {
          const after = url.searchParams.get("after");
          return after === null
            ? new Response(JSON.stringify({ items: [{ n: 1 }], meta: { next: "c2" } }))
            : new Response(JSON.stringify({ items: [{ n: after }] }));
        }
        default:
          // Every sheet whose config is refused before a request belongs here:
          // reaching the wire at all is the failure.
          throw new Error(`no mock answer wired for ${raw}`);
      }
    };
    const fetcher = (url: string) => {
      if (!url.includes(".paged.test")) return Promise.resolve(new Response(`{"ok":true}`));
      calls.push(url);
      return Promise.resolve(answer(url));
    };

    // Read out of the table rather than through `GET /net/:id`: that route is
    // itself a read and spends the very budget asked about below.
    const newest = async (id: string) => {
      const [row]: { body: string; meta: Record<string, unknown> }[] = await sql`
        select body, meta from net where sheet_id = ${id} order by net_id desc limit 1
      `;
      return row;
    };
    const bodyOf = async (id: string) => JSON.parse((await newest(id)).body);
    const errorOf = async (id: string) => String((await bodyOf(id)).error ?? "");
    const sent = (host: string) => calls.filter((u) => new URL(u).hostname === host);

    const at = Date.now() + 91_000_000;
    // A cycle stops starting sheets past POLL_CYCLE_MS, and by here the suite
    // holds more net-http sheets than one cycle takes.
    for (let cycle = 0;; cycle++) {
      if (cycle >= 10) throw new Error(`ten cycles at ${at} and a paged sheet still had not been polled`);
      await pollNetOnce(fetcher, at);
      const [{ n }]: { n: number }[] = await sql`
        select count(distinct sheet_id)::int as n from net where sheet_id = any(${Object.values(ids)})
      `;
      if (n === Object.values(ids).length) break;
    }

    // Polling has no door of its own into a sheet's budget: `spend()` is never
    // called anywhere in `pollNetOnce`, so a hundred pages and one page leave
    // the same nothing behind. Asked before anything below reads a sheet.
    assertEquals(Object.values(ids).filter((id) => hookBuckets.has(id)), []);

    // Config the form should have refused. Every one of these is the sheet's
    // own failure, made before a request goes out, and names the field to fix.
    for (
      const [id, host, headline, source] of [
        [ids.noParam, "noparam", "asked for its next page by a query parameter", "the page_param field"],
        [ids.noPath, "nopath", "names where in its answer the next cursor sits", "the page_path field"],
        [ids.badBy, "badby", "That is not a way this server reads a feed's pages.", "the page_by field"],
        // Past 64 characters: the regex is the gate, not a length check on top.
        [ids.paramLong, "paramlong", "asked for its next page by a query parameter", "the page_param field"],
        // Past eight dotted names.
        [ids.pathDeep, "pathdeep", "names where in its answer the next cursor sits", "the page_path field"],
        [
          ids.cursorCollide,
          "cursorcollide",
          "the same query parameter as this sheet's own cursor",
          "the page_param and cursor fields",
        ],
      ] as const
    ) {
      assertEquals(sent(`${host}.paged.test`).length, 0, `${host} asked the wire before its config was refused`);
      const said = await errorOf(id);
      assert(said.includes(headline), said);
      assert(said.includes(`${source} on this net-http sheet`), said);
    }

    // A cursor that names an object cannot be sent back on the next request,
    // and the poll says so instead of stringifying whatever it found.
    assertEquals(sent("cursorobject.paged.test").length, 1);
    assert((await errorOf(ids.cursorObject)).includes("not a value that can be sent back"));

    // Two arrays in the envelope is a guess about which one holds the rows, and
    // the wrong guess is a sheet of nothing under a green run row. Refused by
    // name, both names.
    assertEquals(sent("twoarrays.paged.test").length, 1);
    const twoArrays = await errorOf(ids.twoArrays);
    assert(twoArrays.includes("holds more than one array"), twoArrays);
    assert(twoArrays.includes("warnings, items"), twoArrays);

    // page_path through a name page one does not hold is a path this feed never
    // had, not a one-page feed: a renamed envelope read as "last page" on every
    // poll. A later page dropping the envelope is still the last page.
    assertEquals(sent("nometa.paged.test").length, 1);
    const noMeta = await errorOf(ids.noMeta);
    assert(noMeta.includes("nothing at meta on page 1"), noMeta);
    assertEquals(sent("metadropped.paged.test").length, 2);
    assertEquals(await bodyOf(ids.metaDropped), [{ n: 1 }, { n: "c2" }]);

    // A parse failure on page two, not page one, names the page it broke on --
    // the counter in the message is the page, not a request tally -- and the
    // repro replays that page, not the one that answered.
    assertEquals(sent("jsonbad.paged.test").length, 2);
    const jsonBad = await bodyOf(ids.jsonBad);
    assert(String(jsonBad.error).includes("Page 2 of this feed is not JSON"), jsonBad.error);
    assert(String(jsonBad.repro).includes("page=2"), `repro replays the wrong page: ${jsonBad.repro}`);

    // A 404 on page two is this feed's answer, not the wire's: the sheet is
    // right to have asked, and the fix says so.
    assertEquals(sent("status404.paged.test").length, 2);
    assert((await errorOf(ids.status404)).includes("This feed answered 404 on page 2."));

    // The host says "later" on page two: the pages already read are dropped,
    // the failure row carries the attempt and the page-two response, and its
    // repro fetches page two, which is the request that actually failed.
    assertEquals(sent("retrymidrun.paged.test").length, 2);
    const retryRow = await bodyOf(ids.retryMidRun);
    assert(!Array.isArray(retryRow), `page one's row must not survive the poll: ${JSON.stringify(retryRow)}`);
    assert(String(retryRow.error).includes("429"), JSON.stringify(retryRow));
    assertEquals(retryRow.attempt, 1);
    // The host's Retry-After (45s) outweighs the first backoff step (30s), so
    // the later of the two -- what `holdHost` keeps -- is the host's own answer.
    assertEquals(retryRow.retry_at, new Date(at + 45_000).toISOString(), JSON.stringify(retryRow));
    assert(String(retryRow.repro).includes("page=2"), `repro replays the wrong page: ${retryRow.repro}`);

    // A feed that stops exactly at the cap succeeds: the last page it allows is
    // the last one allowed, not the first one refused.
    assertEquals(sent("atcap.paged.test").length, PAGE_MAX, "every page the cap allows, all of them kept");
    const atCapBody = await bodyOf(ids.atCap);
    assertEquals(atCapBody.length, PAGE_MAX - 1, "a row a page, and an empty last one that stops it");
    assertEquals(atCapBody[0], { n: 1 });
    assertEquals(atCapBody[PAGE_MAX - 2], { n: PAGE_MAX - 1 });

    // Two pages under the per-response cap sum past it, and the failure names
    // the run's total rather than either page's -- the run-total guard's own
    // headline, not the per-response guard's, which "too large to store" alone
    // would also match.
    assertEquals(sent("overcap.paged.test").length, 2, "the second page is read, then refused for its sum");
    assert((await errorOf(ids.overCap)).includes("This feed's pages are too large to store."));

    // The decoy is never fetched: a relation that merely holds "next" as a
    // substring ("x-next") is not the "next" relation.
    assertEquals(sent("relambig.paged.test").map((u) => new URL(u).pathname), ["/p1", "/p2"]);
    assertEquals(await bodyOf(ids.relAmbig), [{ n: 1 }, { n: 2 }]);

    // A next page naming the same host on plain http is not the origin the
    // sheet was pointed at, exactly as a different host is not.
    assertEquals(sent("scheme.paged.test").length, 1);
    assert((await errorOf(ids.scheme)).includes("not on the host this sheet names"));

    // A cursor feed's envelope with no array in it at all is the same refusal
    // as one holding something else where the rows belong.
    assertEquals(sent("noarray.paged.test").length, 1);
    assert((await errorOf(ids.noArray)).includes("not an array of rows"));

    // page_path pointing into the rows array itself cannot be traversed --
    // "items" holds the array pageRows already read as the rows, and there is
    // no ".next" inside it. Reading that as "no more pages" would run a
    // misconfigured sheet one page short forever, a 200 in its log and nothing
    // to say why.
    assertEquals(sent("arraypath.paged.test").length, 1);
    assert((await errorOf(ids.arrayPath)).includes("not an object"));

    // A cursor holding a space, an ampersand, an equals sign, a slash, a plus,
    // a hash and an emoji comes back exactly as it went out.
    assertEquals(sent("encoded.paged.test").length, 2);
    assertEquals(new URL(sent("encoded.paged.test")[1]).searchParams.get("after"), cursorValue);
    assertEquals(await bodyOf(ids.encoded), [{ n: "a" }, { n: cursorValue }]);

    // A cursor that is a JSON number, not a string, is still sent back --
    // `atPath` reads either -- and comes back as the digits alone.
    assertEquals(sent("numericcursor.paged.test").map((u) => new URL(u).searchParams.get("after")), [null, "42"]);
    assertEquals(await bodyOf(ids.numericCursor), [{ n: 1 }, { n: "42" }]);

    // A page of bare numbers concatenates like any other: nothing here assumes
    // a row is an object, and a page with no objects has no shape.
    assertEquals(await bodyOf(ids.bareValues), [10, 20, 30]);
    assertEquals((await newest(ids.bareValues)).meta.shape, null, "no object rows, so no shape");

    // page_param overwrites a same-named parameter already baked into the
    // sheet's own url: paging starts fresh, at zero and not at whatever the
    // url happened to hold.
    assertEquals(sent("paraminurl.paged.test").map((u) => new URL(u).searchParams.get("from")), ["0", "1", "2", "3"]);

    // One run, two pages of different shape: shapeOf sees the concatenation,
    // not either page alone, so the merged shape carries every key either page
    // answered with, and the first run is a baseline rather than a change.
    assertEquals((await newest(ids.shaped)).meta.shape, { a: "number", b: "string" });
    assertEquals((await newest(ids.shaped)).meta.shape_change, undefined);

    // The next run answers one page of a different shape. The change compares
    // against the run before -- never page one against page two, which is one
    // body and one shape.
    shapedAnswer = [`[{"a":"text now"}]`];
    // This one sheet only: the rest of `ids` just answered above and are not
    // due again, so the tick would find nothing to do but scan the pile of
    // net-http sheets every earlier step left to confirm that.
    await pollFeed(ids.shaped, fetcher, at + 65_000);
    assertEquals((await newest(ids.shaped)).meta.shape, { a: "string" });
    assertEquals((await newest(ids.shaped)).meta.shape_change, { added: [], dropped: ["b"], retyped: ["a"] });

    // Parked out of every later step's clock, the way the step before parks its
    // own: a step that re-polls a hundred-page sheet it does not care about
    // spends the suite's ten seconds walking it.
    for (const id of Object.values(ids)) netDue.set(id, Number.MAX_SAFE_INTEGER);
    hostDue.clear();
  });

  // Every run was a row, and a feed that answers its whole table every poll built a log of a hundred copies of one
  // answer -- NET_KEEP rows deep, every reader downstream paging through the lot. A sheet now says what a good run
  // does to the runs before it: `replace` keeps the newest good run alone, `upsert` supersedes only the earlier runs
  // that answered for a record this one answered for again, and `rows_path` says which array in an envelope is the
  // rows, for a feed that hands back two on purpose.
  await t.step("A feed can replace or upsert its runs rather than append", async () => {
    const { jwt } = await usr("nell@example.com");
    const feed = async (data: NetHttp) => {
      const hand = automerge.create<Sheet>({ type: "net-http", data: [data] });
      const id = `net-http:${hand.documentId}`;
      await put(jwt, `/library/${id}`, {});
      return id;
    };
    const ids = {
      replaced: await feed({ url: "https://replaced.keep.test/feed", interval: 3600, mode: "replace" }),
      failing: await feed({ url: "https://failing.keep.test/feed", interval: 3600, mode: "replace" }),
      upserted: await feed({ url: "https://upserted.keep.test/feed", interval: 3600, mode: "upsert", key: "id" }),
      enveloped: await feed({
        url: "https://enveloped.keep.test/feed",
        interval: 3600,
        page_by: "page",
        page_param: "page",
        rows_path: "data",
      }),
      wrapped: await feed({
        url: "https://wrapped.keep.test/feed",
        interval: 3600,
        mode: "upsert",
        key: "id",
        rows_path: "data",
      }),
      // Refused off the document, before a request goes out.
      badMode: await feed({ url: "https://badmode.keep.test/feed", interval: 3600, mode: "keep" as "replace" }),
      keyKept: await feed({ url: "https://keykept.keep.test/feed", interval: 3600, mode: "replace", key: "id" }),
      noKey: await feed({ url: "https://nokey.keep.test/feed", interval: 3600, mode: "upsert" }),
      badPath: await feed({ url: "https://badpath.keep.test/feed", interval: 3600, rows_path: "a..b" }),
      // Refused by what arrived, on the one request each makes.
      noRows: await feed({
        url: "https://norows.keep.test/feed",
        interval: 3600,
        page_by: "page",
        page_param: "page",
        rows_path: "items",
      }),
      noRowKey: await feed({ url: "https://norowkey.keep.test/feed", interval: 3600, mode: "upsert", key: "id" }),
    };

    let round = 0;
    const asked = new Map<string, number>();
    const calls: string[] = [];
    const answer = (raw: string): Response => {
      const url = new URL(raw);
      const page = Number(url.searchParams.get("page") ?? 0);
      switch (url.hostname) {
        case "replaced.keep.test":
          return new Response(JSON.stringify([{ n: round }]));
        case "failing.keep.test":
          return round === 1 ? new Response(JSON.stringify([{ n: 1 }])) : new Response("gone", { status: 404 });
        case "upserted.keep.test":
          return new Response(
            JSON.stringify(round === 1 ? [{ id: "a" }, { id: "b" }] : round === 2 ? [{ id: "c" }] : [{ id: "b" }]),
          );
        case "enveloped.keep.test":
          // Two arrays on purpose, the way JSON:API answers `data` beside
          // `included`. Neither the first nor the only one: rows_path is what
          // says which is the rows.
          return new Response(
            JSON.stringify(page === 1 ? { included: [{ x: 1 }], data: [{ n: round }] } : { data: [] }),
          );
        case "wrapped.keep.test":
          return new Response(
            JSON.stringify({ included: [], data: round === 1 ? [{ id: "w1" }] : [{ id: "w1" }, { id: "w2" }] }),
          );
        case "norows.keep.test":
          return new Response(JSON.stringify({ data: [{ n: 1 }], count: 1 }));
        case "norowkey.keep.test":
          return new Response(JSON.stringify([{ id: "k1" }, { n: 2 }]));
        default:
          throw new Error(`no mock answer wired for ${raw}`);
      }
    };
    const fetcher = (url: string) => {
      if (!url.includes(".keep.test")) return Promise.resolve(new Response(`{"ok":true}`));
      calls.push(url);
      asked.set(new URL(url).hostname, round);
      return Promise.resolve(answer(url));
    };
    // The feeds a round waits for. It shrinks as each says its last thing:
    // a poll costs the round the same whether or not anything reads it.
    let wired: readonly (keyof typeof ids)[] = ["replaced", "failing", "upserted", "enveloped", "wrapped"];

    const rowsOf = async (id: string) => {
      const got: { body: string; meta: Record<string, unknown> }[] = await sql`
        select body, meta from net where sheet_id = ${id} order by net_id desc
      `;
      return got;
    };
    const bodyOf = async (id: string) => JSON.parse((await rowsOf(id))[0].body);
    const sent = (host: string) => calls.filter((u) => new URL(u).hostname === host);
    // A cycle stops starting sheets past POLL_CYCLE_MS, and by here the suite
    // holds more net-http sheets than one cycle takes.
    const pollRound = async (when: number) => {
      round += 1;
      for (let cycle = 0;; cycle++) {
        if (cycle >= 10)
          throw new Error(`${cycle} cycles at ${when} and round ${round} still had not polled ${wired.join(", ")}`);
        await pollNetOnce(fetcher, when);
        if (wired.every((name) => asked.get(new URL(`https://${name}.keep.test`).hostname) === round)) return;
      }
    };

    const at = Date.now() + 92_000_000;
    await pollRound(at);

    // Every mode's first run is one row: there is nothing yet for it to
    // supersede, and append is what a first poll always does.
    for (const name of wired) assertEquals((await rowsOf(ids[name])).length, 1, `${name} landed its first run`);
    // rows_path names which array is the rows. Without it this envelope is two
    // arrays and a refusal; with it the rows are the ones the sheet named, not
    // the first key that happened to hold an array.
    assertEquals(await bodyOf(ids.enveloped), [{ n: 1 }], "rows_path picks the array the sheet named");
    assertEquals((await rowsOf(ids.wrapped))[0].meta.keys, ["w1"], "an upsert run records what it answered for");

    // Config a form should have refused, each named by its own field and each
    // refused before a request goes out.
    for (
      const [id, host, headline, source] of [
        [ids.badMode, "badmode", "That is not a way this server stores a feed's runs.", "the mode field"],
        [ids.keyKept, "keykept", "A replace feed supersedes nothing by key.", "the mode and key fields"],
        [ids.noKey, "nokey", "An upsert feed names the field its rows are identified by.", "the key field"],
        [
          ids.badPath,
          "badpath",
          "That is not where in an answer this server reads a feed's rows.",
          "the rows_path field",
        ],
      ] as const
    ) {
      assertEquals(sent(`${host}.keep.test`).length, 0, `${host} asked the wire before its config was refused`);
      const said = String((await bodyOf(id)).error);
      assert(said.includes(headline), said);
      assert(said.includes(`${source} on this net-http sheet`), said);
    }

    // rows_path aimed where this feed holds no array is a failure row, not a
    // feed of no rows: a renamed envelope otherwise reads as an empty page and
    // stops the walk under a green run row.
    assertEquals(sent("norows.keep.test").length, 1);
    const noRows = String((await bodyOf(ids.noRows)).error);
    assert(noRows.includes("holds no rows where rows_path says they sit"), noRows);
    assert(noRows.includes("items holds nothing"), noRows);

    // A row with nothing at the key is refused by its position rather than
    // skipped: a run that dropped half its keys supersedes half of what it
    // should, and the sheet keeps two rows for the one record.
    assertEquals(sent("norowkey.keep.test").length, 1);
    const noRowKey = String((await bodyOf(ids.noRowKey)).error);
    assert(noRowKey.includes("A row of this upsert feed holds no key."), noRowKey);
    assert(noRowKey.includes("row 2 of 2"), noRowKey);

    // Each refusal has said its one thing, and a refused poll costs the round
    // what a good one does. Parked out of the rounds below.
    const refused = ["badMode", "keyKept", "noKey", "badPath", "noRows", "noRowKey"] as const;
    for (const name of refused) netDue.set(ids[name], Number.MAX_SAFE_INTEGER);

    await pollRound(at + 3_600_001);

    // The newest good run is the sheet.
    assertEquals((await rowsOf(ids.replaced)).length, 1, "replace keeps the newest good run alone");
    assertEquals(await bodyOf(ids.replaced), [{ n: 2 }], "and it is the newest one");

    // A poll that answered nothing supersedes nothing: one bad poll must not
    // empty the sheet, and the failure row is the log saying why.
    const failing = await rowsOf(ids.failing);
    assertEquals(failing.length, 2, `a failed run keeps what is here: ${JSON.stringify(failing.map((r) => r.meta))}`);
    assertEquals(failing[0].meta.status, 404, "the newest row is the failure");
    assertEquals(JSON.parse(failing[1].body), [{ n: 1 }], "and the good run before it is untouched");

    // An upsert answering for a record nothing here holds supersedes nothing.
    assertEquals((await rowsOf(ids.upserted)).length, 2, "an unrelated run is left alone");

    // A feed whose rows sit in an envelope is read for its keys the same way:
    // the array at rows_path, and never the envelope around it.
    const wrapped = await rowsOf(ids.wrapped);
    assertEquals(wrapped.length, 1, "the run before answered for w1, and this one answers for it again");
    assertEquals(wrapped[0].meta.keys, ["w1", "w2"], "sorted and deduplicated");
    // And what it stores is those rows, not the envelope holding them. This sheet
    // makes one request, and rows_path used to be read on the paged path alone --
    // so the setting found the keys and the body kept the `included` beside them.
    assertEquals(
      JSON.parse(wrapped[0].body),
      [{ id: "w1" }, { id: "w2" }],
      "rows_path is read on a feed that answers in one request too",
    );

    // Append is still append: the mode is per sheet and this one names none.
    assertEquals((await rowsOf(ids.enveloped)).length, 2, "a sheet with no mode keeps every run");

    for (const name of ["replaced", "failing", "enveloped", "wrapped"] as const)
      netDue.set(ids[name], Number.MAX_SAFE_INTEGER);
    wired = ["upserted"];

    await pollRound(at + 7_200_002);

    // The run log stays the unit: this run answered for b, so the run that
    // answered for a and b is superseded, and the one that answered for c is
    // not.
    const upserted = await rowsOf(ids.upserted);
    assertEquals(upserted.length, 2, "the overlapping run goes and the unrelated one stays");
    assertEquals(JSON.parse(upserted[0].body), [{ id: "b" }]);
    assertEquals(JSON.parse(upserted[1].body), [{ id: "c" }]);

    // Parked out of every later step's clock, the way the steps before park
    // their own.
    for (const id of Object.values(ids)) netDue.set(id, Number.MAX_SAFE_INTEGER);
    hostDue.clear();
  });

  // A 304 moves the row it validated rather than appending, and that row keeps
  // being the run it always was: `meta.keys` is what an upsert reads to know
  // which earlier runs a later one supersedes, and a quiet tick between two
  // real ones must not erase it. A 304 handler that rebuilt meta from scratch
  // rather than keeping what the row already held would drop `keys` (and
  // `sig`) on the floor, and the run after it would supersede nothing: the
  // stale row survives beside the new one instead of being replaced.
  await t.step("An upsert survives a quiet poll in between", async () => {
    const { jwt } = await usr("odin@example.com");
    const hand = automerge.create<Sheet>({
      type: "net-http",
      data: [{ url: "https://quiet.upsert.test/feed", interval: 3600, mode: "upsert", key: "id" } as NetHttp],
    });
    const id = `net-http:${hand.documentId}`;
    await put(jwt, `/library/${id}`, {});
    let answer = () => new Response(JSON.stringify([{ id: "a" }]), { headers: { etag: `"v1"` } });
    const fetcher = (url: string) => Promise.resolve(url.includes("quiet.upsert.test") ? answer() : new Response(`{}`));
    const rows = async (): Promise<{ body: string; meta: Record<string, unknown> }[]> =>
      await sql`select body, meta from net where sheet_id = ${id} order by net_id desc`;

    const t0 = Date.now() + 93_000_000;
    await pollFeed(id, fetcher, t0);
    assertEquals((await rows()).length, 1, "the first run lands");
    assertEquals((await rows())[0].meta.keys, ["a"], "and it is recorded as answering for a");

    // A 304: the row moves, and nothing about what it was answering for is
    // supposed to change.
    answer = () => new Response(null, { status: 304 });
    await pollFeed(id, fetcher, t0 + 3_600_001);
    const quiet = await rows();
    assertEquals(quiet.length, 1, "a 304 appends nothing");
    assertEquals(quiet[0].meta.not_modified, true, "graded the healthy quiet tick it is");
    assertEquals(quiet[0].meta.keys, ["a"], "and the row it moved is still the run that answered for a");

    // A real run naming a and b again: it supersedes the run before it, which
    // is the one the 304 moved, because that row still says it answered for a.
    answer = () => new Response(JSON.stringify([{ id: "a" }, { id: "b" }]), { headers: { etag: `"v2"` } });
    await pollFeed(id, fetcher, t0 + 7_200_002);
    const after = await rows();
    assertEquals(after.length, 1, "the quiet run's row is superseded, not left behind as a stale duplicate");
    assertEquals(JSON.parse(after[0].body), [{ id: "a" }, { id: "b" }]);

    netDue.set(id, Number.MAX_SAFE_INTEGER);
    hostDue.clear();
  });

  // JSON has no integer past 2^53: two distinct ids past it can parse to the
  // same float, and a key that trusted the parsed number would silently fold
  // two different records into one -- the opposite of what upsert promises.
  // Refused by name rather than merged, the same way a missing key is.
  await t.step("An upsert key past safe integer precision is refused, not silently merged", async () => {
    const { jwt } = await usr("perry@example.com");
    const hand = automerge.create<Sheet>({
      type: "net-http",
      data: [{ url: "https://wide.upsert.test/feed", interval: 3600, mode: "upsert", key: "id" } as NetHttp],
    });
    const id = `net-http:${hand.documentId}`;
    await put(jwt, `/library/${id}`, {});
    // Two ids that are distinct as written and identical once JSON.parse has
    // rounded them -- the collision is in the parser, not in this server.
    const fetcher = (url: string) =>
      Promise.resolve(
        url.includes("wide.upsert.test")
          ? new Response(`[{"id": 9007199254740993}, {"id": 9007199254740992}]`)
          : new Response(`{}`),
      );
    const rows = async (): Promise<{ body: string; meta: Record<string, unknown> }[]> =>
      await sql`select body, meta from net where sheet_id = ${id} order by net_id desc`;

    await pollFeed(id, fetcher, Date.now() + 94_000_000);
    const said = String(JSON.parse((await rows())[0].body).error);
    assert(said.includes("A row of this upsert feed holds a key past safe precision."), said);
    assert(said.includes("row 1 of 2"), said);

    netDue.set(id, Number.MAX_SAFE_INTEGER);
    hostDue.clear();
  });

  // A response was decoded to text and the text was stored, so a feed answering anything but JSON built a sheet of
  // one column holding the whole file: shapeOf saw no shape, pageRows refused every page, and a query over it read
  // one string. What a body means is what its own content-type says it means. A CSV and a TSV go through the
  // importer's parser, an NDJSON is one JSON value a line, and a gzip is decompressed and read by what comes out --
  // each stored as the JSON array it means, so a feed in any of them is the sheet a JSON feed is.
  await t.step("A feed's body is parsed by the type it declares", async () => {
    const { jwt } = await usr("orla@example.com");
    const feed = async (url: string, extra: Record<string, unknown> = {}) => {
      const hand = automerge.create<Sheet>({ type: "net-http", data: [{ url, interval: 3600, ...extra }] });
      const id = `net-http:${hand.documentId}`;
      await put(jwt, `/library/${id}`, {});
      return id;
    };
    const ids = {
      csv: await feed("https://csv.body.test/feed"),
      tsv: await feed("https://tsv.body.test/feed"),
      ndjson: await feed("https://ndjson.body.test/feed"),
      zipped: await feed("https://zipped.body.test/feed"),
      zippedCsv: await feed("https://zippedcsv.body.test/feed"),
      bomb: await feed("https://bomb.body.test/feed"),
      inflated: await feed("https://inflated.body.test/feed"),
      ragged: await feed("https://ragged.body.test/feed"),
      plain: await feed("https://plain.body.test/feed"),
      rss: await feed("https://rss.body.test/feed"),
      atom: await feed("https://atom.body.test/feed"),
      xml: await feed("https://xml.body.test/feed"),
      badxml: await feed("https://badxml.body.test/feed"),
      zip: await feed("https://zip.body.test/feed"),
      zipTwo: await feed("https://ziptwo.body.test/feed"),
      zipNone: await feed("https://zipnone.body.test/feed"),
      zipBomb: await feed("https://zipbomb.body.test/feed"),
      nsAtom: await feed("https://nsatom.body.test/feed"),
      htmlRss: await feed("https://htmlrss.body.test/feed"),
      emptyXml: await feed("https://emptyxml.body.test/feed"),
      latin1: await feed("https://latin1.body.test/feed"),
      utf16: await feed("https://utf16.body.test/feed"),
      csvLatin1: await feed("https://csvlatin1.body.test/feed"),
      csvBare: await feed("https://csvbare.body.test/feed"),
      csvUnknown: await feed("https://csvunknown.body.test/feed"),
      csvWrong: await feed("https://csvwrong.body.test/feed"),
      ndjsonLatin1: await feed("https://ndjsonlatin1.body.test/feed"),
      itemText: await feed("https://itemtext.body.test/feed"),
      zipJson: await feed("https://zipjson.body.test/feed"),
      zipCrc: await feed("https://zipcrc.body.test/feed"),
      zipComment: await feed("https://zipcomment.body.test/feed"),
      zipForge: await feed("https://zipforge.body.test/feed"),
      html: await feed("https://html.body.test/feed"),
      htmlTwo: await feed("https://htmltwo.body.test/feed"),
      htmlNone: await feed("https://htmlnone.body.test/feed"),
      htmlRagged: await feed("https://htmlragged.body.test/feed"),
      htmlLatin1: await feed("https://htmllatin1.body.test/feed"),
      htmlHeader: await feed("https://htmlheader.body.test/feed"),
      rowsJson: await feed("https://rowsjson.body.test/feed", { rows_path: "data" }),
      rowsOne: await feed("https://rowsone.body.test/feed", { rows_path: "data.row" }),
      rowsMiss: await feed("https://rowsmiss.body.test/feed", { rows_path: "data" }),
      rowsScalar: await feed("https://rowsscalar.body.test/feed", { rows_path: "data" }),
      rowsArray: await feed("https://rowsarray.body.test/feed", { rows_path: "data" }),
      htmlFoot: await feed("https://htmlfoot.body.test/feed"),
      htmlBreak: await feed("https://htmlbreak.body.test/feed"),
      htmlHidden: await feed("https://htmlhidden.body.test/feed"),
      parquet: await feed("https://parquet.body.test/feed"),
      parquetBig: await feed("https://parquetbig.body.test/feed"),
      parquetNot: await feed("https://parquetnot.body.test/feed"),
      parquetCodec: await feed("https://parquetcodec.body.test/feed"),
      zipParquet: await feed("https://zipparquet.body.test/feed"),
      gzipParquet: await feed("https://gzipparquet.body.test/feed"),
      gzipPar1Csv: await feed("https://gzippar1csv.body.test/feed"),
    };
    const gzip = async (body: string | Uint8Array<ArrayBuffer>) =>
      new Uint8Array(
        await new Response(new Blob([body]).stream().pipeThrough(new CompressionStream("gzip"))).arrayBuffer(),
      );
    const zippedJson = await gzip(`[{"n":1}]`);
    const zippedCsv = await gzip("a,b\n1,2\n");
    // Four times the cap out of a few hundred bytes. The refusal must name what
    // it had read when it stopped, which is how a bomb is refused before the
    // whole of it is in memory.
    const bomb = await gzip("x".repeat(BODY_CAP * 4));
    // A CSV names its columns once and the JSON it means names them on every
    // row, so a few kilobytes on the wire is over a megabyte stored. Long names
    // and short values is the whole of it -- no compression involved.
    const wide = [1, 2, 3].map((n) => `${"c".repeat(300)}${n}`).join(",");
    const inflating = `${wide}\n${"1,2,3\n".repeat(1300)}`;
    // A real archive, deflated and with a correct CRC, rather than bytes shaped
    // like one: the reader under test is the only thing that should be able to
    // be wrong about a zip.
    const crcTable = new Uint32Array(256);
    for (let i = 0; i < 256; i++) {
      let c = i;
      for (let k = 0; k < 8; k++) c = c & 1 ? 0xEDB88320 ^ (c >>> 1) : c >>> 1;
      crcTable[i] = c >>> 0;
    }
    const crc32 = (bytes: Uint8Array) => {
      let c = 0xFFFFFFFF;
      for (const byte of bytes) c = crcTable[(c ^ byte) & 0xFF] ^ (c >>> 8);
      return (c ^ 0xFFFFFFFF) >>> 0;
    };
    const zipOf = async (
      members: Record<string, string | Uint8Array<ArrayBuffer>>,
    ): Promise<Uint8Array<ArrayBuffer>> => {
      const parts: Uint8Array[] = [];
      const dir: Uint8Array[] = [];
      let at = 0;
      for (const [name, text] of Object.entries(members)) {
        const raw = typeof text === "string" ? new TextEncoder().encode(text) : text;
        const packed = new Uint8Array(
          await new Response(new Blob([raw]).stream().pipeThrough(new CompressionStream("deflate-raw"))).arrayBuffer(),
        );
        const named = new TextEncoder().encode(name);
        const local = new Uint8Array(30 + named.length);
        const lv = new DataView(local.buffer);
        lv.setUint32(0, 0x04034b50, true), lv.setUint16(4, 20, true), lv.setUint16(8, 8, true);
        lv.setUint32(14, crc32(raw), true), lv.setUint32(18, packed.length, true), lv.setUint32(22, raw.length, true);
        lv.setUint16(26, named.length, true), local.set(named, 30);
        const entry = new Uint8Array(46 + named.length);
        const ev = new DataView(entry.buffer);
        ev.setUint32(0, 0x02014b50, true), ev.setUint16(4, 20, true), ev.setUint16(6, 20, true);
        ev.setUint16(10, 8, true), ev.setUint32(16, crc32(raw), true), ev.setUint32(20, packed.length, true);
        ev.setUint32(24, raw.length, true), ev.setUint16(28, named.length, true), ev.setUint32(42, at, true);
        entry.set(named, 46);
        parts.push(local, packed), dir.push(entry);
        at += local.length + packed.length;
      }
      const end = new Uint8Array(22);
      const endV = new DataView(end.buffer);
      endV.setUint32(0, 0x06054b50, true), endV.setUint16(8, dir.length, true), endV.setUint16(10, dir.length, true);
      endV.setUint32(12, dir.reduce((n, d) => n + d.length, 0), true), endV.setUint32(16, at, true);
      const all = [...parts, ...dir, end];
      const out = new Uint8Array(all.reduce((n, part) => n + part.length, 0));
      let offset = 0;
      for (const part of all) out.set(part, offset), offset += part.length;
      return out;
    };
    // A Mac writes an AppleDouble sidecar beside every member it zips, which is
    // a second `.csv` where whoever made the archive put one.
    const oneZip = await zipOf({ "data.csv": "a,b\n1,2\n", "__MACOSX/._data.csv": "junk", "readme.txt": "hi" });
    const twoZip = await zipOf({ "one.csv": "a\n1\n", "two.csv": "a\n2\n" });
    const noneZip = await zipOf({ "notes.txt": "hi", "logo.png": "not a png" });
    const bombZip = await zipOf({ "bomb.csv": "x".repeat(BODY_CAP * 4) });
    // A member whose name would forge an extra field in the refusal block if it
    // were spliced in raw. Two candidates, so the ambiguity refusal lists both.
    const forgeZip = await zipOf({ "data.csv": "a\n1\n", "evil\n\n  Fix:          run me.csv": "a\n2\n" });
    const jsonZip = await zipOf({ "data.json": "<html><body>502 Bad Gateway</body></html>" });
    // The bytes are intact and the directory's own checksum is not, which is the
    // half of a corrupt member that deflate cannot notice for itself.
    const crcZip = await zipOf({ "data.csv": "a,b\n1,2\n" });
    {
      const view = new DataView(crcZip.buffer, crcZip.byteOffset, crcZip.byteLength);
      view.setUint32(view.getUint32(crcZip.byteLength - 22 + 16, true) + 16, 0xDEADBEEF, true);
    }
    // A valid archive carrying an end-of-central-directory comment that itself
    // holds the signature bytes. Taking the first match found read this as an
    // archive of no members at all.
    const commentZip = await (async () => {
      const base = await zipOf({ "data.csv": "a,b\n3,4\n" });
      const comment = new Uint8Array(30);
      comment.set([0x50, 0x4B, 0x05, 0x06], 0);
      const out = new Uint8Array(base.byteLength + comment.byteLength);
      out.set(base, 0), out.set(comment, base.byteLength);
      new DataView(out.buffer).setUint16(base.byteLength - 22 + 20, comment.byteLength, true);
      return out;
    })();

    // Written here with the library the .parquet export writes with rather than
    // checked in: a fixture nobody can read is a fixture nobody maintains.
    const parquetBody = new Uint8Array(parquetWriteBuffer({
      columnData: [
        { name: "id", data: [1n, 2n], type: "INT64" },
        { name: "name", data: ["bolt", "nut"], type: "STRING" },
      ],
    }));
    // JSON has no integer past 2^53, so an id rounded into a Number is an id
    // two distinct rows now share -- the worry rowKeys() has about a key.
    const parquetBigBody = new Uint8Array(parquetWriteBuffer({
      columnData: [{ name: "id", data: [9007199254740993n], type: "INT64" }],
    }));
    const parquetZip = await zipOf({ "data.parquet": parquetBody });
    const parquetGzip = await gzip(parquetBody);
    // Magic at the front only: a CSV whose first header opens with it.
    const par1CsvGzip = await gzip("PAR1,b\n1,2\n");
    // A file that declares GZIP and holds bytes nobody compressed: the reader
    // refuses on the codec before it looks at a page, which is the only codec
    // this server has no decompressor for that a test can build without one.
    const parquetGzipBody = new Uint8Array(parquetWriteBuffer({
      columnData: [{ name: "id", data: [1n], type: "INT64" }],
      codec: "GZIP",
      compressors: { GZIP: (input: Uint8Array) => input },
    }));

    // Latin-1 is one byte per code point, so the text is its own encoder. 0xE9
    // is é there and is not valid UTF-8 at all, so the same bytes are a cell, a
    // replacement character or a refusal depending only on what the answer said
    // about them.
    const latin1 = (text: string) => Uint8Array.from(text, (ch) => ch.charCodeAt(0));
    const latin1Csv = latin1("t\ncaf\u00E9\n");

    const typed = (body: string | Uint8Array<ArrayBuffer>, type: string) =>
      new Response(body, { headers: { "content-type": type } });
    const fetcher = (url: string) => {
      if (!url.includes(".body.test")) return Promise.resolve(new Response(`{"ok":true}`));
      switch (new URL(url).hostname) {
        case "csv.body.test":
          // The importer's own typing: digits are a number, a blank is a null
          // and not a zero, and true/false is a bool.
          return Promise.resolve(typed("id,qty,ok\n1,10,true\n2,,false\n", "text/csv; charset=utf-8"));
        case "tsv.body.test":
          return Promise.resolve(typed("a\tb\n1\t2\n", "text/tab-separated-values"));
        case "ndjson.body.test":
          return Promise.resolve(typed(`{"n":1}\n\n{"n":2}\n`, "application/x-ndjson"));
        case "zipped.body.test":
          return Promise.resolve(typed(zippedJson, "application/gzip"));
        case "zippedcsv.body.test":
          return Promise.resolve(typed(zippedCsv, "application/x-gzip"));
        case "bomb.body.test":
          return Promise.resolve(typed(bomb, "application/gzip"));
        case "inflated.body.test":
          return Promise.resolve(typed(inflating, "text/csv"));
        case "ragged.body.test":
          return Promise.resolve(typed("a,b\n1\n", "text/csv"));
        case "rss.body.test":
          // RSS 2.0: the items sit under <channel>.
          return Promise.resolve(typed(
            `<?xml version="1.0"?><rss version="2.0"><channel><title>Feed</title>` +
              `<item><title>A</title><link>https://a.test</link></item>` +
              `<item><title>B</title><link>https://b.test</link></item></channel></rss>`,
            "application/rss+xml",
          ));
        case "atom.body.test":
          // One entry, and its url is an attribute rather than a text node.
          return Promise.resolve(typed(
            `<feed xmlns="http://www.w3.org/2005/Atom"><title>Feed</title>` +
              `<entry><title>A</title><link href="https://a.test" rel="alternate"/></entry></feed>`,
            "application/atom+xml",
          ));
        case "xml.body.test":
          return Promise.resolve(typed(
            `<data><meta><n>2</n></meta><row><id>007</id></row><row><id>8</id></row></data>`,
            "text/xml; charset=utf-8",
          ));
        case "badxml.body.test":
          return Promise.resolve(typed(`<a><b></a>`, "application/xml"));
        case "zip.body.test":
          return Promise.resolve(typed(oneZip, "application/zip"));
        case "ziptwo.body.test":
          return Promise.resolve(typed(twoZip, "application/zip"));
        case "zipnone.body.test":
          return Promise.resolve(typed(noneZip, "application/x-zip-compressed"));
        case "zipbomb.body.test":
          return Promise.resolve(typed(bombZip, "application/zip"));
        case "zipjson.body.test":
          return Promise.resolve(typed(jsonZip, "application/zip"));
        case "zipcrc.body.test":
          return Promise.resolve(typed(crcZip, "application/zip"));
        case "zipcomment.body.test":
          return Promise.resolve(typed(commentZip, "application/zip"));
        case "zipforge.body.test":
          return Promise.resolve(typed(forgeZip, "application/zip"));
        case "html.body.test":
          // A caption, a <thead>, an entity, and a stray table written inside a
          // <script> that must not count as one.
          return Promise.resolve(typed(
            `<html><body><h1>Stock</h1><script>var s = "<table><tr><td>fake</td></tr></table>";</script>` +
              `<table><caption>Stock</caption><thead><tr><th>name</th><th>qty</th></tr></thead>` +
              `<tbody><tr><td>nut &amp; bolt</td><td>4</td></tr><tr><td>washer</td><td></td></tr></tbody>` +
              `</table></body></html>`,
            "text/html; charset=utf-8",
          ));
        case "htmltwo.body.test":
          return Promise.resolve(typed(
            `<table id="nav"><tr><td>menu</td></tr></table><table><caption>Data</caption><tr><td>a</td></tr></table>`,
            "text/html",
          ));
        case "htmlnone.body.test":
          return Promise.resolve(typed(`<html><body><p>Nothing here.</p></body></html>`, "text/html"));
        case "htmlragged.body.test":
          return Promise.resolve(typed(
            `<table><tr><th>a</th><th>b</th></tr><tr><td>1</td><td>2</td></tr><tr><td>3</td></tr></table>`,
            "text/html",
          ));
        case "htmllatin1.body.test":
          return Promise.resolve(typed(
            Uint8Array.from([
              ...new TextEncoder().encode(`<html><head><meta charset="iso-8859-1"></head><body><table>`),
              ...new TextEncoder().encode(`<tr><th>t</th></tr><tr><td>caf`),
              0xE9,
              ...new TextEncoder().encode(`</td></tr></table></body></html>`),
            ]),
            "text/html",
          ));
        case "htmlheader.body.test":
          // The answer names the charset and the document does not, which is the
          // page that was refused on its first accented character.
          return Promise.resolve(typed(
            Uint8Array.from([
              ...new TextEncoder().encode(`<table><tr><th>t</th></tr><tr><td>na`),
              0xEF,
              ...new TextEncoder().encode(`ve</td></tr></table>`),
            ]),
            "text/html; charset=iso-8859-1",
          ));
        case "rowsjson.body.test":
          return Promise.resolve(typed(`{"meta":{"n":2},"data":[{"id":1},{"id":2}]}`, "application/json"));
        case "rowsone.body.test":
          // One <row>, which is the day a reader that let it collapse to an object
          // broke on. rows_path names the element, so isArray holds it to a list.
          return Promise.resolve(typed(
            `<data><meta><n>1</n></meta><row><id>7</id></row></data>`,
            "application/xml",
          ));
        case "rowsmiss.body.test":
          return Promise.resolve(typed(`{"meta":1}`, "application/json"));
        case "rowsscalar.body.test":
          return Promise.resolve(typed(`{"data":[10,20]}`, "application/json"));
        case "rowsarray.body.test":
          // An RSS feed under a sheet that also names a rows_path: readFeedBody
          // already answered the items, so there is no envelope left to name.
          return Promise.resolve(typed(
            `<rss><channel><item><t>A</t></item></channel></rss>`,
            "application/rss+xml",
          ));
        case "htmlfoot.body.test":
          // HTML 4.01 told authors to write <tfoot> before <tbody>, and plenty of
          // pages still do. Read in tree order the footer is the header.
          return Promise.resolve(typed(
            `<table><tfoot><tr><td>Total</td><td>100</td></tr></tfoot>` +
              `<thead><tr><th>name</th><th>amount</th></tr></thead>` +
              `<tbody><tr><td>Alice</td><td>50</td></tr><tr><td>Bob</td><td>50</td></tr></tbody></table>`,
            "text/html",
          ));
        case "htmlbreak.body.test":
          return Promise.resolve(typed(
            `<table><tr><th>n</th><th>note</th></tr><tr><td>10<br>20</td><td><p>a</p><p>b</p></td></tr></table>`,
            "text/html",
          ));
        case "htmlhidden.body.test":
          // A charset that only looks like a declaration, written before the real
          // one, with nothing in the answer's own header to outrank it.
          return Promise.resolve(typed(
            new TextEncoder().encode(
              `<script>var s = '<meta charset="windows-1252">';</script><meta charset="utf-8">` +
                `<table><tr><th>t</th></tr><tr><td>caf\u00E9</td></tr></table>`,
            ),
            "text/html",
          ));
        case "parquet.body.test":
          return Promise.resolve(typed(parquetBody, "application/vnd.apache.parquet"));
        case "parquetbig.body.test":
          return Promise.resolve(typed(parquetBigBody, "application/x-parquet"));
        case "parquetnot.body.test":
          return Promise.resolve(typed(`<html><body>502 Bad Gateway</body></html>`, "application/parquet"));
        case "parquetcodec.body.test":
          return Promise.resolve(typed(parquetGzipBody, "application/vnd.apache.parquet"));
        case "zipparquet.body.test":
          return Promise.resolve(typed(parquetZip, "application/zip"));
        case "gzipparquet.body.test":
          return Promise.resolve(typed(parquetGzip, "application/gzip"));
        case "gzippar1csv.body.test":
          return Promise.resolve(typed(par1CsvGzip, "application/gzip"));
        case "nsatom.body.test":
          // Prefixed Atom, which is most of the Atom on the internet.
          return Promise.resolve(typed(
            `<a:feed xmlns:a="http://www.w3.org/2005/Atom"><a:title>Feed</a:title>` +
              `<a:entry><a:title>A</a:title></a:entry></a:feed>`,
            "application/atom+xml",
          ));
        case "htmlrss.body.test":
          // What a provider behind a 200-ing proxy answers. It is well-formed
          // XML, so only its root says it is not a feed.
          return Promise.resolve(typed(
            `<html><head><title>502 Bad Gateway</title></head><body><h1>502</h1></body></html>`,
            "application/rss+xml",
          ));
        case "emptyxml.body.test":
          return Promise.resolve(typed("", "application/xml"));
        case "itemtext.body.test":
          return Promise.resolve(typed(
            `<rss><channel><item>hello</item></channel></rss>`,
            "application/rss+xml",
          ));
        case "latin1.body.test":
          // 0xE9 is é in Latin-1 and is not valid UTF-8 at all.
          return Promise.resolve(typed(
            Uint8Array.from([
              ...new TextEncoder().encode(`<?xml version="1.0" encoding="ISO-8859-1"?><rss><channel><item><t>caf`),
              0xE9,
              ...new TextEncoder().encode(`</t></item></channel></rss>`),
            ]),
            "application/rss+xml",
          ));
        case "csvlatin1.body.test":
          // A Latin-1 export, and the answer is the only thing that says so: a
          // CSV carries no declaration of its own the way a prolog does.
          return Promise.resolve(typed(latin1Csv, "text/csv; charset=iso-8859-1"));
        case "csvbare.body.test":
          // The same bytes with nothing declared, which is every feed running
          // today: the decode stays the non-fatal UTF-8 one it always was.
          return Promise.resolve(typed(latin1Csv, "text/csv"));
        case "csvunknown.body.test":
          return Promise.resolve(typed("t\nplain\n", "text/csv; charset=x-nope"));
        case "csvwrong.body.test":
          return Promise.resolve(typed(latin1Csv, "text/csv; charset=utf-8"));
        case "ndjsonlatin1.body.test":
          return Promise.resolve(typed(latin1(`{"t":"caf\u00E9"}\n`), "application/x-ndjson; charset=iso-8859-1"));
        case "utf16.body.test": {
          const text = `<?xml version="1.0" encoding="UTF-16"?><rss><channel><item><t>wide</t></item></channel></rss>`;
          const wide = new Uint8Array(2 + text.length * 2);
          new DataView(wide.buffer).setUint16(0, 0xFEFF, true);
          for (let i = 0; i < text.length; i++)
            new DataView(wide.buffer).setUint16(2 + i * 2, text.charCodeAt(i), true);
          return Promise.resolve(typed(wide, "application/rss+xml"));
        }
        default:
          // A type nothing on the list names is the text it arrived as, which
          // is what every feed answered before any of them were parsed.
          return Promise.resolve(typed(`{"n":1}`, "application/json"));
      }
    };

    const at = Date.now() + 96_000_000;
    const wanted = Object.values(ids);
    // One cycle per sheet plus slack, and not a flat ten: pollNetOnce stops
    // starting sheets once a cycle has spent POLL_CYCLE_MS, so how many of these
    // it gets through per cycle is a function of how loaded the machine is. A
    // flat ten fitted nine feeds and became a flake at seventeen, under
    // `deno task test` alone -- eight files in parallel is when that budget
    // bites. The refusal names the sheets still unpolled, because "a body sheet"
    // named none of them and left nothing to look at.
    for (let cycle = 0;; cycle++) {
      const polled: { sheet_id: string }[] = await sql`
        select distinct sheet_id from net where sheet_id = any(${wanted})
      `;
      if (polled.length === wanted.length) break;
      if (cycle >= wanted.length + 10) {
        const missing = wanted.filter((id) => !polled.some((row) => row.sheet_id === id));
        throw new Error(
          `${cycle} cycles at ${at} and ${missing.length} of ${wanted.length} body sheets had not been polled: ${
            missing.join(", ")
          }`,
        );
      }
      await pollNetOnce(fetcher, at);
    }
    const newest = async (id: string) => {
      const [row]: { body: string; meta: Record<string, unknown> }[] = await sql`
        select body, meta from net where sheet_id = ${id} order by net_id desc limit 1
      `;
      return row;
    };
    const bodyOf = async (id: string) => JSON.parse((await newest(id)).body);
    const errorOf = async (id: string) => String((await bodyOf(id)).error ?? "");

    // A CSV lands as the rows it holds, typed the way the importer types them.
    assertEquals(await bodyOf(ids.csv), [{ id: 1, qty: 10, ok: true }, { id: 2, qty: null, ok: false }]);
    // And every reader downstream sees what a JSON feed hands them: the shape
    // is the columns the file answered with, not one string.
    assertEquals((await newest(ids.csv)).meta.shape, { id: "number", ok: "boolean", qty: "number" });
    assertEquals(await bodyOf(ids.tsv), [{ a: 1, b: 2 }], "a tab is the only thing a TSV disagrees about");
    // One JSON value a line, and a blank line is no record.
    assertEquals(await bodyOf(ids.ndjson), [{ n: 1 }, { n: 2 }]);
    // A gzip says only that it is compressed, so what came out is read by its
    // first character: a bracket is JSON and everything else is a CSV.
    assertEquals(await bodyOf(ids.zipped), [{ n: 1 }]);
    assertEquals(await bodyOf(ids.zippedCsv), [{ a: 1, b: 2 }]);
    assertEquals(await bodyOf(ids.plain), { n: 1 }, "a type nothing parses is stored as it arrived");

    // RSS and Atom are the two formats that name the element a row sits in, so
    // their rows need no guess: every <item> and every <entry> in the document,
    // wherever the version of the format puts it.
    assertEquals(await bodyOf(ids.rss), [
      { title: "A", link: "https://a.test" },
      { title: "B", link: "https://b.test" },
    ]);
    assertEquals((await newest(ids.rss)).meta.shape, { link: "string", title: "string" });
    // One entry is still a list of one -- a reader that collapsed it to an
    // object would answer a different shape on the feed's quiet day -- and an
    // attribute is kept, because half of Atom's payload lives in them.
    assertEquals(await bodyOf(ids.atom), [{
      title: "A",
      link: { "@href": "https://a.test", "@rel": "alternate" },
    }]);
    // Generic XML names no row element, so what it means is the whole document
    // and rows_path is how a sheet says where the rows sit -- the same field a
    // JSON feed answering an envelope already uses. Text stays text: an id of
    // `007` read as a number is 7, which is a different id.
    assertEquals(await bodyOf(ids.xml), { data: { meta: { n: "2" }, row: [{ id: "007" }, { id: "8" }] } });
    // Half-read XML is a failure row naming where it stopped being XML. This
    // reader takes `<a><b></a>` as `{a:{b:""}}` without raising, so the
    // validator runs first or the sheet stores a document nobody wrote.
    const broken = await errorOf(ids.badxml);
    assert(broken.includes("is not XML"), broken);
    assert(broken.includes("line 1"), broken);

    // An archive is a container, and the file inside it is the body: one member
    // this reader can name, read as the type its name declares. The Mac sidecar
    // beside it is not a second candidate.
    assertEquals(await bodyOf(ids.zip), [{ a: 1, b: 2 }]);
    // Two is a guess about which one is the rows, refused naming both, for the
    // reason pageRows refuses a page holding two arrays.
    const two = await errorOf(ids.zipTwo);
    assert(two.includes("holds 2 files this reader can take"), two);
    assert(two.includes("one.csv") && two.includes("two.csv"), two);
    // And an archive holding nothing it can name says what it found instead.
    const none = await errorOf(ids.zipNone);
    assert(none.includes("holds nothing this reader can take"), none);
    assert(none.includes("notes.txt"), none);
    // A deflate bomb is bounded by the slice-fed reader a gzip bomb is, which
    // is the whole reason both come through the one function.
    const zbomb = await errorOf(ids.zipBomb);
    assert(zbomb.includes("decompresses to more than can be stored"), zbomb);
    // A member name is up to 64k of sender text that may hold newlines, so it is
    // quoted rather than spliced: raw, it forged a `Fix:` line of its own in the
    // refusal block that a reader could not tell from the real one.
    const forged = await errorOf(ids.zipForge);
    assert(forged.includes("2 files this reader can take"), forged);
    assert(!/\n\s*Fix:\s+run me/.test(forged), `a member name must not forge a field: ${forged}`);
    assert(forged.includes("run me.csv"), `and must still be named: ${forged}`);
    // The member's name says JSON, and this server derived that type itself off a
    // filename, so it is a claim to check: unchecked, an HTML error page inside
    // `data.json` was stored verbatim under a green run row.
    const zjson = await errorOf(ids.zipJson);
    assert(zjson.includes("is not JSON"), zjson);
    assert(zjson.includes("data.json in https://zipjson.body.test/feed"), `the member is the source: ${zjson}`);
    // Raw deflate carries no checksum and a stored member carries nothing at all,
    // so the directory's CRC is the only thing that says a member arrived whole.
    const zcrc = await errorOf(ids.zipCrc);
    assert(zcrc.includes("does not match its own checksum"), zcrc);
    // The signature bytes inside a comment are not a record. Taking the first
    // match found read a one-member archive as an archive of none, and told the
    // owner to zip the file they had already zipped.
    assertEquals(await bodyOf(ids.zipComment), [{ a: 3, b: 4 }]);

    // A prefixed feed is a feed. Matching the literal element name answered `[]`
    // under a green run row, which is a sheet that is empty forever.
    assertEquals(await bodyOf(ids.nsAtom), [{ title: "A" }]);
    // And a well-formed document that is not a feed at all is refused by its
    // root, rather than parsed, found to hold no <item>, and stored as `[]` --
    // which had no shape, so no shape_change, so nothing ever graded it failed.
    const notFeed = await errorOf(ids.htmlRss);
    assert(notFeed.includes("is not an RSS feed"), notFeed);
    assert(notFeed.includes("<html>"), `it names the root it found: ${notFeed}`);
    // An <item> holding text rather than fields is not a row. Stored as one it
    // made shapeOf answer null for the whole run.
    const bare = await errorOf(ids.itemText);
    assert(bare.includes("holds no named fields"), bare);
    // An empty body answers a line and no column, and `column undefined` reads as
    // a place in the document that is not one.
    const blank = await errorOf(ids.emptyXml);
    assert(blank.includes("is not XML") && blank.includes("line 1"), blank);
    assert(!blank.includes("undefined"), `a missing column is left unnamed: ${blank}`);
    // An XML body declares its own encoding, and decoding it as UTF-8 anyway
    // stored every accented character as U+FFFD under a green run row.
    assertEquals(await bodyOf(ids.latin1), [{ t: "caf\u00E9" }]);
    // A sixteen-bit document cannot be sniffed for its prolog as UTF-8, so the
    // byte-order mark is what says how wide it is. This was refused forever.
    assertEquals(await bodyOf(ids.utf16), [{ t: "wide" }]);
    // A CSV states no encoding of its own, so the answer's charset is the whole
    // of what says these bytes are Latin-1 -- and read as UTF-8 anyway the cell
    // held U+FFFD, which nothing downstream can tell from what the feed sent.
    assertEquals(await bodyOf(ids.csvLatin1), [{ t: "caf\u00E9" }]);
    assertEquals(await bodyOf(ids.ndjsonLatin1), [{ t: "caf\u00E9" }]);
    // And a body that declares nothing is decoded exactly as it was before there
    // was a label to read, replacement character and all: no feed running today
    // reads differently.
    assertEquals(await bodyOf(ids.csvBare), [{ t: "caf\uFFFD" }]);
    // A label this runtime has no decoder for is its own refusal, naming it.
    const unknown = await errorOf(ids.csvUnknown);
    assert(unknown.includes("declares an encoding this server cannot read"), unknown);
    assert(unknown.includes("x-nope"), `it names the label: ${unknown}`);
    // A declared label the bytes are not is the other one: a feed wrong about
    // its own encoding is a failure row, never a cell nobody can read back.
    const wrongLabel = await errorOf(ids.csvWrong);
    assert(wrongLabel.includes("is not utf-8 text"), wrongLabel);

    // An HTML table is a header and a grid, so it goes through the reader every
    // delimited body takes: the digits are numbers and the blank is a null, the
    // way a CSV feed's are. The <table> written inside a <script> is not a table.
    assertEquals(await bodyOf(ids.html), [{ name: "nut & bolt", qty: 4 }, { name: "washer", qty: null }]);
    assertEquals((await newest(ids.html)).meta.shape, { name: "string", qty: "number" });
    // Two is a guess about which one holds the rows, refused naming both -- the
    // rule a zip's members already take.
    const twoTables = await errorOf(ids.htmlTwo);
    assert(twoTables.includes("holds 2 tables"), twoTables);
    assert(twoTables.includes(`"nav"`) && twoTables.includes(`"Data"`), `each is named: ${twoTables}`);
    const noTable = await errorOf(ids.htmlNone);
    assert(noTable.includes("holds no table"), noTable);
    // A row that does not match its header is refused by number, in the same
    // words a ragged CSV is, because it is the same reader.
    const ragged2 = await errorOf(ids.htmlRagged);
    assert(ragged2.includes("Line 3") && ragged2.includes("does not match its header"), ragged2);
    // HTML declares its encoding too, in a <meta> rather than a prolog.
    assertEquals(await bodyOf(ids.htmlLatin1), [{ t: "caf\u00E9" }]);
    // And the answer's own charset outranks the document's, which is the order the
    // web reads them in and what lets a page carrying no <meta> be read at all.
    assertEquals(await bodyOf(ids.htmlHeader), [{ t: "na\u00EFve" }]);

    // `rows_path` is read on the feed that answers in one request, not only on a
    // paged one: set on a sheet with no page_by it did nothing at all, and the
    // envelope was stored whole under a green run.
    assertEquals(await bodyOf(ids.rowsJson), [{ id: 1 }, { id: 2 }]);
    // And the other half of the same fact: a generic XML feed names no row element
    // of its own, so rows_path is what holds one `<row>` to a list. Without it
    // this is an object on the day a feed answers once and an array every other
    // day, and shapeOf reports `{data: "object"}` either way.
    assertEquals(await bodyOf(ids.rowsOne), [{ id: "7" }]);
    // An answer that holds something else where the sheet said its rows are is
    // this poll's failure row, never a guess at the envelope.
    const missed = await errorOf(ids.rowsMiss);
    assert(missed.includes("holds no rows where rows_path says they sit"), missed);
    assert(missed.includes("data holds nothing"), `it names what sits there: ${missed}`);
    // A row keyed by nothing is not a row -- the guard xmlRows puts on an <item>,
    // for the same reason: stored, it makes shapeOf answer null for the whole run,
    // so there is never a shape to change and POLL_OK grades it healthy forever.
    const scalars = await errorOf(ids.rowsScalar);
    assert(scalars.includes("holds no named fields"), scalars);
    assert(scalars.includes("row 1 of 2"), `it names which row: ${scalars}`);
    // And an answer that is already the rows has no envelope to name. Reading the
    // setting as satisfied would hide a sheet that is wrong about its own feed.
    const already = await errorOf(ids.rowsArray);
    assert(already.includes("already answers an array of rows"), already);
    assert(already.includes("take rows_path out"), `the fix is the one to make: ${already}`);

    // A <tfoot> written before <tbody> is an HTML 4.01 idiom, and reading rows in
    // tree order made that footer the header and the header a row.
    assertEquals(await bodyOf(ids.htmlFoot), [
      { name: "Alice", amount: 50 },
      { name: "Bob", amount: 50 },
      { name: "Total", amount: 100 },
    ]);
    // A <br> and a block element are a break with no width of their own, so
    // `10<br>20` read as the single number 1020 -- in neither cell.
    assertEquals(await bodyOf(ids.htmlBreak), [{ n: "10 20", note: "a b" }]);
    // A <meta charset> inside a script string is not a declaration, and this scan
    // runs before there is a parser that could know it.
    assertEquals(await bodyOf(ids.htmlHidden), [{ t: "caf\u00E9" }]);

    // Parquet is binary, so it never takes the text decode: the rows come out
    // keyed by column name, which is what every reader downstream keys on.
    assertEquals(await bodyOf(ids.parquet), [{ id: 1, name: "bolt" }, { id: 2, name: "nut" }]);
    assertEquals((await newest(ids.parquet)).meta.shape, { id: "number", name: "string" });
    // A gzip names nothing of what it holds, and parquet says so itself at both
    // ends; one end alone is a CSV that happens to open with the magic.
    assertEquals(await bodyOf(ids.gzipParquet), [{ id: 1, name: "bolt" }, { id: 2, name: "nut" }]);
    assertEquals(await bodyOf(ids.gzipPar1Csv), [{ PAR1: 1, b: 2 }]);
    // An INT64 arrives as a BigInt, which JSON.stringify throws on. Past 2^53 it
    // is its decimal text, never a Number that rounded two ids into one.
    assertEquals(await bodyOf(ids.parquetBig), [{ id: "9007199254740993" }]);
    // A body that declares parquet and is not one is refused by its magic, the
    // way a feed that is not the feed it says it is is refused by its root.
    const notParquet = await errorOf(ids.parquetNot);
    assert(notParquet.includes("is not a parquet file"), notParquet);
    assert(notParquet.includes("PAR1"), `it names what was expected: ${notParquet}`);
    // A codec this reader has no decompressor for is a failure row quoting the
    // codec, not a 500: gzip, zstd and brotli want a second dependency.
    const codec = await errorOf(ids.parquetCodec);
    assert(codec.includes("uses a codec this server cannot read"), codec);
    assert(codec.includes("GZIP"), `it names the codec: ${codec}`);
    // And a zip is a container whichever body is inside it.
    assertEquals(await bodyOf(ids.zipParquet), [{ id: 1, name: "bolt" }, { id: 2, name: "nut" }]);

    // A bomb is the 413 an oversized body is, refused on what came out of the
    // decompressor rather than on the few hundred bytes that carried it -- and
    // refused holding a fraction of it.
    const burst = await errorOf(ids.bomb);
    assert(burst.includes("decompresses to more than can be stored"), burst);
    const held = Number(burst.match(/at least (\d+)/)?.[1]);
    assert(held > BODY_CAP, `the refusal must name what it had read: ${burst}`);
    assert(held < BODY_CAP * 4, `and must stop before the whole body is in memory: ${burst}`);

    // The cap is spent on what a body means, not on what it arrived as. The
    // wire check passes a file this size and the rows it means are over the
    // cap, which is the same hole a gzip page's wire bytes were.
    const fat = await errorOf(ids.inflated);
    assert(fat.includes("means more rows than can be stored"), fat);
    assert(fat.includes(`${inflating.length} bytes holding`), `it must name both numbers: ${fat}`);

    // A body its own declared type cannot parse is a failure row naming the
    // line, never a blob stored under a green run.
    const ragged = await errorOf(ids.ragged);
    assert(ragged.includes("Line 2"), ragged);
    assert(ragged.includes("does not match its header"), ragged);
    assert(ragged.includes("https://ragged.body.test/feed"), `the refusal names where the line is: ${ragged}`);

    // The same reader on the other door: a sender that posts a CSV lands the
    // rows a feed answering one lands, and the signature still covers the bytes
    // as they were sent.
    const hookHand = automerge.create<Sheet>({ type: "net-hook", data: [] });
    const hookId = `net-hook:${hookHand.documentId}`;
    await put(jwt, `/library/${hookId}`, {});
    const sent = "name,qty\nbolt,4\n";
    const posted = await app.request(`/net/${hookId}`, {
      method: "POST",
      headers: new Headers({
        "Content-Type": "text/csv",
        "scrapsheets-signature": await hookSign(await hookSecret(hookId), `/net/${hookId}`, sent),
      }),
      body: sent,
    });
    assert(posted.ok, `a CSV delivery: ${posted.status} ${await posted.text()}`);
    assertEquals(JSON.parse((await newest(hookId)).body), [{ name: "bolt", qty: 4 }]);

    for (const id of Object.values(ids)) netDue.set(id, Number.MAX_SAFE_INTEGER);
    hostDue.clear();
  });

  // The cross-page cap summed the wire bytes of each page, which for a gzip
  // page is the compressed size and not what lands in `rows`. Pages each
  // comfortably under BODY_CAP once decompressed, and each passing readFeedBody's
  // own per-page check, summed past it with a compressed total of a few
  // kilobytes: three of them stored twice the cap under a green run row.
  await t.step("A paged gzip feed is capped by what its pages decompress to, not by the wire", async () => {
    const { jwt } = await usr("ines@example.com");
    const hand = automerge.create<Sheet>({
      type: "net-http",
      data: [
        {
          url: "https://gzippages.body.test/feed",
          interval: 3600,
          page_by: "page",
          page_param: "page",
        } as NetHttp,
      ],
    });
    const id = `net-http:${hand.documentId}`;
    await put(jwt, `/library/${id}`, {});

    const gzip = async (text: string) =>
      new Uint8Array(
        await new Response(new Blob([text]).stream().pipeThrough(new CompressionStream("gzip"))).arrayBuffer(),
      );
    const pad = (n: number) => `[{"pad":"${"x".repeat(n)}"}]`;
    // Each page decompresses to 70% of BODY_CAP -- comfortably under it alone
    // -- but two of them together are 40% over.
    const pageSize = Math.floor(BODY_CAP * 0.7);
    const pageGz = await gzip(pad(pageSize - 12));
    const typed = (body: Uint8Array<ArrayBuffer>, type: string) =>
      new Response(body, { headers: { "content-type": type } });
    const fetcher = (url: string) => {
      if (!url.includes(".body.test")) return Promise.resolve(new Response(`{"ok":true}`));
      const page = new URL(url).searchParams.get("page");
      // Three pages of it, then an empty one to stop -- the empty page's
      // content-type does not matter, since a header-only CSV and a bare `[]`
      // both answer zero rows.
      return Promise.resolve(Number(page) <= 3 ? typed(pageGz, "application/gzip") : new Response("[]"));
    };

    const at = Date.now() + 99_000_000;
    await pollFeed(id, fetcher, at);
    const [{ body, meta }]: { body: string; meta: Record<string, unknown> }[] = await sql`
      select body, meta from net where sheet_id = ${id} order by net_id desc limit 1
    `;
    // Three pages at 70% of BODY_CAP decompressed is 210% of it: this must be
    // the same "too large to store" refusal a plain JSON feed gets for the
    // same reason, not a stored body several times over the cap.
    assert(
      body.length < BODY_CAP,
      `three gzip pages each under BODY_CAP decompressed, summing well over it, must be refused rather than stored at ${body.length} bytes: ${
        JSON.stringify(meta).slice(0, 300)
      }`,
    );
    const parsed = JSON.parse(body);
    assert(
      typeof parsed === "object" && parsed !== null && "error" in parsed,
      `and the row must be the failure it is, not a payload of rows: ${body.slice(0, 200)}`,
    );
    assert(String(parsed.error).includes("too large to store"), String(parsed.error));

    netDue.set(id, Number.MAX_SAFE_INTEGER);
    hostDue.clear();
  });

  // An unlisted content type is the branch of readFeedBody that returns what
  // arrived without reading it -- which is where a JSON feed's answer lands --
  // so a byte Postgres cannot hold reached the insert and came back as a
  // driver's words under a status-0 row, the unexplained failure this refusal
  // exists to replace. The delivery door's half is asserted where the signature
  // schemes are, and asserting it a second time here would say nothing new.
  await t.step("A NUL byte in a polled body is a named refusal", async () => {
    const { jwt } = await usr("nerea@example.com");
    const feed = async (url: string) => {
      const hand = automerge.create<Sheet>({ type: "net-http", data: [{ url, interval: 3600 }] });
      const id = `net-http:${hand.documentId}`;
      await put(jwt, `/library/${id}`, {});
      return id;
    };
    const ids = {
      raw: await feed("https://nulraw.body.test/feed"),
      escaped: await feed("https://nulescaped.body.test/feed"),
      zipped: await feed("https://nulgzip.body.test/feed"),
    };
    // The byte itself, beside the six characters a JSON encoder writes it as --
    // which are text, and store like any other six.
    const nulBody = `{"d":"a\u0000b"}`;
    const escaped = String.raw`{"d":"a\u0000b"}`;
    const zipped = new Uint8Array(
      await new Response(
        new Blob(["a\nx\u0000y\n"]).stream().pipeThrough(new CompressionStream("gzip")),
      ).arrayBuffer(),
    );
    const fetcher = (url: string) =>
      Promise.resolve(
        url.startsWith("https://nulraw.")
          ? new Response(nulBody, { headers: { "content-type": "application/json" } })
          : url.startsWith("https://nulescaped.")
          ? new Response(escaped, { headers: { "content-type": "application/json" } })
          : new Response(zipped, { headers: { "content-type": "application/gzip" } }),
      );
    const at = Date.now() + 99_000_000;
    for (const id of Object.values(ids)) await pollFeed(id, fetcher, at);
    const newest = async (id: string) => {
      const [row]: { body: string; meta: Record<string, unknown> }[] = await sql`
        select body, meta from net where sheet_id = ${id} order by net_id desc limit 1
      `;
      return row;
    };

    const refused = await newest(ids.raw);
    assertEquals(Number(refused.meta.status), 0, "a poll whose body cannot be stored is a failure row");
    const error = String(JSON.parse(refused.body).error);
    assert(error.includes("NUL byte at offset 7"), `naming the byte and where it is: ${error}`);
    assert(error.includes("https://nulraw.body.test/feed"), `and the feed it was reading: ${error}`);
    const [{ count }] = await sql`select count(*) from net where sheet_id = ${ids.raw}`;
    assertEquals(Number(count), 1, "and nothing of the body it refused is stored beside it");

    // Text that spells the byte is not the byte, and refusing it would be this
    // reader deciding what the feed meant.
    assertEquals((await newest(ids.escaped)).body, escaped);
    // Nor is a NUL inside a gzip: what is stored is the rows that came out of
    // it, and a JSON encoder writes the cell's byte as those same six characters.
    assertEquals(JSON.parse((await newest(ids.zipped)).body), [{ a: "x\u0000y" }]);

    for (const id of Object.values(ids)) netDue.set(id, Number.MAX_SAFE_INTEGER);
    hostDue.clear();
  });

  await stack.close();
});
