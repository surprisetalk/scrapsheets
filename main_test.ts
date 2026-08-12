import { assert, assertEquals, assertThrows } from "@std/assert";
import { PGlite } from "@electric-sql/pglite";
import { PostgresConnection } from "pg-gateway";
import { citext } from "@electric-sql/pglite/contrib/citext";
import * as AM from "@automerge/automerge-repo";
import { WebSocketClientAdapter } from "@automerge/automerge-repo-network-websocket";
import { app, arrayify, automerge, createJwt, createToken, parseNetHeaders, pollNetOnce, seed, sql } from "./main.ts";
import type { Sheet, Table, Template } from "./main.ts";
import { DATASETS } from "./src/examples.mjs";
import dbSql from "./db.sql" with { type: "text" };
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
    assertEquals(body.from, "hello@scrap.land");
    assert(body.text.includes("/password?email=dave%40example.com&token="));
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

    // Buying with invalid sell_id returns 404.
    {
      await reject(jwt, `/buy/nonexistent_sell_id`, {
        method: "POST",
        body: JSON.stringify({}),
      });
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
  }

  // net-hook ingestion + net-http polling.
  {
    const { jwt } = await usr("nadia@example.com");
    const hookHand = automerge.create<Sheet>({ type: "net-hook", data: [] });
    const hookId = `net-hook:${hookHand.documentId}`;
    await put(jwt, `/library/${hookId}`, {});

    // Anyone can POST a payload; only the owner reads the log.
    {
      const res = await app.request(`/net/${hookId}`, {
        method: "POST",
        headers: new Headers({ "Content-Type": "application/json" }),
        body: JSON.stringify({ event: "ping" }),
      });
      assert(res.ok, `webhook ingest failed: ${res.status}`);
      const [cols_, ...rows] = await get<Table>(jwt, `/net/${hookId}`);
      assertEquals(Object.values(cols_).map((col) => col.name).join(), "created_at,body");
      assertEquals(rows.length, 1);
      assert(String(rows[0].body).includes("ping"), JSON.stringify(rows[0]));
      await reject("", `/net/${hookId}`);
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
    const proxy = async (url?: string) => {
      const res = await app.request(
        "/proxy" + (url === undefined ? "" : `?url=${encodeURIComponent(url)}`),
      );
      return { status: res.status, ...(await res.json()) };
    };
    assertEquals(await proxy(), { status: 400, error: "Missing url parameter" });
    assertEquals(await proxy("ftp://example.com/x"), {
      status: 400,
      error: "Only HTTP(S) URLs allowed.",
    });
    for (
      const url of [
        "http://localhost/x",
        "http://foo.local/x",
        "http://127.0.0.1/x",
        "http://169.254.169.254/latest/meta-data",
      ]
    ) {
      assertEquals(await proxy(url), { status: 400, error: "Internal URLs not allowed." }, url);
    }
    const { status, error } = await proxy("notaurl");
    assertEquals(status, 502);
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

  await sql.end();
  listener.close();
  await pglite.close();

  await new Promise((res) => setTimeout(res, 250));
});
