import { assert, assertEquals } from "@std/assert";
import * as AM from "@automerge/automerge-repo";
import { WebSocketClientAdapter } from "@automerge/automerge-repo-network-websocket";
import Stripe from "stripe";
import pg from "postgresjs";
import { ByteWriter, parquetWrite } from "hyparquet-writer";
import { Cron } from "croner";
import {
  accountBuckets,
  app,
  APPEND_ROWS_MAX,
  arrayify,
  automerge,
  BODY_CAP,
  callerIp,
  cannotConnect,
  CODEX_PREVIEW_MAX,
  DSN_KEEP,
  errorLogged,
  flushFolds,
  flushWebhooks,
  HOOK_BYTES_PER_WINDOW,
  HOOK_ROWS_PER_WINDOW,
  HOOK_WINDOW_S,
  hookBucket,
  hookBuckets,
  hookSecret,
  hookSign,
  HOST_GAP_MS,
  hostDue,
  LICENSES,
  LOG_EVERY_MS,
  netDue,
  OVERDUE_MAX,
  pendingSync,
  POLL_STALE_S,
  pollAlertOnce,
  pollAlertSheet,
  pollNetOnce,
  pollNetSheet,
  pollOnce,
  RATE_LIMIT_KEYS_MAX,
  rateLimitBuckets,
  safeFetch,
  seed,
  sql,
  status,
  USER_AGENT,
  USER_ALERTS_PER_DAY,
  USER_SHEETS_MAX,
  WEBHOOK_FAILS_MAX,
  WEBHOOKS_PER_SHEET_MAX,
  webhookTimer,
} from "./main.ts";
import type { Alert, Col, Query, Sheet, Table } from "./main.ts";
import { cohortSql, MAX_NAMES, MAX_QUERY_ROWS, MAX_REF_DEPTH } from "./src/sql.mjs";
import examplesSql from "./examples.sql" with { type: "text" };
import {
  boot,
  CODEX_DSN,
  DB_PORT,
  DEAD_PORTS,
  deliver,
  get,
  OWN_DSN,
  pollAlert,
  pollFeed,
  post,
  put,
  reject,
  request,
  usr,
} from "./main_harness.ts";

Deno.test(async function allTests(t) {
  const stack = await boot();

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
      const badCol = await call(jwt, "write_cells", { cells: [{ row: 0, col: "prise", value: 1 }] });
      assertEquals(badCol.isError, true);
      assert(
        badCol.content[0].text.includes(`"item"`) && /Did you mean: *string "price"/.test(badCol.content[0].text),
        `should list the names and suggest the near one, got: ${badCol.content[0].text}`,
      );
      // A cell is addressed by column name only. The key is the document's own spelling: accepted, it named a
      // column by something no read answers, and the refusal listed every column's key back to the caller.
      const byKey = await call(jwt, "write_cells", { cells: [{ row: 0, col: "1", value: 3 }] });
      assertEquals(byKey.isError, true, "a column key is not a column name");
      assert(!byKey.content[0].text.includes(`"0"`), `and no key comes back: ${byKey.content[0].text}`);
      // A cell that is not an object was a TypeError out of the destructuring, answered as its bare message.
      const notCell = await call(jwt, "write_cells", { cells: [null] });
      assert(notCell.content[0].text.startsWith("That is not a cell."), notCell.content[0].text);
      // Two columns of one name cannot key a cell, the way they cannot key a read.
      hand.change((d) => {
        (d.data as Table)[0][1].name = "item";
      });
      const twice = await call(jwt, "write_cells", { cells: [{ row: 0, col: "item", value: "fig" }] });
      hand.change((d) => {
        (d.data as Table)[0][1].name = "price";
      });
      assert(twice.content[0].text.includes("two columns with the same name"), twice.content[0].text);
      assertEquals(JSON.stringify(hand.doc().data[1]), `{"0":"apple","1":2.25}`, "and nothing was written");
      const badType = await call(jwt, "write_cells", { cells: [{ row: 0, col: "price", value: "expensive" }] });
      assertEquals(badType.isError, true);
      assert(
        badType.content[0].text.includes("usd") && badType.content[0].text.includes("price"),
        badType.content[0].text,
      );
      const badRow = await call(jwt, "write_cells", { cells: [{ row: 9, col: "price", value: 1 }] });
      assertEquals(badRow.isError, true);
      assert(badRow.content[0].text.includes("appends"), badRow.content[0].text);
      // The suggestion is an edit distance, col.length times each name's length, on the one thread every request
      // shares. A col near the body cap against a long name stalled the whole server for seconds.
      hand.change((d) => {
        (d.data as Table)[0][1].name = "p".repeat(1000);
      });
      const t0 = performance.now();
      const huge = await call(jwt, "write_cells", { cells: [{ row: 0, col: "z".repeat(900_000), value: 1 }] });
      const took = performance.now() - t0;
      hand.change((d) => {
        (d.data as Table)[0][1].name = "price";
      });
      assert(huge.content[0].text.startsWith("That sheet has no column of that name."), huge.content[0].text);
      assert(took < 500, `an oversized col must not run the edit distance: took ${took} ms`);
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

    // Reading a sheet is not writing it, nor reading who holds it. A viewer, a stranger on the public sheet and a
    // buyer of its listing all read it; write_cells asked only that, so each of them wrote, and the member list
    // handed each of them every member's address. A refused write spends nothing of the sheet's budget.
    {
      const viewer = await usr("mcp-viewer@example.com");
      const editor = await usr("mcp-editor@example.com");
      const stranger = await usr("mcp-stranger@example.com");
      const buyer = await usr("mcp-buyer@example.com");
      await post(jwt, `/library/${sheet_id}/share`, { email: "mcp-viewer@example.com", role: "viewer" });
      await post(jwt, `/library/${sheet_id}/share`, { email: "mcp-editor@example.com", role: "editor" });
      await post(jwt, `/library/${sheet_id}/public`, { public: true });
      await post(jwt, `/sell/${sheet_id}`, { price: 0, license: "own" });
      const [{ sell_id }] = await sql`select sell_id from sheet where sheet_id = ${sheet_id}`;
      await post(buyer.jwt, `/buy/${sell_id}`, {});
      const members = (who: string) =>
        app.request(`/library/${sheet_id}/share`, { headers: new Headers({ Authorization: `Bearer ${who}` }) });
      for (const [name, who] of [["public stranger", stranger], ["buyer", buyer]] as const) {
        const list = await members(who.jwt);
        const said = await list.text();
        assertEquals(list.status, 403, `a ${name} cannot read the members: ${said}`);
        assert(said.includes("/share") && !said.includes("@example.com"), `naming the door, no address: ${said}`);
      }
      const listed = await members(viewer.jwt);
      assertEquals(listed.status, 200, `a viewer is a member and reads the members: ${await listed.text()}`);

      // The budget refills while calls run, so only the write calls sit between the reset and the refusal.
      const before = JSON.stringify(hand.doc().data);
      hookBuckets.delete(sheet_id);
      hookBucket(sheet_id).rows = 1;
      for (const [name, who] of [["viewer", viewer], ["public stranger", stranger], ["buyer", buyer]] as const) {
        const denied = await call(who.jwt, "write_cells", { cells: [{ row: 0, col: "item", value: "pear" }] });
        assertEquals(denied.isError, true, `a ${name} cannot write`);
        assert(denied.content[0].text.includes("owner or editor"), denied.content[0].text);
      }
      assertEquals(JSON.stringify(hand.doc().data), before, "no refused write reached the document");

      const wrote = await call(editor.jwt, "write_cells", { cells: [{ row: 0, col: "item", value: "pear" }] });
      assertEquals(wrote.structuredContent, { written: 1, rows: 2 }, "an editor writes, on the unit nobody spent");
      const spent = await call(editor.jwt, "write_cells", { cells: [{ row: 0, col: "item", value: "plum" }] });
      assert(spent.content[0].text.includes("too many writes"), spent.content[0].text);
      hookBuckets.delete(sheet_id);
      assertEquals((await call(jwt, "read_sheet", {})).structuredContent.rows[0].item, "pear");
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

  // An agent is something you hand a key to, not an account. The key reaches the MCP endpoint for its own sheet, an
  // `api-read` key is that key with the writes taken away, and neither a tool argument nor an @ref is a way back out
  // to the rest of the minter's library -- which is the one thing a borrowed identity must never buy.
  await t.step("An MCP client carries one sheet's key, and api-read carries no writes", async () => {
    const { jwt } = await usr("iris@example.com");
    const sheetOf = async (name: string) => {
      const hand = automerge.create<Sheet>({
        type: "table",
        data: [arrayify([{ name: "n", type: "int", key: "0" }]), { 0: 1 }],
      });
      const sheet_id = `table:${hand.documentId}`;
      await put(jwt, `/library/${sheet_id}`, { name });
      return sheet_id;
    };
    const a = await sheetOf("keyed");
    const b = await sheetOf("elsewhere");
    const mint = async (name: string) => (await post(jwt, `/library/${a}/secret`, { name })).data.key as string;
    const write = await mint("api");
    const read = await mint("api-read");

    const call = async (key: string, name: string, args: unknown) => {
      const res = await app.request(`/mcp/${a}`, {
        method: "POST",
        headers: new Headers({ "Content-Type": "application/json", "scrapsheets-key": key }),
        body: JSON.stringify({ jsonrpc: "2.0", id: 1, method: "tools/call", params: { name, arguments: args } }),
      });
      const body = await res.text();
      assert(res.ok, `mcp ${name} under a key answered ${res.status}: ${body}`);
      return JSON.parse(body).result;
    };

    // Both keys reach the endpoint the JWT reached, for the one sheet they name.
    {
      const out = await call(write, "read_sheet", {});
      assertEquals(out.isError, false, out.content?.[0]?.text);
      assertEquals(out.structuredContent.rows, [{ n: 1 }]);
      assertEquals((await call(read, "read_sheet", {})).structuredContent.rows, [{ n: 1 }]);
    }

    // One writes; the other is refused by the name it was minted under, on the tool and on the route it also opens.
    {
      assertEquals((await call(write, "write_cells", { cells: [{ row: 1, col: "n", value: 2 }] })).structuredContent, {
        written: 1,
        rows: 2,
      });
      const tool = await call(read, "write_cells", { cells: [{ row: 0, col: "n", value: 9 }] });
      assertEquals(tool.isError, true);
      assert(
        tool.content[0].text.includes("api-read") && tool.content[0].text.includes("writes nothing"),
        tool.content[0].text,
      );
      const route = await app.request(`/sheet/${a}`, {
        method: "POST",
        headers: new Headers({ "Content-Type": "application/json", "scrapsheets-key": read }),
        body: JSON.stringify({ rows: [{ n: 3 }] }),
      });
      assertEquals(route.status, 403);
      const text = await route.text();
      assert(text.includes("api-read") && text.includes("writes nothing"), text);
      // Every refusal left the sheet exactly where the writing key put it.
      assertEquals((await get<Table>(jwt, `/sheet/${a}`)).slice(1), [{ n: 1 }, { n: 2 }]);
    }

    // Under either key the library is that one sheet, and a second sheet is refused by name rather than answered
    // with the authority the key borrows.
    for (const key of [write, read]) {
      const ls = await call(key, "list_sheets", {});
      assertEquals(ls.structuredContent.sheets.map((s: { sheet_id: string }) => s.sheet_id), [a]);
      const own = await call(key, "query_sheet", { code: `select sum(n) as tally from @${a}` });
      assertEquals(own.isError, false, own.content?.[0]?.text);
      assertEquals(own.structuredContent.rows, [{ tally: 3 }]);
      const foreign = await call(key, "query_sheet", { code: `select n from @${b}` });
      assertEquals(foreign.isError, true);
      assert(
        foreign.content[0].text.includes(`@${b}`) && foreign.content[0].text.includes(`@${a}`),
        foreign.content[0].text,
      );
      const elsewhere = await call(key, "read_sheet", { sheet_id: b });
      assertEquals(elsewhere.isError, true);
      assert(elsewhere.content[0].text.includes(`opens ${a}`), elsewhere.content[0].text);
    }

    // A snippet body is a ref too: a key on the snippet table cannot reach another sheet through one, and a key on
    // a sheet cannot name another sheet as its snippet table.
    {
      const hand = automerge.create<Sheet>({
        type: "table",
        data: [
          arrayify([{ name: "name", type: "text", key: "0" }, { name: "expr", type: "text", key: "1" }]),
          { 0: "mine", 1: "1" },
          { 0: "theirs", 1: `(select max(n) from @${b})` },
        ],
      });
      const snips = `table:${hand.documentId}`;
      await put(jwt, `/library/${snips}`, { name: "snippets" });
      const key = (await post(jwt, `/library/${snips}/secret`, { name: "api" })).data.key as string;
      const callOn = async (code: string) => {
        const res = await app.request(`/mcp/${snips}`, {
          method: "POST",
          headers: new Headers({ "Content-Type": "application/json", "scrapsheets-key": key }),
          body: JSON.stringify({
            jsonrpc: "2.0",
            id: 1,
            method: "tools/call",
            params: { name: "query_sheet", arguments: { code } },
          }),
        });
        return JSON.parse(await res.text()).result;
      };
      const own = await callOn(`using @${snips} select {{mine}} as one`);
      assertEquals(own.isError, false, own.content?.[0]?.text);
      assertEquals(own.structuredContent.rows, [{ one: 1 }]);
      for (const code of [`using @${snips} select {{theirs}} as x`, `using @${b} select 1 as x`]) {
        const refused = await callOn(code);
        assertEquals(refused.isError, true, code);
        assert(refused.content[0].text.includes(`This key opens ${snips}`), refused.content[0].text);
      }
    }

    // A key's authority is the sheet's created_by, read again on every request through assertSheetEditor, so a
    // downgrade of the creator reaches a key already minted. PUT /library/:id grants "owner" through pendingSync for
    // 30 seconds, until its sheet_usr row exists; a finished claim must leave no grant to outvote the downgrade.
    {
      const [{ usr_id }] = await sql`select usr_id from usr where email = 'iris@example.com'`;
      assert(
        !pendingSync.has(`${usr_id}/${a.split(":")[1]}`),
        "the claim that made a owner already finished, so nothing should still be pending for it",
      );
      await post(jwt, `/library/${a}/share`, { email: "iris@example.com", role: "viewer" });
      const after = await call(write, "write_cells", { cells: [{ row: 0, col: "n", value: 99 }] });
      assertEquals(after.isError, true, `a downgraded creator's key should stop writing: ${JSON.stringify(after)}`);
      assert(after.content[0].text.includes("owner or editor"), after.content[0].text);
      assertEquals((await get<Table>(jwt, `/sheet/${a}`)).slice(1), [{ n: 1 }, { n: 2 }], "and nothing was written");
      await post(jwt, `/library/${a}/share`, { email: "iris@example.com", role: "owner" });
    }
  });

  // The half of the protocol a client reads rather than calls: a resource per sheet it may list, whose body is the
  // csv it would have exported, and one prompt built from the same read `describe @ref` answers with.
  await t.step("MCP resources are sheets as csv, and its one prompt describes them", async () => {
    const { jwt } = await usr("juno@example.com");
    const hand = automerge.create<Sheet>({
      type: "table",
      data: [
        arrayify([{ name: "item", type: "text", key: "0" }, { name: "price", type: "usd", key: "1" }]),
        // Past one page of the default read, so a resource over a sheet the database pages has something to lose.
        ...Array.from({ length: 60 }, (_, i) => ({ 0: `item ${i}`, 1: i === 0 ? null : i / 2 })),
      ],
    });
    const sheet_id = `table:${hand.documentId}`;
    await put(jwt, `/library/${sheet_id}`, {});
    const rpc = async (method: string, params?: unknown) => {
      const res = await app.request(`/mcp/${sheet_id}`, {
        method: "POST",
        headers: new Headers({ "Content-Type": "application/json", Authorization: `Bearer ${jwt}` }),
        body: JSON.stringify({ jsonrpc: "2.0", id: 1, method, params }),
      });
      const body = await res.text();
      assert(res.ok, `mcp ${method} answered ${res.status}: ${body}`);
      return JSON.parse(body);
    };

    // A client that cannot see them listed will never ask for them.
    {
      const { result } = await rpc("initialize", { protocolVersion: "2025-06-18" });
      assert(result.capabilities.resources, JSON.stringify(result.capabilities));
      assert(result.capabilities.prompts, JSON.stringify(result.capabilities));
    }

    // The resource is the sheet, and reading it is the csv export byte for byte.
    {
      const { result } = await rpc("resources/list");
      const mine = result.resources.find((r: { uri: string }) => r.uri === `sheet://${sheet_id}`);
      assert(mine, JSON.stringify(result.resources));
      assertEquals(mine.mimeType, "text/csv");
      const read = await rpc("resources/read", { uri: mine.uri });
      assertEquals(read.result.contents[0].mimeType, "text/csv");
      const exported = await app.request(`/export/${sheet_id}.csv`, {
        headers: new Headers({ Authorization: `Bearer ${jwt}` }),
      });
      assertEquals(read.result.contents[0].text, await exported.text());
      // A uri nobody named is the one refusal here with no route to blame, so it says what a uri looks like.
      const stray = await rpc("resources/read", { uri: "file:///etc/passwd" });
      assertEquals(stray.error.code, -32002);
      assert(stray.error.message.includes("sheet://"), stray.error.message);
    }

    // A table sheet answers whole whatever is asked; the database pages a query sheet, and a resource that took the
    // default stopped at one page and answered a csv that was wrong rather than short.
    {
      const q = automerge.create<Sheet>({
        type: "query",
        data: [{ lang: "sql", code: `select item, price from @${sheet_id}`, args: [] }],
      });
      const query_id = `query:${q.documentId}`;
      await put(jwt, `/library/${query_id}`, {});
      const { result } = await rpc("resources/read", { uri: `sheet://${query_id}` });
      assertEquals(result.contents[0].text.split("\n").length, 61);
      const exported = await app.request(`/export/${query_id}.csv`, {
        headers: new Headers({ Authorization: `Bearer ${jwt}` }),
      });
      assertEquals(result.contents[0].text, await exported.text());
      // The prompt describes the sheet, not the page: a row count off the first page is a wrong description.
      const prompt = await rpc("prompts/get", { name: "describe_sheet", arguments: { sheet_id: query_id } });
      const text = prompt.result.messages[0].content.text;
      assert(text.includes("holds 60 rows") && text.includes("price (usd), 1 blank"), text);
    }

    // The prompt names every column, its type and how much of it is blank.
    {
      const { result } = await rpc("prompts/list");
      assertEquals(result.prompts.map((p: { name: string }) => p.name), ["describe_sheet"]);
      const got = await rpc("prompts/get", { name: "describe_sheet", arguments: {} });
      const text = got.result.messages[0].content.text;
      assert(text.includes(sheet_id), text);
      assert(text.includes("item (text), 0 blank") && text.includes("price (usd), 1 blank"), text);
      const stray = await rpc("prompts/get", { name: "nope" });
      assertEquals(stray.error.code, -32602);
    }

    // A read is a row in the log, under the name the method has.
    {
      const rows = JSON.stringify(await get<Table>(jwt, `/sheet/library:audit`));
      assert(rows.includes("mcp resources/read") && rows.includes("mcp prompts/get"), rows.slice(0, 500));
    }

    // A multi column takes an array of its options through write_cells, and refuses a pick outside them.
    {
      const tagged = automerge.create<Sheet>({
        type: "table",
        data: [arrayify([{ name: "tags", type: "multi:red,green,blue", key: "0" }]), { 0: ["red"] }],
      });
      const tagged_id = `table:${tagged.documentId}`;
      await put(jwt, `/library/${tagged_id}`, {});
      const tag = async (value: unknown) =>
        (await (await app.request(`/mcp/${tagged_id}`, {
          method: "POST",
          headers: new Headers({ "Content-Type": "application/json", Authorization: `Bearer ${jwt}` }),
          body: JSON.stringify({
            jsonrpc: "2.0",
            id: 1,
            method: "tools/call",
            params: { name: "write_cells", arguments: { cells: [{ row: 0, col: "tags", value }] } },
          }),
        })).json()).result;
      assertEquals((await tag(["green", "blue"])).structuredContent, { written: 1, rows: 1 });
      assertEquals(JSON.stringify(tagged.doc().data[1]), `{"0":["green","blue"]}`);
      for (const bad of [["purple"], ["red", "red"], "red"]) {
        const refusal = await tag(bad);
        assertEquals(refusal.isError, true);
        assert(
          refusal.content[0].text.includes("red, green, blue") || refusal.content[0].text.includes("multi:"),
          refusal.content[0].text,
        );
      }
      assertEquals(JSON.stringify(tagged.doc().data[1]), `{"0":["green","blue"]}`, "a refused pick wrote nothing");
    }

    // A money column takes a number, as a usd column does, and refuses text.
    {
      const priced = automerge.create<Sheet>({
        type: "table",
        data: [arrayify([{ name: "fee", type: "money:EUR", key: "0" }]), { 0: 1 }],
      });
      const priced_id = `table:${priced.documentId}`;
      await put(jwt, `/library/${priced_id}`, {});
      const fee = async (value: unknown) =>
        (await (await app.request(`/mcp/${priced_id}`, {
          method: "POST",
          headers: new Headers({ "Content-Type": "application/json", Authorization: `Bearer ${jwt}` }),
          body: JSON.stringify({
            jsonrpc: "2.0",
            id: 1,
            method: "tools/call",
            params: { name: "write_cells", arguments: { cells: [{ row: 0, col: "fee", value }] } },
          }),
        })).json()).result;
      assertEquals((await fee(12.5)).structuredContent, { written: 1, rows: 1 });
      assertEquals((await fee("12.5")).isError, true);
      assertEquals(JSON.stringify(priced.doc().data[1]), `{"0":12.5}`);
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

    // A multi column takes an array of its options, a blank, and nothing else; the array is stored as sent and every
    // export writes it.
    {
      const tagged = automerge.create<Sheet>({
        type: "table",
        data: [
          arrayify([{ name: "tags", type: "multi:red,green,blue", key: "0" }, { name: "n", type: "text", key: "1" }]),
          {
            0: ["red"],
            1: "a",
          },
        ],
      });
      const tagged_id = `table:${tagged.documentId}`;
      await put(jwt, `/library/${tagged_id}`, {});
      const { data } = await post(jwt, `/sheet/${tagged_id}`, {
        rows: [{ tags: ["red", "blue"], n: "b" }, { tags: null, n: "c" }, { tags: [], n: "d" }],
      });
      assertEquals(data, { appended: 3, rows: 4 });
      assertEquals((await get<Table>(jwt, `/sheet/${tagged_id}`)).slice(1), [
        { tags: ["red"], n: "a" },
        { tags: ["red", "blue"], n: "b" },
        { tags: null, n: "c" },
        { tags: [], n: "d" },
      ]);
      for (const tags of [["purple"], ["red", "red"], "red", [1]]) {
        const res = await refused({ rows: [{ tags, n: "x" }] }, tagged_id);
        assert(res.text.includes("multi:red,green,blue") && res.text.includes("row 1"), res.text);
      }
      const text = await refused({ rows: [{ item: ["a"], qty: 1 }] });
      assert(text.text.includes("no cell can hold"), "an array under a text column is still refused");
      const { data: [, ...queried] }: { data: Table } = await post(jwt, `/query`, {
        lang: "sql",
        code: `select n from @${tagged_id} order by n`,
        args: [],
      });
      assertEquals(queried.length, 4);
      for (const format of ["csv", "md", "xlsx", "parquet", "json", "ndjson"]) {
        const res = await app.request(`/export/${tagged_id}.${format}`, {
          headers: { Authorization: `Bearer ${jwt}` },
        });
        assertEquals(res.status, 200, `${format}: ${await res.clone().text()}`);
        if (format === "csv") assertEquals((await res.text()).split("\n")[2], `"red,blue",b`);
        else await res.arrayBuffer();
      }
    }

    // A column's check is enforced at both doors, on the rows the write touched, and a refusal writes nothing. The
    // keys differ from the names, so a check that read the wrong spelling would check nothing. The first two rows
    // already repeat a sku: they are not blamed, and a good append still lands.
    {
      const checked = automerge.create<Sheet>({
        type: "table",
        data: [
          arrayify([
            { name: "sku", type: "text", key: "7", check: { required: true, unique: true } },
            { name: "pct", type: "int", key: "3", check: { min: 0, max: 100 } },
            { name: "size", type: "enum:s,m,l", key: "5", check: {} },
            { name: "note", type: "enum:a,b", key: "9" },
          ]) as never,
          { 7: "a1", 3: 10, 5: "s", 9: "zzz" },
          { 7: "a1", 3: 20, 5: "m", 9: "zzz" },
        ],
      });
      const checked_id = `table:${checked.documentId}`;
      await put(jwt, `/library/${checked_id}`, {});
      const row = (over: Record<string, unknown>) => ({ sku: "b2", pct: 50, size: "l", note: "anything", ...over });
      const snapshot = () => JSON.stringify(checked.doc().data);
      const refuse = async (rows: unknown[], want: string[]) => {
        const before = snapshot();
        const res = await refused({ rows }, checked_id);
        for (const w of want) assert(res.text.includes(w), `${w}: ${res.text}`);
        assertEquals(snapshot(), before, "a refused append wrote nothing");
      };
      const cell = async (cells: unknown[]) =>
        (await (await app.request(`/mcp/${checked_id}`, {
          method: "POST",
          headers: new Headers({ "Content-Type": "application/json", Authorization: `Bearer ${jwt}` }),
          body: JSON.stringify({
            jsonrpc: "2.0",
            id: 1,
            method: "tools/call",
            params: { name: "write_cells", arguments: { cells } },
          }),
        })).json()).result;

      assertEquals((await post(jwt, `/sheet/${checked_id}`, { rows: [row({})] })).data, { appended: 1, rows: 3 });
      await refuse([row({ sku: "" })], ["required", `"sku"`, "row 4"]);
      await refuse([row({ sku: null })], ["required"]);
      await refuse([row({ sku: "b2" })], ["unique", `"b2"`, "row 3"]);
      await refuse([row({ sku: "c3" }), row({ sku: "c3" })], ["unique", "row 5"]);
      await refuse([row({ sku: "z9", pct: 101 })], ["max", "100", "101"]);
      await refuse([row({ sku: "z9", pct: -1 })], ["min", "-1"]);
      await refuse([row({ sku: "z9", size: "xl" })], ["size", "xl", "s, m, l"]);
      assertEquals(
        (await post(jwt, `/sheet/${checked_id}`, { rows: [row({ sku: "d4", pct: 100, note: "free" })] })).data,
        {
          appended: 1,
          rows: 4,
        },
      );

      // write_cells: the same rule, a cell written with its own value passes, and a refused call changes nothing.
      const before = snapshot();
      for (
        const [cells, want] of [
          [[{ row: 2, col: "sku", value: "a1" }], "unique"],
          [[{ row: 2, col: "pct", value: 101 }], "max"],
          [[{ row: 2, col: "size", value: "xl" }], "size"],
          [[{ row: 4, col: "pct", value: 5 }], "required"],
        ] as const
      ) {
        const res = await cell([...cells]);
        assert(res.isError === true && res.content[0].text.includes(want), JSON.stringify(res));
      }
      assertEquals(snapshot(), before, "every refused call left the document as it was");
      assertEquals((await cell([{ row: 2, col: "sku", value: "b2" }])).structuredContent, { written: 1, rows: 4 });
      assertEquals((await cell([{ row: 4, col: "sku", value: "e5" }])).structuredContent, { written: 1, rows: 5 });

      // Seams: a check on a multi: column (an empty array is a blank), on a money column (a numeric string is the
      // number the column holds), and a stored numeric string in a touched row (the page reads it as a number).
      {
        const seam = automerge.create<Sheet>({
          type: "table",
          data: [
            arrayify([
              { name: "tags", type: "multi:a,b", key: "0", check: { required: true, unique: true } },
              { name: "fee", type: "money:EUR", key: "1", check: { min: 0, max: 100 } },
              { name: "qty", type: "int", key: "2", check: { max: 100 } },
              { name: "note", type: "text", key: "3" },
            ]) as never,
            { 0: ["a"], 1: "500", 2: "500", 3: "x" },
          ],
        });
        const seam_id = `table:${seam.documentId}`;
        await put(jwt, `/library/${seam_id}`, {});
        const mk = (over: Record<string, unknown>) => ({ tags: ["b"], fee: 5, qty: 1, note: "", ...over });
        for (
          const [over, want] of [
            [{ tags: [] }, "required"],
            [{ tags: null }, "required"],
            [{ tags: ["a"] }, "unique"],
            [{ fee: 101 }, "max"],
            [{ fee: "101" }, "max"],
            [{ fee: -1 }, "min"],
          ] as const
        ) {
          const res = await refused({ rows: [mk(over)] }, seam_id);
          assert(res.text.includes(want), `${JSON.stringify(over)}: ${res.text}`);
        }
        assertEquals((await post(jwt, `/sheet/${seam_id}`, { rows: [mk({ fee: "7.5" })] })).data.appended, 1);
        const call = async (cells: unknown[]) =>
          (await (await app.request(`/mcp/${seam_id}`, {
            method: "POST",
            headers: new Headers({ "Content-Type": "application/json", Authorization: `Bearer ${jwt}` }),
            body: JSON.stringify({
              jsonrpc: "2.0",
              id: 1,
              method: "tools/call",
              params: { name: "write_cells", arguments: { cells } },
            }),
          })).json()).result;
        for (
          const [cells, want] of [
            [[{ row: 1, col: "fee", value: "5" }], "does not match"],
            [[{ row: 1, col: "fee", value: 150 }], "max"],
            [[{ row: 1, col: "tags", value: [] }], "required"],
            [[{ row: 0, col: "note", value: "y" }], "max"],
          ] as const
        ) {
          const res = await call([...cells]);
          assert(res.isError === true && res.content[0].text.includes(want), JSON.stringify(res));
        }
      }

      // A check that cannot be read is refused where it is read, never skipped.
      for (const check of [{ min: "0" }, { bogus: true }, { required: "yes" }, { min: 5, max: 1 }, 5]) {
        const odd = automerge.create<Sheet>({
          type: "table",
          data: [arrayify([{ name: "n", type: "int", key: "0", check }]) as never, { 0: 1 }],
        });
        const odd_id = `table:${odd.documentId}`;
        await put(jwt, `/library/${odd_id}`, {});
        const res = await refused({ rows: [{ n: 2 }] }, odd_id);
        assertEquals(res.status, 400);
        for (const w of ["Expected", "Received", "Source", "Fix", "check"]) assert(res.text.includes(w), res.text);
      }
      const text = automerge.create<Sheet>({
        type: "table",
        data: [arrayify([{ name: "t", type: "text", key: "0", check: { min: 1 } }]) as never, { 0: "a" }],
      });
      await put(jwt, `/library/table:${text.documentId}`, {});
      const res = await refused({ rows: [{ t: "b" }] }, `table:${text.documentId}`);
      assert(res.text.includes("numeric"), res.text);
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
          { name: "fee", type: "money:EUR", key: "5" },
        ]),
      ],
    });
    const sheet_id = `table:${hand.documentId}`;
    await put(jwt, `/library/${sheet_id}`, {});
    const doc = await request(jwt, `/openapi/${sheet_id}`);

    assertEquals(doc.openapi, "3.1.0");
    assertEquals(doc.components.schemas.Row.required, ["item", "qty", "price", "due", "paid", "fee"]);
    assertEquals(doc.components.schemas.Row.properties, {
      item: { type: "string" },
      qty: { type: "integer" },
      price: { type: "number" },
      due: { type: "string", format: "date" },
      paid: { type: "boolean" },
      fee: { type: "number" },
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

  await t.step("Every seeded example must survive a visit with no ?q= param", () => {
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
    // The age is this test's clock read against the server's, so a loaded
    // machine puts a second between them and an exact match is a race. What
    // must hold is that the refusal names the skew it measured, and that the
    // skew is the one this signature carries.
    const aged = Number(staleText.match(/which is (\d+) seconds old/)?.[1]);
    assert(aged >= 3600 && aged < 3610, `it must name the skew, got: ${staleText}`);
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
      // Slowness is not a refusal: only find's own answer or its rejection
      // counts. The bound is a crash that reports the time it waited.
      const started = Date.now();
      const bound = 30000;
      let timer: ReturnType<typeof setTimeout> | undefined;
      try {
        return await Promise.race([
          repo.find<Sheet>(hand.documentId).then(() => "shared", () => "denied"),
          new Promise<never>((_, reject) => {
            timer = setTimeout(
              () =>
                reject(
                  new Error(
                    `${peer}: repo.find gave no answer after ${
                      Date.now() - started
                    } ms (bound ${bound} ms); the machine may be overloaded`,
                  ),
                ),
              bound,
            );
          }),
        ]);
      } finally {
        clearTimeout(timer);
        adapter.disconnect();
      }
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

  // A LIST<INT64> nests a BigInt where parquetRows' own top-level map never
  // looked, and JSON.stringify throws on one wherever it sits. POST /net/:id
  // has no try/catch around readFeedBody the way the poller's pollNetSheet
  // does, so the raw throw used to reach app.onError and answer the generic
  // 500 -- exactly the unexplained crash the NUL-byte check above exists to
  // keep out of this same door, just one layer deeper in the value. Signed
  // by hand rather than through hookSign, which only takes a string body:
  // decoding a binary parquet file to a string to sign and re-encoding it to
  // send would sign different bytes than the ones that go out.
  await t.step("A parquet delivery with a nested INT64 past 2^53 does not crash the door", async () => {
    const { jwt } = await usr("nadia@example.com");
    const hand = automerge.create<Sheet>({ type: "net-hook", data: [] });
    const id = `net-hook:${hand.documentId}`;
    await put(jwt, `/library/${id}`, {});
    const target = `/net/${id}`;

    // A three-level LIST<INT64>, the shape a real writer uses for a repeated
    // field: hyparquet-writer's own columnData shorthand refuses a bare
    // REPEATED column and auto-detects a plain array of BigInt as something
    // narrower, so only the explicit schema reaches the nested-BigInt case.
    const writer = new ByteWriter();
    parquetWrite({
      writer,
      columnData: [{ name: "tags", data: [[9007199254740993n, 2n], [3n]] }],
      schema: [
        { name: "root", num_children: 1 },
        { name: "tags", repetition_type: "REQUIRED", converted_type: "LIST", num_children: 1 },
        { name: "list", repetition_type: "REPEATED", num_children: 1 },
        { name: "element", type: "INT64", repetition_type: "REQUIRED" },
      ],
    });
    const body = new Uint8Array(writer.getBuffer());

    const stamp = Math.floor(Date.now() / 1000);
    const key = await crypto.subtle.importKey(
      "raw",
      new TextEncoder().encode(await hookSecret(id)),
      { name: "HMAC", hash: "SHA-256" },
      false,
      ["sign"],
    );
    const prefix = new TextEncoder().encode(`${stamp}\n${target}\n`);
    const message = new Uint8Array(prefix.length + body.length);
    message.set(prefix);
    message.set(body, prefix.length);
    const mac = new Uint8Array(await crypto.subtle.sign("HMAC", key, message));
    const sig = `t=${stamp},v2=${Array.from(mac).map((b) => b.toString(16).padStart(2, "0")).join("")}`;

    const res = await app.request(target, {
      method: "POST",
      headers: new Headers({ "Content-Type": "application/vnd.apache.parquet", "scrapsheets-signature": sig }),
      body,
    });
    assertEquals(res.status, 200, `a nested BigInt must not crash the delivery door: ${await res.text()}`);
    const [row] = await sql`select body from net where sheet_id = ${id} order by net_id desc limit 1`;
    assertEquals(JSON.parse(row.body), [{ tags: ["9007199254740993", 2] }, { tags: [3] }]);
  });

  await t.step("A key pasted into a cell is not published by accident", async () => {
    const { jwt } = await usr("vera@example.com");
    // Shaped like a Stripe key and worth nothing: the scan is over the shape,
    // and a real key in a test file is the accident this step is about.
    const leaked = "sk_live_" + "0000EXAMPLEKEYNOTREAL0000";
    const hand = automerge.create<Sheet>({
      type: "table",
      data: [
        arrayify([
          { name: "service", type: "text", key: 0 },
          { name: "token", type: "text", key: 1 },
        ]),
        { 0: "billing", 1: "ask ops" },
        { 0: "shipping", 1: leaked },
      ],
    });
    const id = `table:${hand.documentId}`;
    await put(jwt, `/library/${id}`, {});

    const refused = await app.request(`/library/${id}/public`, {
      method: "POST",
      headers: new Headers({ "Content-Type": "application/json", Authorization: `Bearer ${jwt}` }),
      body: JSON.stringify({ public: true }),
    });
    assertEquals(refused.status, 400, "a sheet holding a key does not go public");
    const text = await refused.text();
    assert(text.includes("Stripe"), `the refusal names the shape it matched: ${text}`);
    assert(text.includes("token"), `and the column it is in: ${text}`);
    assert(text.includes("row 2"), `and the row, 1-based: ${text}`);
    assert(!text.includes(leaked), "no refusal is an oracle: the value never comes back");
    const [still]: { public: boolean }[] = await sql`select public from sheet where sheet_id = ${id}`;
    assertEquals(still.public, false, "and the sheet stayed private");

    // Taking a sheet private publishes nothing, so nothing is scanned: a key
    // must never be what stops its own sheet from being locked down.
    await post(jwt, `/library/${id}/public`, { public: false });
    // A listing is the other door a stranger reads a sheet through.
    await reject(jwt, `/sell/${id}`, { method: "POST", body: JSON.stringify({ price: 0, license: "own" }) });

    hand.change((doc) => {
      (doc.data as Table)[2][1] = "";
    });
    await post(jwt, `/library/${id}/public`, { public: true });
    const [now]: { public: boolean }[] = await sql`select public from sheet where sheet_id = ${id}`;
    assertEquals(now.public, true, "with the cell cleared, the same sheet publishes");
    await post(jwt, `/sell/${id}`, { price: 0, license: "own" });

    // An incoming-webhook url is a bearer credential: whoever holds it posts to
    // the channel. An alert's destination is a cell, and every shape here was a
    // token, so the one credential this app itself tells you to paste in was the
    // one it published.
    const posts = automerge.create<Sheet>({
      type: "table",
      data: [
        arrayify([{ name: "where", type: "text", key: 0 }]),
        // Short on purpose: a url with the real T…/B…/token widths is one GitHub
        // push protection refuses to let into the repository, and our shape
        // reads only the host path.
        { 0: "https://hooks.slack.com/services/T0/B0/zzz" },
        { 0: "https://discord.com/api/webhooks/0/zzz" },
      ],
    });
    const hooked = `table:${posts.documentId}`;
    await put(jwt, `/library/${hooked}`, {});
    const held = await app.request(`/library/${hooked}/public`, {
      method: "POST",
      headers: new Headers({ "Content-Type": "application/json", Authorization: `Bearer ${jwt}` }),
      body: JSON.stringify({ public: true }),
    });
    assertEquals(held.status, 400, "a sheet holding a webhook url does not go public");
    assert((await held.text()).includes("Slack webhook url"), "the refusal names the shape it matched");
    const [kept]: { public: boolean }[] = await sql`select public from sheet where sheet_id = ${hooked}`;
    assertEquals(kept.public, false, "and the credential stayed private");

    // A Teams Incoming Webhook is the same credential pasted into the same
    // cell. Its own sheet, because the scan names the first shape it meets.
    const teams = automerge.create<Sheet>({
      type: "table",
      data: [
        arrayify([{ name: "where", type: "text", key: 0 }]),
        { 0: "https://contoso.webhook.office.com/webhookb2/00/IncomingWebhook/11/22" },
      ],
    });
    const teamed = `table:${teams.documentId}`;
    await put(jwt, `/library/${teamed}`, {});
    const card = await app.request(`/library/${teamed}/public`, {
      method: "POST",
      headers: new Headers({ "Content-Type": "application/json", Authorization: `Bearer ${jwt}` }),
      body: JSON.stringify({ public: true }),
    });
    assertEquals(card.status, 400, "a sheet holding a Teams webhook url does not go public");
    assert((await card.text()).includes("Teams webhook url"), "the refusal names the shape it matched");
  });

  // The scan reads whatever a browser synced in, and it used to run before
  // anything decided whose sheet it was reading.
  await t.step("A publish scan is bounded, and is nobody's peephole", async () => {
    const { jwt } = await usr("scanned@example.com");
    const { jwt: stranger } = await usr("stranger@example.com");

    // Nothing bounded a cell's length, and the signed-token shape walked the
    // rest of the cell from every `eyJ` in it: four times the cell cost sixteen
    // times the time, and a megabyte -- which a synced row may hold -- cost
    // minutes of a blocked event loop.
    //
    // What is asserted is that the publish finishes, not how fast: an
    // unbounded scan over 48 KB of `eyJ` costs minutes, which the suite's own
    // ten-second budget is the alarm for. A millisecond count read against a
    // second millisecond count is a test that fails on a busy machine instead.
    const scanned = `table:${
      automerge.create<Sheet>({
        type: "table",
        data: [arrayify([{ name: "note", type: "text", key: 0 }]), { 0: "eyJ".repeat(16_000) }],
      }).documentId
    }`;
    await put(jwt, `/library/${scanned}`, {});
    await post(jwt, `/library/${scanned}/public`, { public: true });
    const [went]: { public: boolean }[] = await sql`select public from sheet where sheet_id = ${scanned}`;
    assertEquals(went.public, true, "a 48 KB cell of near-misses publishes rather than blocking the isolate");

    // The scan ran before the update that asks whose sheet this is, so a
    // stranger holding an id learned what a private document holds: that it
    // holds a credential, which one, which column, and which row.
    const leaked = "sk_live_" + "0000EXAMPLEKEYNOTREAL0000";
    const hand = automerge.create<Sheet>({
      type: "table",
      data: [
        arrayify([{ name: "service", type: "text", key: 0 }, { name: "payments_token", type: "text", key: 1 }]),
        { 0: "billing", 1: leaked },
      ],
    });
    const id = `table:${hand.documentId}`;
    await put(jwt, `/library/${id}`, {});
    const theirs = await app.request(`/sell/${id}`, {
      method: "POST",
      headers: new Headers({ "Content-Type": "application/json", Authorization: `Bearer ${stranger}` }),
      body: JSON.stringify({ price: 0, license: "own" }),
    });
    assertEquals(theirs.status, 403, "a listing is the owner's to make");
    const told = await theirs.text();
    for (const leakage of ["Stripe", "payments_token", "row 1"])
      assert(!told.includes(leakage), `a stranger learns ${leakage} about a sheet they have no role on: ${told}`);
    const mine = await app.request(`/sell/${id}`, {
      method: "POST",
      headers: new Headers({ "Content-Type": "application/json", Authorization: `Bearer ${jwt}` }),
      body: JSON.stringify({ price: 0, license: "own" }),
    });
    assertEquals(mine.status, 400, "and the owner still hears the whole refusal");
    assert((await mine.text()).includes("payments_token"), "naming the column");
  });

  // A key has no override: the publish simply does not happen. Personal data
  // has one, because a mailing list is a sheet somebody publishes on purpose.
  await t.step("Personal data goes public only when the publisher says it belongs there", async () => {
    const { jwt } = await usr("pia@example.com");
    // A sheet holding one contact detail, which is the accident this is about.
    const holding = async (value: string) => {
      const hand = automerge.create<Sheet>({
        type: "table",
        data: [
          arrayify([{ name: "who", type: "text", key: 0 }, { name: "contact", type: "text", key: 1 }]),
          { 0: "a customer", 1: value },
        ],
      });
      const id = `table:${hand.documentId}`;
      await put(jwt, `/library/${id}`, {});
      return id;
    };
    const publish = (id: string, body: unknown) =>
      app.request(`/library/${id}/public`, {
        method: "POST",
        headers: new Headers({ "Content-Type": "application/json", Authorization: `Bearer ${jwt}` }),
        body: JSON.stringify(body),
      });

    for (
      const [value, shape] of [
        ["someone@example.com", "email address"],
        // E.164, and then the way North America writes one.
        ["+14155552671", "phone number"],
        ["(415) 555-2671", "phone number"],
        ["078-05-1120", "social security number"],
        // The issuer's own test number: sixteen digits that pass Luhn.
        ["4111 1111 1111 1111", "payment card number"],
      ]
    ) {
      const id = await holding(value);
      const refused = await publish(id, { public: true });
      assertEquals(refused.status, 400, `a sheet holding ${shape} does not go public`);
      const text = await refused.text();
      assert(text.includes(shape), `the refusal names the shape it matched: ${text}`);
      assert(text.includes("contact"), `and the column it is in: ${text}`);
      assert(text.includes("row 1"), `and the row, 1-based: ${text}`);
      assert(!text.includes(value), "no refusal is an oracle: the value never comes back");
      assertEquals((await publish(id, { public: true, personal: true })).status, 200, `${shape}: a claim publishes it`);
    }

    // A run of digits is an order id at least as often as it is a card, and the
    // checksum is the whole of what separates them.
    const orders = await holding("4111 1111 1111 1112");
    assertEquals((await publish(orders, { public: true })).status, 200, "digits that fail Luhn are not a card");

    // A credential keeps its refusal whatever the publisher claims.
    const keyed = await holding("sk_live_" + "0000EXAMPLEKEYNOTREAL0000");
    const kept = await publish(keyed, { public: true, personal: true });
    assertEquals(kept.status, 400, "a key is not personal data and has no override");
    assert((await kept.text()).includes("Stripe"), "and the refusal is still about the key");

    // A listing is the other door a stranger reads a sheet through.
    const listed = await holding("someone@example.com");
    await reject(jwt, `/sell/${listed}`, { method: "POST", body: JSON.stringify({ price: 0, license: "own" }) });
    await post(jwt, `/sell/${listed}`, { price: 0, license: "own", personal: true });

    // Not read for its truthiness: a sheet must not go out on the word "no".
    const said = await publish(listed, { public: true, personal: "yes" });
    assertEquals(said.status, 400, "a claim that is not a boolean is refused rather than believed");
    assert((await said.text()).includes("personal"), "naming the field");

    // An explicit null is not the field being absent: `??` reads both the same
    // way, and would have let a sheet go out on a `personal` nobody set to
    // anything -- exactly the "no" this function exists to never read from
    // silence. `isPublic` two lines above this one in the route already
    // refuses null with a plain `typeof`; `personal` must refuse it the same
    // way rather than default through it.
    const nulled = await publish(listed, { public: true, personal: null });
    assertEquals(nulled.status, 400, "null is not a claim, the way absent is not a claim, but they are not each other");
    assert((await nulled.text()).includes("personal"), "naming the field");
  });

  // The refs are in the documents and nowhere else, and nothing could read them
  // without opening every query by hand.
  await t.step("What feeds a sheet", async () => {
    const { jwt, usr_id } = await usr("lina@example.com");
    const table = automerge.create<Sheet>({
      type: "table",
      data: [arrayify([{ name: "n", type: "num", key: 0 }]), { 0: 1 }],
    });
    const tableId = `table:${table.documentId}`;
    await put(jwt, `/library/${tableId}`, { name: "numbers" });

    // A self-join names the same sheet twice, which is one dependency.
    const query = automerge.create<{ data: Sheet["data"] }>({
      data: [{ lang: "sql", code: `select a.n from @${tableId} a join @${tableId} b on a.n = b.n`, args: [] }],
    });
    const queryId = `query:${query.documentId}`;
    await put(jwt, `/library/${queryId}`, { name: "joined" });

    const alert = automerge.create<{ data: [{ code: string; to: string; interval: number }] }>({
      // A condition that answers nothing: every later step's pollAlertOnce polls
      // every due alert there is, and one that fires would be counted as a
      // delivery by the step that counts deliveries.
      data: [{ code: `select * from @${queryId} where n < 0`, to: "lina@example.com", interval: 3600 }],
    });
    const alertId = `alert:${alert.documentId}`;
    await put(jwt, `/library/${alertId}`, { name: "watched" });

    const chart = automerge.create<{ data: [{ source: string; kind: string; x: string; y: string }] }>({
      data: [{ source: `@${tableId}`, kind: "line", x: "n", y: "n" }],
    });
    const chartId = `chart:${chart.documentId}`;
    await put(jwt, `/library/${chartId}`, { name: "drawn" });

    // Two tables that share a column name, which is the pair the honesty rule
    // is about: `qty` written bare in a join over both belongs to neither.
    const catalogue = automerge.create<Sheet>({
      type: "table",
      data: [
        arrayify([
          { name: "qty", type: "num", key: 0 },
          { name: "price", type: "num", key: 1 },
          { name: "total", type: "num", key: 2 },
          // A column named as a word AlaSQL will not parse bare, which is why a
          // statement that reads one writes it in brackets.
          { name: "end", type: "text", key: 3 },
        ]),
        { 0: 2, 1: 3, 2: 6, 3: "friday" },
      ],
    });
    const catalogueId = `table:${catalogue.documentId}`;
    await put(jwt, `/library/${catalogueId}`, { name: "catalogue" });

    const restock = automerge.create<Sheet>({
      type: "table",
      data: [
        arrayify([{ name: "qty", type: "num", key: 0 }, { name: "eta", type: "text", key: 1 }]),
        { 0: 2, 1: "friday" },
      ],
    });
    const restockId = `table:${restock.documentId}`;
    await put(jwt, `/library/${restockId}`, { name: "restock" });

    const picked = automerge.create<{ data: Sheet["data"] }>({
      data: [{ lang: "sql", code: `select qty, price, [end] from @${catalogueId}`, args: [] }],
    });
    const pickedId = `query:${picked.documentId}`;
    await put(jwt, `/library/${pickedId}`, { name: "picked" });

    const everything = automerge.create<{ data: Sheet["data"] }>({
      data: [{ lang: "sql", code: `select * from @${catalogueId}`, args: [] }],
    });
    const everythingId = `query:${everything.documentId}`;
    await put(jwt, `/library/${everythingId}`, { name: "everything" });

    // The third ref is a sheet this caller holds no role on, so its columns are
    // unreadable -- and reading them is not what decides the other two.
    const strange = "table:not-a-sheet-of-mine";
    const mixed = automerge.create<{ data: Sheet["data"] }>({
      data: [{
        lang: "sql",
        code: `select r.eta, c.price, m.mystery from @${catalogueId} c
               join @${restockId} r on c.qty = r.qty join @${strange} m on m.qty = c.qty`,
        args: [],
      }],
    });
    const mixedId = `query:${mixed.documentId}`;
    await put(jwt, `/library/${mixedId}`, { name: "mixed" });

    // chartSql writes every column it names in brackets, and `total` is one of
    // the words AlaSQL will not parse bare.
    const plotted = automerge.create<{ data: [{ source: string; kind: string; x: string; y: string }] }>({
      data: [{ source: `@${catalogueId}`, kind: "line", x: "qty", y: "total" }],
    });
    const plottedId = `chart:${plotted.documentId}`;
    await put(jwt, `/library/${plottedId}`, { name: "plotted" });

    // A box chart aggregates in its own query, so its statement carries five
    // aliases (`lo`, `q1`, `med`, `q3`, `hi`) beside its real `x` and `y` --
    // and none of those aliases is a column this chart actually read.
    const boxed = automerge.create<{ data: [{ source: string; kind: string; x: string; y: string }] }>({
      data: [{ source: `@${catalogueId}`, kind: "box", x: "qty", y: "price" }],
    });
    const boxedId = `chart:${boxed.documentId}`;
    await put(jwt, `/library/${boxedId}`, { name: "boxed" });

    // An alias spelled the same as a real column of the ref it reads, on a
    // column that is not that one: `total` is aliased onto `qty`, so renaming
    // catalogue's own `total` breaks nothing here.
    const aliased = automerge.create<{ data: Sheet["data"] }>({
      data: [{ lang: "sql", code: `select qty as total from @${catalogueId}`, args: [] }],
    });
    const aliasedId = `query:${aliased.documentId}`;
    await put(jwt, `/library/${aliasedId}`, { name: "aliased" });

    // A column named outside ASCII, read back through the bracket syntax
    // chartSql itself writes with.
    const foreign = automerge.create<Sheet>({
      type: "table",
      data: [arrayify([{ name: "café", type: "text", key: 0 }]), { 0: "loyal" }],
    });
    const foreignId = `table:${foreign.documentId}`;
    await put(jwt, `/library/${foreignId}`, { name: "foreign" });

    const unicoded = automerge.create<{ data: Sheet["data"] }>({
      data: [{ lang: "sql", code: `select [café] from @${foreignId}`, args: [] }],
    });
    const unicodedId = `query:${unicoded.documentId}`;
    await put(jwt, `/library/${unicodedId}`, { name: "unicoded" });

    // A statement past namesIn's own MAX_NAMES bound: this dependent's row
    // must still come back, with an honest "?" rather than taking the whole
    // account's lineage read down with it.
    const sprawling = automerge.create<{ data: Sheet["data"] }>({
      data: [{
        lang: "sql",
        code: `select ${Array.from({ length: MAX_NAMES + 1 }, (_, i) => `col${i}`).join(", ")} from @${catalogueId}`,
        args: [],
      }],
    });
    const sprawlingId = `query:${sprawling.documentId}`;
    await put(jwt, `/library/${sprawlingId}`, { name: "sprawling" });

    // A table this caller holds a role on, so `mine` carries it, but no
    // automerge document backs it: readColumns' own load must answer "?"
    // rather than the earlier "outside the library" case, which never opens
    // one at all.
    const ghost_table_doc = `ghost-table-${crypto.randomUUID().replaceAll("-", "")}`;
    const [ghost_table] = await sql`
      insert into sheet (type, doc_id, name, created_by, row_0)
      values ('table', ${ghost_table_doc}, 'ghost table', ${usr_id}, ${sql.json({})})
      returning sheet_id
    `;
    await sql`insert into sheet_usr (sheet_id, usr_id, role) values (${ghost_table.sheet_id}, ${usr_id}, 'owner')`;
    const ghostTableId = String(ghost_table.sheet_id);
    const overGhost = automerge.create<{ data: Sheet["data"] }>({
      data: [{ lang: "sql", code: `select qty from @${ghostTableId}`, args: [] }],
    });
    const overGhostId = `query:${overGhost.documentId}`;
    await put(jwt, `/library/${overGhostId}`, { name: "over ghost" });

    // A sheet whose document never arrived. Claimed by hand, because the claim
    // is the one route that refuses an id with no document behind it.
    const ghost_doc = `ghost-${crypto.randomUUID().replaceAll("-", "")}`;
    const [ghost] = await sql`
      insert into sheet (type, doc_id, name, created_by, row_0)
      values ('query', ${ghost_doc}, 'lost', ${usr_id}, ${sql.json({})})
      returning sheet_id
    `;
    await sql`insert into sheet_usr (sheet_id, usr_id, role) values (${ghost.sheet_id}, ${usr_id}, 'owner')`;

    // A source table that happens to hold a column literally named `active` --
    // a name cohortSql's own generated SQL also uses, for its own count, on
    // every cohort table it writes, with no way to spell it differently.
    const orders = automerge.create<Sheet>({
      type: "table",
      data: [
        arrayify([
          { name: "customer_id", type: "num", key: 0 },
          { name: "ordered_on", type: "text", key: 1 },
          { name: "active", type: "num", key: 2 },
        ]),
        { 0: 1, 1: "2026-01-01", 2: 1 },
      ],
    });
    const ordersId = `table:${orders.documentId}`;
    await put(jwt, `/library/${ordersId}`, { name: "orders" });

    const cohorted = automerge.create<{ data: Sheet["data"] }>({
      data: [{
        lang: "sql",
        code: cohortSql({ source: `@${ordersId}`, date: "ordered_on", key: "customer_id", value: "", grain: "month" }),
        args: [],
      }],
    });
    const cohortedId = `query:${cohorted.documentId}`;
    await put(jwt, `/library/${cohortedId}`, { name: "cohorted" });

    // A window whose own alias is read again by the statement that wraps it,
    // the same shape a cohort table's `group by cohort` reads its own alias.
    const trended = automerge.create<{ data: Sheet["data"] }>({
      data: [{
        lang: "sql",
        code:
          `select customer_id, active, trend(active, 2) over (order by customer_id) as trend from @${ordersId} order by trend`,
        args: [],
      }],
    });
    const trendedId = `query:${trended.documentId}`;
    await put(jwt, `/library/${trendedId}`, { name: "trended" });

    const [cols, ...rows] = await get<Table>(jwt, "/sheet/library:lineage");
    assertEquals(
      Object.values(cols).map((c) => (c as Col).name).join(),
      "sheet_id,name,type,depends_on,depends_on_name,depends_on_type,columns",
      "the lineage sheet has a stable shape, because a query sheet selects from it",
    );
    const edges = (id: string) => rows.filter((r) => String(r.sheet_id) === id);
    assertEquals(
      edges(queryId).map((r) => String(r.depends_on)),
      [tableId],
      "a query names the sheets it selects from",
    );
    assertEquals(String(edges(queryId)[0].depends_on_name), "numbers", "and what they are called");
    assertEquals(String(edges(queryId)[0].depends_on_type), "table");
    assertEquals(edges(alertId).map((r) => String(r.depends_on)), [queryId], "an alert's condition is a dependency");
    assertEquals(edges(chartId).map((r) => String(r.depends_on)), [tableId], "a chart's source is one too");
    assertEquals(edges(tableId), [], "a table holds its own cells and depends on nothing");

    // Which columns of the ref the edge is about: the names this statement
    // holds that exactly one of its own refs carries, or the two states that
    // are not a list of names.
    const claims = (id: string) => Object.fromEntries(edges(id).map((r) => [String(r.depends_on), r.columns]));
    assertEquals(
      claims(pickedId)[catalogueId],
      "end, price, qty",
      "a query claims the columns it names, sorted, a bracket-quoted keyword among them",
    );
    assertEquals(claims(queryId)[tableId], "n", "a qualified name is claimed as its column and not as its alias");
    assertEquals(claims(everythingId)[catalogueId], "*", "a select * is every column the ref holds");
    assertEquals(claims(alertId)[queryId], "*", "and says so over a ref whose own columns nothing could read");
    assertEquals(claims(mixedId)[catalogueId], "price", "a name both joined tables hold is claimed by neither");
    assertEquals(claims(mixedId)[restockId], "eta", "while the name only one of them holds still is");
    assertEquals(claims(mixedId)[strange], "?", "a ref outside this library is unread rather than guessed at");
    assertEquals(claims(plottedId)[catalogueId], "qty, total", "a chart's bracket-quoted axis is claimed bare");
    assertEquals(
      claims(boxedId)[catalogueId],
      "price, qty",
      "a box chart's five aggregate aliases are not columns it read",
    );
    assertEquals(
      claims(aliasedId)[catalogueId],
      "qty",
      "an alias spelled like a real column is not a claim on that column",
    );
    assertEquals(claims(unicodedId)[foreignId], "café", "a name outside ASCII is still one identifier");
    assertEquals(
      claims(sprawlingId)[catalogueId],
      "?",
      "a statement past namesIn's own bound reads as unknown, not as a crash",
    );
    assertEquals(
      claims(overGhostId)[ghostTableId],
      "?",
      "a ref this caller holds a role on, but whose document will not load, is unread rather than guessed at",
    );
    assertEquals(
      claims(cohortedId)[ordersId],
      "customer_id, ordered_on",
      "cohortSql's own aliases (cohort, first_seen, active, month_no), read again in its group by and order by, are " +
        "not columns it read -- even one the source table genuinely holds under that same name, active here",
    );
    assertEquals(
      claims(trendedId)[ordersId],
      "active, customer_id",
      "a window's own alias, read again by the statement wrapping it, is not a column it read",
    );

    const lost = edges(String(ghost.sheet_id));
    assertEquals(lost.length, 1, "a sheet nobody can trace is still in the graph");
    assertEquals(lost[0].depends_on, null, "with nothing it is known to depend on");
    assert(
      String(lost[0].name).includes("could not be read"),
      `and the reason in its name: ${String(lost[0].name)}`,
    );
    assertEquals(lost[0].columns, null, "and no claim about columns, because nothing read any");

    // The refs the read answers with are the ones the engine loads: a query
    // over the lineage sheet resolves the same way freshness does.
    const { data: counted } = await post(jwt, "/query", {
      lang: "sql",
      code: `select count(*) as n from @library:lineage where depends_on = '${tableId}'`,
    });
    assertEquals(Number((counted as Table)[1].n), 2, "the query and the chart both read that table");
    assertEquals(
      (await get<Table>(jwt, "/library/lineage")).length,
      rows.length + 1,
      "GET /library/lineage is the same sheet",
    );
  });

  // "A document that will not load" is only one of the ways a sheet's own code
  // fails to become refs -- a chart with no source, or a query with no code at
  // all, throws before scanRefs ever runs, and that throw sits outside the
  // ghost-document test above. One bad sheet must still be a row and not a
  // 500 that empties the whole account's graph, and a two-query cycle must
  // still be two one-hop edges: lineage never chases a ref to the sheet it
  // names, so there is no chain for a cycle to close.
  await t.step("Lineage survives a sheet whose own code cannot become refs", async () => {
    const { jwt } = await usr("bram@example.com");

    // A chart with none of the settings chartSql requires.
    const brokenChart = automerge.create<{ data: [Record<string, never>] }>({ data: [{}] });
    const brokenChartId = `chart:${brokenChart.documentId}`;
    await put(jwt, `/library/${brokenChartId}`, { name: "unset" });

    // A query document holding no "code" field at all -- not a document that
    // failed to load, but one that loaded and had nothing scanRefs could read.
    const codeless = automerge.create<{ data: [{ args: unknown[] }] }>({ data: [{ args: [] }] });
    const codelessId = `query:${codeless.documentId}`;
    await put(jwt, `/library/${codelessId}`, { name: "silent" });

    // Two queries naming each other. lineage reads one hop per sheet off its
    // own code, so a cycle is two ordinary edges and never a chain to detect
    // a cycle in.
    const a = automerge.create<{ data: Sheet["data"] }>({ data: [{ lang: "sql", code: "", args: [] }] });
    const aId = `query:${a.documentId}`;
    const b = automerge.create<{ data: Sheet["data"] }>({
      data: [{ lang: "sql", code: `select * from @${aId}`, args: [] }],
    });
    const bId = `query:${b.documentId}`;
    await put(jwt, `/library/${aId}`, { name: "a" });
    await put(jwt, `/library/${bId}`, { name: "b" });
    const aHandle = await automerge.find<{ data: [{ code: string; args: unknown[] }] }>(a.documentId);
    aHandle.change((doc) => {
      doc.data[0].code = `select * from @${bId}`;
    });

    const rows = await get<Table>(jwt, "/library/lineage");
    const edges = (id: string) => rows.filter((r) => String(r.sheet_id) === id);

    const chartRow = edges(brokenChartId);
    assertEquals(chartRow.length, 1, "a chart with no source is one row, not zero and not a crash");
    assertEquals(chartRow[0].depends_on, null);
    assert(
      String(chartRow[0].name).includes("could not be read"),
      `and the reason names what chartSql refused: ${String(chartRow[0].name)}`,
    );

    const codelessRow = edges(codelessId);
    assertEquals(codelessRow.length, 1, "a query with no code field is also one row");
    assertEquals(codelessRow[0].depends_on, null);
    assert(String(codelessRow[0].name).includes("could not be read"));

    assertEquals(edges(aId).map((r) => String(r.depends_on)), [bId], "a names b");
    assertEquals(edges(bId).map((r) => String(r.depends_on)), [aId], "b names a, and the read still finished");

    // A query that uses snippets depends on the snippet table, and on what its bodies read.
    const snipTable = automerge.create<Sheet>({
      type: "table",
      data: [
        arrayify([{ name: "name", type: "text", key: "0" }, { name: "expr", type: "text", key: "1" }]),
        { 0: "big", 1: `(select max(n) from @${aId.replace("query:", "table:")})` },
      ],
    });
    const snipId = `table:${snipTable.documentId}`;
    await put(jwt, `/library/${snipId}`, { name: "lineage snippets" });
    const dep = automerge.create<{ data: Sheet["data"] }>({
      data: [{ lang: "sql", code: `using @${snipId} select {{big}} as m`, args: [] }],
    });
    const depId = `query:${dep.documentId}`;
    await put(jwt, `/library/${depId}`, { name: "snippet user" });
    const after = (await get<Table>(jwt, "/library/lineage")).filter((r) => String(r.sheet_id) === depId);
    const snipEdge = after.find((r) => String(r.depends_on) === snipId);
    assertEquals(snipEdge?.columns, "expr, name", "the snippet table is an edge, read by its two columns");
    assert(
      after.some((r) => String(r.depends_on).startsWith("table:") && String(r.depends_on) !== snipId),
      "and a ref inside a body is an edge of its own",
    );
    // A query is a source too: its answer holds the bodies, and it is an edge beside the table.
    const answers = automerge.create<{ data: Sheet["data"] }>({
      data: [{ lang: "sql", code: `select 'one' as name, '(select 1)' as expr`, args: [] }],
    });
    const answersId = `query:${answers.documentId}`;
    await put(jwt, `/library/${answersId}`, { name: "lineage answers" });
    const both = automerge.create<{ data: Sheet["data"] }>({
      data: [{ lang: "sql", code: `using @${snipId}, @${answersId} select {{big}} as m, {{one}} as o`, args: [] }],
    });
    const bothId = `query:${both.documentId}`;
    await put(jwt, `/library/${bothId}`, { name: "two sources" });
    const sources = (await get<Table>(jwt, "/library/lineage")).filter((r) => String(r.sheet_id) === bothId);
    for (const id of [snipId, answersId])
      assertEquals(sources.find((r) => String(r.depends_on) === id)?.columns, "expr, name", `${id} is an edge`);
    const lost = automerge.create<{ data: Sheet["data"] }>({
      data: [{ lang: "sql", code: `using @table:not-mine select {{big}} as m`, args: [] }],
    });
    await put(jwt, `/library/query:${lost.documentId}`, { name: "snippet stranger" });
    const stranger = (await get<Table>(jwt, "/library/lineage")).filter((r) =>
      String(r.sheet_id) === `query:${lost.documentId}`
    );
    assertEquals(stranger.length, 1);
    assertEquals(stranger[0].depends_on, null);
    assert(String(stranger[0].name).includes("could not be read"), String(stranger[0].name));
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
      "sheet_id,name,type,last_run,last_ok,failures_since_ok,last_meta,paused,next_run",
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
    for (const status of ["disconnected", "__proto__", "toString", "constructor", 7, null, ["connected"], undefined])
      await reject(jwt, `/library/${sheet_id}/socket`, { method: "POST", body: JSON.stringify({ status }) });
    // A body that is not an object at all. JSON `null` parses, so the `.catch`
    // does not see it and destructuring it used to be a 500 anyone could send.
    for (const body of ["null", "[]", "7", '"connected"', "{", ""])
      await reject(jwt, `/library/${sheet_id}/socket`, { method: "POST", body });
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
    for (const id of minted)
      await reject(jwt, `/library/${id}/socket`, { method: "POST", body: JSON.stringify({ status: "connected" }) });
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

  // Writing a connection is rotating it: the row lands beside the one before it,
  // and a sheet reading through the old credential keeps reading while the new
  // one is still being granted at the far end.
  await t.step("A rotated credential does not break the sheets reading through it", async () => {
    const { jwt } = await usr("rota@example.com");
    const hand = automerge.create<Sheet>({ type: "codex-db", data: [] });
    const doc_id = hand.documentId;
    const sheet_id = `codex-db:${doc_id}`;
    await put(jwt, `/library/${sheet_id}`, { name: "the warehouse" });

    const live = CODEX_DSN;
    // A port nobody listens on, so the socket is refused as fast as the loop
    // can ask -- and asked of an address this suite already holds the net
    // permission for, rather than of a hostname a resolver has to answer for.
    const dead = (port: number) => `postgresql://postgres:p4ssw0rd@127.0.0.1:${port}/postgres`;
    const meta = async () => {
      const [run] = await sql`select meta from net where sheet_id = ${sheet_id} order by net_id desc limit 1`;
      return run?.meta as { status: number; rolled_over: boolean };
    };

    // One credential, and it answers with the far database's own schema.
    const opened = async () =>
      (await get<Table>(jwt, `/codex/${sheet_id}`)).slice(1).some((row) => row.name === "widget");
    await post(jwt, `/codex-db/${doc_id}`, { dsn: live });
    assert(await opened(), "the stored credential must open the far schema");
    assertEquals((await meta()).rolled_over, false, "the current credential is what answered");

    // The rollover. The credential just written cannot connect, the one before
    // it still can, and the sheets reading through this codex do not notice --
    // while the run row says the newest one is dead, which is what freshness
    // and the sheet's own log read back.
    await post(jwt, `/codex-db/${doc_id}`, { dsn: dead(DEAD_PORTS[0]) });
    assert(await opened(), "the previous credential holds the read up");
    assertEquals((await meta()).rolled_over, true, "and the run names which one answered");
    // The run's body is why the newest credential could not connect, for the
    // members who read this sheet and can fix it -- never any part of it.
    const [rolled] = await sql`select body from net where sheet_id = ${sheet_id} order by net_id desc limit 1`;
    assert(String(rolled.body).length > 0, "a rolled-over run says why the newest credential did not get in");
    assert(!String(rolled.body).includes("p4ssw0rd"), `and carries no part of it: ${rolled.body}`);
    const fresh = (await get<Table>(jwt, `/library/freshness`)).slice(1).find((r) => r.sheet_id === sheet_id);
    assert(fresh, "a codex connection is a run, so it has a freshness");
    assertEquals(JSON.parse(String(fresh.last_meta)).rolled_over, true, "which freshness carries");
    // Graded failed, though the read answered: a dead newest credential under
    // a green freshness would be heard about when the next rotation retires
    // the one holding the sheet up.
    assert(fresh.last_ok !== fresh.last_run, "a rolled-over run is not a good run");

    // A third write evicts the oldest, so the rollover ends somewhere visible:
    // both stored credentials are dead now, and the read is the failure it
    // always was -- counting what it tried and naming none of it.
    await post(jwt, `/codex-db/${doc_id}`, { dsn: dead(DEAD_PORTS[1]) });
    const [{ n }] = await sql`select count(*)::int as n from db where sheet_id = ${sheet_id}`;
    assertEquals(n, DSN_KEEP, "a sheet keeps the current credential and the one before it, and no more");
    const res = await app.request(`/codex/${sheet_id}`, { headers: new Headers({ Authorization: `Bearer ${jwt}` }) });
    assertEquals(res.status, 502, "both credentials failed, so the connection did");
    const said = await res.text();
    assert(said.includes(`${DSN_KEEP} stored credentials`), `the refusal says how many it tried: ${said}`);
    assert(!said.includes("p4ssw0rd"), `and it carries no part of a credential: ${said}`);
    assertEquals((await meta()).status, 502, "a failure is a run like any other");

    // What the rollover is allowed to be spent on. A far server that answered
    // about the statement it was sent is a credential that works, and running
    // that statement again under the previous one would answer from a
    // connection nobody rotated to.
    assert(cannotConnect(new Error("write ECONNREFUSED 10.0.0.1:5432")), "a socket that never opened is not an answer");
    assert(
      cannotConnect(new pg.PostgresError({ message: "password authentication failed", code: "28P01" })),
      "and neither is a server saying the credential is not in",
    );
    assert(
      !cannotConnect(new pg.PostgresError({ message: "relation widget does not exist", code: "42P01" })),
      "a statement the far server answered about is a credential that works",
    );

    // Downstream. A query selecting from the codex is refused by name, so a
    // dead credential cannot read as a far database with nothing in it -- an
    // empty answer here is a join that silently empties whatever is built on it.
    const query = await app.request(`/query`, {
      method: "POST",
      headers: new Headers({ "Content-Type": "application/json", Authorization: `Bearer ${jwt}` }),
      body: JSON.stringify({ lang: "sql", code: `select * from @${sheet_id}`, args: [] }),
    });
    const refused = await query.text();
    assertEquals(query.status, 400, `a query over a codex must refuse: ${refused}`);
    assert(refused.includes(sheet_id), `and the refusal must name the codex sheet: ${refused}`);
  });

  // A newest credential the guard refuses -- unparseable, or aimed at this
  // server's own database -- is not a rollover even with a good credential
  // sitting right behind it: both refusals are about the credential itself, and
  // rolling past one is how a sheet ends up quietly held up by a credential
  // nobody remembers writing. The guard compares URL.hostname text, and
  // postgres: is a "non-special" scheme to that parser, so it does not fold
  // "127.1" or "2130706433" down to the one spelling the block list holds --
  // though the kernel dialer that opens the socket treats every one of them as
  // this machine's own loopback.
  await t.step("A newest credential aimed at this server refuses rather than rolling over", async () => {
    const { jwt } = await usr("selfaim@example.com");
    const hand = automerge.create<Sheet>({ type: "codex-db", data: [] });
    const doc_id = hand.documentId;
    const sheet_id = `codex-db:${doc_id}`;
    await put(jwt, `/library/${sheet_id}`, { name: "self-aimed" });
    const read = () => app.request(`/codex/${sheet_id}`, { headers: new Headers({ Authorization: `Bearer ${jwt}` }) });

    await post(jwt, `/codex-db/${doc_id}`, { dsn: CODEX_DSN });
    assert(
      (await get<Table>(jwt, `/codex/${sheet_id}`)).slice(1).some((row) => row.name === "widget"),
      "the good credential must open the far schema before the rotation",
    );

    // Another spelling of the host and port this server's own DATABASE_URL
    // falls back to in this suite, with that working credential right behind it.
    await post(jwt, `/codex-db/${doc_id}`, { dsn: `postgresql://postgres@127.1:${DB_PORT}/postgres` });
    const spelled = await read();
    const spelledSaid = await spelled.text();
    assertEquals(spelled.status, 403, `"127.1" is "127.0.0.1" to the socket that would open: ${spelledSaid}`);
    assert(spelledSaid.includes("own database"), `the refusal must name what it refused: ${spelledSaid}`);
    assert(
      !spelledSaid.includes("widget"),
      `a refused newest credential must never answer with the older one's schema: ${spelledSaid}`,
    );
    assert(
      !spelledSaid.includes("sheet_usr"),
      `and a bypassed guard would answer with this server's own table names: ${spelledSaid}`,
    );

    // The literal spelling, which is the same refusal by the same guard.
    await post(jwt, `/codex-db/${doc_id}`, { dsn: OWN_DSN });
    assertEquals((await read()).status, 403, "the host on record refuses like every other spelling of it");

    // IPv4 mapped into IPv6, which the URL parser writes as [::ffff:7f00:1]
    // and the dialer opens as 127.0.0.1: neither the block list's spelling nor
    // the app's own, until it is folded back.
    await post(jwt, `/codex-db/${doc_id}`, { dsn: `postgresql://postgres@[::ffff:127.0.0.1]:${DB_PORT}/postgres` });
    assertEquals((await read()).status, 403, "an IPv4-mapped spelling of the host on record refuses too");

    // On a server whose own database is somewhere else, a credential aimed
    // inside this server's network is refused the way safeFetch refuses a url
    // there: the cloud's metadata address is a Postgres nobody meant to open.
    // This suite's own database is on loopback, so the guard is asked only
    // while DATABASE_URL says otherwise.
    const was = Deno.env.get("DATABASE_URL");
    Deno.env.set("DATABASE_URL", "postgresql://app@db.example.net:5432/app");
    try {
      await post(jwt, `/codex-db/${doc_id}`, { dsn: "postgresql://postgres@169.254.169.254:5432/postgres" });
      const inside = await read();
      const insideSaid = await inside.text();
      assertEquals(inside.status, 400, `a private address is refused before it is dialled: ${insideSaid}`);
      assert(insideSaid.includes("inside this server's own network"), insideSaid);
    } finally {
      if (was === undefined) Deno.env.delete("DATABASE_URL");
      else Deno.env.set("DATABASE_URL", was);
    }
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

  await t.step("You preview the first rows of one far table without writing SQL", async () => {
    const { jwt } = await usr("peek@example.com");
    const { jwt: stranger } = await usr("peeker@example.com");
    const hand = automerge.create<Sheet>({ type: "codex-db", data: [] });
    const doc_id = hand.documentId;
    const sheet_id = `codex-db:${doc_id}`;
    await put(jwt, `/library/${sheet_id}`, { name: "the shop floor" });
    await post(jwt, `/codex-db/${doc_id}`, { dsn: CODEX_DSN });
    const preview = (query: string, who = jwt) => {
      rateLimitBuckets.clear();
      return app.request(`/codex/${sheet_id}/preview?${query}`, {
        headers: new Headers({ Authorization: `Bearer ${who}` }),
      });
    };
    const runs = async () =>
      (await sql`select meta from net where sheet_id = ${sheet_id} order by net_id`).map((r: { meta: unknown }) =>
        r.meta as { status: number; rolled_over: boolean }
      );

    const [cols, ...rows] = await get<Table>(jwt, `/codex/${sheet_id}/preview`, { table: "widget" });
    assertEquals(Object.values(cols), [
      { name: "widget_id", type: "text", key: "widget_id" },
      { name: "name", type: "text", key: "name" },
    ], "the columns in table order, typed text as GET /codex/:id types them");
    assertEquals(rows, [{ widget_id: 1, name: "bolt" }, { widget_id: 2, name: "nut" }], "rows keyed by name");
    const [{ n: audited }] = await sql`
      select count(*)::int as n from audit where sheet_id = ${sheet_id} and action = 'GET /codex/:id/preview'
    `;
    assertEquals(audited, 1, "one audit row for the one preview");
    assertEquals((await runs()).length, 1, "one run row for the one connection");
    assertEquals((await get<Table>(jwt, `/codex/${sheet_id}/preview`, { table: "widget", limit: 1 })).length, 2);

    const filtered = async (query: string) => {
      const res = await preview(query);
      const said = await res.text();
      assertEquals(res.status, 200, said);
      return JSON.parse(said).data.slice(1) as Record<string, unknown>[];
    };
    assertEquals(await filtered("table=widget&where=name.eq.nut&where=widget_id.gte.2"), [{
      widget_id: 2,
      name: "nut",
    }]);
    assertEquals((await filtered("table=gadget&where=note.isnull")).map((r) => r.gadget_id), [1]);
    assertEquals((await filtered("table=gadget&where=note.notnull&where=note.like.v1.%25")).map((r) => r.gadget_id), [
      2,
    ]);
    assertEquals(
      (await filtered("table=gadget&where=note.eq.v1.5")).map((r) => r.gadget_id),
      [2],
      "a value holds a dot",
    );
    const { jwt: viewer } = await usr("peekview@example.com");
    await post(jwt, `/library/${sheet_id}/share`, { email: "peekview@example.com", role: "viewer" });
    const viewed = await preview("table=widget&where=name.eq.nut", viewer);
    assertEquals(viewed.status, 403, "a viewer may not filter");
    await viewed.text();

    for (
      const [query, needle] of [
        [
          "table=widget&where=name.eq.a&where=name.eq.b&where=name.eq.c&where=name.eq.d&where=name.eq.e&where=name.eq.f",
          "at most 5 where filters",
        ],
        ["table=widget&where=name", "<column>.<op>.<value>"],
        ["table=widget&where=name.eq", "<column>.<op>.<value>"],
        ["table=widget&where=name.regex.x", "one of eq, neq"],
        ["table=widget&where=name.isnull.x", "takes no value"],
        ["table=widget&where=nam.eq.x", "Did you mean"],
        ["table=widget&where=widget_id.eq.abc", "cannot apply that filter"],
        ["table=widget&where=widget_id.like.1", "cannot apply that filter"],
        ["table=widget%3B%20drop%20table%20widget", "is not in the database"],
        ["table=Widget", "Did you mean"],
        ["table=odd.name", "cannot be previewed by name"],
        ["", "Name the table"],
        ["table=", "Name the table"],
        ...["0", "-1", "1.5", "1e2", "abc", "", String(CODEX_PREVIEW_MAX + 1)].map((limit) =>
          [`table=widget&limit=${limit}`, "not a preview"] as const
        ),
      ] as const
    ) {
      const res = await preview(query);
      const said = await res.text();
      assertEquals(res.status, 400, `?${query} must refuse: ${said}`);
      assert(said.includes(needle), `?${query} must say ${needle}: ${said}`);
    }
    // Five before the filters, four filtered reads, and the three refusals that
    // had to connect (an unknown column, a cast, an op its type lacks).
    assertEquals(
      (await runs()).map((run: { status: number }) => run.status),
      Array(12).fill(200),
      "a refused name, column or cast is one good run; a refused limit, shape or op is none",
    );

    const shut = await preview("table=widget", stranger);
    assertEquals(shut.status, 403);
    assert((await shut.text()).includes("read access"), "a stranger gets the access refusal");

    // A dead newest credential rolls over exactly as GET /codex/:id does.
    await post(jwt, `/codex-db/${doc_id}`, {
      dsn: `postgresql://postgres:p4ssw0rd@127.0.0.1:${DEAD_PORTS[0]}/postgres`,
    });
    assertEquals((await get<Table>(jwt, `/codex/${sheet_id}/preview`, { table: "widget" })).length, 3);
    assertEquals((await runs()).at(-1)?.rolled_over, true, "the run names which credential answered");
    await post(jwt, `/codex-db/${doc_id}`, {
      dsn: `postgresql://postgres:p4ssw0rd@127.0.0.1:${DEAD_PORTS[1]}/postgres`,
    });
    const dead = await preview("table=widget");
    const said = await dead.text();
    assertEquals(dead.status, 502, `a dead connection is a refusal, never an empty answer: ${said}`);
    assert(said.includes(`${DSN_KEEP} stored credentials`) && !said.includes("p4ssw0rd"), said);
    assertEquals((await runs()).at(-1)?.status, 502, "a failure is a run like any other");
  });

  await t.step("You query a connected database's table with a ref", async () => {
    const { jwt } = await usr("refer@example.com");
    const { jwt: viewer } = await usr("referview@example.com");
    const hand = automerge.create<Sheet>({ type: "codex-db", data: [] });
    const sheet_id = `codex-db:${hand.documentId}`;
    await put(jwt, `/library/${sheet_id}`, { name: "the shop floor, by ref" });
    await post(jwt, `/codex-db/${hand.documentId}`, { dsn: CODEX_DSN });
    await post(jwt, `/library/${sheet_id}/share`, { email: "referview@example.com", role: "viewer" });
    const ask = async (code: string, who = jwt) => {
      rateLimitBuckets.clear();
      const res = await app.request("/query", {
        method: "POST",
        headers: new Headers({ "Content-Type": "application/json", Authorization: `Bearer ${who}` }),
        body: JSON.stringify({ lang: "sql", code, args: [] }),
      });
      return { status: res.status, text: await res.text() };
    };
    const rowsOf = (r: { text: string }) => JSON.parse(r.text).data.slice(1);
    const runs = async () => (await sql`select 1 from net where sheet_id = ${sheet_id}`).length;

    const bare = await ask(`select name from @${sheet_id}/widget order by widget_id`);
    assertEquals(bare.status, 200, bare.text);
    assertEquals(rowsOf(bare), [{ name: "bolt" }, { name: "nut" }]);
    assertEquals(await runs(), 1, "one run row for the one read");
    const [{ n: audited }] = await sql`
      select count(*)::int as n from audit where sheet_id = ${sheet_id} and action = 'query'
    `;
    assertEquals(audited, 1, "one audit row, under the codex sheet");

    const one = await ask(`select gadget_id from @${sheet_id}/gadget?note.eq.v1.5`);
    assertEquals(rowsOf(one), [{ gadget_id: 2 }], "a filter, and a value that holds a dot");
    const two = await ask(`select gadget_id from @${sheet_id}/gadget?note.notnull&gadget_id.gte.3`);
    assertEquals(rowsOf(two), [{ gadget_id: 3 }], "filters are AND-ed");
    const spaced = await ask(`select n from @${sheet_id}/crowd?tag.eq.in%20stock`);
    assertEquals(rowsOf(spaced), [{ n: 7 }], "%20 spells a space");
    const joined = await ask(
      `select w.name, g.note from @${sheet_id}/widget w join @${sheet_id}/gadget?gadget_id.eq.2 g on w.widget_id = g.gadget_id`,
    );
    assertEquals(rowsOf(joined), [{ name: "nut", note: "v1.5" }], "two refs to one database join");

    for (
      const [code, needle] of [
        [`select * from @${sheet_id}/crowd`, `more than ${CODEX_PREVIEW_MAX} rows`],
        [`select * from @${sheet_id}/widget?name.between.1`, "That filter op is not one this server knows"],
        [`select * from @${sheet_id}/widget?nam.eq.x`, "Did you mean"],
        [`select * from @${sheet_id}/widgit`, "Did you mean"],
        [`select * from @${sheet_id}/widget?name.eq.%FF`, "valid escapes"],
        [`select * from @${sheet_id}/widget?${Array(6).fill("name.eq.a").join("&")}`, "at most 5 where filters"],
      ] as const
    ) {
      const refused = await ask(code);
      assertEquals(refused.status, code.includes("crowd") ? 413 : 400, `${code}: ${refused.text}`);
      assert(refused.text.includes(needle), `${code} must say ${needle}: ${refused.text}`);
    }
    assertEquals(
      rowsOf(await ask(`select count(*) as n from @${sheet_id}/crowd?n.lte.${CODEX_PREVIEW_MAX}`)),
      [{ n: CODEX_PREVIEW_MAX }],
      "a read of exactly the cap is not past it",
    );

    const shut = await ask(`select * from @${sheet_id}/widget`, viewer);
    assertEquals(shut.status, 403, "a viewer may not read a table by ref");
    assert(shut.text.includes("owner or editor"), shut.text);

    // Lineage: an edge to the codex sheet, named by the table, one per table.
    const query = automerge.create<{ data: Sheet["data"] }>({
      data: [{
        lang: "sql",
        code: `select * from @${sheet_id}/widget union all select * from @${sheet_id}/widget?name.eq.nut`,
        args: [],
      }],
    });
    await put(jwt, `/library/query:${query.documentId}`, { name: "by ref" });
    const edges = (await get<Table>(jwt, "/library/lineage")).slice(1).filter((r) =>
      String(r.sheet_id) === `query:${query.documentId}`
    );
    assertEquals(edges.map((r) => [r.depends_on, r.depends_on_name, r.depends_on_type, r.columns]), [
      [sheet_id, "widget", "codex-db", "*"],
    ]);
  });

  await t.step("A query opens a bounded number of far connections", async () => {
    const { jwt } = await usr("brkref@example.com");
    const hand = automerge.create<Sheet>({ type: "codex-db", data: [] });
    const sheet_id = `codex-db:${hand.documentId}`;
    await put(jwt, `/library/${sheet_id}`, { name: "break by ref" });
    await post(jwt, `/codex-db/${hand.documentId}`, { dsn: CODEX_DSN });
    const ask = async (n: number) => {
      rateLimitBuckets.clear();
      const refs = Array.from(
        { length: n },
        (_, i) => `(select count(*) from @${sheet_id}/widget?name.like.x%25${i}) c${i}`,
      );
      const res = await app.request("/query", {
        method: "POST",
        headers: new Headers({ "Content-Type": "application/json", Authorization: `Bearer ${jwt}` }),
        body: JSON.stringify({ lang: "sql", code: `select ${refs.join(", ")}`, args: [] }),
      });
      return { status: res.status, text: await res.text() };
    };
    const runs = async () => (await sql`select 1 from net where sheet_id = ${sheet_id}`).length;
    assertEquals((await ask(MAX_REF_DEPTH)).status, 200, "the bound itself passes");
    const before = await runs();
    const over = await ask(MAX_REF_DEPTH + 1);
    assertEquals(over.status, 400, over.text);
    assert(over.text.includes(`at most ${MAX_REF_DEPTH}`), over.text);
    assertEquals(await runs(), before, "a refused query opens no connection");
  });

  // Two sockets into the far database at once, which only the lock in serve()
  // keeps apart.
  await t.step("Two previews at once do not cross-wire their answers", async () => {
    const { jwt } = await usr("breaker@example.com");
    const hand = automerge.create<Sheet>({ type: "codex-db", data: [] });
    const doc_id = hand.documentId;
    const sheet_id = `codex-db:${doc_id}`;
    await put(jwt, `/library/${sheet_id}`, { name: "the shop floor, dented" });
    await post(jwt, `/codex-db/${doc_id}`, { dsn: CODEX_DSN });

    const [{ n: before }] = await sql`
      select count(*)::int as n from audit where sheet_id = ${sheet_id} and action = 'GET /codex/:id/preview'
    `;
    rateLimitBuckets.clear();
    const concurrent = (table: string) =>
      app.request(`/codex/${sheet_id}/preview?table=${table}`, {
        headers: new Headers({ Authorization: `Bearer ${jwt}` }),
      });
    const [widget, dotted] = await Promise.all([concurrent("widget"), concurrent("odd.name")]);
    const { data: a } = await widget.json();
    const dottedSaid = await dotted.text();
    assertEquals(a, [
      [{ name: "widget_id", type: "text", key: "widget_id" }, { name: "name", type: "text", key: "name" }],
      { widget_id: 1, name: "bolt" },
      { widget_id: 2, name: "nut" },
    ], "the widget preview must not answer with the other request's data");
    assert(dottedSaid.includes("cannot be previewed by name"), `and the dotted name must still refuse: ${dottedSaid}`);
    const [{ n: after }] = await sql`
      select count(*)::int as n from audit where sheet_id = ${sheet_id} and action = 'GET /codex/:id/preview'
    `;
    assertEquals(after - before, 1, "only the one that answered is audited");
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
        await pollFeed(id, undefined, Date.now() + 70_000_000);
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
    assertEquals(
      (await report({ reason: "relisted, still theirs" }, buyer)).status,
      409,
      "one report per account, still",
    );
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
    assert(
      spelled.includes(`codex-db:${conn.documentId} POST /codex-db/:id`),
      `a codex write is keyed by its sheet: ${spelled}`,
    );
    assert(
      spelled.includes(`net-hook:${hook.documentId} GET /net/:id`),
      `a bare doc id is keyed by its sheet: ${spelled}`,
    );

    // A sheet, so it exports and queries like one.
    const csv = await raw(owner, `/export/library:audit.csv`);
    assert(csv.ok, `export answered ${csv.status}`);
    assertEquals((await csv.text()).split("\n")[0], "created_at,sheet_id,who,action,via,detail");
    const { data: [, ...queried] }: { data: Table } = await post(owner, `/query`, {
      lang: "sql",
      code:
        `select action, count(*) as n from @library:audit where sheet_id = '${sheet_id}' group by action order by action`,
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
    const csv = await app.request(`/export/${sheet_id}.csv`, {
      headers: new Headers({ Authorization: `Bearer ${jwt}` }),
    });
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

    // Requests, whatever address they come from. The account's bucket is
    // emptied right before the request that must be refused and not before the
    // pair: it refills at RATE_LIMIT_REFILL_RATE, so a first request that took
    // longer than one token is worth handed the second one a token nobody
    // granted, and this step failed on a loaded machine and passed on an idle
    // one.
    assert((await raw(`/library`)).ok);
    accountBuckets.set(usr_id, { tokens: 0, lastRefill: Date.now() });
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
      from generate_series(1, ${USER_ALERTS_PER_DAY}) g
    `;
    let sent = 0;
    await pollAlert(alertId, () => {
      sent++;
      return Promise.resolve("sent");
    }, later);
    assertEquals(sent, 0, "nothing is sent past the account's daily quota");
    const [run] = (await get<Table>(jwt, `/sheet/${alertId}`)).slice(1);
    const delivery = String(JSON.parse(String(run.body)).delivery);
    assert(delivery.includes("quota") && delivery.includes(`${USER_ALERTS_PER_DAY}`), `the run names it: ${delivery}`);

    // A url delivery costs what an email does: one quota over both destinations,
    // so a cannon cannot be rebuilt by swapping the address for a webhook.
    alert.change((d: { data: [{ to: string }] }) => {
      d.data[0].to = "https://hooks.slack.com/services/T0/B0/zzz";
    });
    let posted = 0;
    await pollAlert(alertId, () => Promise.resolve("sent"), later + 120_000, () => {
      posted++;
      return Promise.resolve(new Response("ok"));
    });
    assertEquals(posted, 0, "a url delivery is refused past the same quota an email is");

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
    globalThis.fetch = ((url: string, init?: RequestInit) => {
      heard.push({ url, headers: Object.fromEntries(new Headers(init?.headers).entries()), body: String(init?.body) });
      return Promise.resolve(new Response("", { status: answer }));
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
        (await app.request(`/library/${sheet_id}/webhook`, {
          headers: new Headers({ Authorization: `Bearer ${viewer}` }),
        }))
          .status,
        403,
        "a receiver's url is a credential, so a viewer may not read it",
      );
      for (let i = 1; i < WEBHOOKS_PER_SHEET_MAX; i++)
        assertEquals((await register(`http://93.184.216.34/${i}`)).status, 201);
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
    const asked: { url: string; method?: string; body?: string; headers: Record<string, string> }[] = [];
    let answer: () => Response | Promise<Response> = () =>
      new Response(`{"ok":true}`, { status: 200, headers: { "content-type": "application/json" } });
    const origFetch = globalThis.fetch;
    globalThis.fetch = ((url: string, init?: RequestInit) => {
      asked.push({
        url,
        method: init?.method,
        body: init?.body as string | undefined,
        headers: Object.fromEntries(new Headers(init?.headers).entries()),
      });
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

      // The method and the body are the poller's too, templated the same way:
      // a pre-flight that sent a GET would be testing another request.
      const posting = { ...request, method: "POST", body: '{"key":"{{secret:weather}}","since":"{{cursor}}"}' };
      assertEquals((await (await test(posting)).json()).data.status, 200);
      assertEquals(asked.at(-1)?.method, "POST");
      assertEquals(
        asked.at(-1)?.body,
        '{"key":"sk-test","since":""}',
        "with the secret resolved, and nothing where a first poll's watermark would be",
      );
      const patched = await test({ ...request, method: "PATCH" });
      assertEquals(patched.status, 400, "a verb this server does not send is refused before it is sent");
      assert((await patched.text()).includes("PATCH"));

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
    const sheets = async () =>
      (await sql`select count(*)::int as n from sheet where created_by = (select usr_id from usr where email = 'guesser@example.com')`)[
        0
      ].n;

    // The preview: every column with the type it was guessed to carry, the
    // first rows under them, and no sheet.
    const preview = await send("/import/preview");
    assertEquals(preview.status, 200);
    const { data: seen } = await preview.json();
    assertEquals(seen.cols.map((c: { name: string; type: string }) => `${c.name}:${c.type}`), [
      "n:num",
      "flag:bool",
      "label:text",
    ]);
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
    assertEquals(
      (await send("/import/csv" + types({ ghost: "num" }))).status,
      400,
      "a column the file lacks is refused",
    );
    assertEquals((await send("/import/csv?types=not-json")).status, 400, "types that are not JSON are refused");
    assertEquals(await sheets(), 1, "and none of those made a sheet");

    // A text column of comma lists over few options is guessed as multi:, options in first-seen order, and imports as arrays.
    const picks = 'id,tags,note\n1,"red, blue",x\n2,green,y\n3,,z\n4,"blue,green",w\n';
    const guess = await (await send("/import/preview", picks)).json();
    assertEquals(
      guess.data.cols.map((c: { type: string }) => c.type),
      ["num", "multi:red,blue,green", "text"],
    );
    const multi = await send("/import/csv" + types({ tags: "multi:red,blue,green" }), picks);
    assertEquals(multi.status, 201);
    const { sheet_id: multi_id } = await multi.json();
    assertEquals((await get<Table>(jwt, `/sheet/${multi_id}`)).slice(1).map((r) => r.tags), [
      ["red", "blue"],
      ["green"],
      null,
      ["blue", "green"],
    ]);
    const spec = await request(jwt, `/openapi/${multi_id}`);
    assertEquals(spec.components.schemas.Row.properties.tags, {
      type: "array",
      items: { type: "string", enum: ["red", "blue", "green"] },
    });
    const outside = await send("/import/csv" + types({ tags: "multi:red,blue" }), picks);
    assertEquals(outside.status, 400);
    assert((await outside.text()).includes("Line 3"), "a pick outside the options names its line");
    // One token per cell is not a list, and a pick named twice in one cell would be refused, so neither is guessed.
    for (const body of ["t\na\nb\n", 't\n"a,a"\nb\n']) {
      const g = await (await send("/import/preview", body)).json();
      assertEquals(g.data.cols[0].type, "text");
    }

    // has_pick answers membership over the array or its JSON text, null for a null cell, and refuses what is neither.
    const has = await post(jwt, `/query`, {
      lang: "sql",
      code:
        `select has_pick(tags, 'blue') b, has_pick('["a"]', 'a') j, has_pick(null, 'a') n from @${multi_id} where id = 1`,
      args: [],
    });
    assertEquals(has.data[1], { b: true, j: true, n: null });
    const noPick = await app.request("/query", {
      method: "POST",
      headers: new Headers({ "Content-Type": "application/json", Authorization: `Bearer ${jwt}` }),
      body: JSON.stringify({ lang: "sql", code: `select has_pick(note, 'a') h from @${multi_id}`, args: [] }),
    });
    assertEquals(noPick.status, 400);
    assert((await noPick.text()).includes("has_pick() argument 1"));
  });

  // A feed that drops a column read as a sheet of blanks and graded as
  // healthy. The columns a run answered with are recorded beside it, and a run
  // whose columns differ from the run before is a failed run naming the
  // difference -- through POLL_OK, so freshness and the status alarm hear it
  // the way they hear every other failure.
  await t.step("You are told when an upstream feed changes shape", async () => {
    const { jwt } = await usr("shapes@example.com");
    const feed = automerge.create<Sheet>({
      type: "net-http",
      data: [{ url: "https://shaped.test/rows", interval: 60 }],
    });
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
      Number(
        (await get<Table>(jwt, `/sheet/library:freshness`)).slice(1).find((r) => r.sheet_id === feedId)
          ?.failures_since_ok,
      );
    const at = Date.now() + 110_000_000;

    await pollFeed(feedId, fetcher, at);
    assertEquals(
      (await newest()).meta.shape,
      { id: "number", name: "string" },
      "the columns a run answered with ride its row",
    );
    assertEquals(await failures(), 0);

    // A column added, one retyped: the data lands, and the run is a failure
    // naming what changed.
    answer = `[{"id":"1","name":"a","extra":true},{"id":"2","name":null}]`;
    await pollFeed(feedId, fetcher, at + 61_000);
    const changed = await newest();
    assertEquals(changed.body, answer, "the rows that arrived are kept");
    assertEquals(changed.meta.shape, { extra: "boolean", id: "string", name: "string" });
    assertEquals(
      changed.meta.shape_change,
      { added: ["extra"], dropped: [], retyped: ["id"] },
      "and the row names the change",
    );
    assertEquals(await failures(), 1, "which freshness counts as a failed run");
    assert(
      (await status())["Every net-http poll in the past hour returned 2xx in the shape the feed had before."]["0"] < 1,
    );

    // The same shape again is the new normal.
    await pollFeed(feedId, fetcher, at + 122_000);
    assertEquals((await newest()).meta.shape_change, undefined, "the run after is not a change");
    assertEquals(await failures(), 0);

    // A feed that stops answering rows at all dropped every column, once.
    answer = "<html>maintenance</html>";
    await pollFeed(feedId, fetcher, at + 183_000);
    const gone = await newest();
    assertEquals(gone.meta.shape, null, "a body that is not rows has no shape");
    assertEquals(gone.meta.shape_change, { added: [], dropped: ["extra", "id", "name"], retyped: [] });
    await pollFeed(feedId, fetcher, at + 244_000);
    assertEquals((await newest()).meta.shape_change, undefined, "and nothing is compared against no shape");
  });

  // A host with no validators answered the same body every poll and every
  // poll appended it. A run's body is its idempotency key, in the slot the
  // delivery index already refuses a replay on; and a failed run is in the
  // sheet's log and not in a query over it, so one bad poll cannot empty what
  // is built downstream.
  await t.step("A re-run does not double-append", async () => {
    const { jwt } = await usr("rerun@example.com");
    const feed = automerge.create<Sheet>({
      type: "net-http",
      data: [{ url: "https://rerun.test/rows", interval: 60 }],
    });
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

    await pollFeed(feedId, fetcher, at);
    await pollFeed(feedId, fetcher, at + 61_000);
    const [same] = await log();
    assertEquals((await log()).length, 1, "the same body twice is one row");
    const meta = JSON.parse(String(same.meta));
    assert(meta.sig, "keyed by its digest, in the slot a delivery's signature takes");
    assertEquals(meta.repeated, true, "and the row says it came again");

    answer = () => new Response(`[{"n":2}]`);
    await pollFeed(feedId, fetcher, at + 122_000);
    assertEquals((await log()).length, 2, "a different body is a second row");
    answer = () => new Response(`[{"n":1}]`);
    await pollFeed(feedId, fetcher, at + 183_000);
    const back = await log();
    assertEquals(back.length, 2, "a body this sheet already holds is not a third");
    assertEquals(String(back[0].body), `[{"n":1}]`, "the row it matches is the newest again");

    // One failed poll: the log shows it, a query over the feed does not, so a
    // sheet built downstream keeps what it had while freshness says why.
    assertEquals(await queried(), [`[{"n":1}]`, `[{"n":2}]`]);
    answer = () => new Response("boom", { status: 500 });
    await pollFeed(feedId, fetcher, at + 244_000);
    assertEquals((await log()).length, 3, "the failed run is in the log");
    assertEquals(await queried(), [`[{"n":1}]`, `[{"n":2}]`], "and not in a query over the feed");
    const mine = (await get<Table>(jwt, `/sheet/library:freshness`)).slice(1).find((r) => r.sheet_id === feedId);
    assertEquals(Number(mine?.failures_since_ok), 1, "which is where the failure is told");
  });

  await t.step("The bucket a flood is counted against must be one the flood cannot choose", () => {
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

  const feedCondition =
    `Every net-http sheet that has ever polled and is not paused did so within twice its own interval, or within ${
      POLL_STALE_S / 3600
    } hours if that is longer.`;
  const liveCondition = "Every alert sheet that is not paused ran within twice its own interval.";
  const overdueCondition = `No more than ${OVERDUE_MAX} net-http or alert sheets are overdue, paused ones included.`;

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
      (await status())[liveCondition]["0"] < 1,
      "an alert that stopped running must grade as stopped",
    );
    // An alert that has never run at all -- a poller that never fired once, on
    // a cold isolate. An inner join would drop it, and an empty set grades as a
    // pass, so the condition would report healthy on exactly the failure it is
    // for. Graded from the sheet's own creation instead.
    await sql`delete from net`;
    await sql`update sheet set created_at = now() - interval '3 hours' where type = 'alert'`;
    assert(
      (await status())[liveCondition]["0"] < 1,
      "an alert that has never run must grade as never run",
    );

    // A sheet somebody switched off is not a dead poller, and the alarm cannot
    // tell the two apart from SQL: `paused` lives only in the automerge
    // document, and a paused sheet writes no run to record it beside. So the
    // condition opens a document -- for the overdue sheets alone, worst first,
    // stopping at the first that is not paused. `net` is empty by here, so no
    // net-http sheet is in the feeds half and every overdue sheet is an alert.
    await sql`delete from sheet_usr where sheet_id like 'alert:%'`;
    await sql`delete from sheet where type = 'alert'`;
    const switched = automerge.create<{ data: [{ code: string; paused?: boolean }] }>({
      data: [{ code: "select 1" }],
    });
    await sql`
      insert into sheet (created_by, type, doc_id, name, created_at)
      values ((select usr_id from usr where email = ''), 'alert', ${switched.documentId}, 'switched',
              now() - interval '3 hours')
    `;
    assert((await status())[liveCondition]["0"] < 1, "an overdue alert nobody paused is a dead poller");

    switched.change((d: { data: [{ paused?: boolean }] }) => {
      d.data[0].paused = true;
    });
    assertEquals(
      (await status())[liveCondition]["0"],
      1,
      "a sheet its owner switched off is not an outage",
    );

    // An unreadable document must never excuse a stale alert. 'ghost-doc' is
    // not base58, so find rejects on the spelling rather than waiting out the
    // lookup on a document nobody has.
    await sql`
      insert into sheet (created_by, type, doc_id, name, created_at)
      values ((select usr_id from usr where email = ''), 'alert', 'ghost-doc', 'ghost',
              now() - interval '3 hours')
    `;
    const ghosted = await status();
    assert(ghosted[liveCondition]["0"] < 1, "a document that will not open grades as running");
    assertEquals(
      ghosted[overdueCondition]["0"],
      OVERDUE_MAX / 2,
      "the cap counts the paused sheet too, or pausing would be a way to leave the cap behind",
    );

    // The same switch on the other kind of sheet. A feed is graded off its
    // newest poll where an alert is graded off its own interval, so the two
    // walk lists built by two different CTEs and a sheet missing from the feeds
    // one would read as healthy rather than as a feed nobody polls.
    const feed = automerge.create<{ data: [{ url: string; paused?: boolean }] }>({
      data: [{ url: "https://example.invalid/feed" }],
    });
    const feedSheet = `net-http:${feed.documentId}`;
    await sql`
      insert into sheet (created_by, type, doc_id, name)
      values ((select usr_id from usr where email = ''), 'net-http', ${feed.documentId}, 'stale feed')
    `;
    await sql`
      insert into net (sheet_id, method, body, meta, created_at)
      values (${feedSheet}, 'GET', '[]', '{"status":200}'::jsonb, now() - make_interval(secs => ${POLL_STALE_S * 2}))
    `;
    assert((await status())[feedCondition]["0"] < 1, "a feed nobody paused is a poller that stopped");
    feed.change((d: { data: [{ paused?: boolean }] }) => {
      d.data[0].paused = true;
    });
    assertEquals((await status())[feedCondition]["0"], 1, "a feed its owner switched off is not an outage");
    // A feed is graded against twice the interval its newest poll recorded,
    // so a daily one is on time a day after its poll and late three days on.
    feed.change((d: { data: [{ paused?: boolean }] }) => {
      d.data[0].paused = false;
    });
    await sql`
      update net set meta = '{"status":200,"interval":86400}'::jsonb, created_at = now() - interval '20 hours'
      where sheet_id = ${feedSheet}
    `;
    assert((await status())[feedCondition]["0"] >= 1, "a daily feed that polled 20 hours ago is on time");
    await sql`update net set created_at = now() - interval '3 days' where sheet_id = ${feedSheet}`;
    assert((await status())[feedCondition]["0"] < 1, "a daily feed that last polled 3 days ago has stopped");
    await sql`delete from net where sheet_id = ${feedSheet}`;
    await sql`delete from sheet where sheet_id = ${feedSheet}`;

    // Done with the alert sheets: an alert sheet with no runs at all is a dead
    // poller, which is what the liveness condition above just proved.
    //
    // 200 is reachable. The suite has spent the run filling the error log and
    // failing net-http polls on purpose, so a clean log is what proves the
    // healthy path exists at all -- without this the 503 below passes for
    // whatever happens to be true, which is no test.
    await sql`delete from net`;
    await sql`delete from sheet_usr where sheet_id like 'alert:%'`;
    await sql`delete from sheet where type = 'alert'`;
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

  // The switch quiets the alarm up to the cap and not one sheet past it, which
  // is the whole of what stops "pause everything" from being a way to turn this
  // check off.
  await t.step("Past the overdue cap a paused sheet is an outage again", async () => {
    // In memory, and one insert for the lot: a route call per sheet costs more
    // than this step is worth.
    const docs = Array.from(
      { length: OVERDUE_MAX + 1 },
      () =>
        automerge.create<{ data: [{ code: string; paused: boolean }] }>({
          data: [{ code: "select 1", paused: true }],
        }).documentId as string,
    );
    await sql`
      insert into sheet (created_by, type, doc_id, name, created_at)
      select (select usr_id from usr where email = ''), 'alert', d, 'capped', now() - interval '3 hours'
      from unnest(${docs}::text[]) as d
    `;
    const over = await status();
    assert(over[liveCondition]["0"] < 1, "past the cap nothing is asked about any switch");
    assert(over[overdueCondition]["0"] < 1, "and the cap itself says how far past it is");

    await sql`delete from sheet where doc_id = ${docs[0]}`;
    const under = await status();
    assertEquals(under[liveCondition]["0"], 1, "at the cap every overdue sheet is walked and every one is paused");
    assertEquals(under[overdueCondition]["0"], 1, "and the cap is exactly met");
    // Taken away again, so the pause step below polls its own sheet and not
    // eleven of these.
    await sql`delete from sheet where type = 'alert'`;
  });

  // A viewer's changes are refused at the sync socket, but an editor's are
  // not, and nothing stops an editor from writing data[0] itself away to
  // null. That is a document that loaded, not one that failed to -- and
  // `null.paused` used to throw past every catch in liveness() and status(),
  // turning the one endpoint that must never 500 into exactly that.
  await t.step("A document that loaded with data[0] itself null does not crash the alarm", async () => {
    const nullDoc = automerge.create<{ data: [null] }>({ data: [null] });
    await sql`
      insert into sheet (created_by, type, doc_id, name, created_at)
      values ((select usr_id from usr where email = ''), 'alert', ${nullDoc.documentId}, 'null-config',
              now() - interval '3 hours')
    `;
    assert(
      (await status())[liveCondition]["0"] < 1,
      "a document that cannot say whether it is paused must still grade the overdue alert as running",
    );
    await sql`delete from sheet where type = 'alert'`;
  });

  // The guard is over an address, and an address has more than one spelling.
  await t.step("A url inside our own network is refused however it is spelled", async () => {
    const heard: string[] = [];
    const inside = Deno.serve({ hostname: "127.0.0.1", port: 0, onListen: () => {} }, (req) => {
      heard.push(new URL(req.url).pathname);
      return new Response("ok");
    });
    const port = (inside.addr as Deno.NetAddr).port;
    const refusal = (url: string) =>
      safeFetch(url).then(() => "", (err) => (err instanceof Error ? err.message : String(err)));
    try {
      // new URL() normalizes [::ffff:127.0.0.1] to ::ffff:7f00:1, which is the
      // only spelling that ever came out of a url -- and the one the guard did
      // not read as IPv4, so loopback and the metadata address walked out
      // through the IPv6 rules.
      for (
        const url of [
          `http://[::ffff:127.0.0.1]:${port}/hook`,
          `http://[::127.0.0.1]:${port}/hook`,
          `http://[::ffff:169.254.169.254]/latest/meta-data`,
        ]
      ) {
        assert((await refusal(url)).includes("own network"), `${url} must be refused by name: ${await refusal(url)}`);
      }
      assertEquals(heard, [], `a loopback listener was reached: ${heard.join()}`);

      // Deno's resolver reads neither /etc/hosts nor the OS resolver, so a name
      // only this machine knows resolves to nothing here and to loopback in
      // fetch. An empty answer graded as "no blocked address among them".
      const nowhere = await refusal("http://nothing-resolves-here.invalid/x");
      assert(nowhere.includes("no address"), `a host that resolves to nothing is not a public host: ${nowhere}`);
    } finally {
      await inside.shutdown();
    }
  });

  // A destination is a url somebody typed, and both what it answers and what it
  // is spelled like are this server's to hold.
  await t.step("A refused alert delivery is bounded by what it reads and by what it says", async () => {
    const { jwt } = await usr("hostile@example.com");
    const watched = automerge.create<Sheet>({
      type: "table",
      data: [arrayify([{ name: "n", type: "num", key: 0 }]), { 0: 1 }],
    });
    await put(jwt, `/library/table:${watched.documentId}`, {});
    const alertOn = async (to: string, name: string) => {
      const hand = automerge.create<{ data: [{ code: string; to: string; interval: number }] }>({
        data: [{ code: `select n from @table:${watched.documentId}`, to, interval: 60 }],
      });
      const id = `alert:${hand.documentId}`;
      await put(jwt, `/library/${id}`, { name });
      return id;
    };
    const delivery = async (id: string) => {
      const [, run] = await get<Table>(jwt, `/sheet/${id}`);
      return String((JSON.parse(String(run.body)) as { delivery: string }).delivery);
    };
    const mail = () => Promise.resolve("sent");
    // A destination inside our own network, so the background poller's own
    // fetch is refused before it leaves: every request in this step is the stub.
    const endless = await alertOn("http://127.0.0.1:9/hook", "endless");
    let pulled = 0;
    const chunk = new Uint8Array(512 * 1024).fill(65);
    const fetcher = () =>
      Promise.resolve(
        new Response(
          // Nothing is queued ahead of a read, so what was pulled is what the
          // reader asked for rather than what the stream buffered for it.
          new ReadableStream({
            pull(c) {
              if (++pulled >= 64) return c.close();
              c.enqueue(chunk);
            },
          }, { highWaterMark: 0 }),
          { status: 500 },
        ),
      );
    let clock = Date.now() + 200_000_000;
    await pollAlert(endless, mail, clock, fetcher);
    assert((await delivery(endless)).includes("refused it with 500"), "the post was refused");
    const read = pulled * chunk.byteLength;
    // readBody stops one chunk past the cap, which is how it knows it is past it.
    assert(
      read <= BODY_CAP + chunk.byteLength,
      `a refused delivery read ${read} bytes of the receiver's answer, past the ${BODY_CAP}-byte cap`,
    );

    // A url that will not parse is still a credential. new URL()'s TypeError
    // quotes the whole input, and that message was the delivery line everyone
    // the sheet is shared with reads, and the operator's log line.
    const token = "T11111111/B11111111/zzzzZZZZzzzzZZZZzzzzZZZZ";
    const typo = await alertOn(`https://hooks.slack.com:99999/services/${token}`, "typo");
    clock += 120_000;
    await pollAlert(typo, mail, clock, fetcher);
    const line = await delivery(typo);
    assert(!line.includes(token), `the delivery line quoted the whole url: ${line}`);
    for (const field of ["Expected", "Received", "Source", "Fix"])
      assert(line.includes(field), `a refusal states ${field}, got: ${line}`);

    // The other half of the same promise: fetch quotes the url it was handed in
    // the error it throws, and that message was the delivery line whole.
    const path = "B22222222/yyyyYYYYyyyyYYYYyyyyYYYY";
    const unreachable = await alertOn(`http://127.0.0.1:9/services/${path}`, "unreachable");
    clock += 120_000;
    await pollAlert(unreachable, mail, clock, (url: string) => {
      throw new Error(`error sending request for url (${url})`);
    });
    const failed = await delivery(unreachable);
    assert(!failed.includes(path), `the delivery line quoted the url the failure named: ${failed}`);
    assert(failed.includes("127.0.0.1"), `and it still names the host: ${failed}`);
  });

  await t.step("You run a sheet now, pause it, and see when it runs next", async () => {
    const { jwt, usr_id } = await usr("runner@example.com");
    const watched = automerge.create<Sheet>({
      type: "table",
      data: [arrayify([{ name: "n", type: "num", key: 0 }]), { 0: 1 }],
    });
    const table_id = `table:${watched.documentId}`;
    await put(jwt, `/library/${table_id}`, {});

    // An alert is the runnable sheet with no wire in it: it runs a query and
    // writes its row, so "now" is answerable without a fetch. `to` is empty, so
    // the run decides and delivers nothing.
    const alert = automerge.create<
      { data: [{ code: string; to: string; interval: number; paused?: boolean; cron?: string; timezone?: string }] }
    >({
      data: [{ code: `select n from @${table_id}`, to: "", interval: 3600 }],
    });
    const alert_id = `alert:${alert.documentId}`;
    await put(jwt, `/library/${alert_id}`, { name: "on demand" });
    const runs = async () => (await get<Table>(jwt, `/sheet/${alert_id}`)).length - 1;

    // Counted rather than assumed: the background tick runs on the same clock
    // this sheet was made on, and may have taken it already.
    const before = await runs();
    const { data: ran } = await post(jwt, `/library/${alert_id}/run`, {});
    const after = await runs();
    assertEquals(after, before + 1, "running it now writes the run's row");
    assertEquals(ran.method, "ALERT", "and answers the row it wrote");
    assertEquals(JSON.parse(String(ran.body)).status, "firing");

    // The due entry was cleared and then set from the sheet's own interval, so
    // the tick a second later does not run again what a person just ran. On the
    // real clock, because that is the one the run was taken on.
    await pollAlertOnce(() => Promise.resolve("sent"), Date.now());
    assertEquals(await runs(), after, "the tick does not repeat a run that just happened");

    // From here on the clock is far enough ahead that this sheet is due
    // whatever the background tick has done, the way every other poller test in
    // this file does it.
    const clock = Date.now() + 500_000_000;

    // Paused: the tick steps over it without a fetch, without a row, and
    // without touching when it was due -- which the unpause below proves, since
    // the same clock still finds it due.
    alert.change((d: { data: [{ paused?: boolean }] }) => {
      d.data[0].paused = true;
    });
    await pollAlertOnce(() => Promise.resolve("sent"), clock);
    assertEquals(await runs(), after, "a paused alert is not run and writes nothing");

    const refused = await app.request(`/library/${alert_id}/run`, {
      method: "POST",
      headers: new Headers({ "Content-Type": "application/json", Authorization: `Bearer ${jwt}` }),
      body: "{}",
    });
    assertEquals(refused.status, 409);
    assert((await refused.text()).includes("is paused"), "a paused sheet is refused by name");

    alert.change((d: { data: [{ paused?: boolean }] }) => {
      d.data[0].paused = false;
    });
    await pollAlertOnce(() => Promise.resolve("sent"), clock);
    assertEquals(
      await runs(),
      after + 1,
      "a paused sheet keeps the due time it had, so unpausing costs it no interval",
    );

    // A sheet with no poll is refused by name rather than answering an empty
    // run: a table has nothing to go and fetch.
    const wrong = await app.request(`/library/${table_id}/run`, {
      method: "POST",
      headers: new Headers({ "Content-Type": "application/json", Authorization: `Bearer ${jwt}` }),
      body: "{}",
    });
    assertEquals(wrong.status, 400);
    assert((await wrong.text()).includes("no poll to run"), "a table is refused by name");

    // A feed reads the same switch. Its url is inside our own network, so
    // safeFetch refuses it before it leaves and the only thing this proves is
    // whether a row was written at all.
    const feed = automerge.create<Sheet>({
      type: "net-http",
      data: [{ url: "http://127.0.0.1:9/feed.json", interval: 60, paused: true }],
    });
    const feed_id = `net-http:${feed.documentId}`;
    await put(jwt, `/library/${feed_id}`, { name: "paused feed" });
    const polls = async () => (await get<Table>(jwt, `/sheet/${feed_id}`)).length - 1;
    // The one sheet, through the very function the tick hands each due sheet.
    // Not the tick itself: a clock this far ahead makes every feed in this file
    // due, and this step would poll the lot of them.
    await pollNetSheet(feed_id, feed.documentId, safeFetch, clock);
    assertEquals(await polls(), 0, "a paused feed is not polled and writes nothing");

    // Freshness says both halves: the switch off the document, and when the
    // poller takes it next off the map that decides. The paused feed has never
    // been polled, so the map does not know it and the read says null rather
    // than naming a time nobody decided.
    const byId = Object.fromEntries(
      (await get<Table>(jwt, "/sheet/library:freshness")).slice(1).map((r) => [String(r.sheet_id), r]),
    );
    assertEquals(byId[feed_id].paused, true, "freshness reads the switch off the document");
    assertEquals(byId[feed_id].next_run, null, "a sheet the due map has not reached yet is null, never a guess");
    assertEquals(byId[alert_id].paused, false);
    assert(
      String(byId[alert_id].next_run).includes("T"),
      `an alert the poller has run knows when it runs next, got ${byId[alert_id].next_run}`,
    );

    // A cron wins over the interval, read in the sheet's own zone. Both
    // pollers record the gap to the run croner names as meta.interval.
    const weekdays = new Cron("0 9 * * 1-5", { timezone: "America/Chicago", mode: "5-part" }).nextRun(new Date(clock));
    assert(weekdays, "croner names a next weekday morning");
    const gap = Math.ceil((weekdays.getTime() - clock) / 1000);
    const cronFeed = automerge.create<Sheet>({
      type: "net-http",
      data: [{
        url: "https://cron.feeds.test/rows.json",
        interval: 60,
        cron: "0 9 * * 1-5",
        timezone: "America/Chicago",
      }],
    });
    const cronFeedId = `net-http:${cronFeed.documentId}`;
    await put(jwt, `/library/${cronFeedId}`, { name: "weekday feed" });
    await pollNetSheet(
      cronFeedId,
      cronFeed.documentId,
      () => Promise.resolve(new Response(`[{"n":1}]`, { headers: { "Content-Type": "application/json" } })),
      clock,
    );
    alert.change((d: { data: [{ cron?: string; timezone?: string }] }) => {
      d.data[0].cron = "0 9 * * 1-5";
      d.data[0].timezone = "America/Chicago";
    });
    await pollAlertSheet(
      { sheet_id: alert_id, doc_id: alert.documentId, name: "on demand", created_by: usr_id },
      () => Promise.resolve("sent"),
      clock,
    );
    const scheduled = Object.fromEntries(
      (await get<Table>(jwt, "/sheet/library:freshness")).slice(1).map((r) => [String(r.sheet_id), r]),
    );
    assertEquals(scheduled[cronFeedId].next_run, weekdays.toISOString(), "freshness names the run croner names");
    assertEquals(scheduled[alert_id].next_run, weekdays.toISOString());
    const [feedRun] = await sql`
      select meta from net where sheet_id = ${cronFeedId} and meta->>'status' = '200' order by net_id desc limit 1
    `;
    assertEquals(feedRun.meta.interval, gap, "a feed's poll records the gap to its next run");
    const [alertRun] = await sql`select meta from net where sheet_id = ${alert_id} order by net_id desc limit 1`;
    assertEquals(alertRun.meta.interval, gap, "an alert's run records the gap to its next run");

    // A business day picks the day out of the cron's runs: the nth weekday of
    // the month in the sheet's zone, or from the month's end when negative. The
    // cron's month list is a quarter. The oracle counts weekdays itself.
    const chicago = new Intl.DateTimeFormat("en-US", {
      timeZone: "America/Chicago",
      year: "numeric",
      month: "numeric",
      day: "numeric",
    });
    const businessRun = (pattern: string, n: number) => {
      const cron = new Cron(pattern, { timezone: "America/Chicago", mode: "5-part" });
      for (let run = cron.nextRun(new Date(clock)), i = 0; run && i < 100; run = cron.nextRun(run), i++) {
        const [month, day, year] = chicago.format(run).split("/").map(Number);
        const weekdays = Array.from({ length: 31 }, (_, k) => new Date(Date.UTC(year, month - 1, k + 1)))
          .filter((d) => d.getUTCMonth() === month - 1 && d.getUTCDay() % 6 !== 0)
          .map((d) => d.getUTCDate());
        if (weekdays.at(n > 0 ? n - 1 : n) === day) return run;
      }
      throw new Error(`no business day ${n} of ${pattern} within 100 runs of ${new Date(clock).toISOString()}`);
    };
    const businessFeed = automerge.create<Sheet>({
      type: "net-http",
      data: [{
        url: "https://business.feeds.test/rows.json",
        interval: 60,
        cron: "0 9 * * *",
        timezone: "America/Chicago",
        business_day: 3,
      }],
    });
    const businessFeedId = `net-http:${businessFeed.documentId}`;
    await put(jwt, `/library/${businessFeedId}`, { name: "third business day feed" });
    await pollNetSheet(
      businessFeedId,
      businessFeed.documentId,
      () => Promise.resolve(new Response(`[{"n":1}]`, { headers: { "Content-Type": "application/json" } })),
      clock,
    );
    alert.change((d: { data: [{ cron?: string; business_day?: number }] }) => {
      d.data[0].cron = "0 9 * 1,4,7,10 *";
      d.data[0].business_day = -1;
    });
    await pollAlertSheet(
      { sheet_id: alert_id, doc_id: alert.documentId, name: "on demand", created_by: usr_id },
      () => Promise.resolve("sent"),
      clock,
    );
    const third = businessRun("0 9 * * *", 3), last = businessRun("0 9 * 1,4,7,10 *", -1);
    const business = Object.fromEntries(
      (await get<Table>(jwt, "/sheet/library:freshness")).slice(1).map((r) => [String(r.sheet_id), r]),
    );
    assertEquals(business[businessFeedId].next_run, third.toISOString(), "a feed runs on the third weekday");
    assertEquals(
      business[alert_id].next_run,
      last.toISOString(),
      "an alert at -1 runs on the quarter month's last weekday",
    );
    const [businessPoll] = await sql`
      select meta from net where sheet_id = ${businessFeedId} and meta->>'status' = '200' order by net_id desc limit 1
    `;
    assertEquals(businessPoll.meta.interval, Math.ceil((third.getTime() - clock) / 1000));
  });

  await t.step("You re-run a feed from a past watermark", async () => {
    const { jwt } = await usr("backfill@example.com");
    const feed = automerge.create<Sheet>({
      type: "net-http",
      data: [{
        url: "http://93.184.216.91/items",
        interval: 3600,
        cursor: "since",
        method: "POST",
        body: `{"after":"{{cursor}}"}`,
      }],
    });
    const feed_id = `net-http:${feed.documentId}`;
    await put(jwt, `/library/${feed_id}`, { name: "backfill feed" });
    const seen: { url: string; headers: Record<string, string>; body?: string }[] = [];
    const answer = (n: number) =>
      Promise.resolve(
        new Response(`[{"n":${n}}]`, { headers: { "Content-Type": "application/json", ETag: `"v${n}"` } }),
      );
    // An hour back, so the host gap this poll holds is over before the run.
    const first = Date.now() - 3600_000;
    await pollNetSheet(feed_id, feed.documentId, (url, headers, _method, body) => {
      seen.push({ url, headers: { ...headers }, body });
      return answer(1);
    }, first);

    // Through the route, so safeFetch is the door and fetch is the wire.
    const origFetch = globalThis.fetch;
    globalThis.fetch = ((url: string, init?: RequestInit) => {
      seen.push({
        url: String(url),
        headers: Object.fromEntries(new Headers(init?.headers).entries()),
        body: String(init?.body),
      });
      return answer(2);
    }) as typeof fetch;
    let ran: { meta: { cursor: string; status: number } };
    try {
      ({ data: ran } = await post(jwt, `/library/${feed_id}/run`, { cursor: "2024-01-31T00:00:00Z" }));
    } finally {
      globalThis.fetch = origFetch;
    }
    const backfill = seen[1];
    assertEquals(
      new URL(backfill.url).searchParams.get("since"),
      "2024-01-31T00:00:00Z",
      "the url asks from the cursor",
    );
    assertEquals(backfill.body, `{"after":"2024-01-31T00:00:00Z"}`, "and so does the body");
    assertEquals(backfill.headers["if-none-match"], undefined, "a backfill sends no validator");
    assertEquals(ran.meta.status, 200);
    assert(ran.meta.cursor > new Date(first).toISOString(), `the watermark moves to the run, got ${ran.meta.cursor}`);

    // The schedule carries on from the backfill's own start, validators and all.
    await pollNetSheet(feed_id, feed.documentId, (url, headers, _method, body) => {
      seen.push({ url, headers: { ...headers }, body });
      return answer(3);
    }, Date.now() + 3600_000);
    const next = seen[2];
    assertEquals(
      new URL(next.url).searchParams.get("since"),
      ran.meta.cursor,
      "the next poll asks from the new watermark",
    );
    assertEquals(next.body, `{"after":"${ran.meta.cursor}"}`);
    assertEquals(next.headers["If-None-Match"], `"v2"`, "and asks conditionally again");

    const refusal = async (sheet_id: string, cursor: unknown, status: number, headline: string) => {
      const res = await app.request(`/library/${sheet_id}/run`, {
        method: "POST",
        headers: new Headers({ "Content-Type": "application/json", Authorization: `Bearer ${jwt}` }),
        body: JSON.stringify({ cursor }),
      });
      const text = await res.text();
      assertEquals(res.status, status, text);
      assert(text.includes(headline), `${sheet_id} with cursor ${cursor} names ${headline}: ${text}`);
    };
    await refusal(feed_id, "yesterday", 400, "That cursor is not a timestamp.");
    await refusal(feed_id, "2024-01-31T00:00:00", 400, "That cursor is not a timestamp.");
    const alert = automerge.create<{ data: [{ code: string; to: string; interval: number }] }>({
      data: [{ code: "select 1 as n", to: "", interval: 3600 }],
    });
    const alert_id = `alert:${alert.documentId}`;
    await put(jwt, `/library/${alert_id}`, { name: "backfill alert" });
    await refusal(alert_id, "2024-01-31", 400, "An alert sheet has no watermark to rerun from.");
    const replace = automerge.create<Sheet>({
      type: "net-http",
      data: [{ url: "http://93.184.216.91/latest", interval: 3600, mode: "replace" }],
    });
    const replace_id = `net-http:${replace.documentId}`;
    await put(jwt, `/library/${replace_id}`, { name: "replace feed" });
    await refusal(replace_id, "2024-01-31", 409, "A replace feed cannot rerun from a cursor.");
    // Nowhere to send the watermark: the run would be an ordinary poll that
    // answered as if it had backfilled.
    const plain = automerge.create<Sheet>({
      type: "net-http",
      data: [{ url: "http://93.184.216.91/plain", interval: 3600 }],
    });
    const plain_id = `net-http:${plain.documentId}`;
    await put(jwt, `/library/${plain_id}`, { name: "plain feed" });
    await refusal(plain_id, "2024-01-31", 409, "This feed has nowhere to send a cursor.");
    // Date.parse answers March 2 for this one rather than refusing it.
    await refusal(feed_id, "2026-02-30", 400, "That cursor is not a timestamp.");
  });

  await t.step("Within a cycle, the feeds an alert reads are polled before the alert", async () => {
    const { jwt, usr_id } = await usr("cycle@example.com");
    // Two intervals of their own ran the alert against the cycle before's rows.
    // One cycle polls the feed and then runs the alert, so the count below is
    // the run this very cycle stored.
    const fetcher = (url: string) =>
      Promise.resolve(
        new Response(url === "https://cycle.feeds.test/rows.json" ? `[{"n":1}]` : `[]`, {
          headers: { "Content-Type": "application/json" },
        }),
      );
    const feed = automerge.create<Sheet>({
      type: "net-http",
      data: [{ url: "https://cycle.feeds.test/rows.json", interval: 3600 }],
    });
    const feed_id = `net-http:${feed.documentId}`;
    await put(jwt, `/library/${feed_id}`, { name: "cycle feed" });
    const alert = automerge.create<{ data: [{ code: string; to: string; interval: number }] }>({
      data: [{ code: `select created_at from @${feed_id}`, to: "", interval: 3600 }],
    });
    const alert_id = `alert:${alert.documentId}`;
    await put(jwt, `/library/${alert_id}`, { name: "cycle alert" });

    // Neither sheet has ever run, so both are due whatever the clock says; the
    // real clock is what leaves every earlier step's sheet at the due time it
    // was left with.
    await pollOnce(fetcher, () => Promise.resolve("sent"), Date.now());

    const polls = (await get<Table>(jwt, `/sheet/${feed_id}`)).slice(1);
    assertEquals(polls.length, 1, "the feed half of the cycle stored its run");
    const [run] = (await get<Table>(jwt, `/sheet/${alert_id}`)).slice(1);
    const record = JSON.parse(String(run.body)) as { status: string; rows: number };
    assertEquals(
      [record.status, record.rows],
      ["firing", 1],
      "the alert half reads the row the feed half stored this cycle, not the cycle before's",
    );

    // `upstream: true` runs an hourly alert in the cycle its feed stores a new
    // row, through a query between them. The clocks are in the past, so no
    // sheet an earlier step left due in the real future is taken.
    let body = `[{"n":1}]`;
    const upFeed = automerge.create<Sheet>({
      type: "net-http",
      data: [{ url: "https://upstream.feeds.test/rows.json", interval: 60 }],
    });
    const upFeed_id = `net-http:${upFeed.documentId}`;
    await put(jwt, `/library/${upFeed_id}`, { name: "upstream feed" });
    const between = automerge.create<Sheet>({
      type: "query",
      data: [{ lang: "sql", code: `select created_at from @${upFeed_id}`, args: [] }],
    });
    const between_id = `query:${between.documentId}`;
    await put(jwt, `/library/${between_id}`, { name: "upstream query" });
    const alertOver = async (upstream: boolean) => {
      const doc = automerge.create<{ data: [Alert] }>({
        data: [{ code: `select count(*) as n from @${between_id}`, to: "", interval: 3600, upstream }],
      });
      const id = `alert:${doc.documentId}`;
      await put(jwt, `/library/${id}`, { name: `upstream ${upstream}` });
      return id;
    };
    const up_id = await alertOver(true);
    const plain_id = await alertOver(false);
    const runs = async (id: string) =>
      Number((await sql`select count(*)::int as n from net where sheet_id = ${id}`)[0].n);
    const upFetcher = (url: string) =>
      Promise.resolve(
        new Response(url === "https://upstream.feeds.test/rows.json" ? body : `[]`, {
          headers: { "Content-Type": "application/json" },
        }),
      );
    const t0 = Date.now() - 600_000;
    await pollOnce(upFetcher, () => Promise.resolve("sent"), t0);
    assertEquals([await runs(up_id), await runs(plain_id)], [1, 1], "both alerts run once on their first tick");

    body = `[{"n":2}]`;
    const t1 = t0 + 61_000;
    await pollOnce(upFetcher, () => Promise.resolve("sent"), t1);
    assertEquals(await runs(upFeed_id), 2, "the feed stored a new row");
    assertEquals(await runs(up_id), 2, "the upstream alert ran in the cycle its feed stored a new row");
    assertEquals(await runs(plain_id), 1, "an alert without upstream keeps its hour");

    // A second new row inside a minute of the last run waits for the minute.
    body = `[{"n":3}]`;
    netDue.set(upFeed_id, 0);
    await pollOnce(upFetcher, () => Promise.resolve("sent"), t1 + 30_000);
    assertEquals(await runs(upFeed_id), 3, "the feed stored a third row");
    assertEquals(await runs(up_id), 2, "a new row inside a minute of the last run does not run it again yet");
    await pollOnce(upFetcher, () => Promise.resolve("sent"), t1 + 61_000);
    assertEquals(await runs(up_id), 3, "the minute after its last run, the deferred row runs it");

    // The same body again moves the row it matches; nothing new arrived.
    netDue.set(upFeed_id, 0);
    await pollOnce(upFetcher, () => Promise.resolve("sent"), t1 + 150_000);
    const [moved] = await sql`select meta from net where sheet_id = ${upFeed_id} order by created_at desc limit 1`;
    assertEquals(moved.meta.repeated, true, "the feed's poll was a repeat");
    assertEquals(await runs(up_id), 3, "a repeated body does not make the alert due");
    assertEquals(await runs(plain_id), 1, "the plain alert still waits for its hour");

    // A feed named only in a snippet body wakes an upstream alert like any other.
    const bodyTable = automerge.create<Sheet>({
      type: "table",
      data: [
        arrayify([{ name: "name", type: "text", key: "0" }, { name: "expr", type: "text", key: "1" }]),
        { 0: "feed", 1: `(select count(*) from @${upFeed_id})` },
      ],
    });
    const bodyTable_id = `table:${bodyTable.documentId}`;
    await put(jwt, `/library/${bodyTable_id}`, { name: "feed snippets" });
    const viaBody = automerge.create<{ data: [Alert] }>({
      data: [{ code: `using @${bodyTable_id} select {{feed}} as n`, to: "", interval: 3600, upstream: true }],
    });
    const viaBody_id = `alert:${viaBody.documentId}`;
    await put(jwt, `/library/${viaBody_id}`, { name: "upstream via snippet" });
    await pollOnce(upFetcher, () => Promise.resolve("sent"), t1 + 300_000);
    const ranVia = await runs(viaBody_id);
    body = `[{"n":4}]`;
    netDue.set(upFeed_id, 0);
    await pollOnce(upFetcher, () => Promise.resolve("sent"), t1 + 400_000);
    assertEquals(await runs(upFeed_id) > 3, true, "the feed stored a fourth row");
    assertEquals(await runs(viaBody_id), ranVia + 1, "an alert whose snippet body names the feed ran in that cycle");

    // A query that answers the snippets is a source: the feed in its answer's body wakes the alert too.
    const bodyQuery = automerge.create<{ data: [Query] }>({
      data: [{
        lang: "sql",
        code: `select 'feed' as name, '(select count(*) from @${upFeed_id})' as expr`,
        args: [],
      } as Query],
    });
    const bodyQuery_id = `query:${bodyQuery.documentId}`;
    await put(jwt, `/library/${bodyQuery_id}`, { name: "feed snippet query" });
    const viaQuery = automerge.create<{ data: [Alert] }>({
      data: [{ code: `using @${bodyQuery_id} select {{feed}} as n`, to: "", interval: 3600, upstream: true }],
    });
    const viaQuery_id = `alert:${viaQuery.documentId}`;
    await put(jwt, `/library/${viaQuery_id}`, { name: "upstream via snippet query" });
    await pollOnce(upFetcher, () => Promise.resolve("sent"), t1 + 500_000);
    const ranQuery = await runs(viaQuery_id);
    body = `[{"n":5}]`;
    netDue.set(upFeed_id, 0);
    await pollOnce(upFetcher, () => Promise.resolve("sent"), t1 + 600_000);
    assertEquals(
      await runs(viaQuery_id),
      ranQuery + 1,
      "an alert whose snippet query names the feed ran in that cycle",
    );

    // A delivery a net-hook stores counts the same as a feed's new row.
    const hook = automerge.create<Sheet>({ type: "net-hook", data: [] });
    const hook_id = `net-hook:${hook.documentId}`;
    await put(jwt, `/library/${hook_id}`, { name: "upstream hook" });
    const hookAlert = automerge.create<{ data: [Alert] }>({
      data: [{ code: `select body from @${hook_id}`, to: "", interval: 3600, upstream: true }],
    });
    const hookAlert_id = `alert:${hookAlert.documentId}`;
    await put(jwt, `/library/${hookAlert_id}`, { name: "upstream hook alert" });
    await pollAlertSheet(
      { sheet_id: hookAlert_id, doc_id: hookAlert.documentId, name: "hook", created_by: usr_id },
      () => Promise.resolve("sent"),
      t0,
    );
    assertEquals((await deliver(hook_id, `{"x":1}`)).status, 200, "the signed delivery is stored");
    await pollAlertOnce(() => Promise.resolve("sent"), Date.now());
    assertEquals(await runs(hookAlert_id), 2, "a stored delivery runs the alert that reads it");
  });

  await t.step("run now: a runaway interval must not break freshness for the whole account", async () => {
    // Nothing bounds `interval` from above, and freshness turns the due map's
    // number straight into `new Date(due).toISOString()`. An interval whose
    // milliseconds overflow past Number.MAX_VALUE lands Infinity in the due
    // map, and Date#toISOString throws RangeError on that -- which would take
    // every other sheet's freshness row down with it, not just this one's.
    const { jwt, usr_id } = await usr("runaway@example.com");
    const alert = automerge.create<{ data: [{ code: string; to: string; interval: number }] }>({
      data: [{ code: `select 1 as n`, to: "", interval: 3600 }],
    });
    const alert_id = `alert:${alert.documentId}`;
    // Claimed with an ordinary interval -- row_0 is a snapshot taken at claim
    // time, so this is where a number automerge cannot round-trip as a JS
    // number (it comes back a BigInt) would break the claim itself, and that
    // is a different bug in a route this lane does not own. The huge value is
    // written after the claim, straight into the live document.
    await put(jwt, `/library/${alert_id}`, { name: "runaway interval" });
    alert.change((d: { data: [{ interval: number }] }) => {
      d.data[0].interval = 1e307;
    });
    await pollAlertSheet(
      { sheet_id: alert_id, doc_id: alert.documentId, name: "runaway interval", created_by: usr_id },
      () => Promise.resolve("sent"),
      Date.now(),
    );
    const res = await app.request(`/library/freshness`, {
      headers: new Headers({ Authorization: `Bearer ${jwt}` }),
    });
    assertEquals(
      res.status,
      200,
      `one sheet's overflowed interval must not break freshness for the whole account: ${await res.text()}`,
    );
    const byId = Object.fromEntries(
      (await get<Table>(jwt, "/sheet/library:freshness")).slice(1).map((r) => [String(r.sheet_id), r]),
    );
    assert(
      byId[alert_id].next_run === null || String(byId[alert_id].next_run).includes("T"),
      `next_run is either a real timestamp or null, never a value that crashes reading it, got ${
        byId[alert_id].next_run
      }`,
    );
  });

  await t.step("A schedule the poller cannot follow is the sheet's own failure, naming the field", async () => {
    const { jwt, usr_id } = await usr("unscheduled@example.com");
    type Schedule = { cron?: string; timezone?: string; business_day?: unknown };
    const feed = automerge.create<{ data: [{ url: string; interval: number } & Schedule] }>({
      data: [{ url: "https://unscheduled.feeds.test/rows.json", interval: 3600 }],
    });
    const feed_id = `net-http:${feed.documentId}`;
    await put(jwt, `/library/${feed_id}`, { name: "unscheduled feed" });
    const alert = automerge.create<{ data: [{ code: string; to: string; interval: number } & Schedule] }>({
      data: [{ code: "select 1 as n", to: "", interval: 3600 }],
    });
    const alert_id = `alert:${alert.documentId}`;
    await put(jwt, `/library/${alert_id}`, { name: "unscheduled alert" });
    const clock = Date.now() + 500_000_000;
    const newest = async (sheet_id: string) =>
      (await sql`select body, meta from net where sheet_id = ${sheet_id} order by net_id desc limit 1`)[0];
    const cases: [Schedule, string][] = [
      [{ cron: "0 9 * *" }, "is not a pattern croner can read"],
      [{ cron: "0 9 * * *", timezone: "Mars/Olympus" }, "is not a zone croner knows"],
      [{ cron: "0 0 30 2 *" }, "never fires again"],
      [{ timezone: "America/Chicago" }, "has no cron to apply to"],
      [{ cron: "* * * * * *" }, "is not a pattern croner can read"],
      // croner's refusal quotes the whole zone back, so this one broke the
      // row's size before the refusal cut croner's message.
      [{ cron: "0 9 * * *", timezone: "z".repeat(10_000) }, "is not a zone croner knows"],
      [{ cron: "0 9 * * *", business_day: 0 }, "is not a weekday a month can hold"],
      [{ cron: "0 9 * * *", business_day: -24 }, "is not a weekday a month can hold"],
      [{ cron: "0 9 * * *", business_day: "3" }, "is not a weekday a month can hold"],
      [{ business_day: 3 }, "needs a cron for its time and months"],
      [{ cron: "0 9 1 * *", business_day: 3 }, "names days"],
      [{ cron: "0 9 * * 1-5", business_day: 3 }, "names days"],
    ];
    for (const [schedule, headline] of cases) {
      const write = (d: { data: [Schedule] }) => {
        delete d.data[0].cron;
        delete d.data[0].timezone;
        delete d.data[0].business_day;
        Object.assign(d.data[0], schedule);
      };
      feed.change(write);
      alert.change(write);
      await pollNetSheet(feed_id, feed.documentId, () => {
        throw new Error(`a feed whose schedule is refused fetched anyway: ${JSON.stringify(schedule)}`);
      }, clock);
      const polled = await newest(feed_id);
      assert(String(polled.body).includes(headline), `${JSON.stringify(schedule)} names ${headline}: ${polled.body}`);
      assert(
        String(polled.body).length < 2_000,
        `a refused schedule is a bounded row, got ${String(polled.body).length}`,
      );
      assertEquals(polled.meta.interval, 3600, "the failure row records the hour it waits");
      assertEquals(netDue.get(feed_id), clock + 3600_000, "and the feed stays due on the ordinary retry");
      await pollAlertSheet(
        { sheet_id: alert_id, doc_id: alert.documentId, name: "unscheduled alert", created_by: usr_id },
        () => Promise.resolve("sent"),
        clock,
      );
      const ran = await newest(alert_id);
      const record = JSON.parse(String(ran.body)) as { status: string; error: string };
      assertEquals(record.status, "error", `${JSON.stringify(schedule)} is an error run`);
      assert(record.error.includes(headline), `${JSON.stringify(schedule)} names ${headline}: ${record.error}`);
      assert(record.error.length < 2_000, `a refused schedule is a bounded run, got ${record.error.length}`);
      assertEquals(ran.meta.interval, 3600);
    }
  });

  await stack.close();
});
