// The library, and the sheets you open from it — without a browser.
//
// The other half of page_test.ts, and split off it for the reason that file and
// glue_test.ts are two files: `deno test --parallel` runs files side by side and
// not tests, so the whole suite waited on whichever single file held every
// booted page. The seam is what a test opens. page_test.ts keeps the table and
// the query sheet — how they render, sort, arrange and take the keyboard. This
// file keeps the library itself, the sheet types opened from it (a feed, an
// alert, a chart, a dashboard), the palette and the shortcut sheet over them,
// and the parts of `src/page.mjs` that need no page at all: the thumbnail, the
// proxy target, an origin's own words, and the query resolver.
//
// The two are kept even in boots, because a boot is Elm's first paint into
// jsdom and that is what either file costs. Counted rather than eyeballed:
// `grep -c "await boot(" page_test.ts library_test.ts`.
import { assert, assertEquals, assertThrows } from "@std/assert";
import { JSDOM } from "jsdom";
import {
  API_BASE,
  atomToJson,
  docThumb,
  foldView,
  httpErrorDetail,
  httpFailure,
  httpTarget,
  httpUnparsed,
  httpUnreachable,
  importKey,
  library,
  mergeView,
  PORTALS,
  rememberedTypes,
  sheets,
  trapStep,
} from "./src/page.mjs";
import alasql from "./src/alasql.mjs";
import { boot, El, refused, resolver, rowsOf, shelf, until } from "./page_harness.ts";

// A feed's first poll used to be the first news of a wrong url or a missing
// secret. The button asks for the request now, and what comes back is shown
// where the fields are, before the sheet has to wait for the poller.
Deno.test("a feed's request can be tested before the poller runs it", async () => {
  const { app, all, text, click, settle } = await boot("http://localhost/");
  const asked: { id: string; data: { url: string; headers: string; method: string; body: string } }[] = [];
  app.ports.preflight.subscribe((ask: (typeof asked)[number]) => asked.push(ask));
  app.ports.docSelected.send({
    id: "net-http:feed",
    data: {
      doc: {
        type: "net-http",
        data: [{
          url: "https://example.com/feed.json",
          interval: 3600,
          headers: "X-Api-Key: {{secret:weather}}",
          method: "POST",
          body: '{"since":"{{cursor}}"}',
        }],
      },
    },
  });
  await settle();

  await click(all("button.chip").find((b) => b.textContent === "test the request"));
  // The whole request, and not the url alone: a pre-flight that dropped the
  // method and the body would test something the poller never sends.
  assertEquals(asked, [{
    id: "net-http:feed",
    data: {
      url: "https://example.com/feed.json",
      headers: "X-Api-Key: {{secret:weather}}",
      method: "POST",
      body: '{"since":"{{cursor}}"}',
    },
  }]);

  // An answer for another sheet is not this sheet's.
  app.ports.preflightLoaded.send({
    id: "net-http:other",
    data: { status: 500, ms: 1, bytes: 0, content_type: "", body: "" },
  });
  await settle();
  assertEquals(all("pre.preflight").length, 0, "another sheet's answer is not shown here");

  app.ports.preflightLoaded.send({
    id: "net-http:feed",
    data: { status: 200, ms: 12, bytes: 11, content_type: "application/json", body: '{"ok":true}' },
  });
  await settle();
  const shown = all("pre.preflight")[0]?.textContent ?? "";
  assert(shown.includes("200 · 12 ms · 11 bytes · application/json"), `the status line: ${shown}`);
  assert(shown.includes('{"ok":true}'), `and the body: ${shown}`);

  app.ports.preflightLoaded.send({
    id: "net-http:feed",
    data: { error: "This sheet does not hold {{secret:weather}}." },
  });
  await settle();
  assert(text().includes("does not hold {{secret:weather}}"), "a refusal is shown in the poller's own words");
});

// A feed you could not stop and could not start. The switch is a field on the
// document like every other, the chip beside "test the request" is the poll
// itself rather than a preview of it, and when the poller takes this sheet next
// is the freshness the library already reads.
Deno.test("a feed can be paused, run now, and says when it runs next", async () => {
  const { dom, app, all, text, click, settle } = await boot("http://localhost/");
  const asked: string[] = [];
  const patches: { action: string; path: unknown[]; value: unknown }[] = [];
  app.ports.runNow.subscribe((id: string) => asked.push(id));
  app.ports.changeDoc.subscribe((sent: { data: typeof patches }) => patches.push(...sent.data));
  app.ports.docSelected.send({
    id: "net-http:feed",
    data: { doc: { type: "net-http", data: [{ url: "https://example.com/feed.json", interval: 3600 }] } },
  });
  await settle();

  const box = () =>
    all("#paused")[0] as unknown as { checked: boolean; dispatchEvent: (e: unknown) => boolean } | undefined;
  assert(box(), "a feed offers the switch");
  assertEquals(box()?.checked, false, "a document with no paused field is not paused");

  await click(all("button.chip").find((b) => b.textContent === "run now"));
  assertEquals(asked, ["net-http:feed"], "the chip runs this sheet and names it");

  // An answer for another sheet is not this sheet's, exactly as a pre-flight's
  // is not: a run started on one sheet must not land on the next one opened.
  app.ports.runLoaded.send({
    id: "net-http:other",
    data: { created_at: "2026-08-23T14:02:11.000Z", method: "GET", body: "[]", meta: { status: 200, ms: 9 } },
  });
  await settle();
  assertEquals(all("pre.run").length, 0, "another sheet's run is not shown here");

  app.ports.runLoaded.send({
    id: "net-http:feed",
    data: { created_at: "2026-08-23T14:02:11.000Z", method: "GET", body: "[]", meta: { status: 200, ms: 9 } },
  });
  await settle();
  const line = all("pre.run")[0]?.textContent ?? "";
  assert(line.includes("2026-08-23 14:02:11"), `the run's own row, in one line: ${line}`);
  assert(line.includes("GET") && line.includes("HTTP 200 · 9 ms"), `the run's own row, in one line: ${line}`);

  // A feed's row and an alert's row are told apart by the `method` the row
  // itself carries, never by which shape happens to decode. A feed whose
  // fetched body is a JSON object carrying its own "status" and "delivery"
  // keys -- a webhook-shaped API is exactly this shape -- would otherwise read
  // as the alert verdict those two words mean on an alert's row.
  app.ports.runLoaded.send({
    id: "net-http:feed",
    data: {
      created_at: "2026-08-23T14:02:11.000Z",
      method: "GET",
      body: '{"status":"ok","delivery":"fast"}',
      meta: { status: 200, ms: 9 },
    },
  });
  await settle();
  assert(
    (all("pre.run")[0]?.textContent ?? "").includes("HTTP 200 · 9 ms"),
    `a feed's row reads its meta, never its body's own "status"/"delivery" keys: ${all("pre.run")[0]?.textContent}`,
  );

  // The other half of the same rule: an alert run that failed never reached a
  // delivery, so its row says why under `error` and there is no `delivery`
  // field to find.
  app.ports.runLoaded.send({
    id: "net-http:feed",
    data: {
      created_at: "2026-08-23T14:02:11.000Z",
      method: "ALERT",
      body: '{"status":"error","rows":0,"error":"no such column: nope"}',
      meta: { ms: 3 },
    },
  });
  await settle();
  assert(
    (all("pre.run")[0]?.textContent ?? "").includes("error · no such column: nope"),
    `an alert run that failed says why, rather than a decoder's complaint about a missing delivery: ${
      all("pre.run")[0]?.textContent
    }`,
  );

  app.ports.runLoaded.send({ id: "net-http:feed", data: { error: "Sheet net-http:feed is paused." } });
  await settle();
  assert(text().includes("is paused"), "a refusal is shown in the server's own words");

  // Ticking the box writes the document, the way every other field on this form
  // does: one patch, one field.
  const ticked = box();
  assert(ticked, "the switch is still there");
  ticked.checked = true;
  ticked.dispatchEvent(new dom.window.Event("change", { bubbles: true }));
  await settle();
  assertEquals(patches, [{ action: "set", path: [0, "paused"], value: true }]);

  // The schedule is two text fields written the same way. The page checks
  // neither: the poller's refusal is the check, and the run row shows it.
  for (const [label, value] of [["cron schedule", "0 9 * * 1-5"], ["cron timezone", "America/Chicago"]]) {
    const input = all(`input[aria-label="${label}"]`)[0] as unknown as
      | { value: string; dispatchEvent: (e: unknown) => boolean }
      | undefined;
    assert(input, `a feed offers the ${label}`);
    input.value = value;
    input.dispatchEvent(new dom.window.Event("input", { bubbles: true }));
  }
  await settle();
  assertEquals(patches.slice(1), [
    { action: "set", path: [0, "cron"], value: "0 9 * * 1-5" },
    { action: "set", path: [0, "timezone"], value: "America/Chicago" },
  ]);

  // Off the freshness the page already receives. A paused sheet says so rather
  // than naming a time its due entry still holds and no longer means.
  app.ports.freshnessLoaded.send([
    { sheet_id: "net-http:feed", last_run: null, failures_since_ok: 0, next_run: "2026-08-23T15:02:11.000Z" },
  ]);
  await settle();
  assert(text().includes("next run 2026-08-23 15:02"), `expected the next run, got: ${text().slice(0, 400)}`);

  app.ports.docSelected.send({
    id: "net-http:feed",
    data: {
      doc: { type: "net-http", data: [{ url: "https://example.com/feed.json", interval: 3600, paused: true }] },
    },
  });
  await settle();
  assertEquals(box()?.checked, true, "a paused document draws the switch ticked");
  assert(!text().includes("next run "), "and says paused instead of a time that no longer means anything");
});

// Silence without deletion. The chip writes the document the way every other
// field on this form does, and what is drawn is what the document holds: the
// line and its way out only while the moment it names is still ahead.
Deno.test("an alert can be snoozed for a day, and the snooze runs out on its own", async () => {
  const { app, all, text, click, settle } = await boot("http://localhost/");
  const patches: { action: string; path: unknown[]; value: unknown }[] = [];
  app.ports.changeDoc.subscribe((sent: { data: typeof patches }) => patches.push(...sent.data));
  const open = async (snoozed: Record<string, string>) => {
    app.ports.docSelected.send({
      id: "alert:burn",
      data: { doc: { type: "alert", data: [{ code: "select 1", to: "ops@example.com", interval: 3600, ...snoozed }] } },
    });
    await settle();
  };
  const chip = (label: string) => all("button.chip").find((b) => b.textContent === label);

  await open({});
  assert(chip("snooze a day"), "an alert nobody silenced offers the chip");
  assert(!text().includes("snoozed until"), "and says nothing about a snooze");

  await click(chip("snooze a day"));
  assertEquals(patches.length, 1, "one chip, one patch, one field");
  assertEquals([patches[0].action, patches[0].path], ["set", [0, "snoozed_until"]]);
  const ahead = Date.parse(String(patches[0].value)) - Date.now();
  assert(
    ahead > 86_000_000 && ahead <= 86_400_000,
    `the chip writes a timestamp a day ahead, received ${patches[0].value} (${ahead} ms away)`,
  );

  await open({ snoozed_until: new Date(Date.now() + 86_400_000).toISOString() });
  assert(text().includes("snoozed until"), `a snooze still ahead says so, got: ${text().slice(0, 300)}`);
  assertEquals(chip("snooze a day"), undefined, "and is not offered a second time");
  await click(chip("unsnooze"));
  assertEquals(patches[1], { action: "set", path: [0, "snoozed_until"], value: "" });

  // Nothing has to clear it: the page reads the same clock the poller does.
  await open({ snoozed_until: new Date(Date.now() - 86_400_000).toISOString() });
  assert(!text().includes("snoozed until"), "a snooze whose moment has passed is over");
  assert(chip("snooze a day"), "so the chip is offered again");
});

// The verbs that only mean something over the sheet that is open. Neither is on
// the shortcut sheet -- neither has a key -- so the palette is where they live,
// and each opens a door the footer already has a row for.
Deno.test("the palette subscribes to the sheet that is open, and builds a cohort table from it", async () => {
  const { dom, doc, app, settle } = await boot("http://localhost/table:countries");
  const made: { type: string; data: Record<string, unknown>[] }[] = [];
  app.ports.newDoc.subscribe((sent: { type: string; data: Record<string, unknown>[] }) => made.push(sent));
  const key = async (el: El, init: Record<string, unknown>) => {
    el.dispatchEvent(new dom.window.KeyboardEvent("keydown", { bubbles: true, ...init }));
    await settle();
  };
  const rows = () => [...doc.querySelectorAll(".scrim .panel button")].map((b: El) => b.textContent ?? "");
  const type = async (value: string) => {
    const input = doc.getElementById("palette");
    input.value = value;
    input.dispatchEvent(new dom.window.Event("input", { bubbles: true }));
    await settle();
  };

  await key(doc.body, { key: "k", ctrlKey: true });
  assert(
    !rows().some((row) => row.includes("subscribe to this sheet")),
    `a page that does not know the account has nowhere to address an alert, got: ${rows().join("|")}`,
  );
  await key(doc.getElementById("palette"), { key: "Escape" });

  // The other verb that only means something over the sheet that is open. It
  // needs no login, unlike subscribing: the fields it sends carry no address
  // the way an alert's destination does, and the footer's own new-query door
  // asks nothing either. The columns are guessed -- the first date, the first
  // name ending in _id ahead of the plain text column before it, the first
  // money column -- and what goes out is the fields, because src/sql.mjs is
  // what turns them into the statement.
  app.ports.docSelected.send({
    id: "table:signups",
    data: {
      doc: {
        type: "table",
        data: [
          [
            { name: "plan", type: "text", key: "0" },
            { name: "signup_id", type: "text", key: "1" },
            { name: "joined_on", type: "date", key: "2" },
            { name: "fee", type: "usd", key: "3" },
            { name: "seats", type: "int", key: "4" },
          ],
          { "0": "pro", "1": "a", "2": "2024-01-03", "3": 5, "4": 2 },
        ],
      },
    },
  });
  await settle();
  await key(doc.body, { key: "k", ctrlKey: true });
  await type("build a cohort");
  assert(
    rows().some((row) => row.includes("build a cohort table")),
    `a logged-out visitor can still build a cohort table, got: ${rows().join("|")}`,
  );
  await key(doc.getElementById("palette"), { key: "ArrowDown" });
  await key(doc.getElementById("palette"), { key: "Enter" });
  assertEquals(made.length, 1, "the cohort command opens one sheet");
  assertEquals(made[0].type, "query");
  assertEquals(made[0].data[0], {
    lang: "sql",
    cohort: {
      source: "@table:signups",
      date: "joined_on",
      key: "signup_id",
      value: "fee",
      grain: "month",
    },
  });

  // The same three guesses score the customers, into five buckets.
  await key(doc.body, { key: "k", ctrlKey: true });
  await type("rfm");
  await key(doc.getElementById("palette"), { key: "ArrowDown" });
  await key(doc.getElementById("palette"), { key: "Enter" });
  assertEquals(made.length, 2, "the score command opens one sheet");
  assertEquals(made[1].type, "query");
  assertEquals(made[1].data[0], {
    lang: "sql",
    rfm: { source: "@table:signups", date: "joined_on", key: "signup_id", value: "fee", buckets: 5 },
  });

  // The same key segments the rows by every numeric column, into three.
  await key(doc.body, { key: "k", ctrlKey: true });
  await type("k-means");
  await key(doc.getElementById("palette"), { key: "ArrowDown" });
  await key(doc.getElementById("palette"), { key: "Enter" });
  assertEquals(made.length, 3, "the segment command opens one sheet");
  assertEquals(made[2].type, "query");
  assertEquals(made[2].data[0], {
    lang: "sql",
    kmeans: { source: "@table:signups", key: "signup_id", columns: ["fee", "seats"], k: 3 },
  });

  // A cohort counts keys with no money column; a score has nothing to score,
  // and one numeric column is a sort, not a segmentation.
  app.ports.docSelected.send({
    id: "table:unpriced",
    data: {
      doc: {
        type: "table",
        data: [[{ name: "plan", type: "text", key: "0" }, { name: "joined_on", type: "date", key: "1" }, {
          name: "seats",
          type: "int",
          key: "2",
        }], { "0": "pro", "1": "2024-01-03", "2": 2 }],
      },
    },
  });
  await settle();
  await key(doc.body, { key: "k", ctrlKey: true });
  await type("this sheet");
  assert(
    rows().some((row) => row.includes("build a cohort table")) &&
      !rows().some((row) => row.includes("(RFM)") || row.includes("(k-means)")),
    `a sheet with no money column and one numeric column has a cohort but no score and no segments, got: ${
      rows().join("|")
    }`,
  );
  await key(doc.getElementById("palette"), { key: "Escape" });

  // A date column with no id-shaped or text column beside it has no key to
  // group by, and a command that can only fail is not a command.
  app.ports.docSelected.send({
    id: "table:numbers-only",
    data: {
      doc: {
        type: "table",
        data: [
          [
            { name: "measured_on", type: "date", key: "0" },
            { name: "count", type: "num", key: "1" },
            { name: "total", type: "usd", key: "2" },
          ],
          { "0": "2024-01-03", "1": 3, "2": 5 },
        ],
      },
    },
  });
  await settle();
  await key(doc.body, { key: "k", ctrlKey: true });
  await type("this sheet");
  assert(
    !rows().some((row) => row.includes("build a cohort table") || row.includes("(RFM)") || row.includes("(k-means)")),
    `a sheet with no key column has no cohort, no customers to score and no rows to segment, got: ${rows().join("|")}`,
  );
  await key(doc.getElementById("palette"), { key: "Escape" });

  // A sheet with no date column has no cohort to build either.
  app.ports.docSelected.send({
    id: "table:undated",
    data: {
      doc: {
        type: "table",
        data: [[{ name: "plan", type: "text", key: "0" }, { name: "fee", type: "usd", key: "1" }], {
          "0": "pro",
          "1": 5,
        }],
      },
    },
  });
  await settle();
  await key(doc.body, { key: "k", ctrlKey: true });
  await type("this sheet");
  assert(
    !rows().some((row) => row.includes("build a cohort table") || row.includes("(RFM)")),
    `a sheet with no date column has no cohort to build and no customers to score, got: ${rows().join("|")}`,
  );
  await key(doc.getElementById("palette"), { key: "Escape" });

  // Back to the sheet the rest of this test builds the alert from -- only the
  // id matters from here, since nothing after this reads table:countries' own
  // columns.
  app.ports.docSelected.send({
    id: "table:countries",
    data: { doc: { type: "table", data: [[{ name: "a", type: "text", key: "0" }], { "0": "x" }] } },
  });
  await settle();

  app.ports.authResult.send({ usr_id: "u1", email: "ops@example.com" });
  await settle();
  await key(doc.body, { key: "k", ctrlKey: true });
  await type("subscribe");
  await key(doc.getElementById("palette"), { key: "ArrowDown" });
  await key(doc.getElementById("palette"), { key: "Enter" });
  assertEquals(made.length, 4, "one command, one sheet");
  assertEquals(made[3].type, "alert");
  assertEquals(made[3].data[0], {
    code: "select * from @table:countries",
    to: "ops@example.com",
    interval: 3600,
    digest: false,
    when: "added",
  });

  // A chart's rows are the query it draws, and a run log is not a thing to
  // watch either: the verb is offered over the sheets whose rows are data.
  app.ports.docSelected.send({
    id: "chart:spend",
    data: { doc: { type: "chart", data: [{ source: "@table:countries", kind: "line", x: "a", y: "b" }] } },
  });
  await settle();
  await key(doc.body, { key: "k", ctrlKey: true });
  await type("subscribe");
  assert(
    !rows().some((row) => row.includes("subscribe to this sheet")),
    `a chart is not a sheet to subscribe to, got: ${rows().join("|")}`,
  );
  await key(doc.getElementById("palette"), { key: "Escape" });

  // A chart keeps no columns of its own -- `arrangeable` answers Nothing for
  // it -- so there is nothing to guess a cohort's date and key from either.
  await key(doc.body, { key: "k", ctrlKey: true });
  await type("this sheet");
  assert(
    !rows().some((row) => row.includes("build a cohort table") || row.includes("(RFM)")),
    `a chart has no cohort to build and no customers to score, got: ${rows().join("|")}`,
  );
});

Deno.test("a chart sheet draws one bar per row and offers both ways to save it", async () => {
  const { app, all, settle } = await boot("http://localhost/chart:burn-by-department");
  // src/index.html turns a chart's settings into SQL with the same chartSql the
  // server uses and sends the answer back through this port; examples_test.ts is
  // what checks the SQL. What is checked here is that the page draws it.
  app.ports.docQueried.send({
    id: "chart:burn-by-department",
    data: [
      [{ key: "x", name: "x", type: "text" }, { key: "y", name: "y", type: "num" }],
      { x: "Police", y: 1.05 },
      { x: "Fire", y: 1.02 },
      { x: "Parks", y: 0.81 },
      { x: "Library", y: 0.97 },
      { x: "Planning", y: 0.93 },
      { x: "Public Works", y: 1.24 },
    ],
  });
  await settle();

  assertEquals(all("svg").length, 1, "a chart sheet should draw one chart");
  assertEquals(all("svg rect").length, 6, "a bar chart should draw one bar per row");
  assertEquals(
    all("button.chip").map((b) => b.textContent).filter((t) => t === "svg" || t === "png").join(),
    "svg,png",
    "a chart should offer both ways to save it",
  );
});

// A box is the one kind whose rows are not points: five numbers per x, already
// grouped by its own query. What is checked here is that the page draws all five
// -- a box with no whisker is the failure mode, and it looks like a chart.
Deno.test("a box chart draws a box and its whiskers at each x", async () => {
  const { app, all, settle } = await boot("http://localhost/chart:dim-spread");
  app.ports.docQueried.send({
    id: "chart:dim-spread",
    data: [
      [
        { key: "x", name: "x", type: "text" },
        { key: "lo", name: "lo", type: "num" },
        { key: "q1", name: "q1", type: "num" },
        { key: "med", name: "med", type: "num" },
        { key: "q3", name: "q3", type: "num" },
        { key: "hi", name: "hi", type: "num" },
      ],
      { x: "A", lo: 1, q1: 2, med: 3, q3: 4, hi: 5 },
      { x: "B", lo: 2, q1: 3, med: 4, q3: 5, hi: 6 },
    ],
  });
  await settle();

  assertEquals(all("svg").length, 1, "a box chart is still one chart");
  assertEquals(all("svg rect").length, 2, "one quartile box per x");
  // Four rules per box: the whisker, its two caps, and the median across it.
  assertEquals(all("svg g line").length, 8, "a whisker, two caps and a median per box");
});

// Two scales, kept apart all the way down: the left labels are the first
// column's extent and the right labels are the second's, which is the whole
// point -- on one scale the second series would be a flat line along the bottom.
Deno.test("a chart with a second column draws a second scale on the right", async () => {
  const { app, all, settle } = await boot("http://localhost/chart:pair-ratio-z");
  app.ports.docQueried.send({
    id: "chart:pair-ratio-z",
    data: [
      [
        { key: "x", name: "x", type: "text" },
        { key: "y", name: "y", type: "num" },
        { key: "y2", name: "y2", type: "num" },
      ],
      { x: "2026-01-01", y: 100, y2: 1 },
      { x: "2026-01-02", y: 300, y2: 3 },
    ],
  });
  await settle();

  const labels = all("svg text").map((t) => t.textContent);
  assert(labels.includes("300") && labels.includes("3"), `expected both scales' tops, received ${labels.join(",")}`);
  // Anchored at the right-hand edge is how the second scale is told from the
  // first, which sits at x 4.
  const right = all("svg text").filter((t) => t.getAttribute("text-anchor") === "end").map((t) => t.textContent);
  assert(right.includes("3"), `expected the second scale's top at the right edge, received ${right.join(",")}`);
  // One dashed polyline: the second y is always a line, whatever the kind.
  assertEquals(
    all("svg polyline").filter((p) => p.getAttribute("stroke-dasharray")).length,
    1,
    "the second scale is drawn as one dashed line",
  );
  // A mark the chart's own document carries, placed on the day axis.
  assertEquals(all("svg line[stroke-dasharray]").length, 1, "the bundled chart's annotation is drawn as one rule");
  assert(labels.includes("earnings"), `expected the mark's label, received ${labels.join(",")}`);
});

Deno.test("a dashboard lays its tiles out as embeds of the sheets it names", async () => {
  const { all } = await boot("http://localhost/dashboard:budget-watch");
  const srcs = all("iframe").map((f) => f.getAttribute("src"));
  assertEquals(
    srcs,
    ["/chart:burn-by-department?embed=1", "/query:budget-burn?embed=1"],
    "each tile should embed the sheet it names, in order",
  );
});

Deno.test("Ctrl+/ opens the shortcut sheet and Escape closes it", async () => {
  const { dom, doc, settle, text } = await boot("http://localhost/table:countries");
  const key = async (init: Record<string, unknown>) => {
    doc.body.dispatchEvent(new dom.window.KeyboardEvent("keydown", { bubbles: true, ...init }));
    await settle();
  };
  await key({ key: "/", ctrlKey: true });
  assert(text().includes("Keyboard shortcuts"), "Ctrl+/ should open the shortcut sheet");
  await key({ key: "Escape" });
  assert(!text().includes("Keyboard shortcuts"), "Escape should close it again");
});

Deno.test("the library says which feeds ran and which are failing", async () => {
  const { app, all, settle, doc } = await boot("http://localhost/");
  // The header row is the one carrying the sort handles; a column's position is
  // read off it rather than counted, so a column added before it cannot silently
  // move what these assertions read.
  const header = () => [...doc.querySelectorAll("tbody tr")].find((r: El) => r.querySelector("span.sort"));
  const columns = () => [...header().querySelectorAll("span.sort")].map((s: El) => s.textContent);
  const freshnessOf = (id: string) => {
    const at = [...header().querySelectorAll("td")]
      .findIndex((td: El) => td.querySelector("span.sort")?.textContent?.startsWith("freshness"));
    const row = [...doc.querySelectorAll("tbody tr")].find((r: El) => r.querySelector(`a[href="/${id}"]`));
    assert(row, `no library row for ${id}`);
    return [...row.querySelectorAll("td")][at].textContent?.trim() ?? "";
  };

  // An anonymous visitor is sent nothing, so there is no column at all. A blank
  // one over every sheet would read as a library where nothing is wrong.
  assert(!columns().includes("freshness"), `expected no freshness column yet, got: ${columns().join("|")}`);

  // Two sheets of the caller's own, which is where a net-http or alert sheet
  // lives; the bundled examples are neither.
  app.ports.librarySynced.send({
    ...shelf,
    "net-http:feed": { name: "prices feed", tags: [] },
    "alert:budget": { name: "budget alert", tags: [] },
  });
  app.ports.freshnessLoaded.send([
    { sheet_id: "net-http:feed", last_run: "2026-08-23T14:02:11.000Z", failures_since_ok: "3", next_run: null },
    { sheet_id: "alert:budget", last_run: null, failures_since_ok: 0, next_run: null },
  ]);
  await settle();

  assert(columns().includes("freshness"), `expected a freshness column, got: ${columns().join("|")}`);
  assertEquals(freshnessOf("net-http:feed"), "2026-08-23 14:02 · 3 failed");
  // A sheet that has never run is exactly the failure this read is for, which is
  // why both of its joins are lateral. It must not read as a sheet with no feed.
  assertEquals(freshnessOf("alert:budget"), "never run");
  // library:freshness answers for net-http and alert sheets only. Everything
  // else has no freshness, and no freshness is nothing -- not a zero.
  assertEquals(freshnessOf("table:countries"), "");
  assert(all("tbody tr").length > 3, "the library still lists its sheets");
});

Deno.test("a feed health answer that cannot be read is reported, not swallowed", async () => {
  // The alternative is a column that quietly shows nothing because the server
  // renamed a field: a healthy-looking library is the one lie this must not tell.
  const { app, settle, text } = await boot("http://localhost/");
  app.ports.freshnessLoaded.send([{ sheet_id: "net-http:feed", last_run: null, failures: 2 }]);
  await settle();
  assert(text().includes("feed health"), `expected the failure named, got: ${text().slice(0, 300)}`);
});

Deno.test("a failing sheet is marked in the gallery strip, where it is opened from", async () => {
  const { app, all, settle } = await boot("http://localhost/");
  const chip = () => all("a.chip").find((a) => a.getAttribute("title") === "query:budget-burn");
  assert(chip(), "expected the budget demo in the strip");
  assert(!(chip()?.textContent ?? "").includes("⚠"), "a sheet with no freshness carries no mark");

  app.ports.freshnessLoaded.send([
    { sheet_id: "query:budget-burn", last_run: "2026-08-23T14:02:11.000Z", failures_since_ok: 2, next_run: null },
  ]);
  await settle();
  assert((chip()?.textContent ?? "").includes("⚠"), "a failing sheet is marked in the strip");
  // The title is still the bare id: it is how the strip is read back, here and
  // in the test above.
  assertEquals(chip()?.getAttribute("title"), "query:budget-burn");
});

Deno.test("Ctrl+K opens the palette, which jumps to a sheet and runs a command", async () => {
  const { dom, doc, settle, text } = await boot("http://localhost/");
  const key = async (el: El, init: Record<string, unknown>) => {
    el.dispatchEvent(new dom.window.KeyboardEvent("keydown", { bubbles: true, ...init }));
    await settle();
  };
  const type = async (value: string) => {
    const input = doc.getElementById("palette");
    input.value = value;
    input.dispatchEvent(new dom.window.Event("input", { bubbles: true }));
    await settle();
  };
  // A row is its label followed by the id or key it runs, so the label is what
  // is left once the hint is taken off the end.
  const rows = () =>
    [...doc.querySelectorAll(".scrim .panel button")].map((b: El) => {
      const whole = b.textContent ?? "";
      return whole.slice(0, whole.length - (b.querySelector("span.mono")?.textContent ?? "").length).trim();
    });
  const hints = () => [...doc.querySelectorAll(".scrim .panel button span.mono")].map((s: El) => s.textContent ?? "");

  await key(doc.body, { key: "k", ctrlKey: true });
  assert(doc.getElementById("palette"), "Ctrl+K should open the palette");
  // Nothing typed: every shortcut the sheet lists as runnable is on offer, which
  // is the point of the palette reading that list rather than keeping its own.
  for (const label of ["select all", "copy", "find", "replace", "undo", "redo", "shortcut sheet"])
    assert(rows().includes(label), `expected "${label}" in the palette, got: ${rows().join("|")}`);

  // Enter on a palette nobody has pointed at runs nothing. It opened on the
  // first row, and the first row is a verb that deletes rows -- two keystrokes,
  // no confirmation. The arrow is what points at one, and a whole-sheet verb on
  // the library then says why it cannot run: the library lists sheets, it holds
  // no rows of its own, and it is the sheet the palette is opened from most.
  await key(doc.getElementById("palette"), { key: "Enter" });
  assert(doc.getElementById("palette"), "Enter on an untouched palette runs nothing, so the palette stays open");
  assertEquals(doc.location.pathname, "/", "and nothing was opened");
  // An emptied box is that same state. Typing set the selection to the first
  // row whatever was typed, so a character and a backspace put Enter back on
  // the verb that deletes rows -- three keystrokes from opening the palette.
  await type("s");
  await type("");
  await key(doc.getElementById("palette"), { key: "Enter" });
  assert(doc.getElementById("palette"), "an emptied box points at nothing again, so the palette stays open");
  await key(doc.getElementById("palette"), { key: "ArrowDown" });
  await key(doc.getElementById("palette"), { key: "Enter" });
  assert(
    text().includes("lists your sheets rather than holding rows of its own"),
    `a verb the library cannot run should say so, got: ${text().slice(0, 300)}`,
  );

  await key(doc.body, { key: "k", ctrlKey: true });
  await type("budget");
  assert(!rows().includes("select all"), `typing narrows the list, got: ${rows().join("|")}`);
  assert(rows().length > 1, "several sheets match budget");

  // The hint on each row is the sheet's id, so the second row names where
  // ArrowDown then Enter must land -- without hard-coding which sheet that is.
  const second = hints()[1];
  await key(doc.getElementById("palette"), { key: "ArrowDown" });
  await key(doc.getElementById("palette"), { key: "Enter" });
  assertEquals(doc.location.pathname, "/" + second, "Enter opens the row the arrow moved to");
  assertEquals(doc.getElementById("palette"), null, "running a row closes the palette");

  // A command, not a sheet. The palette is a second door onto what already
  // exists, so this opens the very sheet that lists it.
  await key(doc.body, { key: "k", ctrlKey: true });
  await type("shortcut");
  await key(doc.getElementById("palette"), { key: "Enter" });
  assert(text().includes("Keyboard shortcuts"), "the palette should run the command it offered");
  assert(text().includes("Ctrl/⌘+K"), "and the shortcut sheet should list the palette itself");
});

Deno.test("Escape closes the palette without running anything", async () => {
  const { dom, doc, settle } = await boot("http://localhost/table:countries");
  const at = doc.location.pathname;
  doc.body.dispatchEvent(new dom.window.KeyboardEvent("keydown", { key: "k", ctrlKey: true, bubbles: true }));
  await settle();
  const input = doc.getElementById("palette");
  assert(input, "Ctrl+K should open the palette here too");
  input.dispatchEvent(new dom.window.KeyboardEvent("keydown", { key: "Escape", bubbles: true }));
  await settle();
  assertEquals(doc.getElementById("palette"), null, "Escape closes it");
  assertEquals(doc.location.pathname, at, "and nothing was opened");
});

// A past version is text in a plain table, drawn over the live sheet. Every key
// the live sheet takes is a write, so under the history none of them reaches it.
Deno.test("the history lists a sheet's versions, draws a past one, and no key writes under it", async () => {
  const { dom, doc, app, all, click, fire, settle, text } = await boot("http://localhost/table:countries");
  const chip = () => all("button").find((b) => b.textContent === "history");
  assertEquals(chip(), undefined, "a bundled sheet has no automerge document, so it offers no history");
  const writes: unknown[] = [], loads: unknown[] = [], views: unknown[] = [];
  app.ports.changeDoc.subscribe((w: unknown) => writes.push(w));
  app.ports.historyLoad.subscribe((id: unknown) => loads.push(id));
  app.ports.historyView.subscribe((ask: unknown) => views.push(ask));
  const cols = [{ key: "a", name: "city", type: "text" }, { key: "b", name: "", type: "text" }];
  app.ports.docSelected.send({
    id: "table:mine",
    data: { doc: { type: "table", data: [cols, { a: "Lima", b: "x" }] } },
  });
  await settle();
  const key = async (init: Record<string, unknown>) => {
    doc.body.dispatchEvent(new dom.window.KeyboardEvent("keydown", { bubbles: true, ...init }));
    await settle();
  };
  // One write first, so Ctrl+Z has something it could undo.
  const lima = all("td").find((td) => td.textContent?.trim() === "Lima");
  assert(lima, "the live sheet is drawn");
  for (const type of ["mouseenter", "mousedown", "mouseup"]) await fire(lima, type);
  await key({ key: "Delete" });
  assertEquals(writes.length, 1, "Delete on the live sheet writes");

  await click(chip());
  assertEquals(loads, ["table:mine"], "the chip asks for the open sheet's history");
  assert(text().includes("reading the history"), "and says so until it lands");
  app.ports.historyLoaded.send({ id: "table:other", data: { versions: [], left: 0 } });
  await settle();
  assert(text().includes("reading the history"), "an answer for another sheet is not this one's");
  const version = (hash: string, time: number, seq: number) => ({
    hash,
    time,
    actor: "0123456789abcdef",
    seq,
    message: null,
  });
  app.ports.historyLoaded.send({
    id: "table:mine",
    data: { versions: [version("h2", 1790000000, 2), version("h1", 0, 1)], left: 3 },
  });
  await settle();
  assertEquals(
    all("#versions button").map((b) => b.textContent),
    ["2026-09-21T14:13:20Z01234567", "change 101234567"],
    "each version by its stamp, or by its sequence number when automerge has no time",
  );
  assert(text().includes("3 older versions are not listed."), "and the answer's count of what it left out");

  await click(all("#versions button")[1]);
  assertEquals(views, [{ id: "table:mine", hash: "h1" }], "picking a version asks for its rows");
  app.ports.historyShown.send({
    id: "table:mine",
    data: { hash: "h1", columns: ["city", ""], rows: [["Quito", null]] },
  });
  await until(settle, "the past rows", () => all("#past td").length > 0);
  assertEquals(
    [all("#past th").map((th) => th.textContent), all("#past td").map((td) => td.textContent)],
    [["city", ""], ["Quito", "NULL"]],
    "the past version is its column names over its cells, as text",
  );

  await key({ key: "z", ctrlKey: true });
  await key({ key: "Delete" });
  await key({ key: "x" });
  app.ports.pasteFromClipboard.send("pasted");
  await settle();
  assertEquals(writes.length, 1, "Ctrl+Z, Delete, typing and a paste write nothing under the history");
  assertEquals(all("#new-cell").length, 0, "and no key opened an editor");
  await key({ key: "Escape" });
  assertEquals(all("#past").length, 0, "Escape closes the history");
});

Deno.test("?embed=1 renders the sheet with no chrome around it", async () => {
  const { doc, all } = await boot("http://localhost/table:countries?embed=1");
  assert(all("tbody tr").length > 190, "an embed still renders the sheet");
  for (const sel of ["#title", "#aside", "input[placeholder=search]"])
    assertEquals(doc.querySelector(sel), null, `an embed should carry no ${sel}`);
});

// --- src/page.mjs, on its own
//
// Two of these parse XML, so they need a DOMParser. Installing jsdom's once here
// is what the page gets from the browser for free.
(globalThis as Record<string, unknown>).DOMParser =
  (new JSDOM("", { url: "http://localhost/" }).window as unknown as Record<string, unknown>).DOMParser;

Deno.test("the library merges what is stored under what is bundled", () => {
  const stored = {
    "table:mine": { name: "mine", seen: "2026-09-03T10:00:00.000Z", doc: { type: "table", data: [{}] } },
    // A stale copy of a bundled example must not shadow the real one.
    "table:countries": {
      name: "an old countries",
      seen: "2026-09-04T10:00:00.000Z",
      doc: { type: "table", data: [{}] },
    },
  };
  const shelf = library(stored) as Record<string, { name: string; system?: boolean; thumb?: unknown; seen?: string }>;

  assertEquals(shelf["table:mine"].name, "mine", "a stored sheet survives the merge");
  assertEquals(shelf["table:mine"].seen, "2026-09-03T10:00:00.000Z", "and so does when it was opened");
  assertEquals(shelf["table:countries"].name, "countries", "the bundled countries wins over a stored copy");
  // `seen` is this browser's fact whoever owns the entry, so it is the one
  // stored field a bundled sheet keeps.
  assertEquals(shelf["table:countries"].seen, "2026-09-04T10:00:00.000Z", "but when this browser opened it is kept");
  assertEquals(shelf["table:tutorial"].seen, undefined, "a sheet never opened carries no seen");
  // The same argument, for the same reason: whether this browser threw a sheet
  // away is this browser's fact, so a bundled demo can be put in the trash too.
  const trashed = library({ "table:countries": { trashed: true }, "table:mine": { name: "mine" } }) as Record<
    string,
    { name: string; trashed?: boolean }
  >;
  assertEquals(trashed["table:countries"].trashed, true, "a bundled sheet keeps this browser's trash flag");
  assertEquals(trashed["table:countries"].name, "countries", "and still loses its name to the bundled one");
  assertEquals(trashed["table:mine"].trashed, undefined, "a sheet nobody trashed carries no flag");
  // Restoring writes false rather than null, because Library.set drops a null
  // out of the patch instead of out of the entry.
  assertEquals(
    (library({ "table:countries": { trashed: false } }) as Record<string, { trashed?: boolean }>)["table:countries"]
      .trashed,
    undefined,
    "and a restored sheet carries none either",
  );
  // And the third of them: starring a bundled demo has to outlive the merge, or
  // the star vanishes on the next port send.
  const starred = library({ "table:countries": { starred: true }, "table:mine": { name: "mine" } }) as Record<
    string,
    { name: string; starred?: boolean }
  >;
  assertEquals(starred["table:countries"].starred, true, "a bundled sheet keeps this browser's star");
  assertEquals(starred["table:countries"].name, "countries", "and still loses its name to the bundled one");
  assertEquals(starred["table:mine"].starred, undefined, "a sheet nobody starred carries no flag");
  // And the fourth: a folder is this browser's filing, so a bundled demo can be
  // filed, and unfiling writes "" rather than null.
  const filed = library({ "table:countries": { folder: "work" }, "table:tutorial": { folder: "" } }) as Record<
    string,
    { name: string; folder?: string }
  >;
  assertEquals(filed["table:countries"].folder, "work", "a bundled sheet keeps this browser's folder");
  assertEquals(filed["table:countries"].name, "countries", "and still loses its name to the bundled one");
  assertEquals(filed["table:tutorial"].folder, undefined, "an unfiled bundled sheet carries no folder");
  // Tags are the same kind of fact and the one that merges rather than overlays:
  // a tag this browser put on a bundled demo has to outlive the merge, and the
  // demo's own tags are what the gallery strip filters on.
  const tags = library({ "query:lybunt": { tags: ["mine", "demo"] }, "table:mine": { name: "mine" } }) as Record<
    string,
    { tags?: string[] }
  >;
  assertEquals(tags["query:lybunt"].tags, ["demo", "nonprofit", "query", "mine"], "bundled tags first, stored after");
  assertEquals(tags["table:mine"].tags, undefined, "a sheet nobody tagged carries none");
  // A stored tags field this browser never wrote -- a hand-edited or stale
  // localStorage value -- used to throw spreading a number or an object, and
  // silently exploded a string into one letter per tag. Neither is a sheet the
  // whole library should go down for.
  for (const bad of [5, true, { a: 1 }, "oops"]) {
    const merged = library({ "query:lybunt": { tags: bad } }) as Record<string, { tags?: string[] }>;
    assertEquals(
      merged["query:lybunt"].tags,
      ["demo", "nonprofit", "query"],
      `a stored tags of ${JSON.stringify(bad)} should not reach the merge`,
    );
  }
  assertEquals(shelf[""].name, "library", "the empty id is the library itself");
  for (const p of PORTALS) assert(shelf[`portal:${p}`], `portal:${p} should be listed`);
  assert(shelf["table:tutorial"], "the tutorial is part of the library");
  // Every entry with a doc gets a thumbnail, which is what the library rows draw.
  for (const [id, entry] of Object.entries(shelf)) assert(entry.thumb, `${id} should carry a thumb`);
});

// The library's `opened` column is when this browser last opened the sheet,
// sortable like any other, so the sheet you were just in is one click away.
Deno.test("the library shows when each sheet was opened, and sorts by it", async () => {
  const { app, all, click, settle, text } = await boot("http://localhost/");
  app.ports.librarySynced.send({
    ...shelf,
    "table:mine": { name: "mine", tags: [], seen: "2026-09-04T10:00:00.000Z", doc: { type: "table", data: [[]] } },
  });
  await settle();
  const header = () => all("span.sort").find((s) => s.textContent?.startsWith("opened"));
  assert(header(), "the library has an opened column");
  assert(text().includes("2026-09-04T10:00:00.000Z"), "and the row shows when it was opened");
  await click(header());
  await click(header());
  const firstName = [...all("tbody tr")[3].querySelectorAll("td")][2].textContent;
  assertEquals(firstName, "mine", "descending puts the last opened first, and the never-opened last");
});

// Deleting a sheet used to be one irreversible click on a browser-local shelf,
// under a modal that said so. Trashing is the reversible half, so it asks
// nothing; the modal is kept for the purge, where the warning is true.
Deno.test("a sheet is trashed without a dialog, hidden from the library, and restored from the trash", async () => {
  const { app, all, click, settle, text } = await boot("http://localhost/");
  const sent: { id: string; data: { trashed: boolean | null } }[] = [];
  app.ports.updateLibrary.subscribe((s: (typeof sent)[number]) => sent.push(s));
  app.ports.librarySynced.send({
    ...shelf,
    "table:mine": { name: "mine", tags: [], doc: { type: "table", data: [[]] } },
  });
  await settle();
  assert(text().includes("mine"), "the sheet is in the library to begin with");

  const rowFor = (name: string) =>
    all("tbody tr").find((tr) => [...tr.querySelectorAll("td")].some((td) => td.textContent?.trim() === name));
  const button = (row: El | undefined, label: string) =>
    [...(row?.querySelectorAll("button") ?? [])].find((b) => b.textContent?.trim() === label);

  await click(button(rowFor("mine"), "trash"));
  assertEquals(
    sent.map((s) => [s.id, s.data.trashed]),
    [["table:mine", true]],
    "trashing writes the flag, and only it",
  );
  assertEquals(all(".scrim").length, 0, "and asks nothing first, because it is undoable");

  // The page is told what the browser stored, the way index.html tells it.
  app.ports.librarySynced.send({
    ...shelf,
    "table:mine": { name: "mine", tags: [], trashed: true, doc: { type: "table", data: [[]] } },
  });
  await settle();
  assert(!text().includes("mine"), "a trashed sheet leaves the library");

  const chip = all("button.chip").find((b) => b.textContent?.startsWith("🗑"));
  assert(chip, "the trash says how much is in it");
  assertEquals(chip.textContent?.trim(), "🗑 1");
  await click(chip);
  assert(text().includes("mine"), "and opening it shows what was thrown away");
  assert(!text().includes("table:..."), "with nothing offered to create in there");

  sent.length = 0;
  await click(button(rowFor("mine"), "restore"));
  assertEquals(sent.map((s) => [s.id, s.data.trashed]), [["table:mine", false]], "restoring writes false, never null");
});

// A library of many sheets is navigated by what you keep, so the star rides the
// same port as the trash: this browser's own fact about somebody else's sheet.
// Starred-first stands in for "nobody has sorted this yet" and is not a
// tiebreaker under a real sort: `List.sortWith` is stable, so leaving the star
// pre-sort under a chosen sort kept starred rows first among that sort's ties,
// which reads as the sort not taking.
Deno.test("a sheet is starred from its row, drawn first, dropped by a chosen sort, and unstarred back to false", async () => {
  const { app, all, click, settle } = await boot("http://localhost/");
  const sent: { id: string; data: { starred: boolean | null } }[] = [];
  app.ports.updateLibrary.subscribe((s: (typeof sent)[number]) => sent.push(s));
  // A library of this test's own, so the drawn order is known, and a name that
  // sorts last, so being drawn first can only be the star.
  const lib = (starred: boolean) => ({
    "": { name: "library", system: true, doc: { type: "library" } },
    "table:a": { name: "a", tags: [], doc: { type: "table", data: [[]] } },
    "table:b": { name: "b", tags: [], doc: { type: "table", data: [[]] } },
    "table:c": { name: "c", tags: [], doc: { type: "table", data: [[]] } },
    "table:zebra": { name: "zebra", tags: [], starred, doc: { type: "table", data: [[]] } },
  });
  app.ports.librarySynced.send(lib(false));
  await settle();

  const rowFor = (name: string) =>
    all("tbody tr").find((tr) => [...tr.querySelectorAll("td")].some((td) => td.textContent?.trim() === name));
  const star = (row: El | undefined) =>
    [...(row?.querySelectorAll("button") ?? [])].find((b) => "★☆".includes(b.textContent?.trim() ?? ""));
  // Row 3 is the first data row: the header, the type row and the stats row
  // come first, the way the opened-column test reads them.
  const names = () => all("tbody tr").slice(3).map((tr) => [...tr.querySelectorAll("td")][2]?.textContent?.trim());

  await click(star(rowFor("zebra")));
  assertEquals(
    sent.map((s) => [s.id, s.data.starred]),
    [["table:zebra", true]],
    "starring writes the flag, and only it",
  );

  // The page is told what the browser stored, the way index.html tells it.
  app.ports.librarySynced.send(lib(true));
  await settle();
  assertEquals(names(), ["zebra", "a", "b", "c"], "a starred sheet is drawn first while nobody has chosen a sort");

  // Sort by "tags" -- every row here has none, so this sort is all ties, and a
  // stable sort over the starred pre-sort would still read "zebra" first.
  const tagsHeader = all("span.sort").find((s) => s.textContent?.trim().toLowerCase() === "tags");
  assert(tagsHeader, "a tags header exists");
  await click(tagsHeader);
  assertEquals(names(), ["a", "b", "c", "zebra"], "a chosen sort orders every tie on its own, not on who is starred");

  sent.length = 0;
  await click(star(rowFor("zebra")));
  assertEquals(
    sent.map((s) => [s.id, s.data.starred]),
    [["table:zebra", false]],
    "unstarring writes false, never null",
  );
});

// Trashing one row at a time is how a library of many sheets stays untidy. The
// selection already says which rows, so the shortcut fans one updateLibrary out
// over them -- a top-level Msg, because every DocMsg on the library is refused.
Deno.test("the selected library rows are trashed and tagged together, and an empty selection is refused by name", async () => {
  const { app, all, click, doc, dom, fire, settle, text, type_ } = await boot("http://localhost/");
  const sent: { id: string; data: { trashed: boolean | null; tags: string[] | null; folder: string | null } }[] = [];
  app.ports.updateLibrary.subscribe((s: (typeof sent)[number]) => sent.push(s));
  const key = async (init: Record<string, unknown>) => {
    doc.body.dispatchEvent(new dom.window.KeyboardEvent("keydown", { bubbles: true, ...init }));
    await settle();
  };

  await key({ key: "Backspace", ctrlKey: true, shiftKey: true });
  assertEquals(sent, [], "a selection over no row writes nothing");
  assert(text().includes("holding no sheet"), `expected the refusal by name, got: ${text().slice(0, 200)}`);

  // The tag box refuses the same selection, in its own words.
  await type_(all("input").find((i) => i.getAttribute("placeholder") === "tag"), "mine");
  await click(all("button.chip").find((b) => b.textContent?.trim() === "tag selected"));
  assertEquals(sent, [], "a tag over no row writes nothing");
  assert(text().includes("select the rows to tag"), `expected the refusal by name, got: ${text().slice(0, 200)}`);

  // A library of this test's own, so the rows under the selection are known.
  app.ports.librarySynced.send({
    "": { name: "library", system: true, doc: { type: "library" } },
    "table:a": { name: "a", tags: ["keep"], doc: { type: "table", data: [[]] } },
    "table:b": { name: "b", tags: [], doc: { type: "table", data: [[]] } },
    "table:c": { name: "c", tags: [], doc: { type: "table", data: [[]] } },
  });
  await settle();
  const first = all("td").find((td) => td.textContent?.trim() === "a");
  assert(first, "the library is drawn");
  await fire(first, "mouseenter");
  await fire(first, "mousedown");
  await fire(first, "mouseup");
  await key({ key: "ArrowDown", shiftKey: true });
  await key({ key: "Backspace", ctrlKey: true, shiftKey: true });
  // Sorted, because a Cmd.batch says nothing about the order its commands are
  // delivered in and nothing downstream asks for one: each is one sheet's own
  // flag, and Library.set merges them a patch at a time.
  assertEquals(
    sent.map((s) => [s.id, s.data.trashed]).sort(),
    [["table:a", true], ["table:b", true]],
    "one updateLibrary per selected row, and the row nobody selected is left alone",
  );

  // The same selection and the same fan-out for the strip's tag box, which is
  // an argument rather than a shortcut: the tag is added to what each row
  // already carries, so the tags a demo ships with survive it.
  sent.length = 0;
  const box = all("input").find((i) => i.getAttribute("placeholder") === "tag");
  await type_(box, " mine ");
  box?.dispatchEvent(new dom.window.KeyboardEvent("keydown", { bubbles: true, key: "Enter" }));
  await settle();
  assertEquals(
    sent.map((s) => [s.id, s.data.tags]).sort(),
    [["table:a", ["keep", "mine"]], ["table:b", ["mine"]]],
    "the tag is added to each selected row, trimmed, and the other tags are kept",
  );
  // The harness's updateLibrary subscriber only records what was sent -- the
  // round trip through Library.set and back onto librarySynced is what the
  // real page does, so it is redone here by hand for the checks below to read
  // a library that already reflects the write above. `trashed` is left off on
  // purpose: the earlier trash write never reached this browser's stored
  // library either, in this harness, and folding it in here would drop table:a
  // and table:b out of the drawn table, moving the same y under the selection
  // onto a different sheet -- a real hazard `libraryIdAtRow`'s own doc names
  // ("the rows as drawn"), but not the one this block is testing.
  app.ports.librarySynced.send({
    "": { name: "library", system: true, doc: { type: "library" } },
    "table:a": { name: "a", tags: ["keep", "mine"], doc: { type: "table", data: [[]] } },
    "table:b": { name: "b", tags: ["mine"], doc: { type: "table", data: [[]] } },
    "table:c": { name: "c", tags: [], doc: { type: "table", data: [[]] } },
  });
  await settle();

  sent.length = 0;
  await type_(box, "  ");
  await click(all("button.chip").find((b) => b.textContent?.trim() === "tag selected"));
  assertEquals(sent, [], "a tag of nothing writes nothing");
  assert(
    text().includes("Expected a tag, received nothing"),
    `expected the refusal by name, got: ${text().slice(0, 200)}`,
  );

  // A tags cell is read back by splitting on ", ", so a comma here would come
  // back as two tags nobody typed -- refused rather than split for them.
  sent.length = 0;
  await type_(box, "a, b");
  await click(all("button.chip").find((b) => b.textContent?.trim() === "tag selected"));
  assertEquals(sent, [], "a tag holding a comma writes nothing");
  assert(
    text().includes("Expected one tag, received"),
    `expected the refusal by name, got: ${text().slice(0, 200)}`,
  );

  // Both selected rows already carry "mine" from above, so nothing is written
  // at all -- not an empty selection, and not a refusal either: the sheets
  // simply have nothing to gain from running it again.
  sent.length = 0;
  await type_(box, "mine");
  await click(all("button.chip").find((b) => b.textContent?.trim() === "tag selected"));
  assertEquals(sent, [], "a tag every selected row already carries writes nothing");

  // A tag differing only in case is a different tag: the free-hand cell edit
  // does not lowercase either, so neither does this.
  sent.length = 0;
  await type_(box, "Mine");
  await click(all("button.chip").find((b) => b.textContent?.trim() === "tag selected"));
  assertEquals(
    sent.map((s) => [s.id, s.data.tags]).sort(),
    [["table:a", ["keep", "mine", "Mine"]], ["table:b", ["mine", "Mine"]]],
    "case is not folded, so Mine is added beside mine",
  );

  // A selection that starts on the header row. Arrow-key navigation cannot
  // reach it -- `clampIndex` holds y at 1 or above -- but a mouse drag can:
  // the header is one more row of the same table, drawn by the same
  // viewCell/CellHover pair as any data row. `libraryIdAtRow` answers Nothing
  // at row 0 the same way it does one past the end, so it is dropped from the
  // fan-out rather than crashing or being asked about.
  const headerCell = all("td").find((td) =>
    td.querySelector("span.sort")?.textContent?.trim().toLowerCase() === "tags"
  );
  assert(headerCell, "the tags header cell exists");
  sent.length = 0;
  await fire(headerCell, "mouseenter");
  await fire(headerCell, "mousedown");
  await fire(first, "mouseenter");
  await fire(first, "mouseup");
  await type_(box, "header-safe");
  await click(all("button.chip").find((b) => b.textContent?.trim() === "tag selected"));
  assertEquals(
    sent.map((s) => [s.id, s.data.tags]),
    // Reads model.library, not the "Mine" write two blocks up -- this
    // harness's updateLibrary subscriber never round-trips on its own, and
    // that write's own librarySynced echo was never resent.
    [["table:a", ["keep", "mine", "header-safe"]]],
    "the header row in the selection is silently skipped, not crashed on",
  );

  // The folder box fans out like the tag box, over rows a and b again.
  const folderBox = all("input").find((i) => i.getAttribute("aria-label") === "folder");
  const move = () => click(all("button.chip").find((b) => b.textContent?.trim() === "move to folder"));
  await fire(first, "mouseenter");
  await fire(first, "mousedown");
  await fire(first, "mouseup");
  await key({ key: "ArrowDown", shiftKey: true });
  sent.length = 0;
  await type_(folderBox, "  ");
  await move();
  assertEquals(sent, [], "a folder of nothing writes nothing");
  assert(
    text().includes("Expected a folder, received nothing"),
    `expected the refusal by name, got: ${text().slice(0, 200)}`,
  );
  await type_(folderBox, " work/2026 ");
  await move();
  assertEquals(
    sent.map((s) => [s.id, s.data.folder]).sort(),
    [["table:a", "work/2026"], ["table:b", "work/2026"]],
    "one updateLibrary per selected row, the folder trimmed",
  );
  // Library.set's round trip, by hand: a null field is dropped from the patch.
  const store: Record<string, Record<string, unknown>> = {
    "": { name: "library", system: true, doc: { type: "library" } },
    "table:a": { name: "a", tags: ["keep"], doc: { type: "table", data: [[]] } },
    "table:b": { name: "b", tags: [], doc: { type: "table", data: [[]] } },
    "table:c": { name: "c", tags: [], doc: { type: "table", data: [[]] } },
  };
  for (const { id, data } of sent)
    store[id] = { ...store[id], ...Object.fromEntries(Object.entries(data).filter(([, v]) => v !== null)) };
  assertEquals([store["table:a"].folder, store["table:a"].tags], ["work/2026", ["keep"]], "the store holds the folder");
  app.ports.librarySynced.send(store);
  await settle();
  const cellsOf = (name: string) => {
    const row = all("tbody tr").find((tr) => [...tr.querySelectorAll("td")][2]?.textContent?.trim() === name);
    return row ? [...row.querySelectorAll("td")] : [];
  };
  assertEquals(
    ["a", "b", "c"].map((n) => cellsOf(n)[4]?.textContent?.trim()),
    ["work/2026", "work/2026", ""],
    "each moved row's folder cell reads the name",
  );
  await type_(all("main > input").find((i) => i.getAttribute("aria-label") === "search the rows"), "work/");
  assertEquals(["a", "b", "c"].map((n) => cellsOf(n).length > 0), [true, true, false], "search matches the folder");
  await type_(all("main > input").find((i) => i.getAttribute("aria-label") === "search the rows"), "");

  // The cell holds the box's rule: one folder, no comma. Unfiling is blanking
  // the cell, which writes "" and never null.
  sent.length = 0;
  const folderCell = cellsOf("a")[4];
  for (const type of ["mouseenter", "mousedown", "mouseup", "click", "dblclick"]) await fire(folderCell, type);
  await type_(all("#new-cell")[0], "work, play");
  all("#new-cell")[0].dispatchEvent(new dom.window.FocusEvent("blur"));
  await settle();
  assertEquals(sent, [], "a folder cell holding a comma writes nothing");
  assert(
    text().includes("Expected one folder, received work, play"),
    `expected the refusal, got: ${text().slice(0, 200)}`,
  );
  for (const type of ["mouseenter", "mousedown", "mouseup", "click", "dblclick"]) await fire(cellsOf("a")[4], type);
  await type_(all("#new-cell")[0], "");
  all("#new-cell")[0].dispatchEvent(new dom.window.FocusEvent("blur"));
  await settle();
  assertEquals(sent.map((s) => [s.id, s.data.folder]), [["table:a", ""]], "a blanked folder cell unfiles the sheet");
});

// A rename in the library lands on the sheet whose row was edited, which is the
// row as drawn -- sorted, filtered and searched -- and not the same position in
// the unsorted dictionary. It used to be the latter, so a rename in a sorted
// library renamed a stranger.
Deno.test("renaming a row in a sorted library renames the sheet on that row", async () => {
  const { app, all, fire, type_, click, settle, dom } = await boot("http://localhost/");
  const renamed: { id: string; data: { name: string | null } }[] = [];
  app.ports.updateLibrary.subscribe((sent: (typeof renamed)[number]) => renamed.push(sent));
  await settle();

  const nameHeader = () => all("span.sort").find((s) => s.textContent?.startsWith("name"));
  await click(nameHeader());
  await click(nameHeader());
  assert(nameHeader()?.textContent?.endsWith("▼"), "two clicks sort the library by name, descending");
  const first = all("tbody tr")[3];
  const firstId = [...first.querySelectorAll("a")][0]?.getAttribute("href")?.slice(1);
  assert(firstId, "the first row links to its sheet");
  const nameCell = [...first.querySelectorAll("td")][2];
  for (const type of ["mouseenter", "mousedown", "mouseup", "click", "dblclick"]) await fire(nameCell, type);
  await type_(all("#new-cell")[0], "renamed");
  // The write lands on blur, which `type_` here does not send.
  all("#new-cell")[0].dispatchEvent(new dom.window.FocusEvent("blur"));
  await settle();
  assertEquals(renamed.map((r) => [r.id, r.data.name]), [[firstId, "renamed"]], "the rename names the row's own sheet");
});

// The sync server refuses a viewer's write, and a bundled sheet has no document
// of this browser's to write to at all. Both keep their arrangement here: the
// patches folded into a partial of data[0], merged back over the document on the
// way in.
Deno.test("a remembered header lays its types over the server's guesses", () => {
  const cols = [{ name: "a", type: "num" }, { name: "b", type: "text" }];
  assertEquals(
    rememberedTypes({ [importKey(["a", "b"])]: { a: "text" } }, cols),
    [{ name: "a", type: "text", remembered: true }, { name: "b", type: "text", remembered: false }],
  );
  assertEquals(
    rememberedTypes({ [importKey(["a"])]: { a: "text" } }, cols),
    [{ name: "a", type: "num", remembered: false }, { name: "b", type: "text", remembered: false }],
    "a different header is a different memory",
  );
  assertEquals(
    rememberedTypes(undefined, cols).map((c: { type: string }) => c.type),
    ["num", "text"],
    "no memory keeps the guess",
  );
});

Deno.test("a Tab inside a modal wraps at both ends, and lands nowhere in an empty one", () => {
  assertEquals(trapStep(0, -1, false), -1, "no control: nowhere to land, forward");
  assertEquals(trapStep(0, -1, true), -1, "no control: nowhere to land, back");
  assertEquals([trapStep(1, 0, false), trapStep(1, 0, true), trapStep(1, -1, true)], [0, 0, 0], "one control holds");
  assertEquals([trapStep(3, -1, false), trapStep(3, -1, true)], [0, 2], "from outside: the first, or the last");
  assertEquals([trapStep(3, 0, false), trapStep(3, 1, true)], [1, 0], "a step inside");
  assertEquals([trapStep(3, 2, false), trapStep(3, 0, true)], [0, 2], "and a wrap at each end");
  for (const [count, index, back] of [[0, 0, false], [3, 3, false], [3, -2, true], [2.5, 0, false], [3, 0, "yes"]])
    assertThrows(() => trapStep(count, index, back), Error, "I cannot step focus through a modal from here.");
});

Deno.test("an arrangement this browser has to keep is held by column key", () => {
  type Column = Record<string, unknown>;
  const cols: Column[] = [{ key: "a", name: "a", type: "text" }, { key: "b", name: "b", type: "text", sort: "desc" }];

  // A table's patch names a position and a query's names the column. Both are
  // held under the column's own key: a position is not stable in a document
  // somebody else can reorder, and holding one is how a column you hid becomes a
  // different column you hid.
  const held = foldView(undefined, [{ action: "set", path: [0, "1", "hidden"], value: true }], cols);
  const both = foldView(held, [{ action: "set", path: [0, "view", "b", "width"], value: 220 }], {});
  assertEquals(both, { b: { hidden: true, width: 220 } }, "both homes land under the column's key");

  // Only view fields go out on arrangeDoc. Anything else arriving here is a
  // patch on the wrong port, and folding it away quietly would lose an edit.
  assertThrows(() => foldView(both, [{ action: "move", path: [0], value: [2, 0] }], cols), Error, "arrangeDoc");

  // A cleared field is held as null rather than dropped: a viewer who clears a
  // sort must not get the owner's back on the next reload.
  const cleared = foldView({ b: { sort: "asc" } }, [{ action: "del", path: [0, "1", "sort"], value: null }], cols);
  assertEquals(cleared, { b: { sort: null } }, "a cleared field is held, not forgotten");

  const merged = mergeView(cols, { b: { hidden: true } }) as Column[];
  assertEquals(merged[1], { key: "b", name: "b", type: "text", sort: "desc", hidden: true }, "the merge adds a field");
  assertEquals(cols[1].hidden, undefined, "and copies rather than writing into a document that is not ours");
  assertEquals(
    (mergeView(cols, { b: { sort: null } }) as Column[])[1].sort,
    undefined,
    "a null clears the field the document carries",
  );

  // A held key the document no longer carries is dropped. Creating the column to
  // hold it padded the list with holes, and a hole reads back as a blank column
  // the sheet never had.
  assertEquals(
    mergeView(cols, { gone: { hidden: true } }),
    cols,
    "a column that is gone takes its arrangement with it",
  );

  // A query's held view goes back into the map beside its code, leaving what the
  // document already holds for another column alone.
  assertEquals(
    mergeView({ lang: "sql", code: "select 1", view: { a: { sort: "asc" } } }, { b: { hidden: true } }),
    { lang: "sql", code: "select 1", view: { a: { sort: "asc" }, b: { hidden: true } } },
    "a query's view merges beside its code",
  );
});

Deno.test("a thumbnail is the first numeric column, scaled, or nothing to draw", () => {
  const table = (...rows: unknown[]) => ({
    type: "table",
    data: [{ 0: { key: "0", name: "label", type: "text" }, 1: { key: "1", name: "n", type: "num" } }, ...rows],
  });
  // The text column is skipped; the numeric one is scaled across 0..1.
  assertEquals(
    docThumb(table({ 0: "a", 1: 10 }, { 0: "b", 1: 20 }, { 0: "c", 1: 30 })),
    { kind: "table", cols: 2, rows: 3, spark: [0, 0.5, 1] },
  );
  // Fewer than three numbers is not a line.
  assertEquals(docThumb(table({ 0: "a", 1: 10 }, { 0: "b", 1: 20 })).spark, []);
  // A flat column has no range to scale against, so every point sits in the middle
  // rather than dividing by zero.
  assertEquals(docThumb(table({ 0: "a", 1: 7 }, { 0: "b", 1: 7 }, { 0: "c", 1: 7 })).spark, [0.5, 0.5, 0.5]);
  // A sheet that is not a table has a shape but no line.
  assertEquals(docThumb({ type: "query", data: [] }), { kind: "query", cols: 0, rows: 0, spark: [] });
  assertEquals(docThumb(undefined), { kind: "unknown", cols: 0, rows: 0, spark: [] });
});

Deno.test("only a third-party url goes through the proxy", () => {
  const ours = httpTarget("http://localhost/net/abc", { q: "x" }, "http://localhost");
  assertEquals([ours.viaProxy, ours.url], [false, "http://localhost/net/abc?q=x"]);

  const api = httpTarget(`${API_BASE}/sheet/table:abc`, {}, "http://localhost");
  assertEquals(api.viaProxy, false, "our own api is not third-party");

  const theirs = httpTarget("https://export.arxiv.org/api/query", { search_query: "all:" }, "http://localhost");
  assertEquals(theirs.viaProxy, true);
  assert(theirs.url.startsWith(`${API_BASE}/proxy?url=`), theirs.url);
  // The whole target, query string included, is encoded into one parameter --
  // splitting it would let the second parameter escape into the proxy's own.
  assertEquals(
    decodeURIComponent(theirs.url.split("url=")[1]),
    "https://export.arxiv.org/api/query?search_query=all%3A",
  );
});

Deno.test("an origin's own words are dug out of whatever shape it sent them in", () => {
  // The arxiv regression this exists for: the summary is the only place the
  // reason appears, and reporting "HTTP 400" instead says nothing.
  assertEquals(
    httpErrorDetail(
      `<?xml version='1.0' encoding='UTF-8'?><feed xmlns="http://www.w3.org/2005/Atom"><entry>` +
        `<title>Error</title><summary>Either a search_query or id_list must be specified.</summary></entry></feed>`,
      "application/atom+xml",
    ),
    "Either a search_query or id_list must be specified.",
  );
  assertEquals(httpErrorDetail(`{"error":{"info":"rate limited"}}`, "application/json"), "rate limited");
  assertEquals(httpErrorDetail(`{"error":"nope"}`, "application/json"), "nope");
  assertEquals(httpErrorDetail(`{"message":"bad key"}`, "application/json"), "bad key");
  // Something that claims to be json and is not falls through to the raw body
  // rather than throwing on top of the error it was reporting.
  assertEquals(httpErrorDetail("<html>500</html>", "application/json"), "<html>500</html>");
  assertEquals(httpErrorDetail("   ", "text/plain"), "(empty body)");
});

Deno.test("an http failure says who refused, and shows the url", () => {
  const origin = httpFailure({
    status: 404,
    url: "https://example.test/x?a=1",
    body: `{"message":"no such thing"}`,
    contentType: "application/json",
  });
  assert(origin.includes("This server responded with 404"), origin);
  assert(origin.includes("https://example.test/x?a=1"), origin);
  assert(origin.includes("no such thing"), origin);

  // The same status from our own proxy is a different problem with a different
  // fix, so it must not read as the origin's answer.
  const proxy = httpFailure({
    status: 400,
    url: "http://127.0.0.1/x?",
    body: `{"error":"Internal URLs not allowed."}`,
    contentType: "application/json",
    rejectedByProxy: true,
  });
  assert(proxy.includes("The scrapsheets proxy rejected this request with 400"), proxy);
  assert(proxy.includes("Internal URLs not allowed."), proxy);

  assert(httpUnreachable("https://example.test/x").includes("cross-origin"), "unreachable names the likely cause");
  const unparsed = httpUnparsed({
    url: "https://example.test/x",
    contentType: "application/json",
    body: "<html>hello",
    message: "Unexpected token <",
  });
  assert(unparsed.includes("application/json, which I could not parse"), unparsed);
  assert(unparsed.includes("<html>hello"), "the body is shown, because the content type already lied");
  assertEquals(httpUnparsed({ url: "u", contentType: "c", body: "  ", message: "m" }).includes("(empty body)"), true);
});

Deno.test("an atom feed flattens into rows a query can select from", () => {
  const feed = atomToJson(
    `<?xml version='1.0' encoding='UTF-8'?><feed xmlns="http://www.w3.org/2005/Atom">` +
      `<title>arXiv Query</title><updated>2026-08-22T00:00:00Z</updated>` +
      `<opensearch:totalResults xmlns:opensearch="http://a9.com/-/spec/opensearch/1.1/">2</opensearch:totalResults>` +
      `<entry><id>http://arxiv.org/abs/2601.00001</id><title> Attention Reconsidered </title>` +
      `<summary> A summary. </summary><published>2026-01-02T00:00:00Z</published>` +
      `<author><name>R. Okonkwo</name></author><author><name>M. Petrova</name></author>` +
      `<link href="http://arxiv.org/abs/2601.00001" rel="alternate" type="text/html"/>` +
      `<category term="cs.LG"/></entry>` +
      `<entry><id>http://arxiv.org/abs/2601.00002</id><title>Second</title></entry></feed>`,
  ) as { title: string; totalResults: number; entries: Record<string, unknown>[] };

  assertEquals(feed.title, "arXiv Query");
  assertEquals(feed.totalResults, 2);
  assertEquals(feed.entries.length, 2);
  // Titles and summaries arrive wrapped in whitespace; a query grouping by title
  // would otherwise see two of everything.
  assertEquals(feed.entries[0].title, "Attention Reconsidered");
  assertEquals(feed.entries[0].summary, "A summary.");
  assertEquals(feed.entries[0].authors, ["R. Okonkwo", "M. Petrova"]);
  assertEquals(feed.entries[0].categories, ["cs.LG"]);
  assertEquals(feed.entries[1].authors, [], "an entry with no authors is empty, not missing");
});

Deno.test("a @sheet ref loads the sheet behind it and serves its rows", async () => {
  const rows = await rowsOf("select name, code from @table:countries where code = 'JP'");
  assertEquals(rows, [{ name: "Japan", code: "JP" }]);
});

Deno.test("a @query ref runs that query first, windows and all", async () => {
  // query:budget-burn reads query:budget-ytd, which is where the running totals
  // are computed. Handing the inner query to AlaSQL instead of recursing would
  // answer with zeros, which is the bug the recursion exists to avoid.
  const rows = await rowsOf("select department, burn_ratio from @query:budget-burn order by burn_ratio desc");
  assertEquals(rows.length, 6);
  assertEquals(rows[0].department, "Public Works");
  assert((rows[0].burn_ratio as number) > 1.2, `expected an overspend, got ${rows[0].burn_ratio}`);
});

Deno.test("a cell reference reads one value out of a one-row sheet", async () => {
  const rows = await rowsOf("select @table:assumptions.entry_z as band");
  assertEquals(rows, [{ band: 2 }]);
});

Deno.test("the resolver reports every way a ref can fail, by name", async () => {
  // A typo gets the nearest sheet rather than a list to read.
  const typo = await refused("select * from @table:countrys");
  assert(typo.includes("Did you mean: @table:countries"), typo);

  // Only a table or a query can be read as rows. A portal is a live socket and a
  // chart is a picture of a query, so neither is a relation.
  assert((await refused("select * from @portal:time")).includes("only table and query sheets"));
  assert((await refused("select * from @chart:pair-z")).includes("only table and query sheets"));

  // A sheet nobody has is named, not silently empty.
  const missing = await refused("select * from @table:nothing-like-this-at-all");
  assert(missing.includes("this sheet has no data"), missing);

  // A cell reference needs exactly one row, or it would pick one arbitrarily.
  const many = await refused("select @table:countries.name as n");
  assert(many.includes("one row"), many);
});

Deno.test("a reference cycle is refused as the path that closes it", async () => {
  const chart = { doc: { type: "query", data: [{ code: "select * from @query:loop" }] } };
  const loop = sheets(
    alasql,
    () => ({ ...shelf, "query:loop": chart } as Record<string, unknown>),
    () => Promise.resolve(undefined),
  );
  try {
    await loop.runSql("select * from @query:loop", { "": null });
    throw new Error("a self-referencing query should not run");
  } catch (err) {
    const said = (err as Error).message;
    assert(said.includes("@query:loop -> @query:loop"), said);
  }
});

Deno.test("describe reports a sheet whose cells are wrong, and select still will not", async () => {
  // The one statement that has to work on a broken sheet, because that is the
  // sheet you need to inspect. main_test.ts asserts the same thing of the server;
  // before both engines shared loadRefs, the page refused it and the server did
  // not, and nothing said so.
  const bad = {
    doc: {
      type: "table",
      data: [{ 0: { key: "0", name: "amount", type: "usd" } }, { 0: 10 }, { 0: "n/a" }],
    },
  };
  const engine = () =>
    sheets(alasql, () => ({ ...shelf, "table:bad": bad } as Record<string, unknown>), () => Promise.resolve(undefined));

  const { data } = await engine().runSql("describe @table:bad", { "": null });
  assertEquals(data, [{ column: "amount", type: "usd", rows: 2, nulls: 0, sample: "10" }]);

  try {
    await engine().runSql("select sum(amount) from @table:bad", { "": null });
    throw new Error("a sum over a column holding text should not be answered");
  } catch (err) {
    assert((err as Error).message.includes(`"n/a"`), (err as Error).message);
  }
});

Deno.test("describe answers for a query sheet too, through the chain under it", async () => {
  const { data } = await resolver().runSql("describe @query:budget-burn", { "": null });
  assertEquals(
    (data as Record<string, unknown>[]).map((r) => r.column),
    ["department", "spent_ytd", "adopted_ytd", "burn_ratio", "projected_year", "projected_variance"],
  );
});

Deno.test("a join that would walk more pairs than one run is allowed is refused before the engine", async () => {
  // Same guard as the server's, in the same pass, so the page refuses what
  // the server refuses rather than freezing the tab on it.
  const four = ["a", "b", "c", "d"].map((alias) => `@table:countries ${alias}`).join(", ");
  const said = await refused(`select count(*) as n from ${four}`);
  const n = (shelf["table:countries"] as unknown as { doc: { data: unknown[] } }).doc.data.length - 1;
  assert(said.includes(`${n ** 4} pairs`), said);
  assert(said.includes("filter each large sheet"), said);
  // A self-join of two is what the demos do, and it still runs.
  const [row] = await rowsOf(`select count(*) as n from @table:countries a, @table:countries b where a.code = b.code`);
  assertEquals(row.n, n);
});

Deno.test("explain answers the query's profile in the page, one row per stage", async () => {
  const { columns, data } = await resolver().runSql("explain select * from @table:countries", { "": null });
  assertEquals(columns.map((c: { columnid: string }) => c.columnid), ["stage", "rows_in", "rows_out", "ms"]);
  const rows = data as Record<string, unknown>[];
  assertEquals(rows.map((r) => r.stage), ["load @table:countries", "plan", "engine", "total"]);
  assertEquals(rows[0].rows_in, null, "nothing precedes a load");
  assertEquals(rows.at(-1)!.rows_out, rows[2].rows_out, "total answers what the last stage kept");
  for (const r of rows) assert(typeof r.ms === "number" && (r.ms as number) >= 0, JSON.stringify(r));
  assert((await refused("explain describe @table:countries")).includes("profiles a query"));
  assert((await refused("explain")).includes("nothing after explain"), "a bare explain is ours, not AlaSQL's");
  assert(
    (await refused("explain select nope from @table:countries")).includes("nope"),
    "the query's own checks still stand",
  );

  // A @query ref's load is the nested run, which is what names the slow ref.
  const nested = (await resolver().runSql("explain select * from @query:budget-burn", { "": null }))
    .data as Record<string, unknown>[];
  assertEquals(nested[0].stage, "load @query:budget-burn");
  assert((nested[0].rows_out as number) > 0, "the nested query's rows are what the load stage answered");
  assertEquals(nested[1].rows_in, nested[0].rows_out, "and they are what the plan takes in");
});

Deno.test("a table whose cells contradict their column type is refused as it loads", async () => {
  const bad = {
    doc: {
      type: "table",
      data: [
        { 0: { key: "0", name: "amount", type: "usd" } },
        { 0: 10 },
        { 0: "n/a" },
      ],
    },
  };
  const engine = sheets(
    alasql,
    () => ({ ...shelf, "table:bad": bad } as Record<string, unknown>),
    () => Promise.resolve(undefined),
  );
  try {
    await engine.runSql("select sum(amount) from @table:bad", { "": null });
    throw new Error("a sum over a column holding text should not be answered");
  } catch (err) {
    const said = (err as Error).message;
    assert(said.includes(`"n/a"`) && said.includes("row 2"), said);
  }
});

Deno.test("the resolver remembers the columns a typo should be matched against", async () => {
  const engine = resolver();
  await engine.runSql("select 1 from @table:countries", { "": null });
  const columns = engine.columns();
  for (const name of ["name", "code", "region", "population"])
    assert(columns.includes(name), `expected ${name} among ${columns.join(", ")}`);
  assertEquals(engine.rows("table:countries")?.length, 198);
  assertEquals(engine.types("table:countries")?.length, 8);
});

Deno.test("a query result carries the type its select list produced, not its name's", async () => {
  // The page typed the sheet you are editing off that query document's own
  // `cols` map, so `cast(price as string) as price` still read usd and
  // `count(*) as n` read text, while POST /query answered the truth off the very
  // same text. Two engines disagreeing about a type is what the parity promise
  // forbids. runSql now stamps every column with selectTypes over the loaded
  // sheets, which is the map the server stamps its own answer with, and
  // src/index.html reads the type off the column.
  const typesOf = async (code: string) => {
    const { columns } = await resolver().runSql(code, { "": null });
    return Object.fromEntries(
      (columns as { columnid: string; type?: string }[]).map((c) => [c.columnid, c.type]),
    );
  };

  // The cast is the lie this closes: gdp_usd_b is usd in the sheet, and text
  // once the query casts it. An item nothing can type carries no type at all,
  // which is what leaves the query sheet's own declared cols the last word.
  assertEquals(
    await typesOf(
      "select code, cast(gdp_usd_b as string) as gdp_usd_b, gdp_usd_b * 2 as doubled from @table:countries where code = 'JP'",
    ),
    {
      code: "text",
      gdp_usd_b: "text",
      doubled: undefined,
    },
  );

  // count answers an int whatever it counts. sum and avg answer whatever their
  // argument is, so the money column stays money and the ratio stays a number.
  assertEquals(
    await typesOf(
      "select region, count(*) as n, sum(gdp_usd_b) as gdp, avg(area_km2) as area from @table:countries group by region",
    ),
    {
      region: "enum:Africa,Americas,Asia,Europe,Oceania",
      n: "int",
      gdp: "usd",
      area: "num",
    },
  );

  // And the same types across a @query ref, which is the sheet a downstream
  // query and `describe` both read.
  const { data } = await resolver().runSql("describe @query:lybunt", { "": null });
  assertEquals(
    (data as Record<string, unknown>[]).map((r) => [r.column, r.type]),
    [
      ["donor_id", "int"],
      ["donor", "text"],
      ["segment", "enum:foundation,major,sustainer,annual"],
      [
        "gifts",
        "int",
      ],
      ["lifetime", "usd"],
      ["largest", "usd"],
      ["last_gift", "text"],
    ],
  );
});

Deno.test("a share link is minted with the expiry and password that were typed", async () => {
  // The panel could only ever ask for the default link: index.html posted an
  // empty body, and the port had no field to carry anything else. The server
  // has taken { days, password } and enforced the lock since before any of it
  // was reachable from here.
  const { all, click, type_, asks, app, settle, text } = await boot(
    "http://localhost/table:countries#settings",
  );

  const button = all("button").find((b) => b.textContent?.includes("view-only link"));
  assert(button, `expected a link button in the settings panel, got: ${text().slice(0, 400)}`);

  // Untouched, it asks for what it always asked for: the server reads zero days
  // and no password as the thirty-day link anyone holding the url can open.
  await click(button);
  assertEquals(asks.at(-1)?.action, "link");
  assertEquals(asks.at(-1)?.days, 0);
  assertEquals(asks.at(-1)?.password, "");

  // By placeholder, not by type: the login form owns a password input too.
  await type_(all('input[placeholder="30 days"]')[0], "7");
  await type_(all('input[placeholder="password (optional)"]')[0], "correct horse");
  await click(button);
  assertEquals(asks.at(-1)?.days, 7, "the typed expiry has to reach the port as a number");
  assertEquals(asks.at(-1)?.password, "correct horse");

  // A number of days that is not one is refused rather than rounded down to
  // blank, which would mint a thirty-day link for somebody who asked for seven.
  const sent = asks.length;
  await type_(all('input[placeholder="30 days"]')[0], "7.5");
  await click(button);
  assertEquals(asks.length, sent, "nothing is asked for until the expiry parses");
  assert(text().includes("not a number of days"), `expected the typed value refused, got: ${text().slice(0, 400)}`);

  // And the answer lands: the link is shown, and the panel says the password is
  // not in it -- which is the one thing a reader of this link needs told, since
  // a lock the url carries would be no lock.
  app.ports.shareLoaded.send({
    id: "table:countries",
    action: "link",
    members: [],
    public: false,
    link: "http://localhost/table:countries?share=eyJhbGciOiJIUzI1NiJ9.e30.x",
  });
  await settle();
  // Read off the input, not the page text: the link is rendered as a readonly
  // <input>, so it is a property and never a text node.
  const shown = all("input[readonly]").map((el) => (el as unknown as { value: string }).value);
  assert(
    shown.some((v) => v.includes("?share=")),
    `expected the minted link in a readonly input, got: ${JSON.stringify(shown)}`,
  );
  assert(
    text().includes("password is not in it"),
    `expected the panel to say the password travels separately, got: ${text().slice(0, 400)}`,
  );
});
