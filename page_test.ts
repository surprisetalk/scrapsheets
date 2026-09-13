// The page, booted — without a browser.
//
// jsdom gives Deno a real enough DOM that the compiled Elm in dist/index.js
// initializes, renders, and responds to clicks. That is the half of the old
// headless-Chrome tests worth keeping: whether the app comes up and whether the
// table behaves, rather than whether Chrome can be launched in CI.
//
// deno-dom (the library Deno's own web-testing guide reaches for first) is not
// enough here: it has no `replaceData` on a text node, which is the call Elm's
// virtual-dom makes to patch text in place, so the first re-render throws.
//
// Two harnesses, and the difference matters. `boot` feeds Elm through the ports
// by hand, mirroring what src/index.html sends; `glue` runs that file's own
// module script over the same jsdom, so the ports, the browser store and the
// sync-refusal hook are the real ones. Reach for `boot` for anything about what
// the page renders, and for `glue` for anything about what the glue does.
import { assert, assertEquals, assertThrows } from "@std/assert";
import { JSDOM } from "jsdom";
import { EXAMPLES } from "./src/examples.mjs";
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
} from "./src/page.mjs";
import alasql from "./src/alasql.mjs";
import { boot, El, refused, resolver, rowsOf, shelf } from "./page_harness.ts";

// Three tests boot the library and then only read what it rendered, so they
// share one page. What that saves is Elm's first paint and not the harness
// around it: a jsdom, a re-evaluation of the already-hoisted bundle and Elm's
// init together are a rounding error beside the paint, and it is the same paint
// all three times. Nothing else in the file can share one -- every other boot
// either opens a different url, which is a different paint, or sends a port,
// clicks, types or fires, and a shared page carries whatever the test before it
// did to the model.
// Only the reading surface goes out: `app`, `click`, `type_`, `fire` and
// `settle` stay behind the memo. A shared page nobody can write to needs no
// rule that nobody should -- and the rule was not enforceable, since sending a
// doc down `docSelected` is what forty other tests in this file do to the page
// they booted, and doing it to this one replaced the library for every reader
// after, three tests failing on code nobody had touched.
let libraryPage: ReturnType<typeof boot> | undefined;
const rendered = async () => {
  const { text, all } = await (libraryPage ??= boot("http://localhost/", { tutorial: -1 }));
  return { text, all };
};

Deno.test("the page boots: Elm initializes and renders the library", async () => {
  const { text, all } = await rendered();
  assert(text().includes("scrapsheets"), `expected the app shell, got: ${text().slice(0, 200)}`);
  assert(!text().includes("loading"), "the library should have resolved, not stayed on loading");
  // Every tag in the library becomes a filter chip, so this is also the check
  // that the bundled sheets arrived through the port at all.
  const chips = all("button.chip").map((b) => b.textContent);
  for (const tag of ["demo", "reference", "dataset", "healthcare", "legal"])
    assert(chips.includes(tag), `expected a "${tag}" filter chip, got: ${chips.join("|")}`);
});

// `boot` owns the process globals between `globalize` and Elm's init, and this
// is the check that it never lets go of them mid-boot: a boot that suspends
// there loses the globals to the next one and renders an empty body -- no
// throw, no refusal, just a blank page and an assertion failing somewhere else.
// Awaiting two pages together is exactly what chasing this file's wall time
// invites, so the failure mode is bought back by a test rather than by a rule.
Deno.test("two pages booted at once each render their own document", async () => {
  const [countries, lib] = await Promise.all([
    boot("http://localhost/table:countries"),
    boot("http://localhost/"),
  ]);
  assert(
    countries.text().includes("China"),
    `Expected the page at /table:countries to render its rows, received a body of ` +
      `${countries.text().length} characters. Source: boot() writes the process globals the compiled Elm ` +
      `captures. Fix: do not await between globalize() and Elm's init.`,
  );
  assert(
    lib.text().includes("start from a demo"),
    `Expected the page at / to render the library, received: ${lib.text().slice(0, 160)}. ` +
      `Source: boot() writes the process globals the compiled Elm captures. ` +
      `Fix: do not await between globalize() and Elm's init.`,
  );
});

Deno.test("the gallery strip links every demo pipeline, and only those", async () => {
  const { all, text } = await rendered();
  assert(text().includes("start from a demo"), "expected the gallery strip");

  // The strip renders each demo as an <a class="chip"> whose title is the sheet
  // id, so read the links rather than the page text: every demo's name also
  // appears in the library table below, and matching there would pass even with
  // the strip missing entirely.
  const linked = all("a.chip").map((a) => a.getAttribute("title")).filter((id): id is string => !!id);
  const expected = Object.entries(EXAMPLES as Record<string, { tags: string[] }>)
    .filter(([id, e]) => id.startsWith("query:") && e.tags.includes("demo"))
    .map(([id]) => id);
  assert(expected.length > 20, `the gallery is the point of the demos; only ${expected.length} are tagged`);
  assertEquals(
    linked.slice().sort(),
    expected.slice().sort(),
    "the strip should link exactly the demo-tagged query sheets",
  );
});

Deno.test("a first visit gets the tutorial, and -1 dismisses it", async () => {
  const fresh = await boot("http://localhost/", { tutorial: 0 });
  assert(fresh.text().includes("get started"), "a first visit should show the tutorial card");
  assert(fresh.text().includes("create a table"), "expected the first tutorial step");

  const dismissed = await rendered();
  assert(!dismissed.text().includes("get started"), "-1 means the tutorial was dismissed");
});

Deno.test("a bundled table renders its rows, its grips and its stats", async () => {
  const { doc, all, text } = await boot("http://localhost/table:countries");
  const rows = all("tbody tr");
  assert(rows.length > 190, `expected a row per country, got ${rows.length}`);
  assert(text().includes("China"), "expected the countries themselves");
  assertEquals(all(".grip").length, all("span.sort").length, "every column header carries a resize grip");
  assert(doc.querySelector("td.c0"), "the first column should be addressable for the frozen-column style");
  // The totals row sums the numeric columns over the rows on screen.
  assert(doc.querySelector("tr.totals"), "expected a totals row");
});

Deno.test("clicking a header sorts, and shift-clicking adds a second key", async () => {
  const { all, click } = await boot("http://localhost/table:countries");
  const headers = () => all("span.sort").map((s) => s.textContent).join("|");

  await click(all("span.sort")[0]);
  assert(headers().includes("▲"), `expected an ascending arrow, got: ${headers()}`);

  // A second key gets a rank digit; a single key stays a bare arrow.
  await click(all("span.sort")[1], { shiftKey: true });
  assert(
    headers().includes("▲1") && headers().includes("▲2"),
    `expected ranked arrows, got: ${headers()}`,
  );

  // Shift-clicking the primary key again flips it without losing its rank.
  await click(all("span.sort")[0], { shiftKey: true });
  assert(
    headers().includes("▼1") && headers().includes("▲2"),
    `the primary key should flip and hold rank 1, got: ${headers()}`,
  );
});

// The bug this covers: a header rename emitted a whole rebuilt column object
// with `type` re-encoded beside the name, so renaming a column silently turned
// `int` into `num`, `percentage` into `pct` and `float` into a spelling the
// decoder did not know. One field per patch is the fix, and this is the proof.
Deno.test("renaming a column leaves its type exactly as the document spelled it", async () => {
  const { dom, app, all, settle, type_ } = await boot("http://localhost/table:currencies");
  const patches: { action: string; path: unknown[]; value: unknown }[] = [];
  app.ports.changeDoc.subscribe((sent: { data: typeof patches }) => patches.push(...sent.data));

  // `minor` is table:currencies' int column -- the spelling that used to be
  // flattened. Its header cell is the td the sort span sits in.
  const span = all("span.sort").find((s) => s.textContent?.startsWith("minor"));
  assert(span, "expected a `minor` column in table:currencies");
  const cell = (span as unknown as { closest: (s: string) => El | null }).closest("td");
  assert(cell, "the header span should sit in a td");

  for (const type of ["mouseenter", "click", "dblclick"]) {
    cell.dispatchEvent(new dom.window.MouseEvent(type, { bubbles: true }));
    await settle();
  }
  const editor = all("#new-cell")[0];
  assert(editor, "double-clicking a header should open its editor");
  await type_(editor, "minor units");
  editor.dispatchEvent(new dom.window.FocusEvent("blur", { bubbles: false }));
  await settle();

  assertEquals(patches.length, 1, `a rename is one patch, got ${JSON.stringify(patches)}`);
  assertEquals(patches[0].value, "minor units");
  assertEquals(
    patches[0].path[2],
    "name",
    `a rename must write the name field alone, got ${JSON.stringify(patches[0].path)}`,
  );
  assert(
    !JSON.stringify(patches).includes('"type"'),
    `a rename must not mention the type at all, got ${JSON.stringify(patches)}`,
  );
});

Deno.test("a share answer that cannot be read says which field failed", async () => {
  const here = { id: "table:countries", action: "list" };
  const { app, settle, text } = await boot("http://localhost/table:countries");

  // A member with no role. The panel used to keep the last list it understood
  // and say nothing, which is a permissions UI showing stale permissions as
  // current -- the one lie this screen must not tell.
  app.ports.shareLoaded.send({ ...here, members: [{ email: "a@b.c" }], public: true });
  await settle();
  assert(text().includes("member list"), `expected the field named, got: ${text().slice(0, 400)}`);
  assert(text().includes("role"), `expected the missing field named, got: ${text().slice(0, 400)}`);

  // Every unreadable field, not the first: a banner about the member list while
  // the public flag quietly shows the previous sheet's value is worse than one
  // that names both.
  const both = await boot("http://localhost/table:countries");
  both.app.ports.shareLoaded.send({ ...here, members: [{ email: "a@b.c" }], public: "yes" });
  await both.settle();
  assert(both.text().includes("member list"), `expected the member list named, got: ${both.text().slice(0, 400)}`);
  assert(both.text().includes("public flag"), `expected the public flag named too, got: ${both.text().slice(0, 400)}`);

  // A payload about something else is not a failure: a hook answer carries no
  // member list, and reporting its absence would make every secret an error.
  // A fresh boot, because the banner stays up until it is dismissed.
  const quiet = await boot("http://localhost/table:countries");
  quiet.app.ports.shareLoaded.send({ ...here, action: "hook", hook: { url: "u", secret: "s", repro: "r" } });
  await quiet.settle();
  assert(!quiet.text().includes("could not read"), "an absent field is not an unreadable one");
});

Deno.test("a share answer names the sheet it is about, or it does not land", async () => {
  // A list for sheet A that resolves after the user has opened sheet B used to
  // write A's member list and public flag into B's panel, with nothing on
  // screen to say so. The id is what makes that answer droppable.
  const elsewhere = await boot("http://localhost/table:countries");
  elsewhere.app.ports.shareLoaded.send({
    id: "table:us-states",
    action: "list",
    members: [{ email: "somebody@else.example", role: "owner" }],
    public: true,
  });
  await elsewhere.settle();
  assert(
    !elsewhere.text().includes("somebody@else.example"),
    `another sheet's members must not land here, got: ${elsewhere.text().slice(0, 400)}`,
  );
  assert(
    !elsewhere.text().includes("could not read"),
    "an answer about another sheet is dropped, not reported",
  );

  // An answer that names neither is the one case this cannot resolve, so it
  // says so rather than guessing which sheet asked.
  const nameless = await boot("http://localhost/table:countries");
  nameless.app.ports.shareLoaded.send({ members: [], public: false });
  await nameless.settle();
  assert(
    nameless.text().includes("which sheet"),
    `expected the answer to be refused by name, got: ${nameless.text().slice(0, 400)}`,
  );

  // A field this action promises but did not send is an error. An absent field
  // and a renamed one look the same from here; the action is what tells them
  // apart, and a renamed "members" must not read as an empty member list.
  const renamed = await boot("http://localhost/table:countries");
  renamed.app.ports.shareLoaded.send({ id: "table:countries", action: "list", people: [], public: true });
  await renamed.settle();
  assert(
    renamed.text().includes("member list") && renamed.text().includes("missing, not empty"),
    `expected a renamed field to be named as missing, got: ${renamed.text().slice(0, 400)}`,
  );
});

Deno.test("hiding a column stops it rendering, and show-all brings it back", async () => {
  const { all, click, text } = await boot("http://localhost/table:countries");
  // A hidden column keeps its x coordinate and only stops rendering, so the cell
  // is still in the DOM carrying display:none rather than being spliced out --
  // filtering the column array instead would shift every selection index in the
  // file. The old browser test read innerText, which is layout-aware and so hid
  // the distinction; there is no layout here, so the style is the thing to read.
  const blanked = () =>
    all("tbody td").filter((td) => (td.getAttribute("style") ?? "").includes("display: none")).length;
  const cells = all("tbody td").length;
  assertEquals(blanked(), 0, "nothing is hidden to begin with");

  await click(all("span.funnel")[0]);
  const hide = all("button").find((b) => b.textContent?.trim() === "Hide column");
  await click(hide);
  assert(text().includes("1 columns hidden"), "the filter bar should report the hidden column");
  assert(blanked() > 0, "the hidden column's cells should render as display:none");
  assertEquals(all("tbody td").length, cells, "a hidden column keeps its place in the row");

  const showAll = all("button").find((b) => b.textContent?.trim() === "Show all columns");
  await click(showAll);
  assertEquals(blanked(), 0, "show all should bring the column back");
  assert(!text().includes("columns hidden"), "and the filter bar should stop saying so");
});

// Sort, filter, hidden columns and dragged widths used to live in the model and
// die with the tab. They live on the columns in data[0] now, which is both where
// applyPatches can reach (it is rooted at `data`) and where a share carries them.
Deno.test("arranging a sheet writes the arrangement onto its columns", async () => {
  const { app, all, click } = await boot("http://localhost/table:countries");
  const patches: { action: string; path: unknown[]; value: unknown }[] = [];
  app.ports.arrangeDoc.subscribe((sent: { data: typeof patches }) => patches.push(...sent.data));

  await click(all("span.sort")[1]);
  assertEquals(
    patches.map((p) => [p.path[1], p.path[2], p.value]),
    [["1", "sort", "asc"], ["1", "rank", 1]],
    "a sort click stores which way and which key",
  );

  patches.length = 0;
  await click(all("span.funnel")[0]);
  await click(all("button").find((b) => b.textContent?.trim() === "Hide column"));
  assertEquals(
    patches.map((p) => [p.path[1], p.path[2], p.value]),
    [["0", "hidden", true]],
    "hiding stores only the hidden flag, and only for the column hidden",
  );

  // Closing a filter panel that changed nothing must write nothing: the write is
  // a diff against what the document already holds, not a rewrite of it.
  patches.length = 0;
  await click(all("span.funnel")[1]);
  await click(all("span.funnel")[1]);
  assertEquals(patches, [], `closing an untouched panel should write nothing, got ${JSON.stringify(patches)}`);
});

Deno.test("a sheet opens arranged the way it was left", async () => {
  const { app, all, text, settle } = await boot("http://localhost/table:countries");
  app.ports.docSelected.send({
    id: "table:arranged",
    data: {
      doc: {
        type: "table",
        data: [
          [
            { name: "n", type: "text", key: "0", sort: "asc", rank: 1 },
            { name: "wide", type: "text", key: "1", hidden: true, width: 220 },
            { name: "f", type: "text", key: "2", filter: "yes" },
          ],
          { "0": "b", "1": "x", "2": "yes" },
          { "0": "a", "1": "y", "2": "yes" },
          { "0": "c", "1": "z", "2": "no" },
        ],
      },
    },
  });
  await settle();

  assert(
    all("span.sort").some((s) => s.textContent === "n ▲"),
    `the stored sort should be on screen, got: ${all("span.sort").map((s) => s.textContent).join("|")}`,
  );
  const styles = all("tbody td").map((td) => td.getAttribute("style") ?? "");
  assert(styles.some((style) => style.includes("display: none")), "the stored hidden column should not render");
  assert(styles.some((style) => style.includes("220px")), "the stored width should be on the column");
  assert(text().includes("Showing 2 of 3 rows"), `the stored filter should be applied, got: ${text().slice(0, 300)}`);

  // Ascending on the first column, over the rows the filter left.
  const first = all("tbody tr").map((tr) => tr.querySelector("td")?.textContent).filter((t) => t === "a" || t === "b");
  assertEquals(first, ["a", "b"], "the rows should arrive in the stored order");
});

// A query's rows are computed, so there are no stored columns to write an
// arrangement on and sorting one used to die with the tab. It lives in a `view`
// map beside the type overrides, keyed by column name the way those are.
Deno.test("a query sheet remembers how you were looking at it", async () => {
  const { app, all, click, settle } = await boot("http://localhost/query:budget-burn");
  const patches: { action: string; path: unknown[]; value: unknown }[] = [];
  app.ports.arrangeDoc.subscribe((sent: { data: typeof patches }) => patches.push(...sent.data));

  // Nothing answers queryDoc under jsdom, so the result arrives the way
  // src/index.html sends it: a synthesized header row, then the rows.
  app.ports.docQueried.send({
    id: "query:budget-burn",
    data: [
      [{ key: "department", name: "department", type: "text" }, { key: "burn_ratio", name: "burn_ratio", type: "num" }],
      { department: "Police", burn_ratio: 1.05 },
      { department: "Parks", burn_ratio: 0.62 },
    ],
  });
  await settle();

  await click(all("span.sort").find((s) => s.textContent?.startsWith("burn_ratio")));
  assertEquals(
    patches.map((p) => p.path.concat([p.value])),
    [[0, "view", "burn_ratio", "sort", "asc"], [0, "view", "burn_ratio", "rank", 1]],
    "a query's arrangement is addressed by column name, under view",
  );
});

// A query's columns sort, filter and hide, and its rows are a table the view
// already draws, so the keyboard walks it the way it walks a table. A cell in
// it is computed, so a keystroke that would open an editor is refused by name.
Deno.test("the keyboard moves over a query's result, and a write to it is refused by name", async () => {
  const { dom, doc, app, all, text, fire, settle } = await boot("http://localhost/query:budget-burn");
  app.ports.docQueried.send({
    id: "query:budget-burn",
    data: [
      [{ key: "department", name: "department", type: "text" }, { key: "burn_ratio", name: "burn_ratio", type: "num" }],
      { department: "Police", burn_ratio: 1.05 },
      { department: "Parks", burn_ratio: 0.62 },
      { department: "Fire", burn_ratio: 0.3 },
    ],
  });
  await settle();
  const key = async (init: Record<string, unknown>) => {
    doc.body.dispatchEvent(new dom.window.KeyboardEvent("keydown", { bubbles: true, ...init }));
    await settle();
  };
  const selected = () => all("td.selected:not(.r0)").map((td) => td.textContent?.trim());

  const police = all("td").find((td) => td.textContent?.trim() === "Police");
  assert(police, "the result is drawn");
  await fire(police, "mouseenter");
  await fire(police, "mousedown");
  await fire(police, "mouseup");
  assertEquals(selected(), ["Police"], "a click selects the cell it lands on");

  await key({ key: "ArrowDown" });
  await key({ key: "ArrowDown" });
  assertEquals(selected(), ["Fire"], "ArrowDown walks the result's rows");
  await key({ key: "ArrowRight" });
  assertEquals(selected(), ["0.3"], "ArrowRight walks its columns");
  await key({ key: "ArrowDown" });
  await key({ key: "ArrowRight" });
  assertEquals(selected(), ["0.3"], "and the edge of the result is the edge");
  await key({ key: "Home", ctrlKey: true });
  assertEquals(selected(), ["Police"], "Ctrl+Home is the first cell");
  await key({ key: "End", ctrlKey: true });
  assertEquals(selected(), ["0.3"], "Ctrl+End is the last");
  await key({ key: "a", ctrlKey: true });
  assertEquals(selected().length, 6, "Ctrl+A selects the whole result");

  await key({ key: "x" });
  assertEquals(all("#new-cell").length, 0, "a computed cell opens no editor");
  assert(text().includes("computed, not typed"), `the refusal names the reason, got: ${text().slice(0, 300)}`);
});

Deno.test("a query sheet opens arranged the way it was left", async () => {
  const { app, all, text, settle } = await boot("http://localhost/query:budget-burn");
  app.ports.docSelected.send({
    id: "query:arranged",
    data: {
      doc: {
        type: "query",
        data: [{
          lang: "sql",
          code: "select 1",
          view: { b: { sort: "asc", rank: 1 }, c: { hidden: true, width: 220 } },
        }],
      },
    },
  });
  app.ports.docQueried.send({
    id: "query:arranged",
    data: [
      [{ key: "b", name: "b", type: "text" }, { key: "c", name: "c", type: "text" }],
      { b: "z", c: "1" },
      { b: "a", c: "2" },
    ],
  });
  await settle();

  assert(
    all("span.sort").some((s) => s.textContent === "b ▲"),
    `the stored sort should be on screen, got: ${all("span.sort").map((s) => s.textContent).join("|")}`,
  );
  const styles = all("tbody td").map((td) => td.getAttribute("style") ?? "");
  assert(styles.some((style) => style.includes("display: none")), "the stored hidden column should not render");
  assert(styles.some((style) => style.includes("220px")), "the stored width should be on the column");
  assert(text().includes("1 columns hidden"), `the filter bar should say so, got: ${text().slice(0, 300)}`);
});

// A pinned column stays put while the table scrolls sideways. Its left edge is
// the widths of the sticky columns before it, so a column that sizes itself gets
// one written when it is pinned -- an inexact sum overlaps the columns.
Deno.test("pinning a column sticks it at the sum of the widths before it", async () => {
  const { app, all, click, settle } = await boot("http://localhost/table:countries");
  const patches: { action: string; path: unknown[]; value: unknown }[] = [];
  app.ports.arrangeDoc.subscribe((sent: { data: typeof patches }) => patches.push(...sent.data));

  app.ports.docSelected.send({
    id: "table:pinnable",
    data: {
      doc: {
        type: "table",
        data: [
          [
            { name: "a", type: "text", key: "0", width: 100 },
            { name: "b", type: "text", key: "1", width: 60 },
            { name: "c", type: "text", key: "2" },
          ],
          { "0": "x", "1": "y", "2": "z" },
        ],
      },
    },
  });
  await settle();

  await click(all("span.funnel")[2]);
  await click(all("button").find((b) => b.textContent?.trim() === "Pin column"));
  assertEquals(
    patches.map((p) => [p.path[1], p.path[2], p.value]),
    [["2", "pinned", true], ["2", "width", 140]],
    "pinning a column that sizes itself fixes its width too",
  );

  // Column 0 is sticky whether or not anybody pinned it, so its width counts and
  // column 1's, which nobody pinned, does not.
  const pinned = all("td.pin").map((td) => td.getAttribute("style") ?? "");
  assert(
    pinned.some((style) => style.includes("left: 100px")),
    `the pinned column should sit past column 0, got: ${JSON.stringify(pinned)}`,
  );

  // Column 0 sizes itself here, so nothing knows how wide it renders and the sum
  // would be a guess: a narrow column 0 leaves a gap the rows scroll through, a
  // wide one puts the pinned column underneath it. Pinning fixes column 0 too.
  patches.length = 0;
  app.ports.docSelected.send({
    id: "table:autowide",
    data: {
      doc: {
        type: "table",
        data: [
          [{ name: "a", type: "text", key: "0" }, { name: "b", type: "text", key: "1" }],
          { "0": "x", "1": "y" },
        ],
      },
    },
  });
  await settle();
  await click(all("span.funnel")[1]);
  await click(all("button").find((b) => b.textContent?.trim() === "Pin column"));
  assertEquals(
    patches.map((p) => [p.path[1], p.path[2], p.value]),
    [["0", "width", 140], ["1", "pinned", true], ["1", "width", 140]],
    "every sticky column ends up with a width the sum can use — column 0's included, in column order",
  );
});

// Reorder is a splice on data[0], not a display permutation: rows are keyed by
// col.key, so moving a column moves no cell and the display index stays the
// document index -- which is what keeps every selection index in Main.elm right
// without a display-to-document map. It rides changeDoc rather than arrangeDoc
// because everyone looking at the sheet sees the new order, which makes it an
// edit and not an arrangement -- so it undoes, and a viewer's is refused.
Deno.test("dragging a column onto another moves it there, and off the table moves nothing", async () => {
  const { app, all, dom, doc, fire, settle } = await boot("http://localhost/table:countries");
  const patches: { action: string; path: unknown[]; value: unknown }[] = [];
  app.ports.changeDoc.subscribe((sent: { data: typeof patches }) => patches.push(...sent.data));
  await settle();

  // Grab column 2's handle, hover column 0, let go.
  const firstCell = () => [...all("tbody tr")[0].querySelectorAll("td")][0];
  await fire(all("span.grab")[2], "mousedown");
  await fire(firstCell(), "mouseenter");
  await fire(doc, "mouseup");
  assertEquals(
    patches.map((p) => [p.action, p.path, p.value]),
    [["move", [0], [2, 0]]],
    "a drop on a column moves the dragged one to it",
  );

  // Let go with the pointer off the table and nothing moves.
  patches.length = 0;
  await fire(all("span.grab")[2], "mousedown");
  await fire(all("table")[0], "mouseleave");
  await fire(doc, "mouseup");
  assertEquals(patches, [], `a drop off the table should move nothing, got ${JSON.stringify(patches)}`);

  // A move has an exact inverse, so it undoes rather than leaving Ctrl+Z to
  // reach past it for whatever was edited before.
  patches.length = 0;
  await fire(all("span.grab")[2], "mousedown");
  await fire(firstCell(), "mouseenter");
  await fire(doc, "mouseup");
  patches.length = 0;
  doc.body.dispatchEvent(new dom.window.KeyboardEvent("keydown", { key: "z", ctrlKey: true, bubbles: true }));
  await settle();
  assertEquals(
    patches.map((p) => [p.action, p.value]),
    [["move", [0, 2]]],
    "undoing a move puts the column back where it was",
  );
});

// A row moves the same way: one `move` patch, at the root of `data` rather than
// on `data[0]`. Only while the table is in document order -- a sorted view has
// no honest target for the drop, so the handle is not there to grab.
Deno.test("dragging a row onto another moves it there, and a sorted table offers no handle", async () => {
  const { app, all, dom, doc, fire, settle, click, type_ } = await boot("http://localhost/table:countries");
  const patches: { action: string; path: unknown[]; value: unknown }[] = [];
  app.ports.changeDoc.subscribe((sent: { data: typeof patches }) => patches.push(...sent.data));
  await settle();

  // Data rows start after the three meta rows; row 3 of the document is the
  // fourth data row. Grab its handle, hover row 1, let go.
  const rowHandle = (y: number) =>
    [...all("tbody tr")[2 + y].querySelectorAll(`span.grab[title^="drag onto the row"]`)][0];
  const rowCell = (y: number) => [...all("tbody tr")[2 + y].querySelectorAll("td")][0];
  assert(rowHandle(3), "a table in document order offers a handle on every row");
  await fire(rowHandle(3), "mousedown");
  await fire(rowCell(1), "mouseenter");
  await fire(doc, "mouseup");
  assertEquals(patches.map((p) => [p.action, p.path, p.value]), [["move", [], [3, 1]]]);

  patches.length = 0;
  await fire(rowHandle(3), "mousedown");
  await fire(all("table")[0], "mouseleave");
  await fire(doc, "mouseup");
  assertEquals(patches, [], `a drop off the table should move nothing, got ${JSON.stringify(patches)}`);

  patches.length = 0;
  await fire(rowHandle(3), "mousedown");
  await fire(rowCell(1), "mouseenter");
  await fire(doc, "mouseup");
  patches.length = 0;
  doc.body.dispatchEvent(new dom.window.KeyboardEvent("keydown", { key: "z", ctrlKey: true, bubbles: true }));
  await settle();
  assertEquals(patches.map((p) => [p.action, p.path, p.value]), [["move", [], [1, 3]]], "undo puts the row back");

  // A sort taken with the button still down: the row under the pointer is a
  // display row again, and the drop is refused rather than spliced blind.
  patches.length = 0;
  await fire(rowHandle(3), "mousedown");
  await click(all("span.sort")[0]);
  await fire(rowCell(1), "mouseenter");
  await fire(doc, "mouseup");
  assertEquals(patches, [], `a drop after a mid-drag sort should move nothing, got ${JSON.stringify(patches)}`);
  assertEquals(all(`span.grab[title^="drag onto the row"]`).length, 0, "a sorted table offers no row handle");
  assertEquals(all("span.grab").length, all("span.sort").length, "the column handles are still there");

  // A search is the other way a display row stops being a document row.
  await click(all("span.sort")[0]);
  await click(all("span.sort")[0]);
  assert(all(`span.grab[title^="drag onto the row"]`).length > 0, "unsorted again, the handles are back");
  await type_(all('input[placeholder="search"]')[0], "Fr");
  assertEquals(all(`span.grab[title^="drag onto the row"]`).length, 0, "a searched table offers no row handle");
});

// The two halves composed, which is what src/index.html does on the way in and
// what neither half proves on its own.
Deno.test("a sheet you cannot write opens with the arrangement this browser kept", async () => {
  const { app, all, text, settle } = await boot("http://localhost/table:countries");
  const cols = [{ name: "n", type: "text", key: "0" }, { name: "w", type: "text", key: "1" }];
  const rows = [{ "0": "b", "1": "x" }, { "0": "a", "1": "y" }];
  const held = foldView(undefined, [
    { action: "set", path: [0, "0", "sort"], value: "asc" },
    { action: "set", path: [0, "0", "rank"], value: 1 },
    { action: "set", path: [0, "1", "hidden"], value: true },
  ], cols);

  app.ports.docSelected.send({
    id: "table:held",
    data: { doc: { type: "table", data: [mergeView(cols, held), ...rows] } },
  });
  await settle();

  assert(
    all("span.sort").some((s) => s.textContent === "n ▲"),
    `the kept sort should be on screen, got: ${all("span.sort").map((s) => s.textContent).join("|")}`,
  );
  assert(text().includes("1 columns hidden"), `the kept hidden column should be hidden, got: ${text().slice(0, 300)}`);
  const first = all("tbody tr").map((tr) => tr.querySelector("td")?.textContent).filter((t) => t === "a" || t === "b");
  assertEquals(first, ["a", "b"], "and the kept sort should order the rows");
});

// A feed's rows are the log of what happened to the sheet, and `arrange` has
// nowhere to put an arrangement on one, so a sort there worked and then forgot
// on reload -- which reads as a bug in saving rather than as a sheet with no
// columns of its own. The library keeps its controls: its order is how you read
// the list, not a fact about it, and nobody expects a reload to hold it.
Deno.test("a feed offers no arrangement controls, and a listing still does", async () => {
  const { app, all, text, settle } = await boot("http://localhost/");
  assert(all("span.funnel").length > 0, "the library is a listing, and sorting it is how you read it");

  app.ports.docSelected.send({
    id: "net-http:feed",
    data: { doc: { type: "net-http", data: [{ url: "https://example.com/feed.json", interval: 3600 }] } },
  });
  await settle();

  assert(text().includes("created_at"), "the feed's own columns still render");
  assertEquals(all("span.funnel").length, 0, "a feed offers no filter");
  assertEquals(all(".grip").length, 0, "no resize grip");
  assertEquals(all("span.sort").length, 0, "and nothing to click for a sort");
});

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

// The one verb that only means something over the sheet that is open. It is not
// on the shortcut sheet -- it has no key -- so the palette is where it lives,
// and it opens the same door the footer's new-alert row does.
Deno.test("the palette subscribes to the sheet that is open", async () => {
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

  app.ports.authResult.send({ usr_id: "u1", email: "ops@example.com" });
  await settle();
  await key(doc.body, { key: "k", ctrlKey: true });
  await type("subscribe");
  await key(doc.getElementById("palette"), { key: "ArrowDown" });
  await key(doc.getElementById("palette"), { key: "Enter" });
  assertEquals(made.length, 1, "one command, one sheet");
  assertEquals(made[0].type, "alert");
  assertEquals(made[0].data[0], {
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
Deno.test("the selected library rows are trashed together, and an empty selection is refused by name", async () => {
  const { app, all, doc, dom, fire, settle, text } = await boot("http://localhost/");
  const sent: { id: string; data: { trashed: boolean | null } }[] = [];
  app.ports.updateLibrary.subscribe((s: (typeof sent)[number]) => sent.push(s));
  const key = async (init: Record<string, unknown>) => {
    doc.body.dispatchEvent(new dom.window.KeyboardEvent("keydown", { bubbles: true, ...init }));
    await settle();
  };

  await key({ key: "Backspace", ctrlKey: true, shiftKey: true });
  assertEquals(sent, [], "a selection over no row writes nothing");
  assert(text().includes("holding no sheet"), `expected the refusal by name, got: ${text().slice(0, 200)}`);

  // A library of this test's own, so the rows under the selection are known.
  app.ports.librarySynced.send({
    "": { name: "library", system: true, doc: { type: "library" } },
    "table:a": { name: "a", tags: [], doc: { type: "table", data: [[]] } },
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
  const n = (shelf["table:countries"] as { doc: { data: unknown[] } }).doc.data.length - 1;
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
