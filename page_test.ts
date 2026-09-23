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
//
// This file is the table and the query sheet: how they render, sort, arrange and
// take the keyboard. `library_test.ts` is the other half of the `boot` harness --
// the library itself, the sheets opened from it, and the parts of src/page.mjs
// that need no page at all -- and it is a separate file because `deno test
// --parallel` runs files and not tests side by side. Keep the two even in boots,
// which is what either file costs: `grep -c "await boot(" page_test.ts
// library_test.ts`.
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
  const { app, all, click, settle, type_ } = await boot("http://localhost/table:countries");
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

  // A numeric column is shaded by its values, and the colour goes on a wrapper
  // inside the cell: an inline background on the td itself outranks
  // `td.selected` and the match highlight, so selection and find would vanish
  // on every shaded column.
  patches.length = 0;
  app.ports.docSelected.send({
    id: "table:shaded",
    data: {
      doc: { type: "table", data: [[{ name: "n", type: "num", key: "0" }], { "0": 1 }, { "0": 5 }, { "0": 9 }] },
    },
  });
  await settle();
  await click(all("span.funnel")[0]);
  await type_(all("label.shading select")[0], "bar");
  assertEquals(
    patches.map((p) => [p.path[1], p.path[2], p.value]),
    [["0", "shade", "bar"]],
    "choosing a shade stores one word on the column",
  );

  const shaded = all("tbody td div.shade").map((d) => d.getAttribute("style") ?? "");
  assertEquals(shaded.length, 3, `every data cell of a shaded column draws the wrapper, got ${shaded.join(" | ")}`);
  assert(
    shaded.every((style) => style.includes("linear-gradient")),
    `a bar is drawn as a gradient, got ${shaded.join(" | ")}`,
  );
  assert(
    shaded[0].includes(" 0%") && shaded[2].includes(" 100%"),
    `bars run from nothing at the column's smallest value to full at its largest, got ${shaded.join(" | ")}`,
  );

  // A shade written directly on a text column -- never offered by this panel,
  // whose select sits behind the same numeric gate as decimals and format, but
  // reachable by a document from anywhere else, a collaborator's older client
  // among them -- must draw nothing. `format` and `decimals` already cannot
  // touch a text cell: `cellDecoder`'s `Text` branch never reads them. Shading
  // has no such branch to fall through, so it must refuse by column type
  // itself rather than by what a number-shaped string happens to parse as.
  app.ports.docSelected.send({
    id: "table:shaded-text",
    data: {
      doc: {
        type: "table",
        data: [[{ name: "zip", type: "text", key: "0", shade: "bar" }], { "0": "02139" }, { "0": "94103" }],
      },
    },
  });
  await settle();
  assertEquals(
    all("tbody td div.shade").length,
    0,
    "a shade word on a text column must not paint a background behind digit-shaped text",
  );

  // The grid's own label falls back to "untitled sheet" for a document with no
  // name -- neither "table:shaded" nor "table:shaded-text" above is in the
  // library, so `info.name` defaults to "". No bundled sheet holds a `json`
  // column with a numeric array either, so this is also where the sparkline
  // is proven live in the real table, not just through Test.Html.Query.
  assertEquals(
    all("table[role='grid']").map((t) => t.getAttribute("aria-label")),
    ["untitled sheet"],
    "a synthetic sheet the library has never heard of draws no blank label",
  );

  app.ports.docSelected.send({
    id: "table:json-spark",
    data: {
      doc: {
        type: "table",
        data: [[{ name: "series", type: "json", key: "0" }], { "0": [1, 5, 3] }, { "0": "not an array" }],
      },
    },
  });
  await settle();
  assertEquals(
    all("tbody td div[style*='width: 3px']").length,
    3,
    "the numeric-array cell draws three bars in the real table, not just the fixture Query.fromHtml exercises",
  );
  assertEquals(
    all("tbody tr")[4]?.textContent?.trim(),
    "not an array",
    "a json cell that is not a numeric array still draws the text it always did, beside a sparkline row",
  );
});

// One boot reaches every control a screen reader had nothing to read: the
// fragment opens the settings modal and the tutorial flag draws its panel, so
// the states that need no click are already on screen. A glyph is not a name and
// neither is a placeholder -- a placeholder is gone the moment anything is typed.
Deno.test("every modal, icon button and bare input says what it is", async () => {
  const { dom, doc, app, all, click, settle, type_ } = await boot("http://localhost/table:countries#settings", {
    tutorial: 0,
  });
  const labels = (sel: string) => all(sel).map((el) => el.getAttribute("aria-label") ?? "");
  const key = async (k: string) => {
    doc.body.dispatchEvent(new dom.window.KeyboardEvent("keydown", { key: k, ctrlKey: true, bubbles: true }));
    await settle();
  };

  const grid = all("table[role='grid']");
  assertEquals(grid.length, 1, "the rows are a grid");
  assertEquals(grid[0].getAttribute("aria-label"), "countries", "named off the sheet");

  assertEquals(
    labels("div[role='dialog'][aria-modal='true']"),
    ["sheet settings"],
    "#settings opens one labelled dialog",
  );
  assertEquals(
    labels("a[href='/']").filter((l) => l !== ""),
    ["library"],
    "the ⊞ link is named, and the wordmark beside it needs no label",
  );
  assertEquals(labels("#account input"), ["email", "password"]);
  assertEquals(labels("main > input"), ["search the rows"]);

  // Every icon-only button on screen at once: the settings close and the
  // tutorial dismiss. A `button.x` with nothing but a × reads as "button".
  const crosses = all("button.x");
  assertEquals(crosses.length, 2, `expected the settings and tutorial crosses, got ${crosses.length}`);
  assert(
    crosses.every((b) => (b.getAttribute("aria-label") ?? "").length > 0),
    `every × is named, got ${JSON.stringify(labels("button.x"))}`,
  );

  // One modal at a time: each opens over the last by closing it. Settings
  // closes through the URL, or a reload would open it again.
  await key("k");
  assertEquals(labels("div[role='dialog']"), ["command palette"], "the palette opens over settings by closing it");
  assertEquals(doc.location.hash, "", "and #settings leaves the URL");

  const input = doc.getElementById("palette");
  assertEquals(input.getAttribute("role"), "combobox");
  assertEquals(input.getAttribute("aria-controls"), "palette-list");
  assertEquals(doc.getElementById("palette-list")?.getAttribute("role"), "listbox");
  assertEquals(input.getAttribute("aria-activedescendant"), null, "nothing is pointed at before an arrow");
  input.dispatchEvent(new dom.window.KeyboardEvent("keydown", { key: "ArrowDown", bubbles: true }));
  await settle();
  const lit = all("#palette-list button")
    .filter((b) => !(b.getAttribute("style") ?? "").includes("transparent"))
    .map((b) => b.getAttribute("id"));
  assertEquals(lit.length, 1, "one row is highlighted");
  assertEquals(input.getAttribute("aria-activedescendant"), lit[0], "the input names the highlighted row");
  assertEquals(all("#palette-list [aria-selected='true']").map((b) => b.getAttribute("id")), lit);

  // An arrow over no matches once crashed Elm (`modBy 0`). The crash leaves
  // the DOM as it was, so only the window's error event tells.
  let crashed: unknown = null;
  dom.window.addEventListener("error", (e: unknown) => {
    crashed = e;
  });
  await type_(input, "no such command");
  assertEquals(all("#palette-list button").length, 0, "nothing matches");
  input.dispatchEvent(new dom.window.KeyboardEvent("keydown", { key: "ArrowDown", bubbles: true }));
  await settle();
  assertEquals(input.getAttribute("aria-activedescendant"), null, "nothing to point at");
  assertEquals(crashed, null, `an arrow key over no matches should not throw, got ${(crashed as ErrorEvent)?.message}`);

  await key("/");
  assertEquals(labels("div[role='dialog']"), ["keyboard shortcuts"], "the shortcut sheet closes the palette");

  await key("k");
  assertEquals(labels("div[role='dialog']"), ["command palette"], "and the palette closes the shortcut sheet");
  assertEquals(labels("#palette"), ["jump to a sheet, or run a command"]);

  await key("f");
  assert(
    labels("button.x").includes("close find and replace"),
    `Ctrl+F's × is named, got ${JSON.stringify(labels("button.x"))}`,
  );

  await click(all("span.funnel")[0]);
  assertEquals(labels("div.panel input[placeholder='contains...']"), ["filter flag"]);

  // An import preview closes the others, and nothing opens over it: closing it
  // would drop the file it read.
  assertEquals(labels("div[role='dialog']"), ["command palette"], "the palette is still up when the preview lands");
  app.ports.importPreviewed.send({ filename: "a.csv", name: "a", cols: [], rows: [], count: 0 });
  await settle();
  assertEquals(labels("div[role='dialog']"), ["import a file"]);
  await key("k");
  await key("/");
  assertEquals(labels("div[role='dialog']"), ["import a file"], "Ctrl+K and Ctrl+/ do nothing over an import");
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
  assertEquals(
    all("td[aria-selected]").map((td) => [td.getAttribute("aria-selected"), td.textContent?.trim()]),
    [["true", "0.3"]],
    "the selected cell says so to a screen reader, and no other cell does",
  );
  assertEquals(all("table[role='grid'][aria-multiselectable='true']").length, 1);
  assertEquals(
    all("td[role='columnheader']").length,
    2,
    "each header cell is a column header",
  );
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
