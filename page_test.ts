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
import { cbor } from "@automerge/automerge-repo";
import { JSDOM } from "jsdom";
import { BrowserWebSocketClientAdapter } from "./src/automerge-repo-ws.mjs";
import { EXAMPLES } from "./src/examples.mjs";
import * as pageExports from "./src/page.mjs";
import * as sqlExports from "./src/sql.mjs";
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
  library,
  mergeView,
  PORTALS,
  sheets,
} from "./src/page.mjs";
import alasql from "./src/alasql.mjs";
import { register } from "./src/sql.mjs";

const dir = new URL(".", import.meta.url).pathname;

// Always build rather than trusting whatever is in dist/: a stale bundle would
// let this file pass against code that no longer exists. elm make is a no-op
// when nothing changed, so the cost is a fraction of a second.
let built: Promise<void> | undefined;
const ensureDist = () =>
  built ??= (async () => {
    const { code, stderr } = await new Deno.Command(Deno.execPath(), {
      args: ["task", "build"],
      cwd: dir,
    }).output();
    assertEquals(code, 0, `deno task build failed: ${new TextDecoder().decode(stderr)}`);
  })();

// Read and compiled once. `new Function` over dist/index.js is most of what a
// boot costs, and nothing in it depends on which test is running.
let elmSource: Promise<() => void> | undefined;
const compiled = () =>
  elmSource ??= (async () => new Function(await Deno.readTextFile(dir + "dist/index.js")) as () => void)();

// The library the page itself builds, not a copy of it written for the test:
// that is the reason src/page.mjs exists. Nothing is stored, so this is the
// library a first visit sees.
const shelf = library();

type Ports = Record<string, { send: (v: unknown) => void; subscribe: (f: (v: never) => void) => void }>;
// Deno has no DOM lib, and jsdom ships no types, so the few members these tests
// touch are named here rather than pulling in @types/jsdom for four fields.
type El = {
  dispatchEvent: (event: unknown) => boolean;
  textContent: string | null;
  getAttribute: (name: string) => string | null;
  querySelector: (sel: string) => El | null;
  querySelectorAll: (sel: string) => Iterable<El>;
};

/** jsdom's window, installed as the globals the compiled Elm and src/index.html
 * both read. `defineProperty` rather than assignment: Deno's own `localStorage`
 * is an accessor that a plain assignment does not replace, and Deno's is backed
 * by a file in the user's cache — a test must never end up writing there.
 */
const globalize = (w: Record<string, unknown>) => {
  for (
    const name of [
      "window",
      "document",
      "navigator",
      "location",
      "history",
      "HTMLElement",
      "Node",
      "Event",
      "MouseEvent",
      "KeyboardEvent",
      "CustomEvent",
      "requestAnimationFrame",
      "cancelAnimationFrame",
      "getComputedStyle",
      "MutationObserver",
      "DOMParser",
      "XMLHttpRequest",
      "XMLSerializer",
      "Image",
      "localStorage",
      "FileReader",
      "Blob",
    ]
  ) { Object.defineProperty(globalThis, name, { value: w[name], configurable: true, writable: true }); }
};

const boot = async (url: string, { tutorial = -1 } = {}) => {
  await ensureDist();
  const dom = new JSDOM(`<!doctype html><html><body><div id="elm"></div></body></html>`, {
    url,
    pretendToBeVisual: true, // supplies requestAnimationFrame, which is when Elm paints
  });
  const w = dom.window as unknown as Record<string, unknown>;
  globalize(w);

  // Elm's compiled output is an IIFE that hangs `Elm` off its `this`. A module's
  // `this` is undefined, and a second boot onto the same object crashes with
  // "there are two Elm.Main modules", so each boot gets a scope of its own --
  // the fresh `this` is the only part that has to be new. Reading and compiling
  // half a megabyte per test is not: that is hoisted to module scope, and every
  // boot is one `.call`.
  const scope: { Elm?: { Main: { init: (o: unknown) => { ports: Ports } } } } = {};
  (await compiled()).call(scope);
  assert(scope.Elm?.Main, "dist/index.js should define Elm.Main");

  const app = scope.Elm.Main.init({
    node: dom.window.document.getElementById("elm"),
    flags: { api: API_BASE, tutorial },
  });

  // What the page asked the server for. Nothing here answers -- index.html is
  // what talks to the API -- but what Elm sends is half the contract and was
  // never asserted.
  const asks: Record<string, unknown>[] = [];
  app.ports.shareAction.subscribe((ask: Record<string, unknown>) => asks.push(ask));

  app.ports.librarySynced.send(shelf);
  // src/index.html:850 does this: an id that resolves to nothing falls back to
  // the library rather than leaving the page on "loading" forever.
  app.ports.changeId.subscribe((id: string) =>
    app.ports.docSelected.send({
      id,
      data: { doc: (shelf as Record<string, { doc?: unknown }>)[id]?.doc ?? { type: "library" } },
    })
  );

  const doc = dom.window.document;
  // Elm paints on an animation frame, so every assertion waits rather than
  // reading the DOM the instant a message is sent. It waits for the page to stop
  // changing, not for a fixed count: a click that goes out through a port and
  // back needs several frames, and a fixed count large enough for that was
  // spending 200ms on every assertion that needed one. QUIET_FRAMES of no change
  // is settled; SETTLE_MAX bounds the wait, and reaching it is not an error --
  // a page that never stops changing fails on the test's own assertion, which
  // says more than a timeout would.
  const QUIET_FRAMES = 3;
  const SETTLE_MAX = 24;
  const settle = () =>
    new Promise<void>((resolve) => {
      let frames = 0, quiet = 0, last = "";
      const tick = () => {
        const now = doc.body.innerHTML;
        quiet = now === last ? quiet + 1 : 0;
        last = now;
        if (quiet >= QUIET_FRAMES || ++frames >= SETTLE_MAX) return resolve();
        dom.window.requestAnimationFrame(tick);
      };
      dom.window.requestAnimationFrame(tick);
    });
  const click = async (el: El | undefined | null, init: Record<string, unknown> = {}) => {
    assert(el, "nothing to click");
    el.dispatchEvent(new dom.window.MouseEvent("click", { bubbles: true, ...init }));
    await settle();
  };
  const text = () => doc.body.textContent?.replace(/\s+/g, " ") ?? "";
  const all = (sel: string): El[] => [...doc.querySelectorAll(sel)];
  // Elm's onInput listens for the `input` event, so a value set without one is
  // a value the model never hears about.
  const type_ = async (el: El | undefined | null, value: string) => {
    assert(el, "nothing to type into");
    (el as unknown as { value: string }).value = value;
    el.dispatchEvent(new dom.window.Event("input", { bubbles: true }));
    await settle();
  };

  await settle();
  // A drag is three events on three targets, and no helper covered any of them.
  // `mouseenter` does not bubble, which is what makes the cell under the pointer
  // the thing to dispatch it on.
  const fire = async (
    el: { dispatchEvent: (event: unknown) => boolean },
    type: string,
    init: Record<string, unknown> = {},
  ) => {
    el.dispatchEvent(new dom.window.MouseEvent(type, { bubbles: type !== "mouseenter", ...init }));
    await settle();
  };
  return { dom, doc, app, settle, click, fire, text, all, type_, asks };
};

Deno.test("the page boots: Elm initializes and renders the library", async () => {
  const { text, all } = await boot("http://localhost/");
  assert(text().includes("scrapsheets"), `expected the app shell, got: ${text().slice(0, 200)}`);
  assert(!text().includes("loading"), "the library should have resolved, not stayed on loading");
  // Every tag in the library becomes a filter chip, so this is also the check
  // that the bundled sheets arrived through the port at all.
  const chips = all("button.chip").map((b) => b.textContent);
  for (const tag of ["demo", "reference", "dataset", "healthcare", "legal"])
    assert(chips.includes(tag), `expected a "${tag}" filter chip, got: ${chips.join("|")}`);
});

Deno.test("the gallery strip links every demo pipeline, and only those", async () => {
  const { all, text } = await boot("http://localhost/");
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

  const dismissed = await boot("http://localhost/", { tutorial: -1 });
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
    { sheet_id: "net-http:feed", last_run: "2026-08-23T14:02:11.000Z", failures_since_ok: "3" },
    { sheet_id: "alert:budget", last_run: null, failures_since_ok: 0 },
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
    { sheet_id: "query:budget-burn", last_run: "2026-08-23T14:02:11.000Z", failures_since_ok: 2 },
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
    "table:mine": { name: "mine", doc: { type: "table", data: [{}] } },
    // A stale copy of a bundled example must not shadow the real one.
    "table:countries": { name: "an old countries", doc: { type: "table", data: [{}] } },
  };
  const shelf = library(stored) as Record<string, { name: string; system?: boolean; thumb?: unknown }>;

  assertEquals(shelf["table:mine"].name, "mine", "a stored sheet survives the merge");
  assertEquals(shelf["table:countries"].name, "countries", "the bundled countries wins over a stored copy");
  assertEquals(shelf[""].name, "library", "the empty id is the library itself");
  for (const p of PORTALS) assert(shelf[`portal:${p}`], `portal:${p} should be listed`);
  assert(shelf["table:tutorial"], "the tutorial is part of the library");
  // Every entry with a doc gets a thumbnail, which is what the library rows draw.
  for (const [id, entry] of Object.entries(shelf)) assert(entry.thumb, `${id} should carry a thumb`);
});

// The sync server refuses a viewer's write, and a bundled sheet has no document
// of this browser's to write to at all. Both keep their arrangement here: the
// patches folded into a partial of data[0], merged back over the document on the
// way in.
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

// --- the page's half of the query engine
//
// sheets() over the vendored engine the page loads, with the two things it takes
// from the browser stubbed: the library map, and finding a document that is not
// in it. Nothing here is bundled outside the library, so `find` answers nothing
// and the "this sheet has no data" path is the one that runs.
register(alasql);
alasql.options.modifier = "RECORDSET";
const resolver = () => sheets(alasql, () => shelf, () => Promise.resolve(undefined));
const rowsOf = async (code: string) => {
  const { data } = await resolver().runSql(code, { "": null });
  return data as Record<string, unknown>[];
};
const refused = async (code: string) => {
  try {
    await resolver().runSql(code, { "": null });
  } catch (err) {
    return (err as Error).message;
  }
  throw new Error(`expected this to be refused: ${code}`);
};

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

// --- the glue, actually executed
//
// `boot` above answers every port by hand. src/index.html is what does that for
// real, and none of it had ever run in a test: the ports, `applyPatches`, the
// browser store behind a held arrangement, the query re-run guard, and the hook
// that reads the sync server's refusal off the wire. This runs that file's own
// `<script type="module">` over the same jsdom.
//
// Three things are stubbed and only three: `initializeWasm`, the `Repo`, and its
// storage — everything that would need a network or a WASM document. The
// websocket adapter is the genuine vendored class, because what the refusal hook
// does is override one of its methods, and a stub would prove nothing about
// that.

/** src/index.html's module script, with its imports turned into a destructure of
 * what the harness supplies. jsdom does not run `type="module"`, and Deno cannot
 * resolve `/page.mjs`, so the imports are the one thing rewritten — a name the
 * harness does not supply fails the test rather than arriving undefined.
 */
const glueSource = async (deps: Record<string, unknown>) => {
  const html = await Deno.readTextFile(dir + "src/index.html");
  const open = html.indexOf('<script type="module">');
  const from = html.indexOf(">", open) + 1;
  const names: string[] = [];
  const body = html.slice(from, html.indexOf("</script>", open)).replace(
    /import\s+([^;]*?)\s+from\s+"[^"]+";/g,
    (_m, clause: string) => {
      for (const name of clause.replace(/[{}]/g, "").split(",").map((x) => x.trim()).filter(Boolean)) {
        assert(!name.includes(" as "), `the glue harness does not rewrite a renamed import: ${name}`);
        names.push(name);
      }
      return "";
    },
  );
  assertEquals(
    names.filter((name) => !(name in deps)),
    [],
    "src/index.html imports a name the glue harness does not supply; add it to `deps` in page_test.ts",
  );
  return `const { ${names.join(", ")} } = deps;\n${body}`;
};

/** An automerge handle, as much of one as src/index.html asks for. No
 * `whenReady`: the real one is raced against a ten-second timeout that
 * `Promise.race` cannot cancel, and a test does not need to leave that behind.
 */
const fakeHandle = (documentId: string, doc: Record<string, unknown>) => {
  const listeners: ((d: unknown) => void)[] = [];
  return {
    documentId,
    doc: () => doc,
    change: (fn: (d: Record<string, unknown>) => void) => {
      fn(doc);
      // The shape automerge sends and `DocDelta` in src/Main.elm decodes. A field
      // short of it and the port refuses the whole value.
      for (const listener of listeners) listener({ doc, handle: { documentId }, patchInfo: null, patches: [] });
    },
    on: (_event: string, cb: (d: unknown) => void) => listeners.push(cb),
  };
};

const glue = async (
  url: string,
  {
    stored = {},
    docs = {} as Record<string, Record<string, unknown>>,
    // What a test wants to watch rather than replace: whatever is named here
    // wins over the real export of the same name.
    watching = {} as Record<string, unknown>,
    // What the API answers. `{ data: [] }` is the shape the freshness poll
    // reads, which every page makes whether a test cares or not.
    respond = (_url: string, _init?: RequestInit): unknown => ({ data: [] }),
  } = {},
) => {
  await ensureDist();
  const dom = new JSDOM(`<!doctype html><html><body><div id="elm"></div></body></html>`, {
    url,
    pretendToBeVisual: true,
  });
  const w = dom.window as unknown as Record<string, unknown>;
  globalize(w);
  for (const [key, value] of Object.entries(stored))
    (w.localStorage as Storage).setItem(`scrapsheets-${key}`, JSON.stringify(value));

  // Nothing here talks to the API. Every request is recorded so a test can say
  // what the page asked for, and `respond` says what it hears back.
  const asked: { url: string; method: string; body: unknown }[] = [];
  const define = (name: string, value: unknown) =>
    Object.defineProperty(globalThis, name, { value, configurable: true, writable: true });
  define("fetch", (input: unknown, init?: RequestInit) => {
    const url = String(input);
    asked.push({
      url,
      method: init?.method ?? "GET",
      body: typeof init?.body === "string" ? JSON.parse(init.body) : null,
    });
    return Promise.resolve(
      new Response(JSON.stringify(respond(url, init) ?? { data: [] }), {
        status: 200,
        headers: { "Content-Type": "application/json" },
      }),
    );
  });
  // The page sets a one-minute freshness poll going. Recorded so the harness can
  // stop it, or it outlives the test.
  const timers: unknown[] = [];
  const realInterval = globalThis.setInterval;
  define("setInterval", (...args: Parameters<typeof setInterval>) => {
    const id = realInterval(...args);
    timers.push(id);
    return id;
  });

  const scope: { Elm?: { Main: { init: (o: unknown) => { ports: Ports } } } } = {};
  (await compiled()).call(scope);
  let app!: { ports: Ports };
  define("Elm", { Main: { init: (o: unknown) => (app = scope.Elm!.Main.init(o)) } });

  // The one thing the harness has to reach afterwards: the refusal hook lives on
  // this instance, and `Repo` is where src/index.html hands it over.
  let adapter!: { socket?: unknown; receiveMessage: (data: unknown) => void };
  const deps: Record<string, unknown> = {
    ...pageExports,
    ...sqlExports,
    ...watching,
    alasql,
    initializeWasm: () => Promise.resolve(),
    cbor,
    BrowserWebSocketClientAdapter,
    IndexedDBStorageAdapter: class {},
    Repo: class {
      constructor(options: { network: (typeof adapter)[] }) {
        adapter = options.network[0];
      }
      on() {}
      find(doc_id: string) {
        return Promise.resolve(docs[doc_id] ? fakeHandle(doc_id, docs[doc_id]) : null);
      }
    },
  };
  const AsyncFunction = Object.getPrototypeOf(async function () {}).constructor;
  await new AsyncFunction("deps", await glueSource(deps))(deps);

  const doc = dom.window.document;
  // Elm paints on an animation frame; `ms` is for the parts of the page that
  // wait on a real clock, which is only the query debounce.
  const settle = async (ms = 0) => {
    for (let i = 0; i < 30; i++) await new Promise((r) => dom.window.requestAnimationFrame(() => r(null)));
    if (ms) await new Promise((r) => setTimeout(r, ms));
  };
  await settle();
  return {
    app,
    settle,
    asked,
    all: (sel: string): El[] => [...doc.querySelectorAll(sel)],
    fire: async (el: El, type: string) => {
      el.dispatchEvent(new dom.window.MouseEvent(type, { bubbles: type !== "mouseenter" }));
      await settle();
    },
    keyUp: async () => {
      doc.dispatchEvent(new dom.window.MouseEvent("mouseup", { bubbles: true }));
      await settle();
    },
    type_: async (el: El | undefined | null, value: string) => {
      assert(el, "nothing to type into");
      (el as unknown as { value: string }).value = value;
      el.dispatchEvent(new dom.window.Event("input", { bubbles: true }));
      el.dispatchEvent(new dom.window.FocusEvent("blur", { bubbles: false }));
      await settle();
    },
    text: () => doc.body.textContent?.replace(/\s+/g, " ") ?? "",
    click: async (el: El | undefined | null) => {
      assert(el, "nothing to click");
      el.dispatchEvent(new dom.window.MouseEvent("click", { bubbles: true }));
      await settle();
    },
    held: () => JSON.parse((w.localStorage as Storage).getItem("scrapsheets-views") ?? "null"),
    // The sync server refuses a viewer's write with this frame. The adapter
    // asserts it has a socket before reading one, and connecting for real would
    // open one to the API, so it is given something to find.
    refuse: async (documentId: string, message: string) => {
      adapter.socket = { readyState: 1 };
      adapter.receiveMessage(cbor.encode({ type: "error", senderId: "s", targetId: "t", documentId, message }));
      await settle();
    },
    close: () => {
      for (const id of timers) clearInterval(id as number);
      define("setInterval", realInterval);
      dom.window.close();
    },
  };
};

// The whole of it, for the first time: a click in Elm, out through the real
// `arrangeDoc`, into the real browser store, and back into the document Elm is
// handed on the next load. A bundled sheet has no document of this browser's to
// write to, so the arrangement is kept here or nowhere.
Deno.test({
  name: "a sheet this browser cannot write keeps its arrangement, and opens with it again",
  // The page runs timers by design -- a freshness poll, a query debounce -- and
  // `close` stops the ones it owns.
  sanitizeOps: false,
  sanitizeResources: false,
  fn: async () => {
    const first = await glue("http://localhost/table:countries");
    const name = first.all("span.sort").find((s) => s.textContent?.startsWith("name"));
    await first.click(name);

    const kept = first.held();
    assertEquals(
      kept,
      { "table:countries": { "1": { sort: "asc", rank: 1 } } },
      "the arrangement is held under the column's own key, not its position",
    );
    // The document first and the store last: the bundled doc got the patch too,
    // which is what the session is rendering from.
    const bundled = (EXAMPLES as unknown as Record<string, { doc: { data: Record<string, string>[][] } }>)[
      "table:countries"
    ];
    assertEquals(bundled.doc.data[0][1].sort, "asc", "the document the session reads is patched as well");
    first.close();

    // A bundled document is a module object, and the page patched it in place. A
    // reload re-reads the file, so this has to put it back -- or the sort would
    // come back from memory and the store would be proving nothing.
    delete bundled.doc.data[0][1].sort;
    delete bundled.doc.data[0][1].rank;

    // A reload: a new page over the same browser store.
    const again = await glue("http://localhost/table:countries", { stored: { views: kept } });
    assert(
      again.all("span.sort").some((s) => s.textContent === "name ▲"),
      `the kept arrangement should come back, got: ${again.all("span.sort").map((s) => s.textContent).join("|")}`,
    );
    again.close();
  },
});

// main.ts refuses a viewer's write with a `type: "error"` frame naming the
// document. The vendored adapter logs that frame at a debug namespace and emits
// nothing, so the edit used to vanish with no word to anybody.
Deno.test({
  name: "a refused write reaches the writer, and the arrangement it carried is kept here",
  sanitizeOps: false,
  sanitizeResources: false,
  fn: async () => {
    const doc = {
      type: "table",
      data: [
        [{ name: "a", type: "text", key: "0" }, { name: "b", type: "text", key: "1" }],
        { "0": "x", "1": "y" },
      ],
    };
    const page = await glue("http://localhost/table:shared1", { docs: { shared1: doc } });
    assert(page.text().includes("a"), "the shared sheet should render");

    // Sorted before the refusal arrives, which is the case that used to lose it:
    // the write goes to the document, and only a round trip later does the
    // server say it was not saved.
    await page.click(page.all("span.sort").find((s) => s.textContent?.startsWith("b")));
    assertEquals(page.held(), null, "nothing is held until the server refuses");

    await page.refuse("shared1", "You have viewer access to this sheet, so your edit was not saved.");
    assert(
      page.text().includes("your edit was not saved"),
      `the server's own words should reach the writer, got: ${page.text().slice(-300)}`,
    );
    assertEquals(
      page.held(),
      { "table:shared1": { "1": { sort: "asc", rank: 1 } } },
      "and the arrangement that was refused is kept in this browser instead",
    );
    page.close();
  },
});

// A sort, a filter and a dragged width land on the same document as the query,
// and every change to that document used to start the SQL again. A synced sheet
// is where that shows: the write goes to the handle, the handle reports a
// change, and the change is what used to re-run the query.
Deno.test({
  name: "sorting a query result does not run its SQL again",
  sanitizeOps: false,
  sanitizeResources: false,
  fn: async () => {
    let runs = 0;
    const asked = { type: "query", data: [{ lang: "sql", code: "select 1 as n", cols: {} }] } as {
      data: Record<string, unknown>[];
    };
    const page = await glue("http://localhost/query:asked1", {
      docs: { asked1: asked },
      watching: {
        sheets: (...args: Parameters<typeof sheets>) => {
          const made = sheets(...args);
          const run = made.runSql as (...a: unknown[]) => unknown;
          return { ...made, runSql: (...q: unknown[]) => (runs++, run(...q)) };
        },
      },
    });
    await page.settle(400); // the query editor debounce
    assertEquals(runs, 1, "opening a query sheet runs it once");

    await page.click(page.all("span.sort").find((s) => s.textContent?.startsWith("n")));
    await page.settle(400);
    assertEquals(runs, 1, "sorting the result is not a new question to ask the engine");
    assertEquals(
      (asked.data[0] as { view?: unknown }).view,
      { n: { sort: "asc", rank: 1 } },
      "the arrangement went to the document, under the name its select list gave the column",
    );
    assertEquals(page.held(), null, "and this browser has no reason to hold a copy");
    page.close();
  },
});

// `changeDoc` is the other write port, and `applyPatches` behind it is what puts
// a patch into a document. A synced sheet is where to watch: the write goes to
// the handle, and the object the handle holds is the test's own.
Deno.test({
  name: "a column dragged to a new place moves in the document, carrying what is written on it",
  sanitizeOps: false,
  sanitizeResources: false,
  fn: async () => {
    // `width` is the point: Elm's `Col` carries key, name and type and nothing
    // else, so a column rebuilt on the way back would arrive stripped of it.
    // `applyPatches` moves the value the document already holds instead.
    const doc = {
      type: "table",
      data: [
        [
          { name: "a", type: "text", key: "0" },
          { name: "b", type: "text", key: "1" },
          { name: "c", type: "text", key: "2", width: 220 },
        ],
        { "0": "x", "1": "y", "2": "z" },
      ],
    } as { data: [Record<string, unknown>[], Record<string, string>] };
    const page = await glue("http://localhost/table:moved1", { docs: { moved1: doc } });

    // Grab the third column's handle, hover the first, let go.
    await page.fire(page.all("span.grab")[2], "mousedown");
    await page.fire([...page.all("tbody tr")[0].querySelectorAll("td")][0], "mouseenter");
    await page.keyUp();

    assertEquals(
      doc.data[0].map((col) => col.name),
      ["c", "a", "b"],
      "the column moves in the document, not just on screen",
    );
    assertEquals(doc.data[0][0].width, 220, "and arrives with everything that was written on it");
    assertEquals(doc.data[1], { "0": "x", "1": "y", "2": "z" }, "rows are keyed by column, so no cell moves");
    page.close();
  },
});

Deno.test({
  name: "a cell edit reaches the document and comes back to the screen",
  sanitizeOps: false,
  sanitizeResources: false,
  fn: async () => {
    const doc = {
      type: "table",
      data: [[{ name: "a", type: "text", key: "0" }], { "0": "before" }],
    } as { data: [Record<string, unknown>[], Record<string, string>] };
    const page = await glue("http://localhost/table:edited1", { docs: { edited1: doc } });

    const cell = [...page.all("tbody tr")[3].querySelectorAll("td")][0];
    for (const type of ["mouseenter", "click", "dblclick"]) await page.fire(cell, type);
    await page.type_(page.all("#new-cell")[0], "after");

    assertEquals(doc.data[1], { "0": "after" }, "the write lands in the document the handle holds");
    assert(
      page.text().includes("after"),
      `and the document's answer is what renders, got: ${page.text().slice(0, 200)}`,
    );
    page.close();
  },
});

// The share panel: what Elm asks for is covered above through `boot`. This is
// the middle -- the requests src/index.html actually makes, and the answer
// finding its way back onto the screen.
Deno.test({
  name: "minting a share link asks the server for one and shows what comes back",
  sanitizeOps: false,
  sanitizeResources: false,
  fn: async () => {
    const page = await glue("http://localhost/table:countries#settings", {
      stored: { user: { usr_id: "u1", jwt: "a-token" } },
      respond: (url) => {
        if (url.includes("/link")) return { data: { token: "minted-token" } };
        if (url.includes("/share"))
          return { data: { members: [{ email: "her@example.com", role: "viewer" }], public: false } };
        return { data: [] };
      },
    });

    await page.click(page.all("button").find((b) => b.textContent?.includes("view-only link")));

    const link = page.asked.find((r) => r.url.endsWith("/library/table:countries/link"));
    assert(link, `expected a link request, got: ${JSON.stringify(page.asked.map((r) => r.url))}`);
    assertEquals(link.method, "POST");
    assertEquals(link.body, {}, "an untouched panel asks for the default link, which is an empty body");
    assert(
      page.asked.some((r) => r.method === "GET" && r.url.endsWith("/library/table:countries/share")),
      "and the member list is read back after, so the panel is not left stale",
    );

    const shown = page.all("input[readonly]").map((el) => (el as unknown as { value: string }).value);
    assert(
      shown.some((v) => v.includes("?share=minted-token")),
      `the server's token should be in the link on screen, got: ${JSON.stringify(shown)}`,
    );
    assert(page.text().includes("her@example.com"), "and the members it answered with are listed");
    page.close();
  },
});

Deno.test({
  name: "a share action with nobody logged in says so instead of failing quietly",
  sanitizeOps: false,
  sanitizeResources: false,
  fn: async () => {
    const page = await glue("http://localhost/table:countries#settings");
    await page.click(page.all("button").find((b) => b.textContent?.includes("view-only link")));

    assert(page.text().includes("Log in to share"), `expected the reason, got: ${page.text().slice(-300)}`);
    assertEquals(
      page.asked.filter((r) => r.url.includes("/share") || r.url.includes("/link")),
      [],
      "and nothing was asked of the server",
    );
    page.close();
  },
});
