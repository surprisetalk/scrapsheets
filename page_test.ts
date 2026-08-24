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
// What this still does not cover is the module script in src/index.html — the
// automerge repo, the websocket sync, the real ports. Everything below feeds
// Elm through those ports by hand, mirroring what that file sends.
import { assert, assertEquals } from "@std/assert";
import { JSDOM } from "jsdom";
import { EXAMPLES } from "./src/examples.mjs";
import {
  API_BASE,
  atomToJson,
  docThumb,
  httpErrorDetail,
  httpFailure,
  httpTarget,
  httpUnparsed,
  httpUnreachable,
  library,
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

const boot = async (url: string, { tutorial = -1 } = {}) => {
  await ensureDist();
  const dom = new JSDOM(`<!doctype html><html><body><div id="elm"></div></body></html>`, {
    url,
    pretendToBeVisual: true, // supplies requestAnimationFrame, which is when Elm paints
  });
  const w = dom.window as unknown as Record<string, unknown>;
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
      "localStorage",
      "FileReader",
      "Blob",
    ]
  ) { (globalThis as Record<string, unknown>)[name] = w[name]; }

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
  return { dom, doc, app, settle, click, text, all, type_, asks };
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
