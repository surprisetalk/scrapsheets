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

// The library the page assembles in src/index.html:130-175: two synthetic
// entries, then the bundled examples. The thumbnails and the localStorage half
// are that file's business, not Elm's.
const library: Record<string, { name: string; system: boolean; doc: unknown }> = {
  library: { name: "library", system: true, doc: { type: "library" } },
  shop: { name: "shop", system: true, doc: { type: "shop" } },
  ...(EXAMPLES as Record<string, { name: string; system: boolean; doc: unknown }>),
};

type Ports = Record<string, { send: (v: unknown) => void; subscribe: (f: (v: never) => void) => void }>;
// Deno has no DOM lib, and jsdom ships no types, so the few members these tests
// touch are named here rather than pulling in @types/jsdom for four fields.
type El = {
  dispatchEvent: (event: unknown) => boolean;
  textContent: string | null;
  getAttribute: (name: string) => string | null;
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
  // "there are two Elm.Main modules", so each boot gets a scope of its own.
  const src = await Deno.readTextFile(dir + "dist/index.js");
  const scope: { Elm?: { Main: { init: (o: unknown) => { ports: Ports } } } } = {};
  new Function(src).call(scope);
  assert(scope.Elm?.Main, "dist/index.js should define Elm.Main");

  const app = scope.Elm.Main.init({
    node: dom.window.document.getElementById("elm"),
    flags: { tutorial },
  });

  app.ports.librarySynced.send(library);
  // src/index.html:850 does this: an id that resolves to nothing falls back to
  // the library rather than leaving the page on "loading" forever.
  app.ports.changeId.subscribe((id: string) =>
    app.ports.docSelected.send({ id, data: { doc: library[id]?.doc ?? { type: "library" } } })
  );

  const doc = dom.window.document;
  // Elm paints on an animation frame, so every assertion waits for a few rather
  // than reading the DOM the instant a message is sent.
  const settle = (frames = 12) =>
    new Promise<void>((resolve) => {
      let n = 0;
      const tick = () => (++n >= frames ? resolve() : dom.window.requestAnimationFrame(tick));
      dom.window.requestAnimationFrame(tick);
    });
  const click = async (el: El | undefined | null, init: Record<string, unknown> = {}) => {
    assert(el, "nothing to click");
    el.dispatchEvent(new dom.window.MouseEvent("click", { bubbles: true, ...init }));
    await settle();
  };
  const text = () => doc.body.textContent?.replace(/\s+/g, " ") ?? "";
  const all = (sel: string): El[] => [...doc.querySelectorAll(sel)];

  await settle();
  return { dom, doc, app, settle, click, text, all };
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

Deno.test("?embed=1 renders the sheet with no chrome around it", async () => {
  const { doc, all } = await boot("http://localhost/table:countries?embed=1");
  assert(all("tbody tr").length > 190, "an embed still renders the sheet");
  for (const sel of ["#title", "#aside", "input[placeholder=search]"])
    assertEquals(doc.querySelector(sel), null, `an embed should carry no ${sel}`);
});
