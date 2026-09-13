// The jsdom harness page_test.ts and glue_test.ts share: the compiled Elm,
// the window installed as globals, `boot`, `until`, and the page-side query
// engine. It is a module and not a test file so that the two test files run
// in parallel -- `deno test --parallel` runs files, not tests, side by side --
// and neither registers the other's tests by importing it.
//
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
import { assert } from "@std/assert";
import { JSDOM } from "jsdom";
import { API_BASE, library, sheets } from "./src/page.mjs";
import alasql from "./src/alasql.mjs";
import { register } from "./src/sql.mjs";

export const dir = new URL(".", import.meta.url).pathname;

// A stale dist/ would let this file pass against code that no longer exists,
// so it is refused rather than trusted. Not built here: `deno task test`
// builds once before any file runs, and the files then run in parallel, which
// a compiler racing a reader of its output would make flaky.
let built: Promise<void> | undefined;
export const ensureDist = () =>
  built ??= (async () => {
    const mtime = async (p: string) => (await Deno.stat(dir + p).catch(() => null))?.mtime?.getTime() ?? 0;
    for await (const { name } of Deno.readDir(dir + "src")) {
      const stale = name === "Main.elm" ? "dist/index.js" : `dist/${name}`;
      assert(
        (await mtime(stale)) >= (await mtime(`src/${name}`)),
        `Expected ${stale} at least as new as src/${name}, received an older or missing one. Source: dist/ is built, not checked in. Fix: run deno task build`,
      );
    }
  })();

// Read and compiled once. `new Function` over dist/index.js is most of what a
// boot costs, and nothing in it depends on which test is running.
let elmSource: Promise<() => void> | undefined;
export const compiled = () =>
  elmSource ??= (async () => new Function(await Deno.readTextFile(dir + "dist/index.js")) as () => void)();

// The library the page itself builds, not a copy of it written for the test:
// that is the reason src/page.mjs exists. Nothing is stored, so this is the
// library a first visit sees.
export const shelf = library();

export type Ports = Record<string, { send: (v: unknown) => void; subscribe: (f: (v: never) => void) => void }>;
// Deno has no DOM lib, and jsdom ships no types, so the few members these tests
// touch are named here rather than pulling in @types/jsdom for four fields.
export type El = {
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
export const globalize = (w: Record<string, unknown>) => {
  // jsdom paints on a ~16ms clock, and a settle waits several frames for the
  // page to go quiet. Nothing here depends on real frame timing — only on Elm
  // having had its turn — and that clock was most of this file's wall time, so
  // the frames run as fast as the event loop will carry them. Anything that
  // waits on a real timer instead, like the query debounce, asks for it by name.
  w.requestAnimationFrame = (cb: (t: number) => void) => setTimeout(() => cb(Date.now()), 0);
  w.cancelAnimationFrame = (id: number) => clearTimeout(id);
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
      "File",
      "FormData",
      "Blob",
    ]
  ) { Object.defineProperty(globalThis, name, { value: w[name], configurable: true, writable: true }); }
};

export const boot = async (url: string, { tutorial = -1 } = {}) => {
  await ensureDist();
  // Resolved before the window exists, and nothing between here and Elm's init
  // awaits. `globalize` writes the process-wide `document` and
  // `requestAnimationFrame` that the compiled bundle captures as it evaluates,
  // so a boot that suspends after it hands those globals to whatever boots
  // next: two overlapping boots then leave the first page bound to the second
  // page's document, rendering nothing at all, with no throw to say so.
  const evalElm = await compiled();
  // No `pretendToBeVisual`: `globalize` replaces requestAnimationFrame with the
  // event loop anyway, so the only thing the flag still bought was jsdom's own
  // 16ms frame loop, ticking for the life of every window this file never
  // closes. Sixty of those running at once was a second and a half of the suite.
  const dom = new JSDOM(`<!doctype html><html><body><div id="elm"></div></body></html>`, {
    url,
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
  evalElm.call(scope);
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
  // Quiet is counted off a mutation observer rather than by serializing the
  // body every frame: a 200-row table is 150KB of HTML, and that string was
  // most of what a settle cost.
  let mutations = 0;
  new dom.window.MutationObserver(() => mutations++)
    .observe(doc, { subtree: true, childList: true, attributes: true, characterData: true });
  const settle = () =>
    new Promise<void>((resolve) => {
      let frames = 0, quiet = 0, seen = mutations;
      const tick = () => {
        quiet = mutations === seen ? quiet + 1 : 0;
        seen = mutations;
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

/** Waits for something the page does on a real timer, without waiting the whole
 * timer out: polls until it has happened, and says so by name if it never does.
 * The query editor's debounce is 300ms and three tests slept 400 for it, which
 * was more than a second of this suite's ten spent watching a clock. A wait for
 * something *not* to happen still has to be the flat sleep -- there is no
 * earlier moment that proves it.
 */
export const UNTIL_TRIES = 40;
export const until = async (settle: (ms?: number) => Promise<void>, what: string, done: () => boolean) => {
  for (let tries = 0; tries < UNTIL_TRIES; tries++) {
    if (done()) return;
    await settle(25);
  }
  throw new Error(
    `Expected ${what}, received nothing after ${UNTIL_TRIES} polls 25ms apart. ` +
      `Source: a real timer in src/index.html. Fix: check the timer still fires, or raise UNTIL_TRIES.`,
  );
};

// --- the page's half of the query engine
//
// sheets() over the vendored engine the page loads, with the two things it takes
// from the browser stubbed: the library map, and finding a document that is not
// in it. Nothing here is bundled outside the library, so `find` answers nothing
// and the "this sheet has no data" path is the one that runs.
register(alasql);
alasql.options.modifier = "RECORDSET";
export const resolver = () => sheets(alasql, () => shelf, () => Promise.resolve(undefined));
export const rowsOf = async (code: string) => {
  const { data } = await resolver().runSql(code, { "": null });
  return data as Record<string, unknown>[];
};
export const refused = async (code: string) => {
  try {
    await resolver().runSql(code, { "": null });
  } catch (err) {
    return (err as Error).message;
  }
  throw new Error(`expected this to be refused: ${code}`);
};
