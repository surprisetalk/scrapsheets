// The glue harness: src/index.html's own module script, over a jsdom.
//
// A module and not a test file, for the reason `page_harness.ts` is one: two
// test files import this, and a test file would register its own tests into
// both of them.

import { assert, assertEquals } from "@std/assert";
import { cbor, decodeHeads, encodeHeads, Repo as AutomergeRepo } from "@automerge/automerge-repo";
import { decodeSyncMessage, encodeSyncMessage } from "@automerge/automerge";
import { JSDOM } from "jsdom";
import { BrowserWebSocketClientAdapter } from "./src/automerge-repo-ws.mjs";
import * as pageExports from "./src/page.mjs";
import * as sqlExports from "./src/sql.mjs";
import alasql from "./src/alasql.mjs";
import { compiled, dir, El, ensureDist, globalize, Ports, until } from "./page_harness.ts";

// --- the glue, actually executed
//
// `boot` in page_harness.ts answers every port by hand. src/index.html is what
// does that for
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
  let listeners: ((d: unknown) => void)[] = [];
  // Every change moves the heads on, the way a real document's do.
  let changes = 0;
  const handle = {
    documentId,
    doc: () => doc,
    // What the real handle answers: its heads, base58 the way a URL carries them.
    heads: () => encodeHeads([changes.toString(16).padStart(64, "0")]),
    change: (fn: (d: Record<string, unknown>) => void) => {
      fn(doc);
      changes++;
      // The shape automerge sends and `DocDelta` in src/Main.elm decodes. A field
      // short of it and the port refuses the whole value.
      for (const listener of listeners) listener({ doc, handle, patchInfo: null, patches: [] });
    },
    on: (_event: string, cb: (d: unknown) => void) => listeners.push(cb),
    off: (_event: string, cb: (d: unknown) => void) => {
      listeners = listeners.filter((l) => l !== cb);
    },
  };
  return handle;
};

export const glue = async (
  url: string,
  {
    stored = {},
    docs = {} as Record<string, Record<string, unknown>>,
    // What a test wants to watch rather than replace: whatever is named here
    // wins over the real export of the same name.
    watching = {} as Record<string, unknown>,
    // What the API answers: a `Response` is used as it is, anything else is sent
    // back as a JSON 200. `{ data: [] }` is the shape the freshness poll reads,
    // which every page makes whether a test cares or not.
    respond = (_url: string, _init?: RequestInit): unknown => ({ data: [] }),
    // Automerge itself, rather than a stub of it. Slower, and the only way to
    // find out whether a patch means the same thing to a real document as it
    // does to a plain object.
    realRepo = false,
  } = {},
) => {
  await ensureDist();
  // Resolved before the window exists, for the reason `boot` gives above.
  const evalElm = await compiled();
  // No `pretendToBeVisual`, for the reason `boot` gives above.
  const dom = new JSDOM(`<!doctype html><html><body><div id="elm"></div></body></html>`, {
    url,
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
      // A string body is the JSON these routes send; anything else -- a
      // FormData carrying a file -- is handed over as it is, for the test to read.
      body: typeof init?.body === "string" ? JSON.parse(init.body) : (init?.body ?? null),
    });
    const answer = respond(url, init);
    if (answer instanceof Response) return Promise.resolve(answer);
    return Promise.resolve(
      new Response(JSON.stringify(answer ?? { data: [] }), {
        status: 200,
        headers: { "Content-Type": "application/json" },
      }),
    );
  });
  // A net-socket sheet opens one of these to watch a feed. Recorded so a test
  // can say what was opened and then say what happened to it.
  const sockets: { url: string; onopen?: () => void; onclose?: () => void; onerror?: () => void; closed: boolean }[] =
    [];
  define(
    "WebSocket",
    class {
      url: string;
      readyState = 0;
      closed = false;
      onopen?: () => void;
      onclose?: () => void;
      onerror?: () => void;
      constructor(url: string) {
        this.url = url;
        sockets.push(this);
      }
      close() {
        this.closed = true;
      }
    },
  );
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
  evalElm.call(scope);
  let app!: { ports: Ports };
  define("Elm", { Main: { init: (o: unknown) => (app = scope.Elm!.Main.init(o)) } });

  // The one thing the harness has to reach afterwards: the refusal hook lives on
  // this instance, and `Repo` is where src/index.html hands it over.
  let adapter!: { socket?: unknown; receiveMessage: (data: unknown) => void };
  let repo!: { find: (id: string) => Promise<{ doc: () => unknown } | null> };
  // Documents the page made for itself, in the order it made them.
  const created: Record<string, unknown>[] = [];
  // The handle the page was last given for each document, so a test can ask
  // what a real handle would answer.
  const handles: Record<string, ReturnType<typeof fakeHandle>> = {};
  const deps: Record<string, unknown> = {
    ...pageExports,
    ...sqlExports,
    ...watching,
    alasql,
    initializeWasm: () => Promise.resolve(),
    cbor,
    decodeHeads,
    decodeSyncMessage,
    BrowserWebSocketClientAdapter,
    IndexedDBStorageAdapter: class {},
    Repo: realRepo
      // No storage and no network: a document made here lives and dies in this
      // test, and nothing it does reaches a socket.
      ? class extends AutomergeRepo {
        constructor(options: { network: (typeof adapter)[] }) {
          adapter = options.network[0];
          super({ network: [] });
          repo = this as unknown as typeof repo;
        }
      }
      : class {
        constructor(options: { network: (typeof adapter)[] }) {
          adapter = options.network[0];
          repo = this as unknown as typeof repo;
        }
        on() {}
        find(doc_id: string) {
          if (!docs[doc_id]) return Promise.resolve(null);
          handles[doc_id] ??= fakeHandle(doc_id, docs[doc_id]);
          return Promise.resolve(handles[doc_id]);
        }
        create(doc: Record<string, unknown>) {
          const doc_id = `made${created.length + 1}`;
          created.push(doc);
          docs[doc_id] = doc;
          handles[doc_id] = fakeHandle(doc_id, doc);
          return handles[doc_id];
        }
      },
  };
  const AsyncFunction = Object.getPrototypeOf(async function () {}).constructor;
  await new AsyncFunction("deps", await glueSource(deps))(deps);

  const doc = dom.window.document;
  // jsdom's File implements no `text()`, and src/index.html's drop handler reads
  // one. Everything else about the file is jsdom's, because Elm reads it with
  // jsdom's FileReader and that will not take a foreign Blob.
  const csvFile = (name: string, content: string) => {
    const file = new dom.window.File([content], name, { type: "text/csv" });
    if (typeof (file as unknown as { text?: unknown }).text !== "function")
      Object.defineProperty(file, "text", { value: () => Promise.resolve(content) });
    return file;
  };
  // The same quiet-frames wait `boot` uses, and for the same reason: a fixed
  // count long enough for a click to go out through a port and back was half a
  // second on every assertion. `ms` is for the parts of the page that wait on a
  // real clock instead -- the query debounce, and a file being read.
  let mutations = 0;
  new dom.window.MutationObserver(() => mutations++)
    .observe(doc, { subtree: true, childList: true, attributes: true, characterData: true });
  const settle = async (ms = 0) => {
    await new Promise<void>((resolve) => {
      let frames = 0, quiet = 0, seen = mutations;
      const tick = () => {
        quiet = mutations === seen ? quiet + 1 : 0;
        seen = mutations;
        if (quiet >= 3 || ++frames >= 24) return resolve();
        dom.window.requestAnimationFrame(tick);
      };
      dom.window.requestAnimationFrame(tick);
    });
    if (ms) await new Promise((r) => setTimeout(r, ms));
  };
  await settle();
  // An empty body is the one tell that another harness took the process globals
  // while this page was coming up. `boot` cannot lose them -- it holds them
  // across no await -- but index.html's module script reads `fetch`,
  // `localStorage`, `document` and the socket for the life of the page, and
  // `define` writes them globally, so two glue pages under construction at once
  // share one set and the first renders nothing. Named here rather than left to
  // surface as a blank assertion in whichever test booted second.
  assert(
    dom.window.document.body.textContent,
    `Expected the page at ${url} to render, received an empty body. Source: glue() owns the process ` +
      `globals for the life of its page. Fix: await each glue() before starting the next.`,
  );
  return {
    app,
    settle,
    asked,
    created,
    sockets,
    /** The document the repo holds, read back the way the page would. */
    document: async (id: string) => (await repo.find(id.split(":")[1]))?.doc(),
    path: () => dom.window.location.pathname.slice(1),
    handles,
    // The address bar: a new path, then the popstate Elm's navigation listens to.
    go: async (path: string) => {
      dom.window.history.pushState({}, "", path);
      dom.window.dispatchEvent(new dom.window.PopStateEvent("popstate"));
      await settle();
    },
    /** A file chosen through the footer's file input, which is what Elm listens
     * for. `files` is read-only on the element, and Elm's decoder takes a plain
     * array as readily as a FileList.
     */
    pickFile: async (name: string, content: string) => {
      const input = [...doc.querySelectorAll('input[type="file"]')][0];
      assert(input, "expected a file input on the library sheet");
      Object.defineProperty(input, "files", { value: [csvFile(name, content)], configurable: true });
      // Reading the file is real IO, and a flat sleep after the dispatch lost
      // the race to it on a loaded machine. The page has reacted when it asked
      // the server something or painted a word -- a logged-out import asks
      // nothing and says why.
      const before = { asked: asked.length, text: doc.body.textContent };
      input.dispatchEvent(new dom.window.Event("change", { bubbles: true }));
      await until(
        settle,
        "the page to react to the chosen file",
        () => asked.length > before.asked || doc.body.textContent !== before.text,
      );
    },
    /** A file dropped on the page, which src/index.html handles itself. jsdom
     * builds no `dataTransfer`, so the handler is given the one it reads.
     */
    dropFile: async (name: string, content: string) => {
      const event = new dom.window.Event("drop", { bubbles: true });
      Object.defineProperty(event, "dataTransfer", { value: { files: [csvFile(name, content)] } });
      const before = { asked: asked.length, text: doc.body.textContent };
      dom.window.document.body.dispatchEvent(event);
      await until(
        settle,
        "the page to react to the dropped file",
        () => asked.length > before.asked || doc.body.textContent !== before.text,
      );
    },
    all: (sel: string): El[] => [...doc.querySelectorAll(sel)],
    fire: async (el: El, type: string) => {
      el.dispatchEvent(new dom.window.MouseEvent(type, { bubbles: type !== "mouseenter" }));
      await settle();
    },
    keyUp: async () => {
      doc.dispatchEvent(new dom.window.MouseEvent("mouseup", { bubbles: true }));
      await settle();
    },
    /** A keystroke the page hears the way the global handler hears one: on the
     * body, because Elm's decoder ignores a key typed into an input.
     */
    key: async (init: Record<string, unknown>) => {
      doc.body.dispatchEvent(new dom.window.KeyboardEvent("keydown", { bubbles: true, ...init }));
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
    stored: (key: string) => JSON.parse((w.localStorage as Storage).getItem(`scrapsheets-${key}`) ?? "null"),
    /** A file the page built, read the way the page's own reader reads one. */
    readFile: (file: unknown) =>
      new Promise<string>((resolve, reject) => {
        const reader = new dom.window.FileReader();
        reader.onload = () => resolve(reader.result as string);
        reader.onerror = () => reject(reader.error);
        reader.readAsText(file as Blob);
      }),
    // The sync server refuses a viewer's write with this frame. The adapter
    // asserts it has a socket before reading one, and connecting for real would
    // open one to the API, so it is given something to find.
    refuse: async (documentId: string, message: string) => {
      adapter.socket = { readyState: 1 };
      adapter.receiveMessage(cbor.encode({ type: "error", senderId: "s", targetId: "t", documentId, message }));
      await settle();
    },
    // The sync server's reply once it has taken a write: a sync frame whose
    // heads are the document's, which include the head this browser wrote.
    land: async (documentId: string, heads: string[]) => {
      adapter.socket = { readyState: 1 };
      const data = encodeSyncMessage({ heads, need: [], have: [], changes: [] });
      adapter.receiveMessage(cbor.encode({ type: "sync", senderId: "s", targetId: "t", documentId, data }));
      await settle();
    },
    // The intervals the page started, and nothing else. `dom.window.close()`
    // does not stop jsdom's animation frames, so a frame the page has already
    // asked for fires against a closed window and throws reading `location` off
    // it -- uncatchably, from a timer. `boot` leaves its window open for the
    // same reason, and an idle jsdom costs a timer nobody is watching.
    close: () => {
      for (const id of timers) clearInterval(id as number);
      define("setInterval", realInterval);
    },
  };
};
