// The glue, actually executed. Split out of page_test.ts so the two harnesses
// run side by side: `deno test --parallel` runs files, not tests, in parallel,
// and one file of both was the whole critical path of the suite.

import { assert, assertEquals, assertRejects } from "@std/assert";
import { cbor, decodeHeads, encodeHeads, Repo as AutomergeRepo } from "@automerge/automerge-repo";
import { decodeSyncMessage, encodeSyncMessage } from "@automerge/automerge";
import { JSDOM } from "jsdom";
import { BrowserWebSocketClientAdapter } from "./src/automerge-repo-ws.mjs";
import { EXAMPLES } from "./src/examples.mjs";
import * as pageExports from "./src/page.mjs";
import * as sqlExports from "./src/sql.mjs";
import { API_BASE, sheets } from "./src/page.mjs";
import alasql from "./src/alasql.mjs";
import { boot, compiled, dir, El, ensureDist, globalize, Ports, until } from "./page_harness.ts";

// The paging mode is a field on the document like any other, and the inputs
// beside it -- the page parameter, the cursor path -- follow whatever the
// document answers back rather than the click itself: `InputChange NetPageBy`
// sends the patch and waits, the way every other field on this form does.
Deno.test({
  name: "the paging and keeping forms write the document, and their own fields follow the mode",
  sanitizeOps: false,
  sanitizeResources: false,
  fn: async () => {
    const doc = {
      type: "net-http",
      data: [{ url: "https://example.com/feed.json", interval: 3600, method: "GET", page_by: "page" }],
    };
    const page = await glue("http://localhost/net-http:feed", { docs: { feed: doc } });
    const cfg = () =>
      doc.data[0] as { page_by?: string; page_path?: string; mode?: string; key?: string; rows_path?: string };

    const pageSelect = page.all("select").find((s) =>
      [...s.querySelectorAll("option")].some((o) => o.textContent === "no, one request")
    );
    assert(pageSelect, "the feed offers a way to read every page");
    assertEquals(
      page.all("label").some((l) => l.textContent?.includes("cursor path")),
      false,
      'no cursor path input under "page"',
    );

    await page.type_(pageSelect, "cursor");
    assertEquals(cfg().page_by, "cursor", "choosing a page mode writes it to the document");

    const pathInput = page.all("label").find((l) => l.textContent?.includes("cursor path"))?.querySelector("input");
    assert(pathInput, "a cursor path input appears once the mode asks for one");
    await page.type_(pathInput, "meta.next");
    assertEquals(cfg().page_path, "meta.next", "typing into the cursor path field writes it to the document");

    // What a good run does to the runs before it is the same kind of field: a
    // select over the modes, and the key it supersedes by drawn only under the
    // one mode that reads one.
    const keepSelect = page.all("select").find((s) =>
      [...s.querySelectorAll("option")].some((o) => o.textContent === "append, every run")
    );
    assert(keepSelect, "the feed offers a way to keep less than every run");
    assertEquals(
      page.all("label").some((l) => l.textContent?.includes("key")),
      false,
      "no key input while the feed appends",
    );
    await page.type_(keepSelect, "upsert");
    assertEquals(cfg().mode, "upsert", "choosing a mode writes it to the document");

    const keyInput = page.all("label").find((l) => l.textContent?.includes("key"))?.querySelector("input");
    assert(keyInput, "a key input appears once the mode asks for one");
    await page.type_(keyInput, "id");
    assertEquals(cfg().key, "id", "typing into the key field writes it to the document");

    // The rows path is not per-mode: any feed whose rows sit in an envelope
    // names it, so it is drawn whatever the sheet keeps.
    const rowsInput = page.all("label").find((l) => l.textContent?.includes("rows path"))?.querySelector("input");
    assert(rowsInput, "the rows path is asked for whatever the mode is");
    await page.type_(rowsInput, "data");
    assertEquals(cfg().rows_path, "data", "typing into the rows path field writes it to the document");
    page.close();
  },
});

// A column of "city, country" is two facts in one column, and pulling them apart
// used to mean writing a query. The box says what stands between the parts and
// the button pushes a column per part position, through the same changeDoc every
// other verb in that panel rides.
Deno.test({
  name: "splitting a column from its panel pushes the columns and writes their cells",
  sanitizeOps: false,
  sanitizeResources: false,
  fn: async () => {
    const doc = {
      type: "table",
      data: [
        [{ name: "where", type: "text", key: "0" }],
        { "0": "Oslo, NO" },
        { "0": "Bergen" },
      ],
    } as { type: string; data: unknown[] };
    const was = JSON.stringify(doc.data);
    const page = await glue("http://localhost/table:split1", { docs: { split1: doc } });
    await page.click(page.all("span.funnel")[0]);
    await page.type_(page.all(".split input")[0], ", ");
    await page.click(page.all("button").find((b) => b.textContent === "Split"));
    assertEquals(
      doc.data[0],
      [
        { name: "where", type: "text", key: "0" },
        { name: "where 1", type: "text", key: 1 },
        { name: "where 2", type: "text", key: 2 },
      ],
      "a column per part position, named after the column split and keyed past the ones the sheet had",
    );
    assertEquals(
      doc.data.slice(1),
      [{ "0": "Oslo, NO", "1": "Oslo", "2": "NO" }, { "0": "Bergen", "1": "Bergen" }],
      "one cell per part, the row with fewer parts leaving its later cell unwritten, and the column split from left alone",
    );

    await page.key({ key: "z", ctrlKey: true });
    assertEquals(JSON.stringify(doc.data), was, "undo takes the pushed columns off and the cells it wrote back out");
    page.close();
  },
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

const glue = async (
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
    assert(
      page.text().includes("arranged is kept in this browser"),
      `and the writer is told the arrangement is not lost, got: ${page.text().slice(-300)}`,
    );
    assertEquals(
      page.held(),
      { "table:shared1": { "1": { sort: "asc", rank: 1 } } },
      "and the arrangement that was refused is kept in this browser instead",
    );
    page.close();
  },
});

// A held arrangement exists because the document would not take it. Once the
// document does -- an owner granted this browser editor access -- the document
// is the truth again, and the server says so the only way it can: its next
// sync frame carries the head this browser wrote. Refusals are remembered for
// one open of the sheet, not the whole session, or the grant is never tried.
// Opening a sheet stamps when, on the stored library entry, and only for a
// sheet the library already lists: a sheet opened from somebody's link is not
// thereby yours. A bundled sheet gets a stored entry of this one field.
Deno.test({
  name: "opening a sheet records when, for the library's opened column",
  sanitizeOps: false,
  sanitizeResources: false,
  fn: async () => {
    const doc = { type: "table", data: [[{ name: "a", type: "text", key: "0" }], { "0": "x" }] };
    const page = await glue("http://localhost/", {
      docs: { shared1: doc, nowhere1: doc },
      stored: { library: { "table:shared1": { name: "shared", tags: [] } } },
    });
    await page.go("/table:shared1");
    const seen = page.stored("library")["table:shared1"].seen;
    assert(Date.now() - Date.parse(seen) < 60_000, `seen should be now, got ${seen}`);

    await page.go("/table:countries");
    const bundled = page.stored("library")["table:countries"];
    assert(bundled.seen, "a bundled sheet gets a stored entry for when it was opened");
    assertEquals(bundled.name, undefined, "and nothing else: the bundled entry stays the bundled one");

    await page.go("/table:nowhere1");
    assertEquals(
      page.stored("library")["table:nowhere1"],
      undefined,
      "a sheet the library does not list is not added to it",
    );

    await page.go("/");
    const row = page.all("tbody tr").find((tr) => tr.textContent?.includes("shared"));
    assert(row?.textContent?.includes(seen), "the library row shows when the sheet was opened");
    page.close();
  },
});

Deno.test({
  name: "a held arrangement is dropped once the document takes one",
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
    const sortOn = async (name: string) =>
      await page.click(page.all("span.sort").find((s) => s.textContent?.startsWith(name)));
    await sortOn("b");
    await page.refuse("shared1", "You have viewer access to this sheet, so your edit was not saved.");
    assertEquals(page.held(), { "table:shared1": { "1": { sort: "asc", rank: 1 } } }, "refused, so held");
    await sortOn("a");
    assertEquals(
      (doc.data[0] as { sort?: string }[])[0].sort,
      undefined,
      "a second arrangement in the same open goes to the store, not the document",
    );

    // Granted editor access in the meantime. The next open tries the document
    // again, and this time nothing refuses it.
    await page.go("/");
    assertEquals(page.path(), "", "on the library");
    await page.go("/table:shared1");
    assertEquals(page.path(), "table:shared1", "back on the sheet");
    // The held view opened the sheet sorted on a already, so this click is the
    // toggle -- and it goes to the document.
    await sortOn("a");
    assertEquals(
      (doc.data[0] as { sort?: string }[])[0].sort,
      "desc",
      "on a new open the arrangement is written to the document again",
    );
    assert(page.held()?.["table:shared1"], "and still held until the server says it landed");

    await page.land("shared1", ["cd".repeat(32)]);
    assert(page.held()?.["table:shared1"], "a reply that does not cover the write drops nothing");
    await page.land("shared1", decodeHeads(page.handles.shared1.heads()));
    assertEquals(page.held(), {}, "a reply that covers the write is the document holding it, so nothing is held");
    page.close();
  },
});

// The column's own panel is where a column is hidden and pinned, so it is where
// it is cleaned too. Each verb is one DocMsg, which is what buys undo, the
// viewer refusal and the sync path without any of them being written again.
Deno.test({
  name: "cleaning a column from its panel lands on the document",
  sanitizeOps: false,
  sanitizeResources: false,
  fn: async () => {
    const doc = {
      type: "table",
      data: [
        [{ name: "city", type: "text", key: "0" }, { name: "note", type: "text", key: "1" }],
        { "0": " Oslo ", "1": "a" },
        { "0": "bergen", "1": "b" },
        { "0": "  ", "1": "c" },
      ],
    } as { type: string; data: unknown[] };
    const page = await glue("http://localhost/table:clean1", { docs: { clean1: doc } });
    // The panel is a toggle, and a verb may leave it open or closed depending on
    // whether the row it acted on is still there to re-render under it.
    const verb = async (label: string) => {
      if (!page.all("button").some((b) => b.textContent === label)) await page.click(page.all("span.funnel")[0]);
      await page.click(page.all("button").find((b) => b.textContent === label));
    };

    await verb("Trim");
    assertEquals(doc.data.slice(1), [{ "0": "Oslo", "1": "a" }, { "0": "bergen", "1": "b" }, { "0": "", "1": "c" }]);

    await verb("UPPER");
    assertEquals(
      doc.data.slice(1).map((r) => (r as Record<string, string>)["0"]),
      ["OSLO", "BERGEN", ""],
      "and only the column whose panel was open",
    );
    assertEquals((doc.data[1] as Record<string, string>)["1"], "a", "the other column is untouched");

    await verb("Drop blank rows");
    assertEquals(doc.data.slice(1), [{ "0": "OSLO", "1": "a" }, { "0": "BERGEN", "1": "b" }], "the blank row goes");
    page.close();
  },
});

// A sheet this browser no longer has is a sheet with nothing to hold a view for.
Deno.test({
  name: "trashing a sheet keeps what this browser held for it, and purging it does not",
  sanitizeOps: false,
  sanitizeResources: false,
  fn: async () => {
    const held = { "table:shared1": { "1": { sort: "asc", rank: 1 } }, "table:other": { "0": { width: 80 } } };
    const page = await glue("http://localhost/", {
      stored: {
        library: { "table:shared1": { name: "shared", tags: [] } },
        views: { ...held },
      },
    });
    const button = (label: string) =>
      [...(page.all("tbody tr").find((tr) => tr.textContent?.includes("shared"))?.querySelectorAll("button") ?? [])]
        .find((b) => b.textContent === label);

    await page.click(button("trash"));
    // The flag and the name, not the whole entry: the page also stores a
    // thumbnail for a sheet it has drawn, and whether it has by now is not what
    // this test is about.
    const entry = () =>
      (page.stored("library") as Record<string, { name?: string; trashed?: boolean }>)["table:shared1"];
    assertEquals(
      [entry()?.name, entry()?.trashed],
      ["shared", true],
      "trashing flags the entry rather than removing it",
    );
    // The arrangement has to survive the trash, or restoring gives you back a
    // sheet with somebody else's sort on it.
    assertEquals(page.held(), held, "and leaves the held view alone");

    await page.click(page.all("button.chip").find((b) => b.textContent?.startsWith("🗑")));
    await page.click(button("delete"));
    await page.click(page.all("button").find((b) => b.textContent === "Delete"));
    assertEquals(page.stored("library"), {}, "purging from the trash is what takes the sheet out of the library");
    assertEquals(page.held(), { "table:other": { "0": { width: 80 } } }, "and its held view goes with it");
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
    await until(page.settle, "the debounce to run the query once", () => runs === 1);
    assertEquals(runs, 1, "opening a query sheet runs it once");

    // runs === 1 is runSql entered, not its answer drawn, and the header this
    // clicks exists only once the result rendered. Waiting for the count alone
    // clicked nothing about one run in three on a loaded machine.
    const sortN = () => page.all("span.sort").find((s) => s.textContent?.startsWith("n"));
    await until(page.settle, "the query's answer to draw its columns", () => !!sortN());
    await page.click(sortN());
    // Proving nothing ran has no earlier moment than the debounce itself, so
    // this one stays a flat wait -- 320 for src/index.html's 300, and not a
    // rounder number, because every millisecond over is one the suite spends
    // watching a clock.
    await page.settle(320);
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

// explain answers a table whose columns are not the query's own, so the page's
// column check has to know to leave it alone -- or every profile would be
// refused for having no column the sheets hold.
Deno.test({
  name: "explain renders its profile in a query sheet without tripping the column check",
  sanitizeOps: false,
  sanitizeResources: false,
  fn: async () => {
    const profiled = { type: "query", data: [{ lang: "sql", code: "explain select 1 as n", cols: {} }] };
    const page = await glue("http://localhost/query:profiled1", { docs: { profiled1: profiled } });
    await until(page.settle, "the profile to render its stages", () => page.all("span.sort").length >= 4);
    const headers = page.all("span.sort").map((s) => s.textContent?.replace(/ [▲▼]$/, ""));
    assertEquals(headers.slice(0, 4), ["stage", "rows_in", "rows_out", "ms"]);
    assert(page.text().includes("total"), "the last stage is the whole statement");
    assert(!page.text().includes("No column named"), page.text().slice(0, 300));
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

// The server refuses a publish over an email address, a phone number, an SSN or
// a card number. The claim that answers it is a second checkbox, and what
// matters is that the publish carries it: a box that ticks and is not sent is a
// publisher who thinks they said something.
Deno.test({
  name: "publishing a sheet carries what the publisher said about personal data",
  sanitizeOps: false,
  sanitizeResources: false,
  fn: async () => {
    const page = await glue("http://localhost/table:countries#settings", {
      stored: { user: { usr_id: "u1", jwt: "a-token" } },
      respond: (url) =>
        url.includes("/public")
          ? { data: { public: true } }
          : url.includes("/share")
          ? { data: { members: [], public: false } }
          : { data: [] },
    });
    const tick = async (word: string) => {
      const label = page.all("label").find((l) => l.textContent?.includes(word));
      assert(label, `expected a checkbox labelled "${word}", got: ${page.text().slice(0, 400)}`);
      const input = label.querySelector("input");
      assert(input, `expected an input inside the "${word}" label`);
      // The box is set and the change fired by hand: Elm reads `checked` off the
      // event's target, and a synthetic click is not what put it there.
      (input as unknown as { checked: boolean }).checked = true;
      await page.fire(input, "change");
    };
    const published = () => page.asked.filter((r) => r.url.endsWith("/library/table:countries/public"));

    await tick("private");
    assertEquals(
      published().at(-1)?.body,
      { public: true, personal: false },
      "nobody has claimed anything, and the publish says so rather than leaving the field out",
    );

    await tick("holds personal data");
    assertEquals(published().length, 1, "the claim alone publishes nothing");

    await tick("private");
    assertEquals(
      published().at(-1)?.body,
      { public: true, personal: true },
      "and the next publish carries the claim the publisher made",
    );
    page.close();
  },
});

// CSV import, whichever way the file arrives. Both go to the server: it is the
// one that registers the sheet, syncs it and infers the column types. A dropped
// file used to be parsed in the browser and made into a sheet this browser
// owned, so the same gesture produced two different sheets off two parsers.
// What the server answers a preview with: the guess, and the first rows.
const previewOf = {
  data: {
    name: "countries of the world",
    cols: [{ name: "name", type: "text", key: "0" }, { name: "code", type: "num", key: "1" }],
    rows: [{ "0": "Chile", "1": 56 }],
    count: 1,
  },
};
const imported = {
  stored: { user: { usr_id: "u1", jwt: "a-token" } },
  docs: {
    imported1: { type: "table", data: [[{ name: "name", type: "text", key: "0" }], { "0": "Chile" }] },
  },
  respond: (url: string) =>
    url.endsWith("/import/preview")
      ? previewOf
      : url.includes("/import/csv")
      ? { sheet_id: "table:imported1" }
      : { data: [] },
};

Deno.test({
  name: "a chosen CSV is posted to the server, and the sheet it makes is opened",
  sanitizeOps: false,
  sanitizeResources: false,
  fn: async () => {
    const page = await glue("http://localhost/", imported);
    await page.pickFile("countries of the world.csv", "name,code\nChile,CL\n");

    // First the preview: the file goes over, and what comes back is shown
    // before anything is made -- the guess, and a select to correct it.
    const previewed = page.asked.find((r) => r.url.endsWith("/import/preview"));
    assert(previewed, `expected the preview request, got: ${JSON.stringify(page.asked.map((r) => r.url))}`);
    const sent = (previewed.body as FormData).get("file") as File;
    assertEquals(sent.name, "countries of the world.csv", "the file goes over with the name it had");
    assertEquals(await page.readFile(sent), "name,code\nChile,CL\n", "and the bytes Elm read out of it");
    assert(
      page.text().includes("Correct a type before the sheet is made"),
      `the preview is shown: ${page.text().slice(-300)}`,
    );
    const selects = page.all("select");
    assertEquals(
      selects.map((el) => (el as unknown as { value: string }).value),
      ["text", "num"],
      "with the guess per column",
    );
    assertEquals(page.asked.filter((r) => r.url.includes("/import/csv")).length, 0, "and nothing is made yet");

    // Correct one, then import: the settled types ride the request, and are
    // remembered for the next file with this header.
    await page.type_(selects[1], "text");
    await page.click(page.all("button").find((b) => b.textContent === "Import"));
    const post = page.asked.find((r) => r.url.includes("/import/csv"));
    assert(post, `expected the import request, got: ${JSON.stringify(page.asked.map((r) => r.url))}`);
    assertEquals(post.method, "POST");
    assertEquals(
      JSON.parse(decodeURIComponent(post.url.split("?types=")[1])),
      { name: "text", code: "text" },
      "the types the user settled on go with the file",
    );
    assertEquals(await page.readFile((post.body as FormData).get("file")), "name,code\nChile,CL\n", "the same file");
    assertEquals(
      page.stored("imports"),
      { "name\u0001code": { name: "text", code: "text" } },
      "and are remembered by header",
    );

    // The sheet is the server's, and the library is what this browser holds plus
    // what ships bundled — so it has to be told the sheet exists, or the import
    // lands somewhere it cannot be seen.
    // A thumbnail joins it on the way in, which is every library entry's story.
    const entry = page.stored("library")["table:imported1"] as { name: string; tags: string[] };
    assertEquals(
      [entry.name, entry.tags],
      ["countries of the world", []],
      "the imported sheet joins this browser's library, under the file's name",
    );
    assertEquals(page.path(), "table:imported1", "and the page goes to it");
    assert(
      page.text().includes("Chile"),
      `the sheet the server made is what renders, got: ${page.text().slice(0, 200)}`,
    );
    page.close();
  },
});

Deno.test({
  name: "a file with a header seen before opens with the types settled last time",
  sanitizeOps: false,
  sanitizeResources: false,
  fn: async () => {
    const page = await glue("http://localhost/", {
      ...imported,
      stored: { ...imported.stored, imports: { "name\u0001code": { code: "text" } } },
    });
    await page.pickFile("more countries.csv", "name,code\nPeru,PE\n");
    const selects = page.all("select");
    assertEquals(
      selects.map((el) => (el as unknown as { value: string }).value),
      ["text", "text"],
      "the memory over the guess",
    );
    assert(page.text().includes("remembered"), "and it says which one was remembered");
    page.close();
  },
});

Deno.test({
  name: "a dropped CSV takes the same path as a chosen one",
  sanitizeOps: false,
  sanitizeResources: false,
  fn: async () => {
    const page = await glue("http://localhost/", imported);
    await page.dropFile("prices.csv", 'item,cost\n"widget, large",4.50\n');

    const post = page.asked.find((r) => r.url.endsWith("/import/preview"));
    assert(
      post,
      `a dropped file should be previewed the same way, got: ${JSON.stringify(page.asked.map((r) => r.url))}`,
    );
    assertEquals(await page.readFile((post.body as FormData).get("file")), 'item,cost\n"widget, large",4.50\n');
    assertEquals(page.created, [], "and nothing is parsed and made here any more");
    await page.click(page.all("button").find((b) => b.textContent === "Import"));
    assertEquals(page.path(), "table:imported1", "the page goes to the sheet the server made");
    page.close();
  },
});

Deno.test({
  name: "a CSV import the server refuses says what the server said",
  sanitizeOps: false,
  sanitizeResources: false,
  fn: async () => {
    const page = await glue("http://localhost/", {
      stored: { user: { usr_id: "u1", jwt: "a-token" } },
      respond: (url) =>
        url.endsWith("/import/preview")
          ? new Response("Expected a header row, received an empty file.", { status: 400 })
          : { data: [] },
    });
    await page.pickFile("empty.csv", "");

    assert(
      page.text().includes("Expected a header row"),
      `the server's own words should reach the page, got: ${page.text().slice(-300)}`,
    );
    assertEquals(
      Object.keys(page.stored("library") ?? {}),
      [],
      "and nothing is added to the library for a sheet that was not made",
    );
    page.close();
  },
});

Deno.test({
  name: "a CSV import with nobody logged in says so instead of failing quietly",
  sanitizeOps: false,
  sanitizeResources: false,
  fn: async () => {
    const page = await glue("http://localhost/");
    await page.pickFile("x.csv", "a,b\n1,2\n");

    assert(page.text().includes("log in to import"), `expected the reason, got: ${page.text().slice(-300)}`);
    assertEquals(page.asked.filter((r) => r.url.includes("/import/")), [], "and nothing was asked of the server");
    page.close();
  },
});

// A net-socket sheet's socket is opened by the browser watching it, and nothing
// server-side ever opens one — so this report is the only witness the server has
// that the feed is alive.
Deno.test({
  name: "a socket that opens is reported, and only when its state changes",
  sanitizeOps: false,
  sanitizeResources: false,
  fn: async () => {
    const page = await glue("http://localhost/net-socket:feed1", {
      stored: { user: { usr_id: "u1", jwt: "a-token" } },
      docs: { feed1: { type: "net-socket", data: [{ url: "wss://feed.example/ticks" }] } },
    });
    const reports = () => page.asked.filter((r) => r.url.endsWith("/library/net-socket:feed1/socket"));

    assertEquals(page.sockets.map((s) => s.url), ["wss://feed.example/ticks"], "the sheet's own url is opened");
    assertEquals(reports(), [], "and nothing is reported until something happens to it");

    page.sockets[0].onopen?.();
    await page.settle();
    assertEquals(reports().map((r) => r.body), [{ status: "connected" }], "an open is reported");
    assert(page.text().includes("connected"), `and shown, got: ${page.text().slice(-200)}`);

    page.sockets[0].onopen?.();
    await page.settle();
    assertEquals(reports().length, 1, "a second open is the same state, and the same state is not news");

    page.sockets[0].onerror?.();
    await page.settle();
    assertEquals(reports().map((r) => r.body).at(-1), { status: "error" }, "a change of state is");
    page.close();
  },
});

Deno.test({
  name: "a socket report nobody may write is not a failure, and logged out is not a report",
  sanitizeOps: false,
  sanitizeResources: false,
  fn: async () => {
    // 403 is somebody watching a sheet they can only read. They may watch the
    // socket and may not write its health, which is not a failure of anything.
    const viewer = await glue("http://localhost/net-socket:feed2", {
      stored: { user: { usr_id: "u1", jwt: "a-token" } },
      docs: { feed2: { type: "net-socket", data: [{ url: "wss://feed.example/x" }] } },
      respond: (url) => url.endsWith("/socket") ? new Response("no", { status: 403 }) : { data: [] },
    });
    viewer.sockets[0].onopen?.();
    await viewer.settle();
    assert(!viewer.text().includes("Could not report"), `a 403 must pass quietly, got: ${viewer.text().slice(-200)}`);
    viewer.close();

    const anon = await glue("http://localhost/net-socket:feed2", {
      docs: { feed2: { type: "net-socket", data: [{ url: "wss://feed.example/x" }] } },
    });
    anon.sockets[0].onopen?.();
    await anon.settle();
    assertEquals(anon.asked.filter((r) => r.url.endsWith("/socket")), [], "nobody logged in reports nothing");
    assert(anon.text().includes("connected"), "but the page still says what it can see");
    anon.close();
  },
});

// Forking is what makes "start from a demo" mean anything: the source is a
// bundled example with no document of this browser's behind it.
Deno.test({
  name: "forking a bundled demo copies it into this browser, without its demo tags",
  sanitizeOps: false,
  sanitizeResources: false,
  fn: async () => {
    const page = await glue("http://localhost/query:budget-burn");
    await page.click(page.all("button").find((b) => b.textContent?.trim() === "fork"));

    assertEquals(page.created.length, 1, "a fork is a document this browser makes");
    const copy = page.created[0] as { forked_from: string; name: string; data: unknown[] };
    assertEquals(copy.forked_from, "query:budget-burn", "and it says where it came from");
    assert(copy.name.endsWith("(fork)"), `named after its source, got: ${copy.name}`);
    const entry = page.stored("library")["query:made1"] as { name: string; tags: string[] };
    assertEquals(entry.name, copy.name, "the fork is in this browser's library");
    assertEquals(
      entry.tags.filter((t) => t === "demo" || t === "example"),
      [],
      "and does not inherit the tags that would put it in the gallery pretending to be an original",
    );
    page.close();
  },
});

// Everything above stubs the repo, so every patch has only ever been applied to
// a plain object. Automerge is not a plain object: a list is a CRDT the library
// owns, and `applyPatches` splices one.
Deno.test({
  name: "a new sheet is a real automerge document, and every patch shape lands on it",
  sanitizeOps: false,
  sanitizeResources: false,
  fn: async () => {
    const page = await glue("http://localhost/", { realRepo: true });
    await page.click(page.all("tr").find((tr) => tr.getAttribute("title") === "new table:..."));
    const id = page.path();
    assert(id.startsWith("table:"), `the page should be on the new sheet, got: ${id}`);

    // "→" adds a column: a `push` on data[0], against a real automerge list. A
    // pushed column is nameless until somebody names it, and keyed by the count.
    const add = () => page.all("th").find((th) => th.getAttribute("title") === "add column");
    await page.click(add());
    await page.click(add());
    const made = await page.document(id) as { data: [Record<string, unknown>[], ...Record<string, string>[]] };
    assertEquals(
      made.data[0].map((col) => String(col.key)),
      ["a", "1", "2"],
      "a pushed column lands in the document",
    );

    // A new table has columns and no rows. "↴" pushes one: a `push` at the root
    // of `data`, which is the third patch shape and the last one untried.
    await page.click(page.all("tr").find((tr) => tr.getAttribute("title") === "add row"));
    assertEquals(
      ((await page.document(id)) as { data: unknown[] }).data.length,
      2,
      "a pushed row lands at the root of the document",
    );

    // A cell edit: a set at [row, column key].
    const cell = [...page.all("tbody tr")[3].querySelectorAll("td")][0];
    for (const type of ["mouseenter", "click", "dblclick"]) await page.fire(cell, type);
    await page.type_(page.all("#new-cell")[0], "typed");
    assertEquals(
      ((await page.document(id)) as { data: Record<string, string>[] }).data[1]["a"],
      "typed",
      "and a cell write lands under the column's key",
    );

    // A move: the one patch that takes a value out of an automerge list and puts
    // it back somewhere else. `a` is the only column with a handle to grab — a
    // pushed column is nameless, and a nameless header draws nothing.
    await page.fire(page.all("span.grab")[0], "mousedown");
    await page.fire([...page.all("tbody tr")[3].querySelectorAll("td")][2], "mouseenter");
    await page.keyUp();
    assertEquals(
      ((await page.document(id)) as { data: Record<string, unknown>[][] }).data[0].map((col) => String(col.key)),
      ["1", "2", "a"],
      "a move means the same thing to automerge as it does to a plain object",
    );

    // A row move: the same patch at the root of `data`. A second row, typed
    // into, then dragged above the first; every cell survives the trip.
    await page.click(page.all("tr").find((tr) => tr.getAttribute("title") === "add row"));
    const second = [...page.all("tbody tr")[4].querySelectorAll("td")][2];
    for (const type of ["mouseenter", "click", "dblclick"]) await page.fire(second, type);
    await page.type_(page.all("#new-cell")[0], "two");
    await page.fire(
      [...page.all("tbody tr")[4].querySelectorAll(`span.grab[title^="drag onto the row"]`)][0],
      "mousedown",
    );
    await page.fire([...page.all("tbody tr")[3].querySelectorAll("td")][0], "mouseenter");
    await page.keyUp();
    const moved = (await page.document(id)) as { data: Record<string, string>[] };
    assertEquals(moved.data.slice(1).map((row) => row.a), ["two", "typed"], "a row move reorders the document");
    assertEquals(moved.data.length, 3, "and keeps every row");
    page.close();
  },
});

// A cell edit on a synced sheet is one patch, and `applyCellPatches` in
// src/Main.elm is the path that applies just that one rather than decoding
// the document again. Nothing about the page says which path ran, so the proof
// is a decoy: the document handed over with the patches holds a different
// value, and only the fast path shows the patched one. The patches are what a
// real automerge handle emitted a moment earlier, not a literal, so the shape
// this matches is the shape automerge sends.
Deno.test({
  name: "a cell edit on a synced sheet is applied as one patch, not a re-read",
  sanitizeOps: false,
  sanitizeResources: false,
  fn: async () => {
    const page = await glue("http://localhost/", { realRepo: true });
    await page.click(page.all("tr").find((tr) => tr.getAttribute("title") === "new table:..."));
    await page.click(page.all("tr").find((tr) => tr.getAttribute("title") === "add row"));
    const sent: { patches: unknown[] }[] = [];
    const send = page.app.ports.docChanged.send;
    page.app.ports.docChanged.send = (data: unknown) => {
      sent.push((data as { data: { patches: unknown[] } }).data);
      return send(data);
    };
    const cell = [...page.all("tbody tr")[3].querySelectorAll("td")][0];
    for (const type of ["mouseenter", "click", "dblclick"]) await page.fire(cell, type);
    await page.type_(page.all("#new-cell")[0], "typed");
    page.close();
    const patches = sent.flatMap((d) => d.patches);
    assert(patches.length > 0, "a cell edit reaches docChanged with the patches automerge emitted");

    const { app, all, settle } = await boot("http://localhost/");
    const decoy = { type: "table", data: [[{ key: "a", name: "a", type: "text" }], { a: "decoy" }] };
    app.ports.docSelected.send({ id: "table:decoy", data: { doc: decoy } });
    await settle();
    app.ports.docChanged.send({ id: "table:decoy", data: { doc: decoy, handle: null, patchInfo: null, patches } });
    await settle();
    const cells = all("td").map((td) => td.textContent?.trim());
    assert(
      cells.includes("typed") && !cells.includes("decoy"),
      `the fast path must apply ${JSON.stringify(patches)} without re-reading the document, got: ${cells.join(", ")}`,
    );
  },
});

// `repo.find` hands back the same handle every time, so a change listener per
// open delivered every later change once per open. Harmless while every
// delivery was a full decode; with cell patches applied as they arrive, a
// second delivery of one splice is a doubled cell.
Deno.test({
  name: "opening a sheet again does not listen to it twice",
  sanitizeOps: false,
  sanitizeResources: false,
  fn: async () => {
    const page = await glue("http://localhost/", { realRepo: true });
    await page.click(page.all("tr").find((tr) => tr.getAttribute("title") === "new table:..."));
    const id = page.path();
    await page.click(page.all("tr").find((tr) => tr.getAttribute("title") === "add row"));
    await page.go("/");
    await page.go(`/${id}`);
    await page.go("/");
    await page.go(`/${id}`);
    let sent = 0;
    const send = page.app.ports.docChanged.send;
    page.app.ports.docChanged.send = (data: unknown) => {
      sent++;
      return send(data);
    };
    const cell = [...page.all("tbody tr")[3].querySelectorAll("td")][0];
    for (const type of ["mouseenter", "click", "dblclick"]) await page.fire(cell, type);
    await page.type_(page.all("#new-cell")[0], "once");
    assertEquals(sent, 1, "one change is delivered to the page once");
    page.close();
  },
});

// Fill-down continues the selection's seeds instead of repeating one cell: the
// leading run of filled rows is the series and the rows under it are where it
// lands. Two numbers are the whole of it -- 10, 20 means 30, 40 -- and the
// seeds themselves are never written over.
//
// Spelled the way an imported CSV spells it: numbers, and a JSON null for every
// gap. `cellText` renders a null as the word "NULL", so the seed scan read four
// filled cells and wrote "NULL" into a num column, which then failed the type
// check on every query, export and alert over the sheet.
Deno.test("filling a column down continues the series it starts with", async () => {
  const { app, all, dom, doc, fire, settle } = await boot("http://localhost/table:countries");
  const patches: { action: string; path: unknown[]; value: unknown }[] = [];
  app.ports.changeDoc.subscribe((sent: { data: typeof patches }) => patches.push(...sent.data));
  app.ports.docSelected.send({
    id: "table:series",
    data: {
      doc: {
        type: "table",
        data: [
          [{ name: "n", type: "num", key: "0" }, { name: "d", type: "date", key: "1" }],
          { "0": 10, "1": "2026-02-27" },
          { "0": 20, "1": null },
          { "0": null, "1": null },
          { "0": null, "1": null },
        ],
      },
    },
  });
  await settle();

  const seed = all("td").find((td) => td.textContent?.trim() === "10");
  assert(seed, "the sheet is drawn");
  await fire(seed, "mouseenter");
  await fire(seed, "mousedown");
  await fire(seed, "mouseup");
  const key = async (init: Record<string, unknown>) => {
    doc.body.dispatchEvent(new dom.window.KeyboardEvent("keydown", { bubbles: true, ...init }));
    await settle();
  };
  for (let i = 0; i < 3; i++) await key({ key: "ArrowDown", shiftKey: true });
  patches.length = 0;
  await key({ key: "d", ctrlKey: true });
  assertEquals(
    patches.map((p) => [p.path, p.value]),
    [[[3, "0"], 30], [[4, "0"], 40]],
    "the blank rows under the seeds carry the step on, as numbers the column's own type allows",
  );

  // A single date seed is a series on its own -- a day is the step nobody has
  // to name -- unlike a lone number, which just repeats.
  const dateSeed = all("td").find((td) => td.textContent?.trim() === "2026-02-27");
  assert(dateSeed, "the date seed is drawn");
  await fire(dateSeed, "mouseenter");
  await fire(dateSeed, "mousedown");
  await fire(dateSeed, "mouseup");
  for (let i = 0; i < 2; i++) await key({ key: "ArrowDown", shiftKey: true });
  patches.length = 0;
  await key({ key: "d", ctrlKey: true });
  assertEquals(
    patches.map((p) => [p.path, p.value]),
    [[[2, "1"], "2026-02-28"], [[3, "1"], "2026-03-01"]],
    "one date seed steps the fill on by a day",
  );

  // A table offers both export formats, downloaded off the API rather than
  // built on the page.
  const chips = all("a.chip").map((a) => a.getAttribute("href"));
  assert(chips.includes(`${API_BASE}/export/table:series.csv`), chips.join(", "));
  assert(chips.includes(`${API_BASE}/export/table:series.xlsx`), chips.join(", "));
});

// Dedupe is a whole-sheet verb -- it reads every column of every row -- so the
// palette is its home rather than a column's panel. One DocMsg like every other
// cleaning verb, which is the whole of why undo works on it.
Deno.test({
  name: "the palette deletes the rows that repeat, and undo brings them back",
  sanitizeOps: false,
  sanitizeResources: false,
  fn: async () => {
    const doc = {
      type: "table",
      data: [
        [{ name: "city", type: "text", key: "0" }, { name: "note", type: "text", key: "1" }],
        { "0": "Oslo", "1": "a" },
        { "0": "Bergen", "1": "b" },
        { "0": "Oslo", "1": "a" },
      ],
    } as { type: string; data: unknown[] };
    const was = JSON.stringify(doc.data);
    const page = await glue("http://localhost/table:dupes1", { docs: { dupes1: doc } });
    const rows = () => page.all("tbody tr").length;
    const drawn = rows();

    await page.key({ key: "k", ctrlKey: true });
    // Enter before anything is pointed at, over a document that has rows to
    // lose: the palette opened on its first row and this verb is that row.
    // `page.key` is the body, and the palette's own keys are on its input.
    const input = page.all("#palette")[0];
    assert(input, "the palette is open");
    const window_ = (input as unknown as {
      ownerDocument: { defaultView: { KeyboardEvent: new (t: string, i: unknown) => unknown } };
    })
      .ownerDocument.defaultView;
    input.dispatchEvent(new window_.KeyboardEvent("keydown", { bubbles: true, key: "Enter" }));
    await page.settle();
    assertEquals(doc.data.length, 4, "Enter on a palette nobody has typed into must not delete anything");

    const command = page.all(".scrim .panel button").find((b) => b.textContent?.startsWith("delete duplicate rows"));
    assert(command, "the palette offers the verb the shortcut sheet lists");
    await page.click(command);
    assertEquals(
      doc.data.slice(1),
      [{ "0": "Oslo", "1": "a" }, { "0": "Bergen", "1": "b" }],
      "the third row repeats the first, so the first is the one that stays",
    );
    assertEquals(rows(), drawn - 1, "and the sheet is drawn a row shorter");

    await page.key({ key: "z", ctrlKey: true });
    assertEquals(JSON.stringify(doc.data), was, "undo puts the row back where it left");
    assertEquals(rows(), drawn, "and draws it again");
    page.close();
  },
});

// The count is an arrangement, not data: it rides `arrange` the way a filter
// does -- written when the panel closes, not per keystroke -- lands on the
// column in `data[0]`, and every place a number becomes text -- the cell and the
// totals row below it -- reads the same one.
Deno.test({
  name: "a column's decimal count rides its column and every number in it is written at it",
  sanitizeOps: false,
  sanitizeResources: false,
  fn: async () => {
    const doc = { type: "table", data: [[{ name: "n", type: "num", key: "0" }], { "0": 1.5 }] };
    const page = await glue("http://localhost/table:places1", { docs: { places1: doc } });
    const column = () => (doc.data[0] as { decimals?: number; format?: string }[])[0];
    const reading = (want: string) => page.all("tbody td").filter((td) => td.textContent?.trim() === want).length;
    const plain = reading("1.5");
    assert(plain > 0, `the cell and its total should read 1.5, got: ${page.text().slice(0, 200)}`);

    await page.click(page.all("span.funnel")[0]);
    const box = () => page.all("label.decimals input")[0];
    assert(box(), "a numeric column's panel offers a decimal count");
    await page.type_(box(), "3");
    assertEquals(reading("1.500"), plain, "every number in the column is written at the count it asks for");
    assertEquals(column().decimals, undefined, "and nothing is synced while the box is still being typed in");
    await page.click(page.all("span.funnel")[0]);
    assertEquals(column().decimals, 3, "closing the panel is when the count reaches the column, the way a filter does");

    await page.click(page.all("span.funnel")[0]);
    await page.type_(box(), "");
    await page.click(page.all("span.funnel")[0]);
    assertEquals(reading("1.5"), plain, "an empty box is no count at all");
    assertEquals(column().decimals, undefined, "and takes the field back off the column rather than writing a null");

    // A format is a select, not keystrokes to debounce, so `ColumnFormat`
    // writes the column the moment it changes rather than waiting for the
    // panel to close.
    await page.click(page.all("span.funnel")[0]);
    const format = () => page.all("label.format select")[0];
    assert(format(), "a numeric column's panel offers a number format");
    await page.type_(format(), "scientific");
    assertEquals(column().format, "scientific", "the format reaches the column immediately, unlike the decimal count");
    assertEquals(reading("1.5e0"), plain, "1.5 with no decimal count reads as a mantissa of 1.5 and an exponent of 0");
    page.close();
  },
});

// --- the service worker
//
// src/sw.js is a classic script that nothing imports: the browser loads it by
// url and hands it three globals. So it is run here rather than imported, over
// a hand-made `self`, `caches` and `fetch` -- which is also the only way to take
// the network away, which is the whole of what this file is for.

const ORIGIN = "https://sheets.test";

const swSource = await Deno.readTextFile(dir + "src/sw.js");

/** src/sw.js's own pre-cache list, read out of it: the test is the browser, and
 * a browser does not get to decide what the shell is.
 */
const SHELL = [...(swSource.split("const SHELL = [")[1]?.split("]")[0] ?? "").matchAll(/"([^"]+)"/g)]
  .map((m) => m[1]);
if (SHELL.length < 2) {
  throw new Error(
    `Expected src/sw.js to hold a SHELL array of quoted paths, received ${SHELL.length} of them. ` +
      `Source: page_test.ts parses it so the fake host serves exactly what the worker asks for. ` +
      `Fix: keep SHELL a literal array in src/sw.js, or teach this parser the new shape.`,
  );
}

/** src/sw.js, running. `online` is the network: a Response for a url it answers,
 * and a network that is gone answers nothing at all. The cache is a Map keyed
 * the way a real Cache is -- by resolved url, so a plain "/" and a full href are
 * one entry.
 */
const worker = (
  online: (url: string) => Response | undefined,
  { putFails = false }: { putFails?: boolean } = {},
) => {
  const key = (request: string | { url: string }) =>
    new URL(typeof request === "string" ? request : request.url, ORIGIN).href;
  const store = new Map<string, Response>();
  const fetched = (request: string | { url: string }) => {
    const res = online(key(request));
    // What a browser with no network throws, and the one thing the worker
    // catches: anything else it lets through.
    return res ? Promise.resolve(res) : Promise.reject(new TypeError("Failed to fetch"));
  };
  const caches = {
    open: () =>
      Promise.resolve({
        // The real addAll() fetches every path, and is atomic: a non-ok status
        // on any one of them rejects the whole call and stores none of the
        // others either, so a batch write is the only write -- never a loop of
        // separate ones a later path can fail out of midway.
        addAll: async (paths: string[]) => {
          const answers = await Promise.all(paths.map(async (path) => {
            const res = await fetched(path);
            if (!res.ok) throw new TypeError(`addAll refused ${key(path)}: status ${res.status} is not ok`);
            return [key(path), res] as const;
          }));
          for (const [url, res] of answers) store.set(url, res);
        },
        match: (request: string | { url: string }) => Promise.resolve(store.get(key(request))),
        // A real Cache.put() rejects on a quota error or a response carrying
        // `Vary: *` -- neither is this worker's to fix, but the network answer
        // it was trying to save is already good and must still reach the page.
        put: (request: { url: string }, res: Response) => {
          if (putFails) return Promise.reject(new Error("QuotaExceededError"));
          store.set(key(request), res);
          return Promise.resolve();
        },
      }),
  };
  const listeners: Record<string, (event: unknown) => void> = {};
  const self = {
    addEventListener: (type: string, fn: (event: unknown) => void) => listeners[type] = fn,
    location: { origin: ORIGIN, protocol: new URL(ORIGIN).protocol },
  };
  new Function("self", "caches", "fetch", swSource)(self, caches, fetched);

  return {
    cached: () => [...store.keys()].sort(),
    /** What the cache holds for a path, which is what the next offline open
     * gets. Reading it consumes the body, the way any Response is read once. */
    held: (path: string) => store.get(key(path)),
    install: async () => {
      let held: Promise<unknown> = Promise.resolve();
      listeners.install({ waitUntil: (p: Promise<unknown>) => held = p });
      await held;
    },
    /** What the page hears back, or null for a request the worker did not
     * answer at all -- which is the browser doing what it always did. A list
     * rather than a variable so that nothing narrows the empty case away. */
    hit: async (url: string, init: Record<string, unknown> = {}) => {
      const answered: Promise<Response>[] = [];
      listeners.fetch({
        request: { url: key(url), method: "GET", ...init },
        respondWith: (p: Promise<Response>) => answered.push(p),
      });
      return answered.length ? await answered[0] : null;
    },
  };
};

/** The host src/_redirects describes: a SHELL path answered as itself, every
 * other path answered with the shell, and `build` naming which deploy it is.
 */
const served = (build: () => string) => (url: string) => {
  const path = new URL(url).pathname;
  // What the host actually answers: a module or asset for a SHELL path, the
  // shell for anything else -- and one file it happens to serve as itself.
  if (path === "/robots.txt") return new Response("User-agent: *", { headers: { "content-type": "text/plain" } });
  const shell = path === "/" || !SHELL.includes(path);
  return new Response(`${build()} ${shell ? "/" : path}`, {
    headers: { "content-type": shell ? "text/html; charset=utf-8" : "text/javascript" },
  });
};

Deno.test("the service worker pre-caches the shell, refreshes it, and leaves the API alone", async () => {
  // The deploy this browser has, and then the one after it: what the worker
  // keeps is what the last good answer said, which is why no cache name has a
  // version in it.
  let deployed = "old";
  const sw = worker(served(() => deployed));
  await sw.install();
  assertEquals(
    sw.cached(),
    SHELL.map((path) => new URL(path, ORIGIN).href).sort(),
    "install pre-caches every path the worker lists and nothing else",
  );

  deployed = "new";
  const res = await sw.hit("/index.js");
  assert(res, "a same-origin GET is the worker's to answer");
  assertEquals(await res.text(), "new /index.js", "online, the network is what answers");

  // Not handled at all: no respondWith, so the browser does what it did before
  // there was a worker. The API and the sync socket are a different origin, and
  // a cached POST is not a thing.
  assertEquals(await sw.hit(`${API_BASE}/sheet/table:x`), null, "the API is another origin and is not answered");
  assertEquals(await sw.hit("/import/csv", { method: "POST" }), null, "and a POST is not a thing to cache");

  // A blob: url the page made with URL.createObjectURL (the chart-export
  // download) reports its *origin* as us -- the spec inherits it from the
  // context that created the blob -- even though nothing here created or holds
  // it and its scheme is not one we are ever served over.
  assertEquals(
    await sw.hit(`blob:${ORIGIN}/550e8400-e29b-41d4-a716-446655440000`),
    null,
    "a blob: url is not http(s), even when URL() reports our own origin for it",
  );

  // The deep link a share hands out, and the same link with a query string on
  // it. The host answers both with the shell, so both refresh the one "/" entry
  // and neither leaves one of its own: a browser that only ever opens deep
  // links would otherwise keep the shell the install fetched forever, and open
  // offline on a stale index.html beside a fresh index.js.
  await sw.hit("/table:countries");
  await sw.hit("/?embed=1");
  // A same-origin 200 that is not the shell -- a file the host serves as
  // itself -- is answered and never written over "/": the next offline open
  // would have rendered it in the shell's place.
  const robots = await sw.hit("/robots.txt");
  assertEquals(await robots?.text(), "User-agent: *", "a file the host serves as itself is answered from the network");
  assertEquals(sw.cached().length, SHELL.length, "the cache cannot grow past the shell");
  assertEquals(
    await sw.held("/")?.text(),
    "new /",
    "and the shell it holds is the deploy this browser last saw, not the file that answered last",
  );
  assertEquals(await sw.held("/index.js")?.text(), "new /index.js", "as is every other path it holds");
});

Deno.test("with no network the shell comes out of the cache", async () => {
  let online = true;
  const fresh = served(() => "fresh");
  const sw = worker((url) => online ? fresh(url) : undefined);
  await sw.install();
  online = false;

  const held = await sw.hit("/index.js");
  assert(held, "a same-origin GET is the worker's to answer");
  assertEquals(await held.text(), "fresh /index.js", "a cached path answers with no network");
  // The deep link a share hands out. Nothing was ever cached under it -- the
  // host answers it with the shell, so the worker keeps it under "/".
  const deep = await sw.hit("/table:countries");
  assert(deep, "a deep link is the worker's to answer");
  assertEquals(await deep.text(), "fresh /", "a deep link with no network opens the shell");

  // A worker that registered but whose install never finished -- a first open
  // that lost the network partway through it -- has nothing to fall back to,
  // and says which url and what to do rather than answering with nothing.
  const bare = worker(() => undefined);
  await assertRejects(
    () => bare.hit("/index.js"),
    Error,
    "Expected https://sheets.test/index.js from the network or from the scrapsheets-shell cache, received neither",
    "a path with no network and no cached copy is refused by name, not silently",
  );
});

// The real Cache.addAll() rejects if any fetch answers with a non-2xx status,
// and stores none of them -- not even the ones that came back fine. A mid-deploy
// 404 on one file must not leave the other 15 cached under a stale worker that
// never activates: the next install (once the deploy finishes) starts from
// nothing, not from a half-written shell.
Deno.test("install refuses the whole shell if one path 404s, and keeps none of it", async () => {
  const sw = worker((url) => {
    const path = new URL(url).pathname;
    return path === "/style.css" ? new Response("not found", { status: 404 }) : new Response(`ok ${path}`);
  });
  await assertRejects(() => sw.install(), Error, undefined, "addAll rejects when one path is not ok");
  assertEquals(sw.cached(), [], "a failed install caches nothing, not even the paths that answered fine");
});

// cache.put() rejects for reasons that have nothing to do with the network
// answer being bad -- a full quota, a response carrying `Vary: *`. Caching the
// shell for later is best-effort; it must never cost the page the good answer
// the network just gave it.
Deno.test("a cache write that fails still returns the network answer it was trying to save", async () => {
  const sw = worker(served(() => "ok"), { putFails: true });
  const res = await sw.hit("/index.js");
  assert(res, "a same-origin GET is the worker's to answer even when caching it fails");
  assertEquals(await res.text(), "ok /index.js", "the network answer reaches the page unchanged");
  assertEquals(sw.cached(), [], "the failed write left nothing behind");
});
