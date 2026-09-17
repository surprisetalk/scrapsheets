// The glue, actually executed: what it does to a document.
//
// Split out of page_test.ts so the two harnesses run side by side, and split
// again into this file and sync_test.ts for the same reason: `deno test
// --parallel` runs files, not tests, in parallel, so a single file holding every
// `glue()` was the critical path of the suite on its own. `glue_harness.ts` is
// what the two share.
//
// This half is the document: the arrangement held for a sheet this browser
// cannot write, the cleaning and splitting verbs landing as patches, a cell
// edit, a sort that must not re-run the SQL, and the share and publish
// requests. sync_test.ts is the other half -- what arrives from outside.
//
// The two are kept even in `glue()` calls, because one of those is a jsdom, an
// evaluation of src/index.html's module script and Elm's first paint, and that
// is what either file costs. Counted, not eyeballed:
// `grep -c "await glue(" glue_test.ts sync_test.ts`.
import { assert, assertEquals, assertRejects } from "@std/assert";
import { decodeHeads, encodeHeads } from "@automerge/automerge-repo";
import { decodeSyncMessage } from "@automerge/automerge";
import { EXAMPLES } from "./src/examples.mjs";
import { API_BASE, sheets } from "./src/page.mjs";
import alasql from "./src/alasql.mjs";
import { boot, El, until } from "./page_harness.ts";
import { glue } from "./glue_harness.ts";

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
