// The glue, actually executed: what arrives from outside the page.
//
// The other half of glue_test.ts -- see that file's header for why there are
// two. This one is the edges: a CSV chosen or dropped and posted to the server,
// a net-socket sheet reporting what it saw, a bundled demo forked into this
// browser, a real automerge document taking every patch shape, the row and
// column verbs that rewrite a sheet in place, the query editor's column
// completion, and `src/sw.js` run over a hand-made `self`, `caches` and `fetch`,
// which is the only way to take the network away from a service worker.
import { assert, assertEquals, assertRejects } from "@std/assert";
import { JSDOM } from "jsdom";
import { EXAMPLES } from "./src/examples.mjs";
import { API_BASE } from "./src/page.mjs";
import { boot, dir, El, until } from "./page_harness.ts";
import { glue } from "./glue_harness.ts";

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

    // The same document read back as its history: every change, newest first,
    // and the version before the typed cell holds the row without it.
    type Versions = { versions: { hash: string; seq: number; time: number }[]; left: number };
    const loaded: { id: string; data: Versions }[] = [], shown: { id: string; data: unknown }[] = [];
    const { historyLoaded, historyShown } = page.app.ports;
    const [load, show] = [historyLoaded.send, historyShown.send];
    historyLoaded.send = (d: unknown) => (loaded.push(d as (typeof loaded)[0]), load(d));
    historyShown.send = (d: unknown) => (shown.push(d as (typeof shown)[0]), show(d));
    await page.click(page.all("button").find((b) => b.textContent === "history"));
    await until(page.settle, "the history of the open sheet", () => loaded.length > 0);
    const [{ data: { versions, left } }] = loaded;
    assertEquals(loaded[0].id, id, "the answer names the sheet it is for");
    assertEquals(
      [versions.map((v) => v.seq), left],
      [[3, 2, 1], 0],
      "made, a row pushed, a cell typed: three changes, newest first, none left out",
    );
    assert(versions.every((v) => v.time > 0), "automerge-repo stamps every change with a time");
    await page.click(page.all("#versions button")[1]);
    await until(page.settle, "the older version's rows", () => shown.length > 0);
    assertEquals(
      shown[0],
      { id, data: { hash: versions[1].hash, columns: ["a"], rows: [[undefined]] } },
      "the version before the typed cell answers its one row, blank, by position under the column names",
    );

    // `view` answers `{}` for a hash the document never had, rather than throwing.
    const errored: unknown[] = [];
    const errSend = page.app.ports.docErrored.send;
    page.app.ports.docErrored.send = (msg: unknown) => (errored.push(msg), errSend(msg));
    const bogus = "f".repeat(64);
    historyLoaded.send({ id, data: { versions: [...versions, { ...versions[0], hash: bogus, seq: 0 }], left } });
    await page.settle();
    await page.click(page.all("#versions button")[3]);
    await until(page.settle, "the refusal for a hash the document never had", () => errored.length > 0);
    const refusal =
      `[historyView] Expected a past version of ${id}, received no rows for hash ${bogus}. Source: history. Fix: reopen the sheet's history and pick another version.`;
    assertEquals([errored, shown.length], [[refusal], 1], "a hash this document never had is refused by name");
    assert(page.text().includes(refusal), "inside the modal, where the scrim does not cover it");
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

// Exact dedupe is a palette verb over every cell of every row; this one is a
// column's, because what it compares is text. The preview is the whole point:
// the rows it takes are the ones that do not look alike enough to spot, so
// nobody can check the answer by looking at the sheet first.
Deno.test({
  name: "a column's near-duplicate rows are previewed, then taken, and undo brings them back",
  sanitizeOps: false,
  sanitizeResources: false,
  fn: async () => {
    const doc = {
      type: "table",
      data: [
        [{ name: "company", type: "text", key: "0" }],
        { "0": "Acme Corp" },
        { "0": "Zebra Ltd" },
        { "0": "Acme Corp." },
        // A cell that holds something but not text. A column retyped to text
        // keeps the cells it already had, so this is a real state and not a
        // hypothetical -- and a preview that never read it must say so.
        { "0": 42 },
      ],
    } as { type: string; data: unknown[] };
    const was = JSON.stringify(doc.data);
    const page = await glue("http://localhost/table:near1", { docs: { near1: doc } });

    // The panel opens on the funnel, the way every other column verb is reached.
    const funnel = page.all("span.funnel")[0];
    assert(funnel, "the column header offers the panel");
    await page.click(funnel);
    const near = page.all(".near input")[0];
    assert(near, "the column's panel offers a closeness box");

    await page.type_(near, "70");
    assert(
      page.text().includes("row 3 matches row 1"),
      `expected the preview to name the rows that would go, received: ${page.text()}`,
    );
    assert(
      page.text().includes("1 row holds no text in this column and was not compared"),
      `expected the preview to count what it could not read, received: ${page.text()}`,
    );

    const take = page.all(".panel button").find((b) => b.textContent?.startsWith("Delete 1 near-duplicate"));
    assert(take, `expected a button carrying the count, received: ${page.text()}`);
    await page.click(take);
    assertEquals(
      doc.data.slice(1),
      [{ "0": "Acme Corp" }, { "0": "Zebra Ltd" }, { "0": 42 }],
      "the near spelling under the first goes, the first stays, and the row it could not read is left alone",
    );

    await page.key({ key: "z", ctrlKey: true });
    assertEquals(JSON.stringify(doc.data), was, "undo puts the row back where it left");
    page.close();
  },
});

// The editor completes a sheet id off the library, which needs nothing from the
// glue, and a column name off the sheet itself, which needs everything: the
// columns live behind the automerge repo in the `sheets()` closure, so the only
// way Elm can have them is a port, and the only honest answer is the `describe`
// the typist would have run themselves.
Deno.test({
  name: "the editor completes a column name off the sheet the ref names",
  sanitizeOps: false,
  sanitizeResources: false,
  fn: async () => {
    const cities = {
      type: "table",
      data: [
        [{ name: "city", type: "text", key: "0" }, { name: "founded", type: "int", key: "1" }],
        { "0": "Oslo", "1": 1040 },
      ],
    };
    const page = await glue("http://localhost/query:q1", {
      docs: {
        cities,
        q1: { type: "query", data: [{ lang: "sql", code: "", cols: {} }] },
      },
    });
    const editor = page.all("#code")[0];
    assert(editor, "the query sheet draws an editor");

    // Typed the way the page hears it: the value, then the input event its own
    // listener reads the cursor position out of.
    const typed = async (text: string) => {
      const el = editor as unknown as { value: string; selectionStart: number; dispatchEvent: (e: unknown) => void };
      el.value = text;
      el.selectionStart = text.length;
      await page.type_(editor, text);
      await page.settle();
    };

    // Before the dot the question is which sheet, which the library answers and
    // which needed no port.
    await typed("select * from @table:tria");
    assertEquals(
      page.all("#complete div").map((el) => el.textContent),
      ["@table:trials"],
      "before the dot it is still completing the sheet id",
    );

    // The dot is what turns the question into one about columns, and the answer
    // arrives on a port -- so the list has to appear on this keystroke and not
    // on the next one. `cities` is only in this browser's repo and not in the
    // bundle, which is the case the port exists for.
    await typed("select * from @table:cities.");
    assertEquals(
      page.all("#complete div").map((el) => el.textContent).sort(),
      ["@table:cities.city", "@table:cities.founded"],
      "the dot completes the columns the sheet actually has",
    );

    await typed("select * from @table:cities.fou");
    assertEquals(
      page.all("#complete div").map((el) => el.textContent),
      ["@table:cities.founded"],
      "and what is typed after the dot narrows them",
    );

    await typed("select * from @table:nobody.");
    assertEquals(page.all("#complete").length, 0, "a sheet nobody has draws no dropdown rather than an empty one");
    page.close();
  },
});

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
