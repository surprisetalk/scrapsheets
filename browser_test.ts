import { assertEquals } from "@std/assert";
import { launch } from "astral";
import { serveDir } from "@std/http/file-server";

// The CI container has no usable Chrome sandbox (zygote_host_impl_linux.cc: "No
// usable sandbox!"), and every page here is our own localhost build, so --no-sandbox
// below costs nothing and is what lets these tests run outside a dev machine.

const dir = new URL(".", import.meta.url).pathname;

Deno.test("build dist", async () => {
  const { code, stdout, stderr } = await new Deno.Command(Deno.execPath(), {
    args: ["task", "build"],
    cwd: dir,
    stdout: "piped",
    stderr: "piped",
  }).output();
  if (code !== 0) {
    throw new Error(
      `deno task build failed:\n${new TextDecoder().decode(stdout)}\n${new TextDecoder().decode(stderr)}`,
    );
  }
});

// Test 1: Static analysis of index.html
Deno.test("index.html has correct WASM initialization", async () => {
  const html = await Deno.readTextFile(dir + "src/index.html");

  // Check for import map with slim redirects
  assertEquals(
    html.includes('"@automerge/automerge/slim"'),
    true,
    "Should have import map entry for @automerge/automerge/slim",
  );

  // Check for initializeWasm call
  assertEquals(
    html.includes("initializeWasm"),
    true,
    "Should call initializeWasm",
  );

  // Check for local WASM fetch
  assertEquals(
    html.includes('fetch("/automerge.wasm")'),
    true,
    "Should fetch local automerge.wasm",
  );

  // Check all required imports are present
  assertEquals(html.includes("Repo"), true, "Should import Repo");
  assertEquals(html.includes("IndexedDBStorageAdapter"), true, "Should import IndexedDBStorageAdapter");
  assertEquals(html.includes("BrowserWebSocketClientAdapter"), true, "Should import BrowserWebSocketClientAdapter");

  // Check for Repo instantiation
  assertEquals(html.includes("new Repo"), true, "Should create a Repo instance");
});

// Test 1b: vendored bundles must defer to the import map for automerge
// src/_redirects is an allowlist ending in `/* / 200`, so an asset that is not
// listed is served the SPA shell instead of itself. For a module that surfaces
// only in the browser, as "'text/html' is not a valid JavaScript MIME type".
Deno.test("every root-absolute asset index.html loads is listed in _redirects", async () => {
  const html = await Deno.readTextFile(dir + "src/index.html");
  const redirects = await Deno.readTextFile(dir + "src/_redirects");
  const listed = new Set(
    redirects.split("\n")
      .map((line) => line.trim())
      .filter((line) => line && !line.startsWith("#"))
      .map((line) => line.split(/\s+/)[0]),
  );

  const referenced = new Set(
    [...html.matchAll(/(?:from|src=|href=)\s*["'](\/[^"']+)["']/g)].map((m) => m[1]),
  );

  for (const path of referenced) {
    assertEquals(
      listed.has(path),
      true,
      `src/index.html loads ${path}, but src/_redirects does not list it, so the host will ` +
        `serve index.html for that URL instead of the file. Add a line: ${path} ${path} 200`,
    );
  }
});

// The page must load nothing from a third party: a CDN outage cannot break the
// app, and a root-relative URL inside a vendored bundle cannot collide with our
// /* catch-all. `deno task vendor` rebuilds these self-contained.
Deno.test("nothing the page loads reaches a CDN at runtime", async () => {
  const vendored = [
    "automerge.mjs",
    "automerge-repo.mjs",
    "automerge-repo-ws.mjs",
    "automerge-repo-idb.mjs",
    "alasql.mjs",
    "sql.mjs",
    "examples.mjs",
  ];
  for (const name of vendored) {
    const src = await Deno.readTextFile(dir + "src/" + name);

    // import/export ... from "https://..." — the runtime dependency we removed.
    const remote = src.match(/(?:from|import)\s*\(?\s*["']https?:\/\/[^"']+["']/g);
    assertEquals(
      remote,
      null,
      `src/${name} fetches from a third party at runtime. Run \`deno task vendor\` to inline it. ` +
        `Offending: ${remote?.join(", ")}`,
    );

    // Root-relative CDN paths resolve against our origin, where /* answers with
    // index.html: "'text/html' is not a valid JavaScript MIME type", or for a
    // source map, "JSON Parse error: Unrecognized token '<'".
    const rooted = src.match(/(?:sourceMappingURL=|from\s*["'])\/(?:npm|sm|node)\//g);
    assertEquals(
      rooted,
      null,
      `src/${name} keeps root-relative CDN paths, which our /* catch-all answers with index.html. ` +
        `Run \`deno task vendor\`. Offending: ${rooted?.join(", ")}`,
    );
  }

  // The repo bundles must defer to the import map for automerge, or the browser
  // ends up with a second copy that never had initializeWasm() called on it.
  const html = await Deno.readTextFile(dir + "src/index.html");
  assertEquals(
    /"@automerge\/automerge":\s*"\/automerge\.mjs"/.test(html),
    true,
    "the import map must point @automerge/automerge at the vendored /automerge.mjs",
  );
  assertEquals(
    html.includes("cdn.jsdelivr.net") || html.includes("esm.sh"),
    false,
    "src/index.html must not reference a CDN",
  );
});

// Test 2: WASM file is served correctly
Deno.test("WASM file is served with correct headers", async () => {
  const controller = new AbortController();
  const server = Deno.serve(
    { hostname: "127.0.0.1", port: 0, signal: controller.signal, onListen: () => {} },
    (req) => serveDir(req, { fsRoot: dir + "dist", quiet: true }),
  );
  const port = server.addr.port;

  const wasmRes = await fetch(`http://127.0.0.1:${port}/automerge.wasm`);
  assertEquals(wasmRes.ok, true, "WASM file should be served");

  // Verify it's valid WASM (magic bytes: 0x00 0x61 0x73 0x6d)
  const bytes = new Uint8Array(await wasmRes.arrayBuffer());
  assertEquals(bytes[0], 0x00, "WASM magic byte 0");
  assertEquals(bytes[1], 0x61, "WASM magic byte 1 ('a')");
  assertEquals(bytes[2], 0x73, "WASM magic byte 2 ('s')");
  assertEquals(bytes[3], 0x6d, "WASM magic byte 3 ('m')");

  controller.abort();
  await server.finished;
});

// Test 2b: a 4xx from an origin must surface that origin's own words, not "HTTP 400".
// Regression: `from http('https://export.arxiv.org/api/query', @{search_query:'all:'})`
// used to report only `Failed to fetch data: HTTP 400`.
Deno.test("a failing HTTP() names the url and the origin's error text", async () => {
  const controller = new AbortController();
  const arxivError = `<?xml version='1.0' encoding='UTF-8'?>
<feed xmlns="http://www.w3.org/2005/Atom">
  <entry>
    <title>Error</title>
    <summary>Either a search_query or id_list must be specified for the classic API.</summary>
  </entry>
</feed>`;
  const server = Deno.serve(
    { hostname: "127.0.0.1", port: 0, signal: controller.signal, onListen: () => {} },
    (req) =>
      new URL(req.url).pathname === "/api/query"
        ? new Response(arxivError, {
          status: 400,
          headers: { "Content-Type": "application/atom+xml; charset=utf-8" },
        })
        : serveDir(req, { fsRoot: dir + "dist", quiet: true }),
  );
  const port = server.addr.port;

  const browser = await launch({ args: ["--disable-web-security", "--incognito", "--no-sandbox"] });
  const page = await browser.newPage();
  await page.goto(`http://127.0.0.1:${port}/`);
  for (let i = 0; i < 100; i++) {
    if (await page.evaluate(`typeof globalThis.alasql?.from?.HTTP === 'function'`)) break;
    await new Promise((r) => setTimeout(r, 100));
  }

  const message = await page.evaluate(`(() => {
    try {
      alasql.from.HTTP("http://127.0.0.1:${port}/api/query", { search_query: "all:", max_results: 25 });
      return "(HTTP() returned without throwing)";
    } catch (err) { return err.message; }
  })()`) as string;

  for (
    const want of [
      "400",
      "search_query=all%3A",
      "Either a search_query or id_list must be specified",
    ]
  ) {
    assertEquals(
      message.includes(want),
      true,
      `error should mention ${JSON.stringify(want)}, got:\n${message}`,
    );
  }

  await browser.close();
  controller.abort();
  await server.finished;
});

// Mirrors src/_redirects: extension-less paths (like /table:countries) load the app shell.
// Applies dist/_redirects the way the host does: first matching rule wins, and
// `/* / 200` serves the SPA shell for everything unlisted. Guessing from the
// file extension instead would be MORE permissive than production and would hide
// an asset missing from the allowlist behind a "'text/html' is not a valid
// JavaScript MIME type" error in the browser only.
let rules: string[][] | undefined;
const serveSpa = async (req: Request): Promise<Response> => {
  rules ??= (await Deno.readTextFile(dir + "dist/_redirects"))
    .split("\n")
    .map((line) => line.trim())
    .filter((line) => line && !line.startsWith("#"))
    .map((line) => line.split(/\s+/));

  const { pathname } = new URL(req.url);
  const rule = rules.find(([from]) =>
    from === pathname || (from.endsWith("/*") && pathname.startsWith(from.slice(0, -1)))
  );
  const target = rule ? rule[1] : pathname;
  return serveDir(new Request(new URL(target === "/" ? "/index.html" : target, req.url), req), {
    fsRoot: dir + "dist",
    quiet: true,
  });
};

// Test 2c: bundled example datasets render, @refs join across sheets in-browser,
// and the library chrome (tutorial, new-sheet menu, shortcut sheet) is present.
Deno.test("bundled examples render and cross-sheet queries join", async () => {
  const controller = new AbortController();
  const server = Deno.serve(
    { hostname: "127.0.0.1", port: 0, signal: controller.signal, onListen: () => {} },
    serveSpa,
  );
  const port = server.addr.port;

  const browser = await launch({ args: ["--incognito", "--no-sandbox"] });
  const page = await browser.newPage();

  const waitForText = async (wants: string[], context: string) => {
    let text = "";
    for (let i = 0; i < 100; i++) {
      text = (await page.evaluate(`document.body.innerText`)) as string;
      if (wants.some((want) => text.includes(want))) return;
      await new Promise((r) => setTimeout(r, 100));
    }
    throw new Error(`${context}: expected one of ${JSON.stringify(wants)} in page text, got:\n${text.slice(0, 2000)}`);
  };

  // A bundled dataset renders straight from the library entry (no repo.find).
  await page.goto(`http://127.0.0.1:${port}/table:countries`);
  await waitForText(["China"], "/table:countries");

  // SHEET() resolves system docs synchronously.
  for (let i = 0; i < 100; i++) {
    if (await page.evaluate(`typeof globalThis.alasql?.from?.SHEET === 'function'`)) break;
    await new Promise((r) => setTimeout(r, 100));
  }
  const count = await page.evaluate(
    `alasql("select count(*) as n from SHEET('table:countries')").data[0].n`,
  ) as number;
  assertEquals(count >= 195, true, `expected >=195 countries, got ${count}`);

  // src/sql.mjs registers on the page's alasql too, so the functions the server
  // tests cover are the same ones available here. Same module, one result.
  const udfs = await page.evaluate(
    `JSON.stringify(alasql("select median(x) m, mode(x) mo, levenshtein('kitten','sitting') l, ` +
      `date_trunc('month','2026-08-16T12:00:00Z') d, percentile(array(x), 0.5) p, ` +
      `fiscal_year('2026-10-01',10) fy, fiscal_period('2026-10-01',10) fp ` +
      `from (select 1 as x union all select 3 as x union all select 3 as x)").data[0])`,
  ) as string;
  assertEquals(
    JSON.parse(udfs),
    { m: 3, mo: 3, l: 3, d: "2026-08-01T00:00:00.000Z", p: 3, fy: 2027, fp: 1 },
    "src/sql.mjs UDFs should be registered in the browser engine",
  );

  // Totals sum the rows on screen; the first column stays put when scrolled.
  const totals = await page.evaluate(`document.querySelector("tr.totals")?.innerText ?? ""`) as string;
  assertEquals(
    totals.includes("8017560800"),
    true,
    `expected the population column to total the world population, got: ${totals}`,
  );
  assertEquals(
    await page.evaluate(`getComputedStyle(document.querySelector("td.c0")).position`),
    "sticky",
    "the first column should be frozen",
  );
  assertEquals(
    await page.evaluate(`document.querySelectorAll(".grip").length > 0`),
    true,
    "every column header should carry a resize grip",
  );

  // Multi-column sort, column hide, and fill-down, driven through the real DOM.
  {
    const headerText = () =>
      page.evaluate(`[...document.querySelectorAll("span.sort")].map((s) => s.innerText).join("|")`) as Promise<string>;
    const firstCell = () => page.evaluate(`document.querySelector("tbody tr td")?.innerText ?? ""`) as Promise<string>;
    const clickSort = (n: number, shift: boolean) =>
      page.evaluate(
        `document.querySelectorAll("span.sort")[${n}].dispatchEvent(new MouseEvent("click", { shiftKey: ${shift}, bubbles: true }))`,
      );

    // Elm repaints on the next frame, so every assertion polls rather than reads once.
    const waitForHeader = async (want: (h: string) => boolean, context: string) => {
      let h = "";
      for (let i = 0; i < 100; i++) {
        h = await headerText();
        if (want(h)) return;
        await new Promise((r) => setTimeout(r, 100));
      }
      throw new Error(`${context}: header was ${h}`);
    };

    await clickSort(0, false);
    await waitForText(["Afghanistan"], "ascending sort by the first column");
    await waitForHeader((h) => h.includes("▲"), "expected an ascending arrow");

    // A second key gets a rank digit; a single key stays a bare arrow.
    await clickSort(1, true);
    await waitForHeader((h) => h.includes("▲1") && h.includes("▲2"), "expected ranked arrows");

    // Shift-clicking the primary key again flips it without losing its rank.
    await clickSort(0, true);
    await waitForHeader((h) => h.includes("▼1") && h.includes("▲2"), "expected the primary key to hold rank 1");
    await waitForText(["Zimbabwe"], "descending primary key");

    // Hiding the first column stops it rendering and reports itself in the filter bar.
    const waitForFirstCell = async (want: (c: string) => boolean, context: string) => {
      let c = "";
      for (let i = 0; i < 100; i++) {
        c = await firstCell();
        if (want(c)) return;
        await new Promise((r) => setTimeout(r, 100));
      }
      throw new Error(`${context}: first cell was ${JSON.stringify(c)}`);
    };

    const before = await firstCell();
    await page.evaluate(`document.querySelectorAll("span.funnel")[0].click()`);
    await waitForText(["Hide column"], "hide control in the filter popover");
    await page.evaluate(
      `[...document.querySelectorAll("button")].find((b) => b.innerText === "Hide column").click()`,
    );
    await waitForText(["1 columns hidden"], "filter bar after hiding a column");
    await waitForFirstCell((c) => c !== before, "the hidden column should stop rendering");

    await page.evaluate(
      `[...document.querySelectorAll("button")].find((b) => b.innerText === "Show all columns").click()`,
    );
    await waitForFirstCell((c) => c === before, "show all should bring the column back");
  }

  // The flagship join: @table:events x @table:us-states resolves and renders.
  await page.goto(`http://127.0.0.1:${port}/query:events-by-state`);
  await waitForText(
    ["Texas", "California", "Nevada", "Illinois", "Kentucky", "Iowa", "Florida", "New Mexico"],
    "/query:events-by-state",
  );

  // A query built on another query (@query recursion).
  await page.goto(`http://127.0.0.1:${port}/query:festival-season`);
  await waitForText(["2026-06", "2026-07", "2026-08"], "/query:festival-season");

  // describe never reaches the engine: it reports the shape of the sheet it names.
  await page.goto(`http://127.0.0.1:${port}/query:describe-currencies`);
  await waitForText(["minor"], "/query:describe-currencies");
  const described = await page.evaluate(`document.body.innerText`) as string;
  for (const want of ["column", "nulls", "sample", "code", "symbol", "USD"])
    assertEquals(described.includes(want), true, `describe should report ${want}, got:\n${described.slice(0, 1500)}`);

  // A reference dataset joins to a bundled one through the page engine.
  await page.goto(`http://127.0.0.1:${port}/query:moved-holidays`);
  await waitForText(["Independence Day"], "/query:moved-holidays");

  // A reference table joins to the country spine, and haversine_km runs in the
  // page engine as well as the server one.
  await page.goto(`http://127.0.0.1:${port}/query:port-airport-pairs`);
  await waitForText(["HND"], "/query:port-airport-pairs");
  const km = await page.evaluate(
    `alasql("select round(haversine_km(51.47, -0.45, 40.64, -73.78)) as km").data[0].km`,
  ) as number;
  // LHR to JFK, from the two-decimal coordinates this dataset ships. The exact
  // airport reference points give 5555 km; the rounding costs about 14.
  assertEquals(km, 5541, "haversine_km should agree with the server engine");

  // min_text()/max_text() run in the page engine too, over a reference table that
  // has only text to compare.
  await page.goto(`http://127.0.0.1:${port}/query:ambiguous-nicknames`);
  await waitForText(["Alexandra"], "/query:ambiguous-nicknames");
  const both = await page.evaluate(`document.body.innerText`) as string;
  for (const want of ["Alex", "Chris", "Harry", "Pat"])
    assertEquals(both.includes(want), true, `expected ${want} among the ambiguous nicknames`);

  // Canary on the upstream bug min_text()/max_text() exist to work around: raw
  // alasql drops a text min() rather than erroring, so checkResultColumns has to
  // catch it. If this ever throws instead, the workaround can go.
  const droppedMin = await page.evaluate(
    `JSON.stringify(alasql("select min(c) as m from ?", [[{c:"b"},{c:"a"}]]).data)`,
  ) as string;
  assertEquals(droppedMin, "[{}]", "alasql still drops a text min() silently");

  // Window functions run in the page engine through the same pass the server
  // uses: AlaSQL never sees the over(...) clause in either place. A moving
  // average, a rank inside the week, and a running total all appear here.
  await page.goto(`http://127.0.0.1:${port}/query:store-margin`);
  await waitForText(["Riverside", "Uptown", "Airport", "sales_ma4", "rank_in_week"], "/query:store-margin");

  // A window over a @query chain: pair-zscore reads pair-spread, which is where
  // the rolling band is computed. The entry signal is the point of the demo.
  await page.goto(`http://127.0.0.1:${port}/query:pair-zscore`);
  await waitForText(["long the ratio", "2026-07-16"], "/query:pair-zscore");

  // A demo pipeline whose second sheet reads the first sheet's window columns.
  await page.goto(`http://127.0.0.1:${port}/query:exit-waterfall`);
  await waitForText(["Anders Seed Fund", "Employee option pool"], "/query:exit-waterfall");

  // Library chrome: tutorial checklist, net-* creation rows, shortcut sheet.
  await page.goto(`http://127.0.0.1:${port}/`);
  await waitForText(["get started"], "tutorial card");
  await waitForText(["create a table"], "tutorial step 0");
  await waitForText(["net-hook:..."], "net creation rows");
  await page.evaluate(
    `document.body.dispatchEvent(new KeyboardEvent("keydown", { key: "/", ctrlKey: true, bubbles: true }))`,
  );
  await waitForText(["Keyboard shortcuts"], "shortcut sheet after Ctrl+/");
  await page.evaluate(
    `document.body.dispatchEvent(new KeyboardEvent("keydown", { key: "Escape", bubbles: true }))`,
  );
  for (let i = 0; i < 100; i++) {
    const text = (await page.evaluate(`document.body.innerText`)) as string;
    if (!text.includes("Keyboard shortcuts")) break;
    if (i === 99) throw new Error("Escape should close the shortcut sheet");
    await new Promise((r) => setTimeout(r, 100));
  }

  await browser.close();
  controller.abort();
  await server.finished;
});

// Test 3: Basic page loads without fatal errors
Deno.test("page loads, Elm initializes, and automerge boots", async () => {
  const controller = new AbortController();
  const server = Deno.serve(
    { hostname: "127.0.0.1", port: 0, signal: controller.signal, onListen: () => {} },
    (req) => serveDir(req, { fsRoot: dir + "dist", quiet: true }),
  );
  const port = server.addr.port;

  // Use a fresh browser with cache disabled
  const browser = await launch({ args: ["--disable-web-security", "--incognito", "--no-sandbox"] });
  const page = await browser.newPage();

  const errors: string[] = [];
  page.addEventListener("pageerror", (e) => {
    errors.push(e.detail.message);
  });

  await page.goto(`http://127.0.0.1:${port}/`);

  // Wait briefly for initial page load
  await new Promise((r) => setTimeout(r, 3000));

  // Check if Elm global exists (proves Elm JS loaded)
  const elmExists = (await page.evaluate(`typeof Elm !== 'undefined'`)) as boolean;
  assertEquals(elmExists, true, "Elm global should exist");

  // Check if Elm.Main exists
  const elmMainExists =
    (await page.evaluate(`typeof Elm !== 'undefined' && typeof Elm.Main !== 'undefined'`)) as boolean;
  assertEquals(elmMainExists, true, "Elm.Main should exist");

  // window.__scrapsheets is only assigned after the module script imports
  // @automerge/automerge, initializes the WASM, and constructs a Repo, so it is
  // proof the whole vendored automerge graph in src/automerge* resolved.
  const repoBooted = (await page.evaluate(
    `typeof window.__scrapsheets?.repo?.find === 'function'`,
  )) as boolean;
  assertEquals(
    repoBooted,
    true,
    `automerge Repo should be constructed on window.__scrapsheets.repo. Page errors: ${
      errors.length ? errors.join(" | ") : "(none)"
    }`,
  );

  await browser.close();
  controller.abort();
  await server.finished;
});
