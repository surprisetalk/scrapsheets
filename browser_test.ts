import { assertEquals } from "@std/assert";
import { launch } from "astral";
import { serveDir } from "@std/http/file-server";

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
Deno.test("vendored automerge bundles keep bare automerge specifiers", async () => {
  for (const name of ["automerge-repo.mjs", "automerge-repo-ws.mjs", "automerge-repo-idb.mjs"]) {
    const src = await Deno.readTextFile(dir + "src/" + name);
    const bad = src.match(/"(\/npm\/[^"]*|[^"]*@automerge\/[^"]*@[\d.][^"]*)"/g);
    assertEquals(
      bad,
      null,
      `src/${name} must import automerge as a bare specifier so the import map picks the WASM-initialized copy. ` +
        `Run \`deno task vendor\` to regenerate. Offending imports: ${bad?.join(", ")}`,
    );
  }
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

  const browser = await launch({ args: ["--disable-web-security", "--incognito"] });
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

// Test 3: Basic page loads without fatal errors
Deno.test("page loads, Elm initializes, and automerge boots", async () => {
  const controller = new AbortController();
  const server = Deno.serve(
    { hostname: "127.0.0.1", port: 0, signal: controller.signal, onListen: () => {} },
    (req) => serveDir(req, { fsRoot: dir + "dist", quiet: true }),
  );
  const port = server.addr.port;

  // Use a fresh browser with cache disabled
  const browser = await launch({ args: ["--disable-web-security", "--incognito"] });
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
