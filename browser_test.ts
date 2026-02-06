import { assertEquals } from "@std/assert";
import { launch } from "astral";
import { serveDir } from "jsr:@std/http@^1/file-server";

// Test 1: Static analysis of index.html
Deno.test("index.html has correct WASM initialization", async () => {
  const html = await Deno.readTextFile("src/index.html");

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

// Test 2: WASM file is served correctly
Deno.test("WASM file is served with correct headers", async () => {
  const controller = new AbortController();
  const server = Deno.serve(
    { hostname: "127.0.0.1", port: 0, signal: controller.signal, onListen: () => {} },
    (req) => serveDir(req, { fsRoot: "dist", quiet: true }),
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

// Test 3: Basic page loads without fatal errors
Deno.test("page loads and Elm initializes", async () => {
  const controller = new AbortController();
  const server = Deno.serve(
    { hostname: "127.0.0.1", port: 0, signal: controller.signal, onListen: () => {} },
    (req) => serveDir(req, { fsRoot: "dist", quiet: true }),
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
  const elmMainExists = (await page.evaluate(`typeof Elm !== 'undefined' && typeof Elm.Main !== 'undefined'`)) as boolean;
  assertEquals(elmMainExists, true, "Elm.Main should exist");

  console.log("Page errors:", errors);

  await browser.close();
  controller.abort();
  await server.finished;
});
