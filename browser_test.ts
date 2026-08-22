// Static checks on the built dist/: that it builds at all, that index.html
// wires the WASM and the import map the way the vendored bundles need, and that
// nothing it loads reaches off our origin at runtime.
//
// The tests that drove a real Chrome are gone. What they proved about the query
// engine now lives in examples_test.ts, which runs the page's own vendored
// alasql in-process against every bundled sheet; what they proved about pure
// view logic lives in tests/MainTest.elm. What nothing covers any more is the
// boot itself -- Elm initializing, the automerge WASM resolving, a cell being
// typed into -- and that is a real hole, not an oversight.
import { assertEquals } from "@std/assert";
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
Deno.test("the built bundles still export what the page boots from", async () => {
  // Not a substitute for booting the page, but it is the half of a boot failure
  // that is cheap to catch: a bundle that compiled to nothing, or a vendored
  // automerge rebuilt without the WASM entry point index.html calls.
  const elm = await Deno.readTextFile(dir + "dist/index.js");
  assertEquals(elm.includes("Elm.Main"), true, "dist/index.js should define Elm.Main");
  assertEquals(elm.includes("scrapsheets"), true, "dist/index.js should be this app, not an empty shell");

  const automerge = await Deno.readTextFile(dir + "dist/automerge.mjs");
  assertEquals(
    automerge.includes("initializeWasm"),
    true,
    "the vendored automerge must export initializeWasm; src/index.html calls it with fetch('/automerge.wasm')",
  );

  // Automerge stays external in the repo bundles so all of them share the one
  // copy index.html initializes. Inlining it into each would give the browser
  // three, only one of which has WASM -- and the tell is this symbol appearing
  // anywhere but automerge.mjs itself.
  for (const name of ["automerge-repo.mjs", "automerge-repo-ws.mjs", "automerge-repo-idb.mjs"]) {
    const bundle = await Deno.readTextFile(dir + "dist/" + name);
    assertEquals(
      bundle.includes("initializeWasm"),
      false,
      `${name} has automerge inlined into it; it must import the shared copy instead`,
    );
  }
});

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
