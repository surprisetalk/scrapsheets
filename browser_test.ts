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
import { assert, assertEquals } from "@std/assert";
import { serveDir } from "@std/http/file-server";
import { CANONICAL_TYPES, CHART_KINDS, COLUMN_TYPES } from "./src/sql.mjs";

const dir = new URL(".", import.meta.url).pathname;

/** The manifest a developer edits, refused by name rather than by whatever
 * JSON.parse says about a position. Read here and not from dist/, which
 * `deno task build` only ever copies into: two tests want it, and a message
 * saying which file to open is the whole point of having one reader.
 */
const manifest = async () => {
  const text = await Deno.readTextFile(dir + "src/manifest.webmanifest").catch((err) => {
    throw new Error(
      `Expected src/manifest.webmanifest, received ${(err as Error).message}. ` +
        `Source: src/manifest.webmanifest is what makes the app installable, and the build only ever ` +
        `copies it. Fix: restore it; a stale dist/ copy is not the file the next build ships.`,
    );
  });
  try {
    return JSON.parse(text) as { icons?: { src: string }[]; [field: string]: unknown };
  } catch (err) {
    throw new Error(
      `Expected src/manifest.webmanifest to parse as JSON, received ${JSON.stringify(text.slice(0, 80))}. ` +
        `Source: src/manifest.webmanifest. Fix: ${(err as Error).message}`,
    );
  }
};

// `deno task test` builds once before any file runs, so this file only refuses
// a dist/ older than its source: the files run in parallel, and a compiler
// racing a reader of its output is a flaky suite.
Deno.test("dist is at least as new as src, and holds nothing src has lost", async () => {
  const mtime = async (p: string) => (await Deno.stat(dir + p).catch(() => null))?.mtime?.getTime() ?? 0;
  const sources = new Set<string>();
  for await (const { name } of Deno.readDir(dir + "src")) {
    sources.add(name);
    const stale = name === "Main.elm" ? "dist/index.js" : `dist/${name}`;
    assert(
      (await mtime(stale)) >= (await mtime(`src/${name}`)),
      `Expected ${stale} at least as new as src/${name}, received an older or missing one. Source: dist/ is built, not checked in. Fix: run deno task build`,
    );
  }
  // The build is `cp src/* dist`, which copies and never deletes, so a file
  // deleted from src lives on in dist and every guard that reads dist stays
  // green over a copy no fresh build would produce. The loop above cannot see
  // it: it walks src, and the file is exactly the one that left.
  for await (const { name } of Deno.readDir(dir + "dist")) {
    // elm make's output. Its source is src/Main.elm, checked by the loop above.
    if (name === "index.js") continue;
    assert(
      sources.has(name),
      `Expected src/${name}, the source dist/${name} was copied from, received no such file. ` +
        `Source: deno task build copies src/* to dist and never deletes. ` +
        `Fix: rm dist/${name} if src/${name} was meant to go, or restore src/${name}.`,
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

Deno.test("index.html imports names its modules actually export", async () => {
  // src/index.html is the one file with no runtime coverage: nothing boots it,
  // so a name that was never exported in the first place fails as a blank page
  // rather than as a test. The other direction -- a name used here and never
  // imported -- is the scope analysis in the next test, which catches it as a
  // free identifier whether it is called or merely read.
  const html = await Deno.readTextFile(dir + "src/index.html");
  const imports = [...html.matchAll(/import\s*\{([^}]+)\}\s*from\s*"(\/[^"]+\.mjs)"/g)];
  assert(imports.length >= 2, `expected index.html to import from local modules, found ${imports.length}`);

  for (const [, names, spec] of imports) {
    const mod = await import("./src" + spec);
    for (const name of names.split(",").map((n) => n.trim()).filter(Boolean)) {
      assert(
        name in mod,
        `index.html imports { ${name} } from "${spec}", which does not export it`,
      );
    }
  }
});

// The names the page reaches for that Deno's own global scope does not have.
// Everything else it touches -- fetch, localStorage, WebSocket, console,
// setTimeout, URLSearchParams -- Deno already defines, so no-undef never raises
// it. `Elm` comes from the classic <script src="/index.js"> in <head>.
const BROWSER_GLOBALS = [
  "document",
  "Elm",
  "Image",
  "MutationObserver",
  "requestAnimationFrame",
  "XMLHttpRequest",
  "XMLSerializer",
];

Deno.test("index.html has no undefined identifier and no dead import", async () => {
  // The regex half of this used to match call shapes only, so a value use like
  // PARSERS[ct] was invisible and a `const x` anywhere whitelisted `x`
  // everywhere. deno lint does the real scope analysis -- it resolves module
  // bindings, block scope and hoisting -- and reads from stdin, so index.html
  // needs neither a build step nor to stop being one file to get it.
  const html = await Deno.readTextFile(dir + "src/index.html");
  const open = html.indexOf('<script type="module">');
  assert(open >= 0, 'expected one <script type="module"> in src/index.html, found none');
  const from = html.indexOf(">", open) + 1;
  const script = html.slice(from, html.indexOf("</script>", open));
  // Line numbers come back relative to the slice; this puts them back on the file.
  const offset = html.slice(0, from).split("\n").length - 1;

  const lint = new Deno.Command(Deno.execPath(), {
    // --no-config or the project deno.json applies, which excludes this very
    // file and would restore the default rule tags. An empty --rules-tags
    // clears those, leaving only the two rules named here.
    args: [
      "lint",
      "--json",
      "-",
      "--ext=js",
      "--no-config",
      "--rules-tags=",
      "--rules-include=no-undef,no-unused-vars",
    ],
    cwd: dir,
    stdin: "piped",
    stdout: "piped",
    stderr: "piped",
  }).spawn();
  const w = lint.stdin.getWriter();
  await w.write(new TextEncoder().encode(script));
  await w.close();
  const { stdout, stderr } = await lint.output();
  const out = new TextDecoder().decode(stdout);
  let report;
  try {
    report = JSON.parse(out);
  } catch {
    throw new Error(
      `deno lint did not return JSON for index.html's module script.\n` +
        `stdout: ${out.slice(0, 400)}\nstderr: ${new TextDecoder().decode(stderr).slice(0, 400)}`,
    );
  }
  assertEquals(report.errors, [], "deno lint could not parse index.html's module script");

  // One entry per name, not per call site: `document` alone is 14 diagnostics.
  const found = new Map<string, number>();
  for (const d of report.diagnostics) {
    // no-unused-vars backticks the name, no-undef does not.
    const name = d.message.replace(/ is never used$| is not defined$/, "").replace(/`/g, "");
    if (BROWSER_GLOBALS.includes(name) && d.code === "no-undef") continue;
    if (!found.has(name)) found.set(name, d.range.start.line + offset);
  }
  if (found.size > 0) {
    const lines = [...found].map(([name, line]) => `    ${name} (src/index.html:${line})`).join("\n");
    throw new Error(
      `index.html's module script has bindings that do not resolve.\n` +
        `    expected  every identifier bound by an import, a declaration, or a known browser global\n` +
        `    received  ${found.size} that are not:\n${lines}\n` +
        `    source    deno lint no-undef,no-unused-vars over the <script type="module"> body\n` +
        `    fix       import the name, delete it if it is dead, or -- if it is a browser API the page\n` +
        `              is meant to reach for -- add it to BROWSER_GLOBALS in browser_test.ts`,
    );
  }
});

// src/_redirects is an allowlist ending in `/* / 200`, so an asset with no rule
// above the catch-all is served the SPA shell instead of itself. For a module
// that surfaces only in the browser, as "'text/html' is not a valid JavaScript
// MIME type" -- and index.html's script is a module, so that one refusal takes
// Elm's init down with it and the app never comes up at all.
//
// The walk is the whole module graph, not index.html's own imports: /portals.mjs
// shipped unlisted for fourteen commits because src/page.mjs is what imports it,
// and production answered it with the shell. The manifest's icons ride along for
// the same reason -- nothing but the manifest names them.
const MAX_MODULES = 100;
Deno.test("every asset the page loads is served by _redirects, above the catch-all", async () => {
  const rules = (await Deno.readTextFile(dir + "src/_redirects")).split("\n")
    .map((line) => line.trim())
    .filter((line) => line && !line.startsWith("#"))
    .map((line) => line.split(/\s+/));
  const catchAll = rules.findIndex(([from]) => from === "/*");
  assert(
    catchAll >= 0,
    `Expected src/_redirects to carry a /* catch-all, received ${rules.length} rules without one. ` +
      `Source: src/_redirects. Fix: add a line: /* / 200`,
  );

  // The host applies the first rule that matches, so a rule below the catch-all
  // never runs, and one pointing anywhere but at itself serves the wrong bytes.
  // Reading only a rule's first token saw neither: both mutations left the
  // suite green while the asset came back as index.html.
  for (const [i, [from, to]] of rules.entries()) {
    // The two that answer with the shell on purpose, and the catch-all itself.
    if (from === "/" || from === "/index.html" || from === "/*") continue;
    assertEquals(
      to,
      from,
      `Expected src/_redirects to answer ${from} with ${from}, received "${rules[i].join(" ")}". ` +
        `Source: src/_redirects. Fix: make the destination ${from}.`,
    );
    assert(
      i < catchAll,
      `Expected the rule for ${from} above the /* catch-all, received it ${i - catchAll} lines below. ` +
        `Source: src/_redirects, whose first matching rule wins. Fix: move "${rules[i].join(" ")}" up.`,
    );
  }
  const served = new Set(rules.slice(0, catchAll).filter(([from, to]) => from === to).map(([from]) => from));

  const html = await Deno.readTextFile(dir + "src/index.html");
  const loaded = new Set([...html.matchAll(/(?:from|src=|href=)\s*["'](\/[^"']+)["']/g)].map((m) => m[1]));
  for (const icon of (await manifest()).icons ?? []) loaded.add(icon.src);

  const queue = [...loaded].filter((path) => path.endsWith(".mjs"));
  for (let walked = 0; queue.length; walked++) {
    assert(
      walked < MAX_MODULES,
      `Expected the module graph under src/index.html to close within ${MAX_MODULES} imports, received a ` +
        `queue still holding ${queue.join(", ")} after ${walked}. Source: browser_test.ts walks it. ` +
        `Fix: the graph is a cycle or has grown past the bound.`,
    );
    const source = await Deno.readTextFile(dir + "src" + queue.shift());
    for (const [, spec] of source.matchAll(/from\s*["'](\.?\/[^"']+\.mjs)["']/g)) {
      const path = spec.replace(/^\./, "");
      if (loaded.has(path)) continue;
      loaded.add(path);
      queue.push(path);
    }
  }

  for (const path of loaded) {
    assert(
      served.has(path),
      `Expected src/_redirects to serve ${path}, received an allowlist without it, so the host answers ` +
        `${path} with index.html: a module refused for its MIME type, an icon that never decodes. ` +
        `Source: src/_redirects. Fix: add a line above the /* catch-all: ${path} ${path} 200`,
    );
  }
});

// The manifest is what makes the app installable on a phone. Its icon is
// fetched by the OS at install time and named by the manifest rather than by
// index.html; that both are served rather than shadowed by the SPA shell is the
// _redirects test above, and what they say is here.
//
// Read from src/ and not from dist/, which the build only copies into: deleting
// src/manifest.webmanifest left a stale dist copy behind and this test green
// over a tree that ships no manifest at all. The origin is a resolution base
// for two relative URLs and nothing else, so any origin does.
const ORIGIN = "https://scrapsheets.invalid/";
Deno.test("the app is installable", async () => {
  const web = await manifest();
  for (const field of ["name", "short_name", "start_url", "display", "background_color", "theme_color"]) {
    assert(
      web[field],
      `Expected src/manifest.webmanifest to carry ${field}, received ${JSON.stringify(web[field])}. ` +
        `Source: src/manifest.webmanifest. Fix: without every one of those a browser offers no install.`,
    );
  }
  // Presence is not the requirement for either of these, and the one value that
  // makes the app uninstallable -- "browser" -- is as truthy as the rest.
  assert(
    ["standalone", "fullscreen", "minimal-ui"].includes(web.display as string),
    `Expected src/manifest.webmanifest to declare display as standalone, fullscreen or minimal-ui, ` +
      `received ${JSON.stringify(web.display)}. Source: src/manifest.webmanifest. ` +
      `Fix: "browser" is the one value a browser offers no install prompt for.`,
  );
  const scope = new URL(web.scope as string, ORIGIN).href;
  const start = new URL(web.start_url as string, ORIGIN).href;
  assert(
    start.startsWith(scope),
    `Expected start_url ${web.start_url} inside scope ${web.scope}, received one resolving to ${start}. ` +
      `Source: src/manifest.webmanifest. Fix: a start_url outside scope is ignored and the install fails.`,
  );

  const icon = web.icons?.[0]?.src;
  assert(
    icon,
    `Expected src/manifest.webmanifest to name an icon, received ${JSON.stringify(web.icons)}. ` +
      `Source: src/manifest.webmanifest. Fix: add an icons entry naming /icon.svg.`,
  );
  // Read rather than stat-ed: an icon that does not decode is one of the few
  // things that blocks the install prompt outright, and a truncated or empty
  // file exists exactly as hard as a good one.
  const svg = await Deno.readTextFile(dir + "src" + icon).catch(() => "");
  assert(
    svg.includes("<svg"),
    `Expected src${icon}, the icon src/manifest.webmanifest names, to be an svg, received ` +
      `${svg.length} characters. Source: src/manifest.webmanifest declares it image/svg+xml. ` +
      `Fix: add src${icon}; deno task build copies it to dist.`,
  );
  for (const name of ["manifest.webmanifest", icon.slice(1)]) {
    assertEquals(
      await Deno.readTextFile(dir + "dist/" + name).catch(() => ""),
      await Deno.readTextFile(dir + "src/" + name),
      `Expected dist/${name} to be the copy of src/${name} the host serves, received something else. ` +
        `Source: deno task build copies src/* to dist. Fix: run deno task build`,
    );
  }

  const html = await Deno.readTextFile(dir + "src/index.html");
  assert(
    html.includes('rel="manifest" href="/manifest.webmanifest"'),
    `Expected src/index.html to link the manifest, received a head without it. Source: src/index.html. ` +
      `Fix: add <link rel="manifest" href="/manifest.webmanifest"> to the head.`,
  );
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

// COLUMN_TYPES in src/sql.mjs is the one list, and its own comment says what a
// second one costs. Neither an Elm Dict nor a TypeScript union can be imported
// and asked at runtime, so these read the other two copies as source text.
//
// Both directions, because a one-way check let the page keep writing spellings
// the server did not know -- the bug wearing the guard as a disguise. Names come
// out by regex rather than line by line, so reformatting cannot quietly turn the
// comparison into a no-op.
const quoted = (block: string, pattern: RegExp) => new Set([...block.matchAll(pattern)].map((m) => m[1]));
const elmList = (elm: string, name: string) => {
  const block = elm.split(`${name} =`)[1]?.split("]")[0] ?? "";
  assert(block.includes('( "'), `${name} should still be a list of spellings in src/Main.elm`);
  return quoted(block, /"([^"]+)"/g);
};
const same = (a: Set<string>, b: Set<string>, aName: string, bName: string) => {
  assertEquals([...a].filter((x) => !b.has(x)).sort(), [], `${aName} has a column type ${bName} does not`);
  assertEquals([...b].filter((x) => !a.has(x)).sort(), [], `${bName} has a column type ${aName} does not`);
};

Deno.test("the page and the engine accept exactly the same column types", async () => {
  const elm = await Deno.readTextFile(dir + "src/Main.elm");
  same(
    elmList(elm, "columnTypes"),
    new Set(CANONICAL_TYPES as string[]),
    "src/Main.elm's columnTypes",
    "src/sql.mjs's CANONICAL_TYPES",
  );
  same(
    elmList(elm, "typeAliases"),
    new Set(Object.keys(COLUMN_TYPES).filter((type) => !(CANONICAL_TYPES as string[]).includes(type))),
    "src/Main.elm's typeAliases",
    "src/sql.mjs's aliases",
  );
});

// A chart's kind used to be a plain string nothing checked: the decoder took
// any of them and viewChart drew a line for everything that was not "bar", so a
// typo was invisible. CHART_KINDS is the list now, chartSql refuses what is not
// on it, and this is the guard that keeps the page's own table from drifting off
// it -- the same shape as the two above.
Deno.test("the page draws exactly the chart kinds the engine admits", async () => {
  const elm = await Deno.readTextFile(dir + "src/Main.elm");
  const page = elm.split("kindSpec kind =")[1]?.split("\n\n\n")[0] ?? "";
  assert(page.includes('name = "'), "kindSpec should still name each kind in src/Main.elm");
  same(
    quoted(page, /name = "([^"]+)"/g),
    new Set(CHART_KINDS as string[]),
    "src/Main.elm's kindSpec",
    "src/sql.mjs's CHART_KINDS",
  );
});

// An alert's `when` is spelled on the document, read by the poller in main.ts
// and offered by the select in Main.elm. Same reason, same guard.
Deno.test("the page offers exactly the alert conditions the poller knows", async () => {
  const ts = await Deno.readTextFile(dir + "main.ts");
  const elm = await Deno.readTextFile(dir + "src/Main.elm");
  const poller = ts.split("const ALERT_WHEN = [")[1]?.split("]")[0] ?? "";
  assert(poller.includes('"'), "ALERT_WHEN should still be a list of spellings in main.ts");
  const page = elm.split("whenSpec when =")[1]?.split("\n\n\n")[0] ?? "";
  assert(page.includes('name = "'), "whenSpec should still name each condition in src/Main.elm");
  same(
    quoted(poller, /"([^"]+)"/g),
    quoted(page, /name = "([^"]+)"/g),
    "main.ts's ALERT_WHEN",
    "src/Main.elm's whenSpec",
  );
});

// A feed's method is spelled on the document, sent by the poller in main.ts and
// offered by the select in Main.elm. Same reason, same guard.
Deno.test("the page offers exactly the methods the poller sends", async () => {
  const ts = await Deno.readTextFile(dir + "main.ts");
  const elm = await Deno.readTextFile(dir + "src/Main.elm");
  const poller = ts.split("const NET_METHODS = [")[1]?.split("]")[0] ?? "";
  assert(poller.includes('"'), "NET_METHODS should still be a list of spellings in main.ts");
  const page = elm.split("netMethods =")[1]?.split("]")[0] ?? "";
  assert(page.includes('"'), "netMethods should still be a list of spellings in src/Main.elm");
  same(quoted(poller, /"([^"]+)"/g), quoted(page, /"([^"]+)"/g), "main.ts's NET_METHODS", "src/Main.elm's netMethods");
});

Deno.test("the server's union admits exactly the column types the engine knows", async () => {
  const ts = await Deno.readTextFile(dir + "main.ts");
  const union = ts.split("export type Type =")[1]?.split(";")[0] ?? "";
  // Pinned to the union's own last arm. A stray semicolon anywhere above it --
  // in a comment, say -- cuts this read short, and a short read is a guard that
  // passes because it looked at less.
  assert(union.includes("[k: string]: Type"), "the Type union should end at its structured arm in main.ts");
  // `\| "x"` and not every quoted word: the structured arms carry "array" and
  // "tuple", which are shapes rather than column types.
  same(
    quoted(union, /\|\s*"([^"]+)"/g),
    new Set(Object.keys(COLUMN_TYPES)),
    "main.ts's Type union",
    "src/sql.mjs's COLUMN_TYPES",
  );
});
