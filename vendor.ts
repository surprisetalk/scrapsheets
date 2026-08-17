// Regenerates the browser-side assets in src/.
//
// Everything the page loads at runtime is served by us. esbuild inlines each
// package's dependencies, so there is no CDN in the request path: a CDN outage
// cannot break the app, and a root-relative URL inside a third-party bundle
// cannot resolve against our origin and collide with the /* catch-all.
//
// `@automerge/automerge` stays external in the repo bundles on purpose. The
// import map in src/index.html points it at /automerge.mjs, which is the copy
// initializeWasm() sets up; bundling it three times would give the browser three
// copies, only one of which has WASM.
//
// esbuild is the bundler because the alternative is writing one. It runs at build
// time only and never ships to the browser.

import * as esbuild from "esbuild";
import { denoPlugins } from "@luca/esbuild-deno-loader";
import * as path from "@std/path";

const AUTOMERGE = "3.4.0";
const REPO = "2.5.6";

const AM = ["@automerge/automerge", "@automerge/automerge/*"];
const AM_REPO = [...AM, "@automerge/automerge-repo", "@automerge/automerge-repo/*"];

const configPath = path.resolve("deno.json");

// esbuild-deno-loader vendors its own copy of esbuild's type declarations, so the
// Plugin shapes are identical but the nominal types are not.
const plugins = denoPlugins({ configPath }) as unknown as esbuild.Plugin[];

const bundle = async (label: string, entrySource: string, out: string, external: string[] = []) => {
  const entry = await Deno.makeTempFile({ prefix: "vendor-", suffix: ".ts" });
  try {
    await Deno.writeTextFile(entry, entrySource);
    const built = await esbuild.build({
      plugins,
      entryPoints: [entry],
      bundle: true,
      format: "esm",
      target: "es2022",
      platform: "browser",
      minify: true,
      external,
      write: false,
      metafile: true,
    });

    // Anything left in `imports` is fetched at runtime. Only the externals we
    // asked for may appear, and each must be a bare specifier the import map
    // resolves to a local file.
    if (!built.metafile || !built.outputFiles?.length)
      throw new Error(`esbuild produced no output for ${out}. Expected one bundle and a metafile.`);
    const meta = Object.values(built.metafile.outputs)[0];
    const unexpected = meta.imports
      .map((i) => i.path)
      .filter((p) => !external.includes(p) && !external.some((e) => e.endsWith("/*") && p.startsWith(e.slice(0, -1))));
    if (unexpected.length)
      throw new Error(`${out} would fetch at runtime: ${unexpected.join(", ")}. Add it to external, or let it bundle.`);

    const src = built.outputFiles[0].text;
    const rooted = src.match(/(?:sourceMappingURL=|from\s*["'])\/(?:npm|sm|node)\//g);
    if (rooted) throw new Error(`${out} keeps root-relative CDN paths: ${rooted.join(", ")}`);

    await Deno.writeTextFile(out, src);
    console.log(`${out} <- ${label} (${(src.length / 1024).toFixed(0)} KiB, external: ${meta.imports.length})`);
  } finally {
    await Deno.remove(entry).catch(() => {});
  }
};

// The WASM has no JS to bundle; it is a build-time download, not a runtime one.
const wasm = await fetch(`https://cdn.jsdelivr.net/npm/@automerge/automerge@${AUTOMERGE}/dist/automerge.wasm`);
if (!wasm.ok) throw new Error(`jsDelivr ${wasm.status} for automerge@${AUTOMERGE} wasm`);
await Deno.writeFile("src/automerge.wasm", new Uint8Array(await wasm.arrayBuffer()));
console.log(`src/automerge.wasm <- @automerge/automerge@${AUTOMERGE}`);

await bundle(
  `@automerge/automerge@${AUTOMERGE}/slim`,
  `export * from "npm:@automerge/automerge@${AUTOMERGE}/slim";`,
  "src/automerge.mjs",
);
await bundle(
  `@automerge/automerge-repo@${REPO}`,
  `export * from "npm:@automerge/automerge-repo@${REPO}";`,
  "src/automerge-repo.mjs",
  AM,
);
await bundle(
  `@automerge/automerge-repo-network-websocket@${REPO}`,
  `export * from "npm:@automerge/automerge-repo-network-websocket@${REPO}";`,
  "src/automerge-repo-ws.mjs",
  AM_REPO,
);
await bundle(
  `@automerge/automerge-repo-storage-indexeddb@${REPO}`,
  `export * from "npm:@automerge/automerge-repo-storage-indexeddb@${REPO}";`,
  "src/automerge-repo-idb.mjs",
  AM_REPO,
);
// The page used to load alasql from a CDN on a floating @4 tag while the server
// pinned an exact version, so the two engines could disagree. Resolving the bare
// specifier through deno.json + deno.lock is what makes that impossible now.
await bundle(
  "alasql (deno.json)",
  `export { default } from "alasql";`,
  "src/alasql.mjs",
);

await esbuild.stop();
