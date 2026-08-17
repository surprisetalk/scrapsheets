// Regenerates the browser-side automerge assets in src/.
// jsDelivr bundles rewrite bare imports to root-relative /npm/ paths, which would
// resolve against our own origin and load a second copy of automerge. We rewrite
// them back so the import map in src/index.html decides which copy the browser uses.
// Their //# sourceMappingURL=/sm/<hash>.map is root-relative for the same reason,
// and our /* catch-all answers it with index.html, so the browser reports
// "JSON Parse error: Unrecognized token '<'". Point it at jsDelivr instead.

const AUTOMERGE = "3.4.0";
const REPO = "2.5.6";

const bundle = async (pkg: string, version: string, entry: string, out: string) => {
  const res = await fetch(`https://cdn.jsdelivr.net/npm/${pkg}@${version}/${entry}/+esm`);
  if (!res.ok) throw new Error(`jsDelivr ${res.status} for ${pkg}@${version}/${entry}`);
  const src = (await res.text())
    .replaceAll(/\/npm\/@automerge\/automerge-repo@[\d.]+(\/slim)?\/\+esm/g, "@automerge/automerge-repo")
    .replaceAll(/\/npm\/@automerge\/automerge@[\d.]+(\/slim)?\/\+esm/g, "@automerge/automerge")
    .replaceAll(/(?<!jsdelivr\.net)\/npm\//g, "https://cdn.jsdelivr.net/npm/")
    .replaceAll(/sourceMappingURL=\/sm\//g, "sourceMappingURL=https://cdn.jsdelivr.net/sm/");
  const leftover = src.match(/"(\/npm\/[^"]*|[^"]*@automerge\/[^"]*@[\d.][^"]*)"/g);
  if (leftover) throw new Error(`unrewritten imports in ${out}: ${leftover.join(", ")}`);
  const rooted = src.match(/sourceMappingURL=\/[^\s]*/g);
  if (rooted) throw new Error(`root-relative source maps in ${out}: ${rooted.join(", ")}`);
  await Deno.writeTextFile(out, src);
  console.log(`${out} <- ${pkg}@${version}`);
};

const wasm = await fetch(`https://cdn.jsdelivr.net/npm/@automerge/automerge@${AUTOMERGE}/dist/automerge.wasm`);
if (!wasm.ok) throw new Error(`jsDelivr ${wasm.status} for automerge@${AUTOMERGE} wasm`);
await Deno.writeFile("src/automerge.wasm", new Uint8Array(await wasm.arrayBuffer()));
console.log(`src/automerge.wasm <- @automerge/automerge@${AUTOMERGE}`);

await bundle("@automerge/automerge-repo", REPO, "dist/entrypoints/fullfat.js", "src/automerge-repo.mjs");
await bundle("@automerge/automerge-repo-network-websocket", REPO, "dist/index.js", "src/automerge-repo-ws.mjs");
await bundle("@automerge/automerge-repo-storage-indexeddb", REPO, "dist/index.js", "src/automerge-repo-idb.mjs");

const html = await Deno.readTextFile("src/index.html");
const pinned = html.replaceAll(/@automerge\/automerge@[\d.]+\/slim/g, `@automerge/automerge@${AUTOMERGE}/slim`);
if (pinned !== html) await Deno.writeTextFile("src/index.html", pinned);
console.log(`src/index.html import map pinned to @automerge/automerge@${AUTOMERGE}`);
