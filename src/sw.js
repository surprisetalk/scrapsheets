// The app shell, out of a cache, so an installed app opens with no network.
//
// Network first and the cache second, on every same-origin GET. There is no
// build hash in any of these filenames, so there is nothing for a cache name to
// key on and nothing that says a cached copy is still the deployed one -- the
// network is the only thing that knows. A good answer is written back, so a
// deploy replaces the shell on the next online open and no cache name has to be
// bumped for it.
//
// Nothing cross-origin is answered at all: the API and the sync socket are a
// different origin, and the handler returns without calling respondWith, which
// leaves the browser doing exactly what it did before there was a worker.
//
// Offline means the shell opens. The data still needs the network; what a
// document already synced is in IndexedDB, and the rest is a failed fetch the
// page reports the way it always did.
const CACHE = "scrapsheets-shell";

// Every path src/_redirects serves as itself, plus "/", which it answers with
// the shell. The rule is mechanical so that browser_test.ts can check this list
// against that file and neither can drift.
const SHELL = [
  "/",
  "/index.js",
  "/automerge.wasm",
  "/automerge.mjs",
  "/alasql.mjs",
  "/automerge-repo.mjs",
  "/automerge-repo-idb.mjs",
  "/automerge-repo-ws.mjs",
  "/style.css",
  "/manifest.webmanifest",
  "/icon.svg",
  "/examples.mjs",
  "/sql.mjs",
  "/page.mjs",
  "/portals.mjs",
  "/sw.js",
];

/** What the page hears back, read and written under `path`. */
const answer = async (request, path) => {
  const cache = await caches.open(CACHE);
  let res;
  try {
    res = await fetch(request);
  } catch (err) {
    const held = await cache.match(path);
    if (held) return held;
    throw new Error(
      `Expected ${request.url} from the network or from the ${CACHE} cache, received neither: ` +
        `${err?.message ?? err}. Source: the service worker, offline. ` +
        `Fix: open the app once with a network so the shell is cached.`,
    );
  }
  // Only a good answer. A 404 or a 502 is the network's own word and belongs to
  // the caller; caching it would hand it back offline in place of the copy that
  // worked. `status === 200` rather than `ok`, because a 206 is inside `ok` and
  // `put` refuses a partial response -- and a refusal here would lose the
  // answer the page is waiting for.
  //
  // put() itself can still refuse -- a full quota, a response carrying
  // `Vary: *` -- for reasons that say nothing about the answer in hand.
  // Caching it for next time is best-effort; the page is waiting on this one.
  //
  // Under "/" only an HTML answer: the host answers every path outside SHELL
  // with the shell, but a same-origin 200 that is not the shell -- a file the
  // host happens to serve, a captive portal's interstitial -- would otherwise
  // be written over it and be what the next offline open renders.
  if (res.status === 200 && (path !== "/" || /text\/html/i.test(res.headers.get("content-type") ?? ""))) {
    await cache.put(path, res.clone()).catch((err) =>
      console.error(`service worker: could not cache ${path}: ${err?.message ?? err}`)
    );
  }
  return res;
};

self.addEventListener("install", (event) => {
  // addAll refuses the whole install if one path 404s, which is the answer we
  // want: a shell missing a module is a shell that opens to a blank page.
  event.waitUntil(caches.open(CACHE).then((cache) => cache.addAll(SHELL)));
});

self.addEventListener("fetch", (event) => {
  const url = new URL(event.request.url);
  // A blob: url a page made with URL.createObjectURL -- the chart export's
  // download link -- reports its origin as whatever page created it, ours
  // included, though its scheme is not one we are ever served over and there
  // is nothing of ours to answer it with. Checking the scheme too is what
  // rules it out; origin alone does not.
  if (
    event.request.method !== "GET" ||
    url.origin !== self.location.origin ||
    url.protocol !== self.location.protocol
  ) { return; }
  // The entry the answer belongs under, which is src/_redirects read backwards:
  // a SHELL path is served as itself, and every other path is answered with the
  // shell (`/* / 200`). So a deep link, a share link and a query string are all
  // the one "/" entry -- the cache holds SHELL and nothing else, and every open
  // refreshes the shell rather than leaving it at whatever the install fetched.
  event.respondWith(answer(event.request, SHELL.includes(url.pathname) ? url.pathname : "/"));
});
