// The parts of src/index.html that are functions of their input rather than of
// the browser.
//
// That file is glue: ports, an automerge repo, a websocket, a synchronous
// XMLHttpRequest. Glue is hard to test and mostly not worth testing. What was
// buried inside it and *is* worth testing is here instead — the library the page
// assembles, the thumbnail it computes for each sheet, and the four things it
// does with an HTTP response. page_test.ts checks these directly rather than
// re-implementing them, which is the whole point: a copy written for a test
// proves the copy works.

import { Col, EXAMPLES, Table } from "./examples.mjs";

// --- the library
//
// Seven live feeds the server hosts. They have no document of their own: the
// page opens a socket per portal and the rows arrive over it.
export const PORTALS = ["time", "stonks", "dice", "orbit", "cafe", "forest", "words"];

const tutorial = Table(
  ["step::text", "how::text"].map(Col),
  ["create a table", "click table:... below"],
  ["edit a cell", "click a cell and type"],
  ["create a query", "click query:... below"],
  ["reference a sheet with @", "type @ in the editor and pick a table"],
  ["see live results", "try: select * from @table:countries limit 5"],
);

/** The library the page shows: what this browser has saved, then everything
 * bundled. The order matters — a system entry wins over a stored one of the same
 * id, so a stale copy of a bundled example cannot shadow the real one. The empty
 * id is the library itself, which is what an unrecognised route falls back to.
 */
export const library = (stored = {}) => {
  const merged = {
    ...stored,
    "": { name: "library", system: true, doc: { type: "library" } },
    shop: { name: "shop", system: true, doc: { type: "shop" } },
    ...Object.fromEntries(
      PORTALS.map((p) => [`portal:${p}`, { name: `${p} demo`, system: true, doc: { type: "portal", data: null } }]),
    ),
    ...EXAMPLES,
    "table:tutorial": { name: "tutorial", system: true, doc: tutorial },
  };
  return Object.fromEntries(
    Object.entries(merged).map((
      [id, entry],
    ) => [id, entry.doc && !entry.thumb ? { ...entry, thumb: docThumb(entry.doc) } : entry]),
  );
};

/** The sparkline behind a library row: the first column of a table that holds at
 * least three numbers, scaled to 0..1 over its first 16 values. A sheet that is
 * not a table has a shape but no line to draw.
 */
export const docThumb = (doc) => {
  if (doc?.type !== "table" || !Array.isArray(doc.data))
    return { kind: doc?.type ?? "unknown", cols: 0, rows: 0, spark: [] };
  const cols = Object.values(doc.data[0] ?? {});
  let spark = [];
  for (const col of cols) {
    const nums = doc.data.slice(1, 21).map((row) => parseFloat(row[col.key])).filter(Number.isFinite);
    if (nums.length < 3) continue;
    const take = nums.slice(0, 16);
    const min = Math.min(...take), max = Math.max(...take);
    spark = take.map((n) => Math.round((max === min ? 0.5 : (n - min) / (max - min)) * 1000) / 1000);
    break;
  }
  return { kind: "table", cols: cols.length, rows: doc.data.length - 1, spark };
};

// --- http
//
// The page cannot fetch a third-party URL directly: the browser will not allow
// it. Anything that is not ours goes through the server's proxy, which is also
// where the SSRF guard lives.
export const API_BASE = "https://api.sheets.scrap.land";

/** Where a `from http(...)` call actually sends its request, and whether that
 * went through our proxy — which is what decides who to blame for a 4xx.
 */
export const httpTarget = (url, qs = {}, origin = "") => {
  const full = url + "?" + new URLSearchParams(qs);
  const viaProxy = !url.startsWith(origin) && !url.startsWith(API_BASE);
  return { url: viaProxy ? `${API_BASE}/proxy?url=${encodeURIComponent(full)}` : full, full, viaProxy };
};

/** The origin's own words, dug out of whatever shape it sent them in. A bare
 * "HTTP 400" is the error this exists to stop: the server almost always says
 * what was wrong with the request, in a field nobody thinks to look at.
 */
export const httpErrorDetail = (text, contentType) => {
  if (/xml/.test(contentType)) {
    const detail = new DOMParser()
      .parseFromString(text, "text/xml")
      .querySelector("entry > summary, error, message")
      ?.textContent?.trim();
    if (detail) return detail;
  }
  if (/json/.test(contentType)) {
    try {
      const body = JSON.parse(text);
      const detail = body?.error?.info ?? body?.error?.message ?? body?.error ?? body?.message;
      if (detail) return typeof detail === "string" ? detail : JSON.stringify(detail);
    } catch { /* not actually json; fall through to the raw body */ }
  }
  return text.trim().slice(0, 300) || "(empty body)";
};

/** A request that never got a response. Usually CORS, sometimes a dead host, and
 * the browser will not say which — so the message names both rather than guessing.
 */
export const httpUnreachable = (url) =>
  [
    `I could not reach this server:`,
    ``,
    `  ${url}`,
    ``,
    `The request failed before a response arrived. The host may be down, or it`,
    `may refuse cross-origin requests.`,
  ].join("\n");

/** A response that arrived and said no. `rejectedByProxy` separates our proxy
 * refusing to make the call from the origin refusing to answer it: the same
 * status means two different things, and only one of them is the user's to fix.
 */
export const httpFailure = ({ status, url, body, contentType, rejectedByProxy = false }) =>
  [
    rejectedByProxy
      ? `The scrapsheets proxy rejected this request with ${status}:`
      : `This server responded with ${status}:`,
    ``,
    `  ${url}`,
    ``,
    `It said:`,
    ``,
    `  ${httpErrorDetail(body, contentType)}`,
  ].join("\n");

/** A response that arrived, said yes, and was not what it claimed to be. The
 * first 200 characters are shown because the content type already lied once.
 */
export const httpUnparsed = ({ url, contentType, body, message }) =>
  [
    `This server sent ${contentType}, which I could not parse:`,
    ``,
    `  ${url}`,
    ``,
    `  ${message}`,
    ``,
    `The response starts with:`,
    ``,
    `  ${body.trim().slice(0, 200) || "(empty body)"}`,
  ].join("\n");

/** Atom, flattened into the shape a query can select from. arxiv, and every
 * government feed that never left 2005, answer in this and nothing else.
 */
export const atomToJson = (xml) => {
  const doc = new DOMParser().parseFromString(xml, "text/xml");
  return {
    title: doc.querySelector("feed > title")?.textContent,
    updated: doc.querySelector("feed > updated")?.textContent,
    // arxiv sends this one as <opensearch:totalResults>, and a CSS type selector
    // matches a qualified name: querySelector("totalResults") never found it, so
    // this field was silently 0 on every feed. Match the local name in any
    // namespace instead.
    totalResults: parseInt(doc.getElementsByTagNameNS("*", "totalResults")[0]?.textContent || "0"),
    entries: [...doc.querySelectorAll("entry")].map((entry) => ({
      id: entry.querySelector("id")?.textContent,
      title: entry.querySelector("title")?.textContent?.trim(),
      summary: entry.querySelector("summary")?.textContent?.trim(),
      published: entry.querySelector("published")?.textContent,
      updated: entry.querySelector("updated")?.textContent,
      authors: [...entry.querySelectorAll("author name")].map((a) => a.textContent),
      links: [...entry.querySelectorAll("link")].map((l) => ({
        href: l.getAttribute("href"),
        rel: l.getAttribute("rel"),
        type: l.getAttribute("type"),
      })),
      categories: [...entry.querySelectorAll("category")].map((c) => c.getAttribute("term")),
    })),
  };
};

export const PARSERS = { "application/atom+xml": atomToJson };
