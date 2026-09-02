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
import { PORTALS as FEEDS } from "./portals.mjs";
import {
  applyWindows,
  DESCRIBE_COLUMNS,
  describeRef,
  describeRows,
  loadRefs,
  nearest,
  planQuery,
  scanRefs,
  selectTypes,
  toRecords,
} from "./sql.mjs";

// --- the library
//
// The live feeds the server hosts. They have no document of their own: the page
// opens a socket per portal and the rows arrive over it, so only the names
// matter here -- and they are read off the one list rather than written down a
// second time.
export const PORTALS = FEEDS.map((feed) => feed.name);

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

// --- a view this browser holds
//
// The sync server refuses a viewer's write and says so, so a sheet you can only
// read is arranged locally or not at all. What is kept is not the document --
// that one is not ours -- but the arrangement patches Elm sent for it, folded
// into a partial of `data[0]` and merged back over the document on the way in.
// Both halves are here rather than in src/index.html because both are functions
// of their input, and because a merge nothing checks is a merge that loses a
// filter.

/** The arrangement patches folded into a partial of `data[0]`, in the shape
 * their own paths describe. `path[0]` is always 0 — `data[0]` — so the walk
 * starts after it.
 *
 * A cleared field is held as `null` rather than dropped: a viewer who clears a
 * sort must not get the owner's back on the next reload.
 */
export const foldView = (held, patches) => {
  const out = structuredClone(held ?? {});
  for (const { path, action, value } of patches) {
    // Every arrangement patch names a field on a column. A path that names
    // something else is a patch on the wrong port, and folding it away quietly
    // would lose an edit somebody made.
    if (!Array.isArray(path) || path.length < 3 || path[0] !== 0)
      throw new Error(
        `Expected an arrangement patch addressing data[0], received ${JSON.stringify(path)} from arrangeDoc. ` +
          `Only view fields go out on that port; a row or column edit belongs on changeDoc.`,
      );
    let at = out;
    for (const seg of path.slice(1, -1)) at = at[seg] ??= {};
    at[path[path.length - 1]] = action === "del" ? null : value;
  }
  return out;
};

/** That partial merged back over a document's `data[0]`, copying as it descends
 * — an automerge snapshot is frozen, and a held view must never be written into
 * the document it is standing in for. A null clears the field.
 */
export const mergeView = (data0, held) => {
  if (!held || typeof data0 !== "object" || data0 === null) return data0;
  const out = Array.isArray(data0) ? [...data0] : { ...data0 };
  for (const [key, value] of Object.entries(held)) {
    if (value === null) delete out[key];
    else if (value && typeof value === "object" && !Array.isArray(value)) out[key] = mergeView(out[key] ?? {}, value);
    else out[key] = value;
  }
  return out;
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

// --- the page's half of the query engine
//
// `@type:doc_id` resolves in two steps: runSql rewrites each ref to SHEET('id')
// and pre-loads the document behind it, then the SHEET from-function serves the
// rows. The pre-load has to happen first because finding a document is async and
// an AlaSQL from-function is not.
//
// Two things come from outside: `shelf()`, the library map the page has already
// built, and `find(doc_id)`, which pulls a document out of the automerge repo.
// Everything else — the recursion through referenced queries, the cycle bound,
// the column bookkeeping, the pass order — is the same in a test as in the page.

export const sheets = (alasql, shelf, find) => {
  const rows = new Map();
  const types = new Map();

  alasql.from.SHEET = (id, _opts, cb, idx, query) => {
    let res = rows.get(id) ?? (shelf()[id]?.doc?.data && toRecords(shelf()[id].doc.data));
    if (!res) {
      const hit = nearest(id, [...rows.keys(), ...Object.keys(shelf())]);
      throw new Error([
        `I could not load the sheet "${id}".`,
        ``,
        hit ? `  Did you mean: @${hit}` : `  Loaded:       ${[...rows.keys()].join(", ") || "(nothing yet)"}`,
        `  Source:       the @sheet refs in this query`,
        `  Fix:          ${hit ? `write @${hit} instead` : `reference it as @${id} so it loads before the query runs`}`,
      ].join("\n"));
    }
    if (cb) res = cb(res, idx, query);
    return res;
  };

  // What each loaded sheet's columns are called: the declared type row for a
  // table, and whatever the rows carry for a query. checkCells and rewriteUnpivot
  // both read names rather than types.
  const columnsOf = () =>
    Object.fromEntries(
      [...rows].map(([id, rs]) => [id, types.get(id) ?? Object.keys(rs?.[0] ?? {}).map((name) => ({ name }))]),
    );

  const runSql = async (code, params, path = []) => {
    // `describe @table:abc` never reaches the engine: it loads the one sheet it
    // names and reports its shape. Same statement, same answer, on the server.
    const describing = describeRef(code);
    const { sql: out, ids, cells } = describing ? { sql: "", ids: [describing], cells: [] } : scanRefs(code);

    const { colsOf } = await loadRefs(ids, {
      path,
      describing: !!describing,
      fetch: async (id) => {
        const [type, ref_id] = id.split(":");
        if (!["table", "query"].includes(type))
          throw new Error(`@${id}: only table and query sheets can be referenced in queries.`);
        const doc = shelf()[id]?.doc ?? await find(ref_id);
        if (!doc?.data) {
          const known = Object.keys(shelf()).filter((k) => /^(table|query):/.test(k));
          const hit = nearest(id, known);
          throw new Error([
            `@${id}: this sheet has no data.`,
            ``,
            hit ? `  Did you mean: @${hit}` : `  Available:    ${known.slice(0, 12).join(", ") || "(none yet)"}`,
            `  Source:       the @sheet refs in this query`,
            `  Fix:          ${hit ? `write @${hit} instead` : "open the sheet once so it loads, or check the id"}`,
          ].join("\n"));
        }
        // A referenced query runs through runSql too, so a window inside one is
        // computed rather than handed to AlaSQL. The server recurses in `sheet()`
        // instead, which is the one place the two engines are shaped differently.
        if (type !== "query") return doc.data;
        const inner = await runSql(doc.data[0].code, { "": null }, [...path, id]);
        // A query sheet's columns carry the types its select list produced,
        // which runSql has already stamped on them. Without them
        // `describe @query:x` reported every type as undefined in the page and
        // the real one on the server, off the same query.
        return [
          Object.fromEntries(inner.columns.map((c, i) => [i, {
            key: c.columnid,
            name: c.columnid,
            type: c.type ?? "text",
          }])),
          ...inner.data,
        ];
      },
      // Kept as they load, so SHEET can serve them and a nested query sees what
      // its parent already fetched.
      onLoad: (id, loaded) => rows.set(id, loaded),
    });
    // The declared type row, kept beside the rows: it is what names the columns
    // for unpivot and what describe reports.
    for (const id of Object.keys(colsOf)) if (id.startsWith("table:")) types.set(id, colsOf[id]);

    if (describing) {
      return {
        columns: DESCRIBE_COLUMNS.map((name) => ({ columnid: name })),
        data: describeRows(describing, colsOf[describing], rows.get(describing)),
      };
    }
    const plan = planQuery(out, cells, Object.fromEntries(rows), columnsOf());
    const [, result = {}] = await alasql([[`set @params = ?`, [params]], plan.sql]);
    const answer = plan.windows.length ? applyWindows(result, plan, (q, p) => alasql(q, p).data) : result;
    // Every column carries the type its select item produced, which is the map
    // the server stamps its own answer with -- off the author's own text, not
    // the scanned rewrite, so both engines infer from the same characters. A
    // column selectTypes declines carries no type, and the caller decides what
    // that means: "text" for a referenced sheet above, the query document's own
    // declared cols in src/index.html.
    const known = Object.fromEntries([...types.values()].flatMap((cs) => cs.map((c) => [c.name, c.type])));
    const typed = selectTypes(code, known);
    return {
      ...answer,
      columns: answer.columns.map((c) => ({ ...c, type: typed[c.columnid] ?? known[c.columnid] })),
    };
  };

  return {
    runSql,
    /** Every column the referenced sheets actually have: what a typo'd column
     * name gets matched against. */
    columns: () => [...new Set([...rows.values()].flatMap((rs) => Object.keys(rs?.[0] ?? {})))],
    rows: (id) => rows.get(id),
    types: (id) => types.get(id),
  };
};
