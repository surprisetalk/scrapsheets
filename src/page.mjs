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
  checkResultColumns,
  DESCRIBE_COLUMNS,
  describeRef,
  describeRows,
  explain,
  loadedOf,
  loadRefs,
  nearest,
  planQuery,
  PROFILE_COLUMNS,
  profileRef,
  profileRows,
  scanRefs,
  selectTypes,
  timed,
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
 * `seen` — when this browser last opened the sheet — and `trashed` — whether it
 * threw the sheet away — are this browser's facts whoever owns the entry, so
 * they are the two stored fields that survive a system entry. A bundled demo has
 * to be trashable for the same reason it has to be openable.
 *
 * Restoring writes `trashed` back as `false` rather than null, because
 * Library.set drops a null field out of the patch rather than out of the entry —
 * a null would leave the sheet in the trash.
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
    Object.entries(merged).map(([id, entry]) => {
      const opened = stored[id]?.seen ? { ...entry, seen: stored[id].seen } : entry;
      const e = stored[id]?.trashed ? { ...opened, trashed: stored[id].trashed } : opened;
      return [id, e.doc && !e.thumb ? { ...e, thumb: docThumb(e.doc) } : e];
    }),
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
// The sync server refuses a viewer's write and says so, and a bundled sheet has
// no document of this browser's at all, so those two are arranged locally or not
// at all. What is kept is not the document -- that one is not ours -- but the
// arrangement patches Elm sent for it, merged back over the document on the way
// in. Both halves are here rather than in src/index.html because both are
// functions of their input, and because a merge nothing checks is a merge that
// loses a filter.
//
// Held by column key, never by position. A table's patch addresses `data[0][x]`,
// and the document's own copy of that field can afford to: it lives on the
// column object, so a splice carries it. A copy held out here cannot -- the
// whole reason it is held is that somebody else owns the document and can
// reorder it, and then position 5 is a different column than the one you hid.

/** The arrangement patches folded into what this browser holds for a sheet:
 * `{ [column key]: { field: value } }`, whichever home the patch addressed.
 * `data0` is the document's own `data[0]`, which is how a table's position
 * becomes the key the column carries.
 *
 * A cleared field is held as `null` rather than dropped: a viewer who clears a
 * sort must not get the owner's back on the next reload.
 */
export const foldView = (held, patches, data0) => {
  const out = { ...(held ?? {}) };
  for (const { path, action, value } of patches) {
    // A query addresses `data[0].view.<name>.<field>` and already names the
    // column; a table addresses `data[0][x].<field>`, and x is resolved here.
    const [root, at, ...rest] = Array.isArray(path) ? path : [];
    const query = at === "view";
    const [key, field] = query ? rest : [data0?.[at]?.key, ...rest];
    if (root !== 0 || rest.length !== (query ? 2 : 1) || typeof key !== "string" || typeof field !== "string") {
      throw new Error(
        `Expected an arrangement patch naming one field of one column, received ` +
          `${JSON.stringify(path)} from arrangeDoc against ${JSON.stringify(data0)?.slice(0, 120)}. ` +
          `Only view fields go out on that port; a row or column edit belongs on changeDoc.`,
      );
    }
    out[key] = { ...out[key], [field]: action === "del" ? null : value };
  }
  return out;
};

/** What this browser holds, put back on the columns that carry those keys. A
 * held key the document no longer has is dropped rather than created: the
 * arrangement of a column that is gone is nothing, and a column invented to hold
 * one reads back as a blank column the sheet never had.
 *
 * Copies as it goes — an automerge snapshot is frozen, and a held view must
 * never be written into the document it is standing in for.
 */
export const mergeView = (data0, held) => {
  if (!held || typeof data0 !== "object" || data0 === null) return data0;
  const onto = (col, fields) => {
    const out = { ...col };
    for (const [field, value] of Object.entries(fields)) {
      if (value === null) delete out[field];
      else out[field] = value;
    }
    return out;
  };
  // A table's `data[0]` is the column list, and each column carries its share.
  if (Array.isArray(data0)) return data0.map((col) => (held[col?.key] ? onto(col, held[col.key]) : col));
  // A query's is one object, and the view lives in a map beside `cols`.
  const view = { ...data0.view };
  for (const [key, fields] of Object.entries(held)) view[key] = onto(view[key], fields);
  return { ...data0, view };
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

  /** Every column the referenced sheets actually have: what a typo'd column
   * name gets matched against. */
  const columns = () => [...new Set([...rows.values()].flatMap((rs) => Object.keys(rs?.[0] ?? {})))];

  const runSql = async (code, params, path = []) => {
    // `describe @table:abc` never reaches the engine: it loads the one sheet it
    // names and reports its shape. Same statement, same answer, on the server.
    // `explain <query>` runs the query and answers with its profile instead.
    const describing = describeRef(code);
    const profiling = profileRef(code);
    const query = profiling ?? code;
    const started = performance.now();
    const stages = profiling === undefined ? undefined : [];
    const { sql: out, ids, cells } = describing ? { sql: "", ids: [describing], cells: [] } : scanRefs(query);

    const { docs, colsOf } = await loadRefs(ids, {
      path,
      describing: !!describing,
      stages,
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
    const loaded = loadedOf(docs);
    const plan = await timed(
      stages,
      "plan",
      loaded,
      () => planQuery(out, cells, Object.fromEntries(rows), columnsOf()),
      () => loaded,
    );
    const result = await timed(stages, "engine", loaded, async () => {
      const [, answered] = await alasql([[`set @params = ?`, [params]], plan.sql]);
      // Two statements in, two results out, and the second is a recordset.
      // Anything else is the engine's, not the author's, and used to surface
      // as a TypeError about `.data` two lines later.
      if (!answered?.data) {
        throw new Error(explain(`The engine returned no rows for this query.`, {
          Expected: "a recordset for the select",
          Received: answered === undefined ? "nothing" : typeof answered,
          Source: "the SQL after the pre-engine passes",
          Fix: "this is an engine bug: report it with the query text",
        }));
      }
      return answered;
    }, (r) => r.data.length);
    const answer = plan.windows.length
      ? await timed(
        stages,
        "windows",
        result.data.length,
        () => applyWindows(result, plan, (q, p) => alasql(q, p).data),
        (r) => r.data.length,
      )
      : result;
    if (profiling !== undefined) {
      // The query's own answer is still checked: a profile of a refused query
      // is a profile of nothing, and both hosts must refuse the same thing.
      checkResultColumns(answer.columns, answer.data, columns(), query);
      return { columns: PROFILE_COLUMNS.map((name) => ({ columnid: name })), data: profileRows(stages, docs, started) };
    }
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
    columns,
    rows: (id) => rows.get(id),
    types: (id) => types.get(id),
  };
};

/** The key a CSV's header is remembered under: its column names, in order. */
export const importKey = (names) => names.join("\u0001");

/** The preview's columns with the types this browser settled on the last time
 * it imported a file with this header, over the server's guesses. A column
 * with no memory keeps its guess, and says which it was. */
export const rememberedTypes = (imports, cols) => {
  const remembered = imports?.[importKey(cols.map((col) => col.name))] ?? {};
  return cols.map((col) => ({
    name: col.name,
    type: remembered[col.name] ?? col.type,
    remembered: col.name in remembered,
  }));
};
