// Every bundled example, run through both engines.
//
// The server imports `npm:alasql` and the page imports the vendored
// `src/alasql.mjs` that `deno task vendor` builds. They are two separate engine
// instances, and `src/sql.mjs` is the one module that teaches both of them the
// same UDFs and the same rewrite passes. The promise the project makes is that
// a query means the same thing in either place, so this replays every bundled
// sheet through both and compares the answers row for row.
//
// This used to live in the browser test, which named a dozen sheets by hand and
// needed Chrome to check them. Running the page's own engine in-process covers
// all of them in a second. What it does not cover is the glue in
// `src/index.html` — `runSql`, the ports, the render — which is why the pass
// order below is kept deliberately identical to the one that file documents.
import { assert, assertEquals } from "@std/assert";
import server from "alasql";
import page from "./src/alasql.mjs";
import { DATASETS, EXAMPLES } from "./src/examples.mjs";
import {
  applyWindows,
  chartSql,
  checkCells,
  checkColumnTypes,
  checkPivot,
  checkResultColumns,
  describeRef,
  describeRows,
  register,
  rewriteUnpivot,
  rewriteWindows,
  scanRefs,
} from "./src/sql.mjs";

type Row = Record<string, unknown>;
type Engine = {
  (sql: string, params?: unknown[]): { columns: { columnid: string }[]; data: Row[] };
  options: { modifier: string };
  from: Record<string, unknown>;
  fn: Record<string, (...args: unknown[]) => unknown>;
};

// Both engines are set up the way their own host sets them up: RECORDSET so the
// column list comes back beside the rows (main.ts:160, src/index.html:530), then
// register(), then a SHEET from-function reading the request-scoped params.
const engines: [string, Engine][] = [["server", server as Engine], ["page", page as Engine]];
for (const [, engine] of engines) {
  engine.options.modifier = "RECORDSET";
  register(engine);
  engine.from.SHEET = (id: string, _opts: unknown, cb: unknown, idx: unknown, query: unknown) => {
    const rows = ((query as { params?: Record<string, Row[]>[] })?.params?.[0] ?? {})[id];
    if (!rows) throw new Error(`I could not load the sheet "@${id}".`);
    return cb ? (cb as (r: Row[], i: unknown, q: unknown) => Row[])(rows, idx, query) : rows;
  };
}

const cols = (id: string) =>
  Object.values(
    (DATASETS as { doc_id: string; doc: { data: Row[] } }[]).find((d) => `table:${d.doc_id}` === id)!.doc.data[0],
  );

// The same order both engines run: refs, cells, pivot, unpivot, windows, then
// the result-column check. Anything that diverges here is a bug in one of them.
const replay = (engine: Engine) => {
  const loaded: Record<string, Row[]> = {};
  for (const { doc_id, doc } of DATASETS as { doc_id: string; doc: { data: Row[] } }[]) {
    const [cols_, ...rows] = doc.data;
    const id = `table:${doc_id}`;
    loaded[id] = rows.map((row) =>
      Object.fromEntries((Object.values(cols_) as { name: string; key: string }[]).map((c) => [c.name, row[c.key]]))
    );
    checkColumnTypes(id, Object.values(cols_), loaded[id]);
  }

  const byId = EXAMPLES as unknown as Record<string, { doc: { type: string; data: [{ code: string }] } }>;
  const ran = new Set<string>();
  const run = (id: string, depth = 0): Row[] => {
    if (loaded[id]) return loaded[id];
    assert(depth <= 8, `${id}: @query refs nest deeper than 8`);
    const ex = byId[id];
    assert(ex, `${id} is referenced but not bundled`);
    assertEquals(ex.doc.type, "query", `${id} is referenced as a query`);
    const { code } = ex.doc.data[0];
    for (const ref of code.match(/@query:[A-Za-z0-9_-]+/g) ?? []) run(ref.slice(1), depth + 1);

    const described = describeRef(code);
    let rows: Row[];
    if (described) rows = describeRows(described, cols(described), loaded[described]) as Row[];
    else {
      const { sql, cells } = scanRefs(code);
      const colsOf = Object.fromEntries(
        Object.entries(loaded).map(([ref, rows_]) => [ref, Object.keys(rows_[0] ?? {}).map((name) => ({ name }))]),
      );
      checkCells(cells, loaded, colsOf);
      checkPivot(sql);
      const plan = rewriteWindows(rewriteUnpivot(sql, colsOf));
      let out = engine(plan.sql, [loaded]);
      if (plan.windows.length)
        out = applyWindows(out, plan, (q: string, params: unknown[]) => engine(q, params).data);
      rows = out.data;
      // A window column that came back empty means the pass silently missed it.
      // A lifted one is dropped from the result, so it has nothing to say.
      for (const w of plan.windows as { alias: string }[]) {
        if (w.alias.startsWith("__")) continue;
        assert(
          rows.some((r) => r[w.alias] !== null && r[w.alias] !== undefined),
          `${id}: the window column "${w.alias}" is empty in every row`,
        );
      }
      // AlaSQL answers a column name it does not have with undefined in every
      // row, so a typo in a bundled example reads as a sheet full of blanks
      // rather than as an error. This is the pass that says so.
      checkResultColumns(
        out.columns,
        rows,
        Object.values(loaded).flatMap((rows_) => Object.keys(rows_[0] ?? {})),
        code,
      );
    }
    assert(rows.length > 0, `${id} returned no rows`);
    ran.add(id);
    loaded[id] = rows;
    return rows;
  };

  for (const [id, ex] of Object.entries(byId)) if (ex.doc.type === "query") run(id);

  // Every chart reads a sheet that exists and plots columns it has, and every
  // dashboard tile names a sheet somebody bundled. A chart that only fails when
  // it is opened is a broken storefront too.
  for (const [id, ex] of Object.entries(byId)) {
    const doc = ex.doc.data[0] as unknown as { source: string; x: string; y: string; tiles: string[] };
    if (ex.doc.type === "chart") {
      const source = run(doc.source.slice(1));
      for (const axis of [doc.x, doc.y])
        assert(Object.hasOwn(source[0], axis), `${id} plots "${axis}", which ${doc.source} does not have`);
      // The same SQL both engines build for a chart, so the drawn chart and the
      // exported CSV cannot disagree.
      assert(chartSql(doc).includes(doc.y), `${id} should plot ${doc.y}`);
    }
    if (ex.doc.type === "dashboard") {
      for (const tile of doc.tiles) assert(byId[tile.slice(1)], `${id} names the missing tile ${tile}`);
    }
  }

  return { loaded, ran };
};

Deno.test("every bundled example runs, in both engines, with the same answer", () => {
  const results = engines.map(([name, engine]) => [name, replay(engine)] as const);
  const queries = Object.values(EXAMPLES as Record<string, { doc: { type: string } }>)
    .filter((ex) => ex.doc.type === "query").length;

  const [[, first], [, second]] = results;
  assertEquals(first.ran.size, queries, "every bundled query should have run");
  assertEquals(second.ran.size, queries, "every bundled query should have run in the page engine too");

  // The parity that matters: the same sheet, the same rows, in both engines.
  // A UDF registered in one and not the other, or a vendored bundle built from a
  // different alasql, shows up here rather than in somebody's browser.
  for (const id of first.ran) {
    assertEquals(
      JSON.stringify(second.loaded[id]),
      JSON.stringify(first.loaded[id]),
      `${id} answers differently in the page engine than on the server`,
    );
  }
});

Deno.test("the page engine still needs min_text(), and still has the UDFs", () => {
  // The upstream bug min_text()/max_text() exist to work around: raw alasql
  // drops a text min() rather than erroring, so checkResultColumns has to catch
  // it. If this ever throws instead, the workaround can go.
  for (const [name, engine] of engines) {
    assertEquals(
      JSON.stringify(engine("select min(c) as m from ?", [[{ c: "b" }, { c: "a" }]]).data),
      "[{}]",
      `${name}: alasql still drops a text min() silently`,
    );
    const row = engine(
      `select median(x) m, mode(x) mo, levenshtein('kitten','sitting') l,
              date_trunc('month','2026-08-16T12:00:00Z') d, percentile(array(x), 0.5) p,
              fiscal_year('2026-10-01',10) fy, fiscal_period('2026-10-01',10) fp,
              round(haversine_km(51.47, -0.45, 40.64, -73.78)) km,
              geohash(57.64911, 10.40744, 11) g, width_bucket(5, 0, 30, 6) b,
              min_text(c) lo, max_text(c) hi
       from (select 1 as x, 'b' as c union all select 3, 'a' union all select 3, 'c')`,
    ).data[0];
    assertEquals(row, {
      m: 3,
      mo: 3,
      l: 3,
      d: "2026-08-01T00:00:00.000Z",
      p: 3,
      fy: 2027,
      fp: 1,
      // LHR to JFK from the two-decimal coordinates table:airports ships. The
      // exact airport reference points give 5555 km; the rounding costs about 14.
      km: 5541,
      g: "u4pruydqqvj",
      b: 2,
      lo: "a",
      hi: "c",
    }, `${name}: src/sql.mjs should be registered on this engine`);
  }
});
