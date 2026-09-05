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
import { sheets } from "./src/page.mjs";
import {
  applyWindows,
  chartSql,
  checkColumnTypes,
  checkResultColumns,
  describeRef,
  describeRows,
  knownType,
  NUMERIC_TYPES,
  planQuery,
  register,
  scanRefs,
  selectTypes,
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
}

// Installed per replay rather than once at import: page_test.ts builds its own
// SHEET over the same vendored engine, and whichever module happened to load
// last would otherwise win.
const serveSheets = (engine: Engine) => {
  engine.from.SHEET = (id: string, _opts: unknown, cb: unknown, idx: unknown, query: unknown) => {
    const rows = ((query as { params?: Record<string, Row[]>[] })?.params?.[0] ?? {})[id];
    if (!rows) throw new Error(`I could not load the sheet "@${id}".`);
    return cb ? (cb as (r: Row[], i: unknown, q: unknown) => Row[])(rows, idx, query) : rows;
  };
};

const cols = (id: string) =>
  Object.values(
    (DATASETS as { doc_id: string; doc: { data: Row[] } }[]).find((d) => `table:${d.doc_id}` === id)!.doc.data[0],
  );

// The same order both engines run: refs, cells, pivot, unpivot, windows, then
// the result-column check. Anything that diverges here is a bug in one of them.
const replay = (engine: Engine) => {
  serveSheets(engine);
  const loaded: Record<string, Row[]> = {};
  for (const { doc_id, doc } of DATASETS as { doc_id: string; doc: { data: Row[] } }[]) {
    const [cols_, ...rows] = doc.data;
    const id = `table:${doc_id}`;
    loaded[id] = rows.map((row) =>
      Object.fromEntries((Object.values(cols_) as { name: string; key: string }[]).map((c) => [c.name, row[c.key]]))
    );
    // Every bundled column declares a type the engine knows, checked before the
    // pass below refuses one for the same reason.
    for (const c of Object.values(cols_) as { name: string; type: string }[]) {
      assert(
        knownType(c.type),
        `${id}.${c.name} declares the type "${c.type}", which COLUMN_TYPES in src/sql.mjs does not list`,
      );
    }
    checkColumnTypes(id, Object.values(cols_), loaded[id]);
    // checkColumnTypes is the one coercion there is, so this is what it
    // promises: a numeric column holds numbers and nulls, never a blank that
    // sums as a zero and never a string that concatenates.
    for (const c of Object.values(cols_) as { name: string; type: string }[]) {
      if (!NUMERIC_TYPES.includes(c.type)) continue;
      for (const row of loaded[id]) {
        const v = row[c.name];
        assert(
          v === null || typeof v === "number",
          `${id}.${c.name} is ${c.type} but still holds ${JSON.stringify(v)} after the load`,
        );
      }
    }
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
      const { sql, ids, cells } = scanRefs(code);
      const colsOf = Object.fromEntries(
        Object.entries(loaded).map(([ref, rows_]) => [ref, Object.keys(rows_[0] ?? {}).map((name) => ({ name }))]),
      );
      // The passes in the one order both hosts run them, guards included: a
      // demo that trips MAX_JOIN_ROWS is a demo to rewrite as two sheets.
      const plan = planQuery(sql, cells, loaded, colsOf);
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
      // A result column's type is a claim about its values, and both engines
      // make the same claim off the same text. A wrong entry in SELECT_TYPES
      // reads as a num column full of strings, here rather than in a chart.
      // Typed against the sheets this query names, which is the map the server
      // builds -- flattening all of them instead types a `district` column by
      // whichever of the two sheets holding one loaded last.
      const typeOf: Record<string, string> = {};
      const ambiguous = new Set<string>();
      for (const ref of ids) {
        if (ref.startsWith("table:")) {
          for (const c of cols(ref) as { name: string; type: string }[]) {
            if (typeOf[c.name] && typeOf[c.name] !== c.type) ambiguous.add(c.name);
            typeOf[c.name] = c.type;
          }
        }
      }
      for (const [name, type] of Object.entries(selectTypes(code, typeOf) as Record<string, string>)) {
        // us-states.region is an enum and fema-regions.region is an int, and
        // query:state-crosswalk joins both: a map keyed by name alone cannot
        // tell them apart, and neither can the server's. Naming which sheet a
        // qualified column belongs to needs the from clause, which is a bigger
        // change than this one; until then an ambiguous name has no promise to
        // check.
        if (ambiguous.has(name)) continue;
        for (const row of rows) {
          const v = row[name];
          if (v === null || v === undefined) continue;
          const want = NUMERIC_TYPES.includes(type) ? "number" : type === "text" ? "string" : typeof v;
          assertEquals(typeof v, want, `${id}: "${name}" is typed ${type} but holds ${JSON.stringify(v)}`);
        }
      }
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

Deno.test("a query sheet's columns carry their types in the page as well", async () => {
  // The server stamps a query sheet's columns with the types its select list
  // produced, and a sheet reading that query inherits them. The page builds the
  // same row through page.mjs's sheets(), and used to leave every type
  // undefined -- so `describe @query:x` answered one thing here and another on
  // the server, off the same query.
  const shelf = {
    "table:t": { doc: { type: "table", data: [{ 0: { key: "0", name: "price", type: "usd" } }, { 0: 10 }] } },
    "query:q": {
      doc: {
        type: "query",
        data: [{ code: "select count(*) as n, cast(price as string) as price_text, price from @table:t" }],
      },
    },
  };
  const engine = sheets(page, () => shelf, () => Promise.resolve(undefined));
  const { data } = await engine.runSql("describe @query:q", { "": null });
  assertEquals(
    (data as Row[]).map((r) => [r.column, r.type]),
    [["n", "int"], ["price_text", "text"], ["price", "usd"]],
  );
});
