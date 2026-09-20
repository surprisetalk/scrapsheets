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
import { assert, assertEquals, assertThrows } from "@std/assert";
import server from "alasql";
import page from "./src/alasql.mjs";
import { DATASETS, EXAMPLES } from "./src/examples.mjs";
import { sheets } from "./src/page.mjs";
import {
  applyWindows,
  chartSql,
  checkColumnTypes,
  checkResultColumns,
  cohortSql,
  describeRef,
  describeRows,
  knownType,
  MAX_EXTREMES,
  NUMERIC_TYPES,
  planQuery,
  register,
  rewriteExtremes,
  rewriteUnpivot,
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
    const doc = ex.doc.data[0] as unknown as {
      source: string;
      kind: string | undefined;
      x: string;
      y: string;
      y2: string | undefined;
      series: string | undefined;
      annotations: { at: string; label: string }[] | undefined;
      tiles: string[];
    };
    if (ex.doc.type === "chart") {
      const source = run(doc.source.slice(1));
      // The series column and the second y are read from the same sheet the
      // axes are, so they are checked with them; a chart that plots one thing
      // names neither.
      for (const axis of [doc.x, doc.y, ...(doc.series ? [doc.series] : []), ...(doc.y2 ? [doc.y2] : [])])
        assert(Object.hasOwn(source[0], axis), `${id} plots "${axis}", which ${doc.source} does not have`);
      // A mark is placed by parseDay in src/Main.elm and drawn nowhere at all
      // when it does not read as one, so a bundled chart with an undated mark is
      // a feature that silently does nothing.
      for (const mark of doc.annotations ?? []) {
        assert(/^\d{4}-\d{2}-\d{2}/.test(mark.at), `${id} marks "${mark.at}", which is not a day`);
        assert(mark.label !== "", `${id} draws a mark on ${mark.at} with no label`);
      }
      // The same SQL both engines build for a chart, so the drawn chart and the
      // exported CSV cannot disagree.
      assert(chartSql(doc).includes(doc.y), `${id} should plot ${doc.y}`);
      // And the statement itself is run, in whichever engine this pass is: a
      // chart is the one sheet whose query nobody wrote, so the only place it
      // is read is here.
      const { sql, cells } = scanRefs(chartSql(doc));
      const colsOf = Object.fromEntries(
        Object.entries(loaded).map(([ref, rows_]) => [ref, Object.keys(rows_[0] ?? {}).map((name) => ({ name }))]),
      );
      const drawn = engine(planQuery(sql, cells, loaded, colsOf).sql, [loaded]).data as Row[];
      assert(drawn.length > 0, `${id} draws no points`);
      // A chart that splits its rows carries the series on every point it
      // draws: chartPoints in src/Main.elm groups on that column, and a blank
      // one is the unnamed series a chart with nothing to split by draws, in
      // among the named ones.
      if (doc.series) {
        assert(
          drawn.every((row) => row.series !== null && row.series !== undefined && row.series !== ""),
          `${id} should name a series on every point`,
        );
      }
      // And one point per label per series: chartPoints reads the distinct x
      // labels, so two rows for one pair land on each other -- a line that
      // doubles back, a bar drawn twice in the same place. The fix is a group by
      // in the source query, never here.
      const seen = new Set<string>();
      for (const row of drawn) {
        const at = JSON.stringify([row.series, row.x]);
        assert(!seen.has(at), `${id} draws two points at ${at}; group its source by the pair it plots`);
        seen.add(at);
      }
      // A box answers five numbers per x rather than a y, and chartBoxes in
      // src/Main.elm drops a row missing any of them -- so a box whose source
      // cannot be aggregated draws nothing rather than a short picture.
      if (doc.kind === "box") {
        for (const row of drawn) {
          for (const name of ["lo", "q1", "med", "q3", "hi"])
            assert(typeof row[name] === "number", `${id} answers ${name} = ${JSON.stringify(row[name])}, not a number`);
          assert(
            (row.lo as number) <= (row.q1 as number) && (row.q1 as number) <= (row.med as number) &&
              (row.med as number) <= (row.q3 as number) && (row.q3 as number) <= (row.hi as number),
            `${id} answers a box at ${JSON.stringify(row.x)} whose five numbers are out of order`,
          );
        }
      }
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

Deno.test("a chart plots a column AlaSQL will not parse bare, in both engines", () => {
  // `total`, `store` and `class` are keywords, and no bundled chart names one,
  // so the brackets chartSql puts round every column it splices are checked
  // here rather than by the replay above -- in both engines, because a chart is
  // drawn from the page's statement and exported from the server's.
  const loaded: Record<string, Row[]> = {
    "table:keyword-chart": [{ total: "a", class: 2, store: 5 }, { total: "b", class: 4, store: 6 }],
  };
  const colsOf = { "table:keyword-chart": ["total", "class", "store"].map((name) => ({ name })) };
  type Settings = { source: string; kind: string; x: string; y: string; series?: string };
  const drawn = (doc: Settings) => {
    const { sql, cells } = scanRefs(chartSql(doc));
    return engines.map(([, engine]) => {
      serveSheets(engine);
      return JSON.stringify(engine(planQuery(sql, cells, loaded, colsOf).sql, [loaded]).data as Row[]);
    });
  };
  for (
    const [doc, expected] of [
      [
        { source: "@table:keyword-chart", kind: "line", x: "total", y: "class", series: "store" },
        `[{"x":"a","y":2,"series":5},{"x":"b","y":4,"series":6}]`,
      ],
      // The box branch is the other shape: it splices the same two names into
      // aggregates, a blank filter and a group by, where every other kind
      // splices them into the select list alone.
      [
        { source: "@table:keyword-chart", kind: "box", x: "total", y: "class" },
        `[{"x":"a","lo":2,"q1":2,"med":2,"q3":2,"hi":2},{"x":"b","lo":4,"q1":4,"med":4,"q3":4,"hi":4}]`,
      ],
    ] as [Settings, string][]
  ) {
    const [onServer, inPage] = drawn(doc);
    assertEquals(onServer, expected, `a ${doc.kind} chart over a keyword column draws the wrong rows`);
    assertEquals(inPage, onServer, `a ${doc.kind} chart over a keyword column draws differently in the page engine`);
  }
});

Deno.test("a cohort table answers what the bundled one was written by hand to answer", () => {
  // `cohortSql` runs once, when the palette makes the sheet, so this is the only
  // place its statement is read. What is compared is the rows and never the
  // text: the whitespace in a generated statement is a promise to nobody.
  const dataset = (DATASETS as { doc_id: string; doc: { data: Row[] } }[]).find((d) => d.doc_id === "orders")!;
  const [cols_, ...rows] = dataset.doc.data;
  const orders = rows.map((row) =>
    Object.fromEntries((Object.values(cols_) as { name: string; key: string }[]).map((c) => [c.name, row[c.key]]))
  );
  checkColumnTypes("table:orders", Object.values(cols_), orders);
  const loaded: Record<string, Row[]> = { "table:orders": orders };
  const colsOf = { "table:orders": Object.keys(orders[0]).map((name) => ({ name })) };
  const answer = (engine: Engine, code: string) => {
    serveSheets(engine);
    const { sql, cells } = scanRefs(code);
    return engine(planQuery(sql, cells, loaded, colsOf).sql, [loaded]).data as Row[];
  };
  const byHand =
    (EXAMPLES as unknown as Record<string, { doc: { data: { code: string }[] } }>)["query:cohort-retention"]
      .doc.data[0].code;
  const written = cohortSql({
    source: "@table:orders",
    date: "ordered_on",
    key: "customer_id",
    value: "revenue",
    grain: "month",
  });
  for (const [name, engine] of engines) {
    const want = answer(engine, byHand);
    const got = answer(engine, written);
    // The two name their counts differently -- the bundled one says `customers`
    // where a generalised table says `active` -- so the columns they share are
    // the cohort, the period and the money, and those are compared row for row.
    const shared = Object.keys(want[0]).filter((col) => Object.hasOwn(got[0], col));
    assertEquals([...shared].sort(), ["cohort", "month_no", "revenue"], `${name}: the two share other columns now`);
    assertEquals(
      got.map((row) => shared.map((col) => row[col])),
      want.map((row) => shared.map((col) => row[col])),
      `${name}: the written cohort table answers different rows from the bundled one`,
    );
  }
});

Deno.test("a cohort table groups by a column AlaSQL will not parse bare, in both engines", () => {
  // Every name a cohort table splices goes through the same `chartIdent` a
  // chart's axes do, and it splices them into a join, a group by and two
  // aggregates -- more places than any chart does.
  const loaded: Record<string, Row[]> = {
    "table:keyword-cohort": [
      { store: "a", class: "2024-01-03", total: 5 },
      { store: "a", class: "2024-02-03", total: 7 },
      { store: "b", class: "2024-02-05", total: 9 },
    ],
  };
  const colsOf = { "table:keyword-cohort": ["store", "class", "total"].map((name) => ({ name })) };
  const drawn = (code: string) =>
    engines.map(([, engine]) => {
      serveSheets(engine);
      const { sql, cells } = scanRefs(code);
      return JSON.stringify(engine(planQuery(sql, cells, loaded, colsOf).sql, [loaded]).data as Row[]);
    });
  const settings = { source: "@table:keyword-cohort", date: "class", key: "store", grain: "month" };
  for (
    const [code, expected] of [
      [
        cohortSql({ ...settings, value: "total" }),
        `[{"cohort":"2024-01","month_no":0,"active":1,"total":5,"total_per_active":5},` +
        `{"cohort":"2024-01","month_no":1,"active":1,"total":7,"total_per_active":7},` +
        `{"cohort":"2024-02","month_no":0,"active":1,"total":9,"total_per_active":9}]`,
      ],
      // No value column is the count-only table: the keys that came back, and
      // nothing about what they were worth.
      [
        cohortSql(settings),
        `[{"cohort":"2024-01","month_no":0,"active":1},{"cohort":"2024-01","month_no":1,"active":1},` +
        `{"cohort":"2024-02","month_no":0,"active":1}]`,
      ],
    ]
  ) {
    const [onServer, inPage] = drawn(code);
    assertEquals(onServer, expected, `a cohort table over a keyword column answers the wrong rows`);
    assertEquals(inPage, onServer, `a cohort table over a keyword column answers differently in the page engine`);
  }
});

Deno.test("a cohort table refuses every field it cannot build from, by name", () => {
  const ok = { source: "@table:orders", date: "ordered_on", key: "customer_id", value: "revenue", grain: "month" };
  for (
    const [settings, said] of [
      [{ ...ok, source: "@chart:spend" }, "A cohort table reads one table or query sheet."],
      [{ ...ok, grain: "monthly" }, "That is not a period to group a cohort by."],
      [{ ...ok, date: 7 }, "A cohort table's date column has to be a column name."],
      [{ ...ok, key: "" }, "A cohort table's key column has to be a column name."],
      [{ ...ok, value: 3 }, "A cohort table's value column has to be a column name."],
      // A column that shares a name with one this statement generates for
      // itself is refused on no engine -- both answer rows, and the rows are
      // wrong: a key named `cohort` joins against the truncated first date and
      // matches nothing, so the table is empty; one named `month_no` overwrites
      // the period number; and a value summed off the key or the date column is
      // text AlaSQL drops, so that field is missing from every row. None of
      // them reads as a mistake anywhere downstream.
      [{ ...ok, key: "cohort" }, "A cohort table's key column has to be a column it does not already name."],
      [{ ...ok, key: "month_no" }, "A cohort table's key column has to be a column it does not already name."],
      [{ ...ok, value: "customer_id" }, "A cohort table's value column has to be a column it does not already name."],
      [{ ...ok, value: "ordered_on" }, "A cohort table's value column has to be a column it does not already name."],
      [{ ...ok, value: "cohort" }, "A cohort table's value column has to be a column it does not already name."],
      [{ ...ok, value: "active" }, "A cohort table's value column has to be a column it does not already name."],
      [{ ...ok, value: "month_no" }, "A cohort table's value column has to be a column it does not already name."],
    ] as unknown as [Parameters<typeof cohortSql>[0], string][]
  ) {
    assertThrows(() => cohortSql(settings), Error, said);
  }
  // And the "did you mean" a near miss earns, since a grain is a word somebody
  // types rather than a column they pick.
  assertThrows(() => cohortSql({ ...ok, grain: "monthly" }), Error, "month");
});

// cohortSql and chartSql share the one chartIdent(), given "cohort table" or
// "chart" as the noun rather than each writing its own checker -- so a chart's
// own refusals, over every field chartIdent and chartSql itself can refuse,
// still say "chart" and never leak the other caller's noun.
Deno.test("a chart refuses every field it cannot build from, by name", () => {
  const ok = { source: "@table:orders", kind: "line", x: "ordered_on", y: "revenue" };
  for (
    const [settings, said] of [
      [{ ...ok, source: "@nope:spend" }, "A chart reads one table or query sheet."],
      [{ ...ok, kind: "pie" }, "That is not a kind of chart."],
      [{ ...ok, x: 7 }, "A chart's x column has to be a column name."],
      [{ ...ok, y: "" }, "A chart's y column has to be a column name."],
      [{ ...ok, y2: 3 }, "A chart's second y column has to be a column name."],
      [{ ...ok, series: 3 }, "A chart's series column has to be a column name."],
      [{ ...ok, kind: "box", series: "region" }, "A box chart is already the spread of its rows"],
      [{ ...ok, kind: "box", y2: "margin" }, "A box chart is already the spread of its rows"],
    ] as unknown as [Parameters<typeof chartSql>[0], string][]
  ) {
    assertThrows(() => chartSql(settings), Error, said);
  }
  assertThrows(() => chartSql({ ...ok, kind: "lin" }), Error, "line");
});

Deno.test("a bracketed select item types the way the bare one does", () => {
  // chartSql quotes every column it names and main.ts types a chart's result
  // columns by reading that same select list back as text, so a pass that sees
  // only an unquoted name costs every chart its types.
  const known = { total: "usd", n: "int" };
  for (
    const [plain, quoted] of [
      ["select total as x from @table:t", "select [total] as x from @table:t"],
      ["select sum(total) as x from @table:t", "select sum([total]) as x from @table:t"],
      ["select avg(n) as x from @table:t", "select avg([n]) as x from @table:t"],
      ["select total, n from @table:t", "select [total], [n] from @table:t"],
    ]
  ) {
    assertEquals(selectTypes(quoted, known), selectTypes(plain, known), `${quoted} types differently from ${plain}`);
  }
  // And a type is really read, rather than two empty answers agreeing.
  const typed = (code: string) => (selectTypes(code, known) as Record<string, string>).x;
  assertEquals(typed("select [total] as x from @table:t"), "usd");
  assertEquals(typed("select avg([n]) as x from @table:t"), "num");
});

Deno.test("an unpivot names its two new columns the way it names the wide ones", () => {
  // The clause's ident pattern admits a bracketed name, which may hold anything
  // but a `]` -- and both of these are spliced back out inside brackets.
  const columnsOf = { "table:wide": [{ name: "team" }, { name: "q1" }, { name: "q2" }] };
  const wide = (spec: string) => `select * from SHEET('table:wide') unpivot (${spec})`;
  assertThrows(
    () => rewriteUnpivot(wide("[a b] for q in (q1, q2)"), columnsOf),
    Error,
    "value column takes a column name",
  );
  assertThrows(
    () => rewriteUnpivot(wide("n for [q-1] in (q1, q2)"), columnsOf),
    Error,
    "name column takes a column name",
  );
  assert(rewriteUnpivot(wide("[n] for [q] in (q1, q2)"), columnsOf).includes("'q1' as [q]"));
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
  // A logistic fit is an iteration with no closed form, and no bundled example
  // runs one, so the two engines are held to the same answer here instead: the
  // same ten points through each engine's own compiler, compared coefficient for
  // coefficient.
  const pass = [0, 0, 0, 1, 0, 1, 1, 1, 1, 1];
  const points = pass.map((p, i) => ({ hours: i + 1, pass: p }));
  const fits = engines.map(([, engine]) =>
    engine("select logit(array(pass), array(hours)) b, ols(array(pass), array(hours)) o from ?", [points]).data[0]
  );
  assertEquals(fits[0], fits[1], "the two engines fit different coefficients");
  // A seeded sampler earns its place only if the same call answers the same
  // number wherever it runs, so the three are compared draw for draw rather
  // than pinned to a literal neither engine promises.
  const draws = engines.map(([, engine]) =>
    engine(
      `select sample_uniform(7, 0, 10) u, sample_normal(7, 100, 15) n,
              sample_triangular(7, 800, 1150, 2000) t from ?`,
      [[{ x: 1 }]],
    ).data[0]
  );
  assertEquals(draws[0], draws[1], "the two engines draw different samples from the same seed");
  // Bounds this far apart overflow a double before the fraction is even
  // applied, and the refusal is shared code, so both engines must name the
  // same overflowed draw rather than one of them answering Infinity quietly.
  const overflows = engines.map(([, engine]) => {
    try {
      engine(`select sample_uniform(0, -1.0e308, 1.0e308) u from ?`, [[{ x: 1 }]]);
      return null;
    } catch (e) {
      return (e as Error).message;
    }
  });
  assert(overflows[0] && overflows[0].includes("a draw that fits in a finite number"), overflows[0] ?? "no error");
  assertEquals(overflows[0], overflows[1], "the two engines disagree about an overflowed draw");
  // Both hosts read a result column's type off SELECT_TYPES, so a fit's
  // coefficient array is json wherever it runs and a prediction is a number.
  assertEquals(
    selectTypes(
      `select ols(array(y), array(x)) as coefs, logit(array(y), array(x)) as odds,
              round(ols_predict(c, 1), 2) as fitted, logit_predict(c, 1) as chance,
              sample_uniform(s, 0, 1) as u, sample_normal(s, 0, 1) as n,
              sample_triangular(s, 0, 1, 2) as t from @table:t`,
      {},
    ),
    { coefs: "json", odds: "json", fitted: "num", chance: "num", u: "num", n: "num", t: "num" },
  );
});

Deno.test("min() and max() over a text column answer, in both engines", () => {
  const t = [
    { key: "0", name: "code", type: "text" },
    { key: "1", name: "population", type: "num" },
    { key: "2", name: "region", type: "enum:north,south" },
    { key: "3", name: "due", type: "date" },
  ];
  const one = { "table:t": t };
  // `code` is text here and a number there, so nothing can say which min() the
  // author meant. A name typed two ways is left alone.
  const two = { "table:t": t, "table:u": [{ key: "0", name: "code", type: "num" }] };

  const cases: [string, string, Record<string, { name: string; type: string }[]>][] = [
    ["select min(code) as lo from SHEET('table:t')", "select min_text(code) as lo from SHEET('table:t')", one],
    ["select max(c.code) from SHEET('table:t') c", "select max_text(c.code) from SHEET('table:t') c", one],
    ["select min([code]) from SHEET('table:t')", "select min_text([code]) from SHEET('table:t')", one],
    ["select min(region) from SHEET('table:t')", "select min_text(region) from SHEET('table:t')", one],
    // Untouched, each for its own reason: a number the engine already compares,
    // an expression the pass cannot resolve to a column, a window applyWindows
    // computes itself and rewriteWindows finds by name, text inside a literal
    // that is not a call at all, and the ambiguous name above.
    ["select min(population) from SHEET('table:t')", "select min(population) from SHEET('table:t')", one],
    ["select min(upper(code)) from SHEET('table:t')", "select min(upper(code)) from SHEET('table:t')", one],
    [
      "select min(code) over (partition by region) from SHEET('table:t')",
      "select min(code) over (partition by region) from SHEET('table:t')",
      one,
    ],
    ["select 'min(code)' as note from SHEET('table:t')", "select 'min(code)' as note from SHEET('table:t')", one],
    ["select min(code) from SHEET('table:t')", "select min(code) from SHEET('table:t')", two],
    // register() defines min_text and MIN_TEXT and nothing between, so the
    // author's casing cannot ride along: `MIN(` rewritten to `MIN_text` died as
    // a raw TypeError, which is a worse answer than the empty column was.
    ["SELECT MIN(code) FROM SHEET('table:t')", "SELECT min_text(code) FROM SHEET('table:t')", one],
    ["Select Max(code) From SHEET('table:t')", "Select max_text(code) From SHEET('table:t')", one],
    // A date reaches the engine as the ISO string the document holds, so AlaSQL
    // drops it the same way, and ISO sorts lexicographically.
    ["select min(due) from SHEET('table:t')", "select min_text(due) from SHEET('table:t')", one],
    // A name the query invents for itself is a number wearing a text column's
    // name: comparing it as text answered "10" for a minimum of 9.
    [
      "select min(code) as m from (select population as code from SHEET('table:t')) z",
      "select min(code) as m from (select population as code from SHEET('table:t')) z",
      one,
    ],
  ];
  for (const [code, want, colsOf] of cases) assertEquals(rewriteExtremes(code, colsOf), want);

  const rows = [
    { code: "no", population: 5, region: "north", due: "2026-03-04" },
    { code: "dz", population: 45, region: "south", due: "2026-01-02" },
  ];

  // The bound, and the reason it is there: an unclosed call costs a scan to the
  // end of the statement, so a body of nothing but `min(` was quadratic.
  assertThrows(
    () => rewriteExtremes(`select ${"min(code), ".repeat(MAX_EXTREMES + 1)}1 from SHEET('table:t')`, one),
    Error,
    `more than ${MAX_EXTREMES}`,
  );
  const started = Date.now();
  rewriteExtremes(`select ${"min(".repeat(20_000)}code from SHEET('table:t')`, one);
  assert(Date.now() - started < 1000, "an unbalanced call should stop the pass, not restart the scan");
  for (const [name, engine] of engines) {
    serveSheets(engine);
    const scanned = scanRefs(
      "select min(code) as lo, MAX(code) as hi, min(population) as small, min(due) as first from @table:t",
    );
    const plan = planQuery(scanned.sql, scanned.cells, { "table:t": rows }, one);
    assertEquals(
      engine(plan.sql, [{ "table:t": rows }]).data,
      [{ lo: "dz", hi: "no", small: 5, first: "2026-01-02" }],
      `${name}: a text min() should answer rather than drop its column`,
    );

    // The documented cost of the rewrite: an unaliased call is renamed with its
    // column, so the answer arrives under min_text(code).
    const plain = scanRefs("select min(code) from @table:t");
    const bare = planQuery(plain.sql, plain.cells, { "table:t": rows }, one);
    assertEquals(engine(bare.sql, [{ "table:t": rows }]).data, [{ "min_text(code)": "dz" }], name);
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
