// Shared by both AlaSQL engines: the server imports npm:alasql (main.ts), the
// page imports the vendored /alasql.mjs (index.html). Both call register() with
// their own instance, so a query behaves the same wherever it runs.

// --- errors

const fail = (what, expected, received, fix) =>
  new Error([
    `${what} received ${received}.`,
    ``,
    `  Expected: ${expected}`,
    `  Fix:      ${fix}`,
  ].join("\n"));

// The house error shape: a headline, then aligned expected/received/source/fix
// fields. One formatter so every message in both engines reads the same way.
export const explain = (headline, fields) =>
  [
    headline,
    ``,
    ...Object.entries(fields)
      .filter(([, v]) => v !== undefined && v !== null)
      .map(([k, v]) => `  ${(k + ":").padEnd(13)}${v}`),
  ].join("\n");

// JSON.stringify() renders Infinity and NaN as "null", so a message about a
// number that is not finite used to read "received number null" -- which names
// neither the value nor the problem.
// A refusal quotes what arrived, and what arrived is whatever a caller sent. A
// megabyte of it is a refusal nobody reads and a log line nothing survives, so
// the quote is bounded and says how much it is not showing.
const SHOWN_MAX = 200;
const shorten = (text) => text.length <= SHOWN_MAX ? text : `${text.slice(0, SHOWN_MAX)}… (${text.length} characters)`;
export const show = (v) =>
  v === null
    ? "null"
    : v === undefined
    ? "nothing"
    : typeof v === "number" && !Number.isFinite(v)
    ? `number ${v}`
    : shorten(`${typeof v} ${JSON.stringify(v)}`);

// A function that receives nothing at all has two causes and they look identical
// from inside it: the row has no such column, or the call sits in a `group by`,
// which AlaSQL evaluates against an empty row rather than against each group.
// Naming only the first would send someone hunting a typo that is not there.
const MISSING = "check the column name, or move the call into a subquery and group by the column it produces";

// An empty cell. Number("") is 0 and Number(" ") is 0, so every place that
// reached for a number without asking this first read a reading nobody took as a
// reading of zero. checkColumnTypes() is the one place a blank turns into a
// null; everywhere else it is refused or skipped, never converted.
const absent = (v) => v === null || v === undefined || (typeof v === "string" && v.trim() === "");

const str = (fn, arg, v) => {
  if (typeof v !== "string") {
    throw fail(
      `${fn}() argument ${arg}`,
      "a text value",
      show(v),
      v === undefined ? MISSING : `cast it with cast(x as string)`,
    );
  }
  return v;
};

const nums = (fn, arg, v) => {
  if (!Array.isArray(v)) {
    throw fail(
      `${fn}() argument ${arg}`,
      "an array of numbers",
      show(v),
      v === undefined ? MISSING : `build one with array(x), e.g. ${fn}(array(x), array(y))`,
    );
  }
  return v.map((n) => {
    const f = absent(n) ? null : typeof n === "string" ? Number(n) : n;
    if (typeof f !== "number" || !Number.isFinite(f)) {
      throw fail(
        `${fn}() argument ${arg}`,
        "only finite numbers",
        show(n),
        n === undefined ? MISSING : "filter the blanks out with a where clause",
      );
    }
    return f;
  });
};

const num = (fn, arg, v) => {
  const n = absent(v) ? null : typeof v === "string" ? Number(v) : v;
  if (typeof n !== "number" || !Number.isFinite(n)) {
    throw fail(
      `${fn}() argument ${arg}`,
      "a finite number",
      show(v),
      v === undefined ? MISSING : "filter the blanks out with a where clause",
    );
  }
  return n;
};

const pair = (fn, xs_, ys_) => {
  const xs = nums(fn, 1, xs_), ys = nums(fn, 2, ys_);
  if (xs.length !== ys.length) {
    throw fail(
      `${fn}()`,
      "two arrays of the same length",
      `${xs.length} and ${ys.length} values`,
      "aggregate both columns over the same rows",
    );
  }
  if (xs.length < 2) throw fail(`${fn}()`, "at least 2 pairs", `${xs.length}`, "widen the query so more rows match");
  return [xs, ys];
};

// --- dates
//
// Everything is UTC. AlaSQL's own now()/dateadd() throw on its internal date
// wrapper, so these replace them rather than extend them.

const UNITS = ["year", "quarter", "month", "week", "day", "hour", "minute", "second"];
const DAY = 86400000;

const date = (fn, v) => {
  const d = v instanceof Date ? v : new Date(typeof v === "number" ? v : str(fn, "date", v));
  if (Number.isNaN(d.getTime())) {
    throw fail(
      `${fn}() date`,
      "an ISO timestamp, e.g. '2026-08-16' or '2026-08-16T12:00:00Z'",
      show(v),
      v === undefined ? MISSING : "check the column's type row",
    );
  }
  return d;
};

// A fiscal year is named for the calendar year it ENDS in, unless it starts in
// January: US federal FY2027 runs 2026-10-01 to 2027-09-30. `start` is the
// calendar month the year begins in, 1-12, and is required because no default
// is right for more than one organisation.
const fiscal = (fn, ts, start) => {
  if (!Number.isInteger(start) || start < 1 || start > 12) {
    throw fail(
      `${fn}() argument 2`,
      "a start month from 1 to 12",
      show(start),
      `pass the month the fiscal year begins, e.g. ${fn}(created_at, 10) for an October start`,
    );
  }
  const d = date(fn, ts);
  return { year: d.getUTCFullYear(), month: d.getUTCMonth(), into: (d.getUTCMonth() - (start - 1) + 12) % 12, start };
};

const unit = (fn, u) => {
  const l = str(fn, "unit", u).toLowerCase();
  if (!UNITS.includes(l)) throw fail(`${fn}() unit`, UNITS.join(", "), show(u), `use one of: ${UNITS.join(", ")}`);
  return l;
};

const truncate = (u, d) => {
  const [y, m, day] = [d.getUTCFullYear(), d.getUTCMonth(), d.getUTCDate()];
  if (u === "year") return new Date(Date.UTC(y, 0, 1));
  if (u === "quarter") return new Date(Date.UTC(y, Math.floor(m / 3) * 3, 1));
  if (u === "month") return new Date(Date.UTC(y, m, 1));
  // ISO week: Monday starts the week, so Sunday (0) is 6 days in.
  if (u === "week") return new Date(Date.UTC(y, m, day - ((d.getUTCDay() + 6) % 7)));
  if (u === "day") return new Date(Date.UTC(y, m, day));
  if (u === "hour") return new Date(Date.UTC(y, m, day, d.getUTCHours()));
  if (u === "minute") return new Date(Date.UTC(y, m, day, d.getUTCHours(), d.getUTCMinutes()));
  return new Date(Date.UTC(y, m, day, d.getUTCHours(), d.getUTCMinutes(), d.getUTCSeconds()));
};

const shift = (u, n, d) => {
  const [y, m, day] = [d.getUTCFullYear(), d.getUTCMonth(), d.getUTCDate()];
  const rest = [d.getUTCHours(), d.getUTCMinutes(), d.getUTCSeconds(), d.getUTCMilliseconds()];
  if (u === "year") return new Date(Date.UTC(y + n, m, day, ...rest));
  if (u === "quarter") return new Date(Date.UTC(y, m + n * 3, day, ...rest));
  if (u === "month") return new Date(Date.UTC(y, m + n, day, ...rest));
  const ms = { week: 7 * DAY, day: DAY, hour: 3600000, minute: 60000, second: 1000 }[u];
  return new Date(d.getTime() + n * ms);
};

// Whole units from a to b, so date_diff('day', x, x) is 0 and never negative-zero.
const diff = (u, a, b) => {
  if (u === "year") return b.getUTCFullYear() - a.getUTCFullYear();
  if (u === "quarter") {
    return (b.getUTCFullYear() - a.getUTCFullYear()) * 4 +
      (Math.floor(b.getUTCMonth() / 3) - Math.floor(a.getUTCMonth() / 3));
  }
  if (u === "month") return (b.getUTCFullYear() - a.getUTCFullYear()) * 12 + (b.getUTCMonth() - a.getUTCMonth());
  const ms = { week: 7 * DAY, day: DAY, hour: 3600000, minute: 60000, second: 1000 }[u];
  return Math.trunc((b.getTime() - a.getTime()) / ms);
};

// --- text similarity

export const levenshtein = (a, b) => {
  if (a === b) return 0;
  if (!a.length || !b.length) return a.length || b.length;
  let prev = Array.from({ length: b.length + 1 }, (_, i) => i);
  for (let i = 1; i <= a.length; i++) {
    const row = [i];
    for (let j = 1; j <= b.length; j++)
      row[j] = Math.min(prev[j] + 1, row[j - 1] + 1, prev[j - 1] + (a[i - 1] === b[j - 1] ? 0 : 1));
    prev = row;
  }
  return prev[b.length];
};

const trigrams = (s) => {
  const p = `  ${s.toLowerCase()} `;
  return new Set(Array.from({ length: Math.max(0, p.length - 2) }, (_, i) => p.slice(i, i + 3)));
};

const tokens = (s) => new Set(s.toLowerCase().split(/[^a-z0-9]+/i).filter(Boolean));

const jaccard = (a, b) => {
  if (!a.size && !b.size) return 1;
  let shared = 0;
  for (const x of a) if (b.has(x)) shared++;
  return shared / (a.size + b.size - shared);
};

const soundex = (s) => {
  const up = s.toUpperCase().replace(/[^A-Z]/g, "");
  if (!up) return "";
  const code = (ch) =>
    "BFPV".includes(ch)
      ? "1"
      : "CGJKQSXZ".includes(ch)
      ? "2"
      : "DT".includes(ch)
      ? "3"
      : ch === "L"
      ? "4"
      : "MN".includes(ch)
      ? "5"
      : ch === "R"
      ? "6"
      : "";
  let out = up[0], last = code(up[0]);
  for (const ch of up.slice(1)) {
    const c = code(ch);
    // H and W are transparent: they do not break a run of same-coded letters.
    if (c && c !== last) out += c;
    if (!"HW".includes(ch)) last = c;
    if (out.length === 4) break;
  }
  return out.padEnd(4, "0");
};

// --- statistics

const sorted = (xs) => [...xs].sort((a, b) => a - b);

// Linear interpolation between the bracketing ranks, matching percentile_cont.
const quantile = (xs, p) => {
  const s = sorted(xs), i = (s.length - 1) * p, lo = Math.floor(i);
  return lo === s.length - 1 ? s[lo] : s[lo] + (s[lo + 1] - s[lo]) * (i - lo);
};

const mean = (xs) => xs.reduce((a, b) => a + b, 0) / xs.length;

const fit = (xs, ys) => {
  const [mx, my] = [mean(xs), mean(ys)];
  let sxy = 0, sxx = 0, syy = 0;
  for (let i = 0; i < xs.length; i++) {
    sxy += (xs[i] - mx) * (ys[i] - my);
    sxx += (xs[i] - mx) ** 2;
    syy += (ys[i] - my) ** 2;
  }
  return { mx, my, sxy, sxx, syy };
};

// Sample variance, the n-1 kind: a sheet holds a sample, not a population.
const variance = (xs) => {
  const m = mean(xs);
  return xs.reduce((a, b) => a + (b - m) ** 2, 0) / (xs.length - 1);
};

// Student's t, for t_test() and the mean's confidence interval. There is no
// closed form for either, so both go through the regularized incomplete beta,
// and the interval inverts it by bisection rather than carrying a table that
// only covers the degrees of freedom it happens to list.

// Lanczos approximation, g=7.
const logGamma = (x) => {
  const g = [
    676.5203681218851,
    -1259.1392167224028,
    771.32342877765313,
    -176.61502916214059,
    12.507343278686905,
    -0.13857109526572012,
    9.9843695780195716e-6,
    1.5056327351493116e-7,
  ];
  if (x < 0.5) return Math.log(Math.PI / Math.sin(Math.PI * x)) - logGamma(1 - x);
  let a = 0.99999999999980993;
  for (let i = 0; i < g.length; i++) a += g[i] / (x - 1 + i + 1);
  const t = x - 1 + 7.5;
  return 0.5 * Math.log(2 * Math.PI) + (x - 0.5) * Math.log(t) - t + Math.log(a);
};

// Continued fraction for the incomplete beta (Lentz's method).
const betacf = (a, b, x) => {
  const tiny = 1e-300, qab = a + b, qap = a + 1, qam = a - 1;
  let c = 1, d = 1 - (qab * x) / qap;
  if (Math.abs(d) < tiny) d = tiny;
  d = 1 / d;
  let h = d;
  for (let m = 1; m <= 300; m++) {
    const m2 = 2 * m;
    let aa = (m * (b - m) * x) / ((qam + m2) * (a + m2));
    d = 1 + aa * d, c = 1 + aa / c;
    if (Math.abs(d) < tiny) d = tiny;
    if (Math.abs(c) < tiny) c = tiny;
    d = 1 / d, h *= d * c;
    aa = (-(a + m) * (qab + m) * x) / ((a + m2) * (qap + m2));
    d = 1 + aa * d, c = 1 + aa / c;
    if (Math.abs(d) < tiny) d = tiny;
    if (Math.abs(c) < tiny) c = tiny;
    d = 1 / d;
    const del = d * c;
    h *= del;
    if (Math.abs(del - 1) < 1e-15) return h;
  }
  throw fail(
    "the t distribution",
    "a continued fraction that settles within 300 steps",
    `a=${a}, b=${b}, x=${x}`,
    "this is a bug in sql.mjs, not in the query: report these three numbers",
  );
};

const betaInc = (a, b, x) => {
  if (x <= 0) return 0;
  if (x >= 1) return 1;
  const front = Math.exp(logGamma(a + b) - logGamma(a) - logGamma(b) + a * Math.log(x) + b * Math.log(1 - x));
  return x < (a + 1) / (a + b + 2) ? (front * betacf(a, b, x)) / a : 1 - (front * betacf(b, a, 1 - x)) / b;
};

// The two-sided tail: the share of a t distribution further from zero than t.
const tTail = (t, df) => betaInc(df / 2, 0.5, df / (df + t * t));

// The multiplier for a two-sided interval, by bisection on tTail, which falls
// as t rises. 1000 covers every level a sheet can ask for at df >= 1.
const tCrit = (level, df) => {
  let lo = 0, hi = 1000;
  for (let i = 0; i < 200; i++) {
    const mid = (lo + hi) / 2;
    if (hi - lo < 1e-9) return mid;
    if (tTail(mid, df) > 1 - level) lo = mid;
    else hi = mid;
  }
  throw fail(
    "a confidence interval",
    "a t value that settles within 200 steps",
    `level=${level}, df=${df}`,
    "this is a bug in sql.mjs, not in the query: report both numbers",
  );
};

// --- @sheet references
//
// One scanner for both engines, so `@type:doc_id` means the same thing on the
// server and in the page. The colon is required, which is what keeps AlaSQL's
// own `@params` variable from being mistaken for a sheet reference.
//
// `@type:doc_id.column` is a cell: the one value in that column, from a sheet
// holding a single row. It becomes a scalar subquery, which AlaSQL evaluates
// once, so a query sheet reads its parameters out of cells the same way a
// spreadsheet reads an assumptions block. Nothing is spliced into the SQL as a
// literal, so nothing needs escaping.
export const scanRefs = (code) => {
  let out = "", inStr = false;
  const ids = [], cells = [];
  for (let i = 0; i < code.length; i++) {
    const ch = code[i];
    if (ch === "'") inStr = !inStr;
    const ref = !inStr && ch === "@"
      ? code.slice(i).match(/^@([a-z-]+:[A-Za-z0-9_-]+)(?:\.([A-Za-z_][A-Za-z0-9_]*))?/)
      : null;
    if (ref) {
      ids.push(ref[1]);
      if (ref[2]) cells.push({ id: ref[1], column: ref[2] });
      out += ref[2] ? `(select ${ref[2]} from SHEET('${ref[1]}'))` : `SHEET('${ref[1]}')`;
      i += ref[0].length - 1;
    } else { out += ch; }
  }
  return { sql: out, ids, cells };
};

// A cell reference is only meaningful over one row, and AlaSQL would answer a
// two-row sheet with whichever row it reached first. Both engines run this
// after loading, so the same query fails the same way in each.
export const checkCells = (cells, rowsOf, colsOf = {}) => {
  for (const { id, column } of cells) {
    const rows = rowsOf[id] ?? [];
    if (rows.length !== 1) {
      throw new Error(explain(`A cell reference reads one row, and @${id} does not hold exactly one.`, {
        Expected: "1 row",
        Received: `${rows.length} rows`,
        Source: `@${id}.${column} in this query`,
        Fix: "point it at a sheet holding a single row of settings, or filter that sheet to one row in its own query",
      }));
    }
    const names = colsOf[id]?.map((col) => col.name) ?? Object.keys(rows[0] ?? {});
    if (names.includes(column)) continue;
    const hit = nearest(column, names);
    throw new Error(explain(`The sheet @${id} has no column named "${column}".`, {
      ...(hit ? { "Did you mean": `@${id}.${hit}` } : { Available: names.join(", ") || "(no columns)" }),
      Source: `@${id}.${column} in this query`,
      Fix: hit ? `write @${id}.${hit}` : "check the column names in the sheet's type row",
    }));
  }
};

const MAX_REF_DEPTH = 8;

// Bounds the @query -> @query chain. A cycle is reported as the path that closes
// it, because "nested too deeply" sends you looking for the wrong problem.
const checkRefPath = (path, id) => {
  if (path.includes(id)) {
    throw new Error(
      `Query references form a cycle: ${
        [...path, id].map((s) => "@" + s).join(" -> ")
      }.\n\nBreak the loop by pointing one of these at a table instead.`,
    );
  }
  if (path.length >= MAX_REF_DEPTH) {
    throw new Error(
      `Query references nested more than ${MAX_REF_DEPTH} deep: ${
        [...path, id].map((s) => "@" + s).join(" -> ")
      }.\n\nFlatten the chain, or materialize an intermediate step as a table.`,
    );
  }
};

// --- schema introspection
//
// `describe @table:abc` is not SQL AlaSQL can parse, so both engines intercept it
// before the engine sees the text. One statement, one ref, nothing else: the
// point is to answer "what columns does this sheet have" without writing a query
// that guesses. `explain <query>` below is the other intercepted statement.

export const describeRef = (code) =>
  code.trim().replace(/;+$/, "").match(/^describe\s+@([a-z-]+:[A-Za-z0-9._-]+)\s*$/i)?.[1];

export const DESCRIBE_COLUMNS = ["column", "type", "rows", "nulls", "sample"];

export const describeRows = (id, cols, rows) => {
  if (!cols.length) {
    throw new Error(explain(`The sheet "@${id}" has no columns to describe.`, {
      Received: `${rows.length} rows and an empty column row`,
      Source: "data[0] of the referenced sheet",
      Fix: "add a column to the sheet, then describe it again",
    }));
  }
  return cols.map((col) => {
    const vals = rows.map((row) => row[col.name]);
    const filled = vals.filter((v) => v !== null && v !== undefined && v !== "");
    return {
      column: col.name,
      type: typeof col.type === "string" ? col.type : JSON.stringify(col.type),
      rows: rows.length,
      nulls: rows.length - filled.length,
      sample: filled.length ? String(filled[0]).slice(0, 60) : null,
    };
  });
};

// --- profiling
//
// `explain <query>` runs the query and answers with where the time went, one
// row per stage. Both engines wrap the calls they already make in `timed`, which
// does nothing without a stage list, so a profiled run and a plain run are one
// code path. One `plan` row rather than one per pass: the passes are regex over
// the text and their split is nothing an author can act on. A `load` of a
// @query ref includes the nested run on both hosts, which is the point -- it
// names the ref that is slow. `total` is wall clock rather than a sum, so
// anything a host does between stages still shows up.

export const profileRef = (code) => {
  // A bare `explain` is claimed too: AlaSQL has an EXPLAIN of its own, and
  // what it says about one is a TypeError naming an internal property.
  const match = code.trim().match(/^explain(?:\s+([\s\S]+?))?\s*;*\s*$/i);
  if (!match) return undefined;
  const inner = match[1] ?? "";
  if (!inner || describeRef(inner) || /^explain\s/i.test(inner)) {
    throw new Error(explain(`explain profiles a query, and this is not one.`, {
      Expected: "explain select …",
      Received: inner ? shorten(inner) : "nothing after explain",
      Source: "the statement after explain",
      Fix: "explain a select; describe already answers without running anything",
    }));
  }
  return inner;
};

export const PROFILE_COLUMNS = ["stage", "rows_in", "rows_out", "ms"];

const tenths = (ms) => Math.round(ms * 10) / 10;

export const timed = async (stages, stage, rows_in, fn, count) => {
  if (!stages) return await fn();
  const t0 = performance.now();
  const out = await fn();
  stages.push({ stage, rows_in, rows_out: count(out), ms: tenths(performance.now() - t0) });
  return out;
};

/** Rows loaded across every sheet a run named: what the stages after the loads
 * take in. Counted off the docs rather than parsed back out of a stage name. */
export const loadedOf = (docs) => Object.values(docs).reduce((n, rows) => n + rows.length, 0);

export const profileRows = (stages, docs, started) => {
  if (!stages.length) throw new Error("profileRows: a profile with no stages; the plan and engine stages always run");
  return [
    ...stages,
    {
      stage: "total",
      rows_in: loadedOf(docs),
      rows_out: stages.at(-1).rows_out,
      ms: tenths(performance.now() - started),
    },
  ];
};

// --- type mismatch, and the one coercion table
//
// A numeric column holding "n/a" reaches the engine as a string, and every sum
// over it is wrong without saying so. The check belongs on the source sheet, not
// the result: the sheet is the only place the declared type and the row number
// both exist.
//
// The same pass is where a cell becomes what its column says it is, because the
// declared type and the cell are only both in hand here. Two coercions, and no
// others anywhere: a blank becomes null, and a numeric string becomes its
// number. Both exist because AlaSQL computes with the raw JavaScript value and
// gets a silently wrong answer otherwise — avg() over [1, "", 2, "", 3] answers
// 1.2, counting each blank as a reading of zero, and avg() over ["1","2","3"]
// answers 41, because "+" concatenated them into "123" first. A null it already
// treats as absent, which is why null is what a blank becomes.
//
// Null, empty and zero stay three different things after this: only a numeric
// column is touched, so an empty string in a text column is still the empty
// string, and a blank that reaches a function anyway came from a text column or
// a literal and is refused by name rather than read as a zero.

// Every type a column may declare, in the one spelling the stack writes, and
// what each one is: `json` is the JSON Schema shape GET /openapi reports, and
// `numeric` is whether a cell has to be a number to hold it.
//
// One table, because this was five hand-kept lists and they had already
// drifted. The page spelled a percentage "pct" and nothing else in the stack
// knew the word, so a percent column typed in the editor stopped being
// type-checked at all -- blanks stayed blanks and avg() counted each one as a
// reading of zero, which is the exact failure the pass below exists to stop.
export const COLUMN_TYPES = {
  text: { json: { type: "string" } },
  num: { json: { type: "number" }, numeric: true },
  int: { json: { type: "integer" }, numeric: true },
  float: { json: { type: "number" }, numeric: true },
  usd: { json: { type: "number" }, numeric: true },
  percentage: { json: { type: "number" }, numeric: true },
  duration: { json: { type: "number" }, numeric: true },
  bool: { json: { type: "boolean" } },
  date: { json: { type: "string", format: "date" } },
  timestamp: { json: { type: "string", format: "date-time" } },
  json: { json: { type: "string" } },
  link: { json: { type: "string", format: "uri" } },
  image: { json: { type: "string", format: "uri" } },
  sheet_id: { json: { type: "string" } },
  form: { json: { type: "string" } },
  create: { json: { type: "string" } },

  // Spellings older documents hold, each read as the type it names and never
  // written. They are in this table rather than beside it because everything
  // that asks "is this a column type" has to say yes to them: a sheet spelling
  // its column `pct` is a sheet somebody made, and refusing to query what the
  // page still renders is the same drift as before, pointed the other way. It
  // also puts them in NUMERIC_TYPES, so a legacy percent column is checked --
  // which is the whole bug, fixed for the documents that actually have it.
  number: { as: "num" },
  string: { as: "text" },
  pct: { as: "percentage" },
  percent: { as: "percentage" },
  datetime: { as: "timestamp" },
};

// Checked when this module loads, because every way this table can be wrong is
// a column that quietly stops being checked -- the one failure it exists to
// prevent. An alias names a type and never another alias, so `canonicalType`
// resolves exactly one level and has no depth to bound.
for (const [type, spec] of Object.entries(COLUMN_TYPES)) {
  const named = spec.as === undefined ? undefined : COLUMN_TYPES[spec.as];
  if (spec.as !== undefined && !named)
    throw new Error(`COLUMN_TYPES: "${type}" is an alias of "${spec.as}", which is not in this table.`);
  if (named?.as !== undefined) {
    throw new Error(
      `COLUMN_TYPES: "${type}" is an alias of "${spec.as}", which is itself an alias. An alias names a type.`,
    );
  }
  if (spec.as === undefined && !spec.json)
    throw new Error(`COLUMN_TYPES: "${type}" is a type and states no JSON shape.`);
}

/** What a spelling means: itself, or the type an alias names. */
export const canonicalType = (type) => COLUMN_TYPES[type]?.as ?? type;

// The spellings anything may write. An alias is read and never written, so a
// second one never enters a document -- `columnTypes` in src/Main.elm is this
// list on the other side of the wire.
export const CANONICAL_TYPES = Object.keys(COLUMN_TYPES).filter((type) => !COLUMN_TYPES[type].as);

// The column types a cell has to be a number to hold, aliases included. Derived
// rather than written beside COLUMN_TYPES: two spellings of one fact is how
// "pct" happened.
export const NUMERIC_TYPES = Object.keys(COLUMN_TYPES).filter((type) => COLUMN_TYPES[canonicalType(type)].numeric);

// The column types AlaSQL's own min()/max() drop: every one whose cells reach
// the engine as a string. Read off the JSON shape rather than listed beside
// COLUMN_TYPES -- a second list is exactly how "pct" happened.
//
// `date` and `timestamp` are in. AlaSQL compares a Date, and a cell is never
// one: nothing coerces a date column, so it arrives as the ISO string the
// document holds, and `min(due)` -- the commonest min() a spreadsheet has --
// dropped its column like any other text. ISO 8601 sorts lexicographically, so
// comparing it as text is the same order, and the alternative on offer is no
// answer at all.
//
// `bool` is out: its cells are not strings, and turning min(flag) into "false"
// would be a new wrong answer in place of an old one.
export const TEXT_TYPES = Object.keys(COLUMN_TYPES).filter((type) =>
  COLUMN_TYPES[canonicalType(type)].json.type === "string"
);

/** A type whose values sort as text: the list above, plus the enum family, which
 * carries its options in its name the way knownType() reads it. */
const textType = (type) => TEXT_TYPES.includes(type) || /^enum:.+/.test(type);

/** `enum:a,b` carries its own options, so it is a family rather than a name and
 * is matched by its prefix. */
export const knownType = (type) => Object.hasOwn(COLUMN_TYPES, type) || /^enum:.+/.test(type);

export const checkColumnTypes = (id, cols, rows) => {
  for (const col of cols) {
    // A type nobody knows used to take the `continue` below, which is a column
    // that quietly stops being checked at all -- exactly how a percent column
    // spelled "pct" came to sum its blanks as zeros. `describe @ref` skips this
    // pass, so a sheet refused here is still the one statement that inspects it.
    if (!knownType(col.type)) {
      // Matched against every spelling and answered with what it means: the
      // nearest thing to `PCT` is `pct`, and what a sheet should say is
      // `percentage`. Searching the canonical half alone sent `PCT` to `int`.
      const meant = nearest(String(col.type), Object.keys(COLUMN_TYPES));
      throw new Error(explain(`Column "${col.name}" of @${id} declares a type nobody knows.`, {
        Expected: `one of ${CANONICAL_TYPES.join(", ")}, or enum: followed by the options`,
        Received: show(col.type),
        Source: `data[0] of @${id}, column "${col.name}"`,
        "Did you mean": meant && canonicalType(meant),
        Fix: meant
          ? `set that column's type to ${canonicalType(meant)}, or run describe @${id} to see the sheet as it stands`
          : `set that column's type to one of those, or run describe @${id} to see the sheet as it stands`,
      }));
    }
    if (!NUMERIC_TYPES.includes(col.type)) continue;
    const duration = canonicalType(col.type) === "duration";
    for (let i = 0; i < rows.length; i++) {
      const v = rows[i][col.name];
      if (absent(v)) {
        rows[i][col.name] = null;
        continue;
      }
      if (typeof v === "number") continue;
      // Finite, not merely a number: "Infinity" parses and then serializes to
      // null in every export, which is a blank the sheet never held.
      if (typeof v === "string" && Number.isFinite(Number(v.trim()))) {
        rows[i][col.name] = Number(v.trim());
        continue;
      }
      // A duration also reads the h:mm[:ss] that formatNumber in src/Main.elm
      // writes, so a cell typed the way it is shown lands as its seconds.
      const clock = duration && typeof v === "string" ? /^(-?)(\d+):([0-5]\d)(?::([0-5]\d))?$/.exec(v.trim()) : null;
      const seconds = clock &&
        (clock[1] ? -1 : 1) * (Number(clock[2]) * 3600 + Number(clock[3]) * 60 + Number(clock[4] ?? 0));
      if (Number.isFinite(seconds)) {
        rows[i][col.name] = seconds;
        continue;
      }
      throw new Error(explain(`Column "${col.name}" of @${id} holds a value its type does not allow.`, {
        Expected: duration
          ? `${col.type}, so a number of seconds, h:mm, h:mm:ss or a blank`
          : `${col.type}, so a number or a blank`,
        Received: show(v),
        Source: `row ${i + 1} of @${id}, column "${col.name}"`,
        Fix: `clear that cell, or change the column's type to text`,
      }));
    }
  }
};

// --- cost guards
//
// A single-threaded engine cannot be preempted mid-query, so the only guard that
// actually prevents a runaway is the one applied before the engine starts: cap
// the rows loaded, not the time spent. MAX_QUERY_MS bounds how long a caller
// waits for an answer; the work itself still finishes in the background.
//
// Loaded rows are not the whole cost: a join walks the product of its inputs,
// and a five-way self-join of a 200-row sheet is three hundred billion pairs
// the engine materializes before a where clause sees one -- a native
// out-of-memory, with every request in flight. MAX_JOIN_ROWS bounds that
// product, counted over every occurrence in the from clause so a self-join
// multiplies. It is the pairs walked, not the rows kept: a keyed join over big
// sheets pays it too, and the fix is the same, filter each sheet first. The
// bound sits above every bundled demo, which examples_test.ts proves.

export const MAX_QUERY_ROWS = 200_000;
export const MAX_QUERY_MS = 15_000;
export const MAX_JOIN_ROWS = 10_000_000;

export const checkQueryRows = (total, id) => {
  if (total <= MAX_QUERY_ROWS) return total;
  throw new Error(explain(`This query loads more rows than one run is allowed.`, {
    Received: `${total} rows, the last of them from @${id}`,
    Limit: `${MAX_QUERY_ROWS} rows across every @sheet in one query`,
    Source: "the @sheet refs in this query",
    Fix: "filter the large sheet in its own query sheet, then reference that instead",
  }));
};

export const checkJoinRows = (sql, docs) => {
  // Every SHEET('id') in the scanned SQL is one from-clause occurrence; a cell
  // ref's scalar subquery is one too, over one row, so it multiplies by one.
  const walked = [...sql.matchAll(/SHEET\('([^']+)'\)/g)].map((m) => m[1]).filter((id) => docs[id]);
  const product = walked.reduce((n, id) => n * docs[id].length, 1);
  if (product <= MAX_JOIN_ROWS) return;
  throw new Error(explain(`This query joins more rows than one run is allowed.`, {
    Received: `${product} pairs to walk: ${walked.map((id) => `@${id} (${docs[id].length})`).join(" × ")}`,
    Limit: `${MAX_JOIN_ROWS} pairs across the from clause of one query, counted before any where or on`,
    Source: "the @sheet refs joined in this query",
    Fix: "filter each large sheet in its own query sheet first, then join those",
  }));
};

// --- error formatting

export const nearest = (name, known) => {
  const scored = known
    .filter((k) => k && k !== name)
    .map((k) => ({ k, d: levenshtein(name.toLowerCase(), k.toLowerCase()) }))
    .sort((a, b) => a.d - b.d)[0];
  // Past a third of the name's length the "suggestion" is noise, not a typo fix.
  return scored && scored.d <= Math.max(2, Math.ceil(name.length / 3)) ? scored.k : undefined;
};

// Turns an engine error into a message that points at the offending token.
export const formatQueryError = (error, code) => {
  const msg = error?.message || String(error);
  // AlaSQL discards an exception thrown from a function while an aggregate sits
  // in the same select list, and reports this instead. The message that said
  // what was wrong is gone by the time it reaches us, so the best that can be
  // done is to name what happened and say how to read the real one.
  if (/Cannot read propert(y|ies) of null \(reading 'data'\)/.test(msg)) {
    return explain(`A function in this query failed, and the engine dropped its message.`, {
      Received: msg,
      Cause: "a function threw while a subquery in the from clause was being computed",
      Fix: "read the subquery's rows into a sheet, or run the function on its own, to see what it says",
    });
  }
  const lines = code.split("\n");
  let out = msg;

  const line = msg.match(/line\s+(\d+)/i);
  const pos = msg.match(/position\s+(\d+)/i);
  let n = line ? parseInt(line[1], 10) : null;
  let col = null;
  if (pos && !n) {
    let count = 0;
    for (let i = 0; i < lines.length; i++) {
      if (count + lines[i].length >= parseInt(pos[1], 10)) {
        n = i + 1;
        col = parseInt(pos[1], 10) - count;
        break;
      }
      count += lines[i].length + 1;
    }
  }
  if (n && n <= lines.length) {
    out = `Line ${n}${col ? `:${col}` : ""}: ${out}`;
    out += `\n\n  ${n} │ ${lines[n - 1]}`;
    if (col) out += `\n    │ ${" ".repeat(col - 1)}^`;
  }
  return out;
};

// AlaSQL does not reject an unknown column: `select populaton from ...` returns
// a column of undefined for every row, so a typo reads as "no data" instead of
// an error. Catch that here and name the column, with the nearest real one.
//
// Aliased columns are exempt. `select null as note` also yields undefined, and
// wrongly rejecting a working query is worse than missing a typo the author
// went out of their way to name.
//
// The one list of words a name in a statement's own text is not a column by.
// Widened past the clause keywords the alias capture below needed so namesIn()
// can read it too: an alias spelled as one of the added words is now read as no
// alias, which costs the min()/max() message below on `min(x) as end` and buys
// one keyword list here instead of two that drift. Nothing whose reading is
// also a plausible column name is on it -- QUALIFY_WORDS holds `date`, `int`
// and `number`, and a sheet has columns by all three names.
const KEYWORD =
  /^(select|from|where|group|by|order|having|limit|offset|top|join|inner|left|right|full|outer|cross|natural|on|using|and|or|not|in|is|null|like|between|exists|case|when|then|else|end|as|asc|desc|distinct|all|union|intersect|except|into|values|over|partition|qualify|unpivot|pivot|for|with)$/i;

export const checkResultColumns = (cols, rows, known = [], code = "") => {
  if (!rows.length) return;
  const aliased = new Set(
    [...code.matchAll(/\bas\s+["'`[]?([A-Za-z_][A-Za-z0-9_]*)/gi)].map((m) => m[1]),
  );
  // Every min()/max() in the query, by the name its column will carry: the alias
  // if it has one, otherwise AlaSQL's own MIN(expr) spelling. One level of
  // nesting is allowed in the argument, because `min(upper(code))` is a dropped
  // column too and an argument-shaped hole in this scan is a silent wrong answer
  // wearing the guard as a disguise.
  const extremes = new Set(
    [...code.matchAll(
      /\b(min|max)\s*\(([^()]*(?:\([^()]*\)[^()]*)*)\)(?:\s+(?:as\s+)?(["'`\[])?([A-Za-z_][A-Za-z0-9_]*))?/gi,
    )]
      // A quoted or bracketed alias is a name the author wrote to be a name, the
      // same reading namesIn() gives a bracket ("a name in brackets is in them
      // because it is a column the engine would not otherwise parse") -- so it
      // is never filtered by KEYWORD, which is now widened past the clause
      // words this bare check still needs (`min(x) from t` must not read "from"
      // as an implied alias) to the words namesIn() reads bare too, `end` and
      // `order` among them, which is exactly what `as [end]` writes past.
      .flatMap(([, fn, arg, delim, as]) => [
        `${fn.toUpperCase()}(${arg.trim()})`,
        ...(as && (delim || !KEYWORD.test(as)) ? [as] : []),
      ]),
  );
  for (const { columnid } of cols) {
    if (known.includes(columnid)) continue;
    // Strictly undefined, never null: `select null as x` is a real answer.
    if (!rows.every((row) => row[columnid] === undefined)) continue;
    // The set above reads the query's text; this reads what came back. An
    // unaliased extreme arrives under its own call, whatever spelling AlaSQL
    // gave it, and that shape is the one thing no other column has.
    if (extremes.has(columnid) || /^(min|max)\s*\(/i.test(columnid)) {
      throw new Error(explain(`min() and max() cannot compare the text in "${columnid}".`, {
        Received: `"${columnid}" is empty in all ${rows.length} rows`,
        Cause:
          "AlaSQL computes min() and max() over numbers and dates only, and drops a text value. rewriteExtremes() " +
          "aims the call at min_text()/max_text() when the argument is a bare column the loaded sheets type as text, " +
          "and this one is not: an expression, a name nothing loaded holds, or a name typed two ways",
        Fix: `use min_text() or max_text(), which compare as text`,
      }));
    }
    if (!/^[A-Za-z_][A-Za-z0-9_]*$/.test(columnid)) continue;
    if (aliased.has(columnid)) continue;
    const hit = nearest(columnid, known);
    throw new Error(explain(`No column named "${columnid}".`, {
      ...(hit ? { "Did you mean": hit } : { Available: known.join(", ") || "(the referenced sheets have no columns)" }),
      Source: known.length ? "the sheets this query references" : "no @sheet is referenced",
      Fix: hit ? `rename "${columnid}" to "${hit}"` : "check the column names in the sheet's type row",
    }));
  }
};

// Every identifier a statement mentions, which is every name a column of some
// sheet it reads could be. It answers what is written and never which sheet a
// name belongs to: scope is not something a regex can see, so a caller that
// needs the sheet decides by who else holds the name.
//
// Three things are blanked before a name is read, and all three for the same
// reason -- what is left is only what the statement could be naming a column
// by. String literals go the way rewriteWindows() blanks them. So does every
// sheet ref, down to the column a cell ref names: `@table:x` is an address
// whose own words are not columns, while `@table:x.col` names col. And so does
// the name after an `as`, which is the alias the statement gives its OWN
// result and never a column it read -- `qty as price` claims qty, never price,
// even when the ref this statement reads also holds a column spelled price;
// chartSql's `[x] as x` would otherwise claim a column named `x` on every
// chart that has one, and its box branch five more nobody wrote. Blanked here
// rather than skipped at the match, because deciding it there means reading
// back over the whole statement for every name in it, which is quadratic on
// text a browser syncs in at whatever length it likes.
//
// What is matched is then read once, forwards: a name a `(` follows is a
// function, a name a `.` follows is the qualifier and not the column beside
// it, and a bracketed name is the bare one, because chartSql writes every
// column it names quoted. The identifier is `\p{L}`, not `A-Za-z`, because a
// column named in another script is a name and not a fragment of one --
// matching ASCII alone split `café` into `caf` and a bracket around it, which
// is a wrong guess in the one place this file promises never to guess.
export const MAX_NAMES = 500;

export const namesIn = (code) => {
  let text = code
    .replace(/'[^']*'/g, "''")
    .replace(/@[a-z-]+:[A-Za-z0-9_-]+/g, " ");
  // An alias this statement gives its OWN result is blanked everywhere it is
  // spelled again, not only at its own `as`: a cohort table's `as cohort`
  // reads again in its own `group by cohort` and `c.cohort`, and a window's
  // `as trend` a caller wraps in `where trend > 0` the same way. Missing this
  // is how a query built from cohortSql() or a window claimed its own
  // generated names -- cohort, active, trend -- as columns of the sheet it
  // reads, which a source column of that same name would make look right by
  // coincidence and a bare mis-attribution otherwise. Blanking every mention
  // can lose a genuine same-named column too, but the honesty rule already
  // prefers losing a name to claiming one that is not there.
  const aliases = new Set(
    [...text.matchAll(/\bas\s+\[?([\p{L}_][\p{L}\p{N}_]*)\]?/giu)].map((m) => m[1]),
  );
  // Each alias is one more pass over the whole text, so the aliases are bounded
  // before the names are: a statement a browser synced in could otherwise spell
  // thousands of them and make this loop quadratic again.
  if (aliases.size > MAX_NAMES) {
    throw new Error(explain(`This statement names more than ${MAX_NAMES} aliases.`, {
      Received: `${aliases.size} distinct aliases`,
      Limit: `${MAX_NAMES} distinct aliases in one statement`,
      Source: "the statement's own text",
      Fix: "split it into a query sheet per part, and select from those",
    }));
  }
  for (const alias of aliases) {
    // `x as x` (qualified or bracketed either side) is not an invented name --
    // cohortSql writes its key column exactly this way -- so it is left as the
    // ordinary reference it is rather than blanked into losing the one column
    // lineage most needs to keep, the key a rename would actually break.
    if (new RegExp(`(?:^|[.\\s,(])\\[?${alias}\\]?\\s+as\\s+\\[?${alias}\\]?\\b`, "iu").test(text)) continue;
    text = text.replace(new RegExp(`\\[?\\b${alias}\\b\\]?`, "giu"), " ");
  }
  const names = new Set();
  // The first lookahead is what stops the second from being satisfied by a
  // shorter name: without it `count(` backtracks to `coun`, which no `(`
  // follows, and the function is read as a column.
  const word = /\[[\p{L}_][\p{L}\p{N}_]*\](?!\s*[(.])|[\p{L}_][\p{L}\p{N}_]*(?![\p{L}\p{N}_])(?!\s*[(.])/gu;
  for (const m of text.matchAll(word)) {
    // A keyword is only a keyword bare: a name in brackets is in them because
    // it is a column the engine would not otherwise parse, which is what
    // chartSql writes an axis named `end` or `order` as.
    if (m[0][0] !== "[" && KEYWORD.test(m[0])) continue;
    const name = bare(m[0]);
    names.add(name);
    if (names.size > MAX_NAMES) {
      throw new Error(explain(`This statement names more than ${MAX_NAMES} identifiers.`, {
        Received: `${names.size} distinct names, counted through "${name}"`,
        Limit: `${MAX_NAMES} distinct identifiers in one statement`,
        Source: "the statement's own text",
        Fix: "split it into a query sheet per part, and select from those",
      }));
    }
  }
  return [...names];
};

// --- window functions
//
// AlaSQL parses `over (partition by ...)` and then computes it wrong: `sum(x)
// over (partition by k)` comes back 0, and only row_number() is right. So a
// window never reaches the engine. rewriteWindows() lifts each one out of the
// top-level select list, leaves `null as <alias>` where it stood, and appends
// the plain columns the window reads; applyWindows() computes it over the rows
// the engine returns and drops those columns again.

// The type each window produces. null means "whatever its argument already was",
// exactly as it does in SELECT_TYPES -- sum and avg follow their argument there,
// so a window of one name and a select item of one name have to agree. They did
// not: `sum(amount_usd) over (...)` read usd as a select item and num as a
// window, which is the same fact spelled twice.
export const WINDOW_TYPES = {
  row_number: "int",
  rank: "int",
  dense_rank: "int",
  ntile: "int",
  count: "int",
  percent_rank: "num",
  cume_dist: "num",
  sum: null,
  avg: null,
  stddev: "num",
  lag: null,
  lead: null,
  first_value: null,
  last_value: null,
  nth_value: null,
  min: null,
  max: null,
  trend: null,
  seasonal: null,
  deseasonalized: null,
};

// Ranking and offset functions read the whole partition; a frame never applies.
const OFFSET = ["lag", "lead"];
// The classical additive decomposition, which reads the whole partition too.
export const DECOMPOSE = ["trend", "seasonal", "deseasonalized"];
// The functions that return a row's own value, and so can be asked to look past
// a null to the last row that had one. Everything else skips nulls already.
const NULLABLE = ["lag", "lead", "first_value", "last_value", "nth_value"];
const HIDDEN = /^__w\d+[apo]\d+$/;
// A window lifted out of a qualify clause: computed, filtered on, then dropped.
const LIFTED = /^__q\d+$/;
// The words a qualify condition may hold that are not column names.
const QUALIFY_WORDS =
  /^(and|or|not|in|is|null|between|like|escape|true|false|case|when|then|else|end|as|cast|convert|int|integer|float|number|string|date|boolean|distinct)$/i;

// Depth of every character, -1 inside a string literal, computed in one pass so
// a keyword inside a subquery or a quoted value is never read as a top-level one.
const topLevel = (s) => {
  const depth = new Array(s.length).fill(-1);
  let d = 0, inStr = false;
  for (let i = 0; i < s.length; i++) {
    const ch = s[i];
    if (inStr) {
      if (ch === "'") inStr = false;
      continue;
    }
    if (ch === "'") {
      inStr = true;
      continue;
    }
    if (ch === "(" || ch === "[") {
      depth[i] = d++;
      continue;
    }
    if (ch === ")" || ch === "]") {
      depth[i] = --d;
      continue;
    }
    depth[i] = d;
  }
  return depth;
};

const findAt = (s, depth, re, level, from = 0, to = s.length) => {
  re.lastIndex = from;
  for (let m; (m = re.exec(s));) {
    if (m.index >= to) return null;
    if (depth[m.index] === level) return m;
  }
  return null;
};

// The matching close, or -1. The select-type pass runs before the engine, so an
// unbalanced expression has to reach AlaSQL, which is the one that can point at
// the character.
const closeAt = (s, depth, open) => {
  for (let i = open + 1; i < s.length; i++) if (s[i] === ")" && depth[i] === depth[open]) return i;
  return -1;
};

// The same search for the window pass, where an unbalanced bracket is this
// pass's own to explain: nothing downstream will ever see the over(...) clause.
const closeParen = (s, depth, open) => {
  const i = closeAt(s, depth, open);
  if (i >= 0) return i;
  throw new Error(explain(`A window function is missing a closing bracket.`, {
    Received: s.slice(open, open + 40),
    Source: "the over(...) clause in this query",
    Fix: "close every bracket in the over(...) clause",
  }));
};

const splitAt = (s, depth, from, to, level) => {
  const spans = [];
  let start = from;
  for (let i = from; i < to; i++) {
    if (s[i] === "," && depth[i] === level) {
      spans.push([start, i]);
      start = i + 1;
    }
  }
  spans.push([start, to]);
  return spans.filter(([a, b]) => s.slice(a, b).trim() !== "");
};

const BOUND = /^(?:(unbounded)\s+(preceding|following)|(current)\s+row|(\d+)\s+(preceding|following))$/i;

const bound = (text, spec) => {
  const m = text.trim().match(BOUND);
  if (!m) {
    throw new Error(explain(`A window frame bound is not one I understand.`, {
      Expected: "unbounded preceding, N preceding, current row, N following, or unbounded following",
      Received: JSON.stringify(text.trim()),
      Source: `over (${spec.trim()})`,
      Fix: "write the bound in one of those five forms",
    }));
  }
  if (m[1]) return { at: m[2].toLowerCase() === "preceding" ? -Infinity : Infinity };
  if (m[3]) return { at: 0 };
  return { at: (m[5].toLowerCase() === "preceding" ? -1 : 1) * Number(m[4]) };
};

const parseFrame = (text, spec) => {
  const m = text.trim().match(/^(rows|range)\s+(?:between\s+([\s\S]+?)\s+and\s+([\s\S]+)|([\s\S]+))$/i);
  if (!m) {
    throw new Error(explain(`A window frame clause is not one I understand.`, {
      Expected: "rows|range between <bound> and <bound>",
      Received: JSON.stringify(text.trim()),
      Source: `over (${spec.trim()})`,
      Fix: "e.g. rows between 6 preceding and current row",
    }));
  }
  const mode = m[1].toLowerCase();
  const start = bound(m[2] ?? m[4], spec);
  const end = m[3] === undefined ? { at: 0 } : bound(m[3], spec);
  // A range frame counts peers, not rows, so an offset in rows has no meaning here.
  if (mode === "range" && [start.at, end.at].some((n) => Number.isFinite(n) && n !== 0)) {
    throw new Error(explain(`A range frame cannot count a number of rows.`, {
      Received: JSON.stringify(text.trim()),
      Cause: "range measures peers of the order-by value; only unbounded and current row are defined for it",
      Source: `over (${spec.trim()})`,
      Fix: `write "rows between ..." instead, which counts rows`,
    }));
  }
  return { mode, start, end };
};

// Parses `fn(args) over (spec)` starting at `from`, or returns null if there is
// no call there. Naming it is the caller's job: a select item takes its alias
// from the text after, a window inside qualify gets a generated one.
const parseCall = (code, depth, from, to, index, alias) => {
  const call = code.slice(from, to).match(/^\s*([A-Za-z_][A-Za-z0-9_]*)\s*\(/);
  if (!call) return null;
  const open = from + call[0].length - 1;
  const argEnd = closeParen(code, depth, open);
  const after = code.slice(argEnd + 1, to).match(/^\s*(?:(ignore|respect)\s+nulls\s+)?over\s*\(/i);
  if (!after) return null;
  const ignoreNulls = (after[1] ?? "").toLowerCase() === "ignore";

  const fn = call[1].toLowerCase();
  if (!(fn in WINDOW_TYPES)) {
    const hit = nearest(fn, Object.keys(WINDOW_TYPES));
    throw new Error(explain(`"${fn}()" is not a window function.`, {
      ...(hit ? { "Did you mean": `${hit}()` } : { Available: Object.keys(WINDOW_TYPES).join(", ") }),
      Source: `${fn}(...) over (...) in this query`,
      Fix: hit ? `write ${hit}(...) over (...)` : "use one of the listed functions, or drop the over(...) clause",
    }));
  }

  const specOpen = argEnd + 1 + after[0].length - 1;
  const specEnd = closeParen(code, depth, specOpen);
  const level = depth[specOpen] + 1;
  const spec = code.slice(specOpen + 1, specEnd);

  const named = alias ??
    code.slice(specEnd + 1, to).match(/^\s*(?:as\s+)?["'`[]?([A-Za-z_][A-Za-z0-9_]*)["'`\]]?\s*$/i)?.[1];
  if (!named) {
    throw new Error(explain(`A window function needs a name of its own.`, {
      Expected: `${fn}(...) over (...) as some_name`,
      Received: code.slice(from, to).trim(),
      Source: "the select list of this query",
      Fix: `add "as <name>" after the over(...) clause, and do not wrap it in another expression`,
    }));
  }

  const p = findAt(code, depth, /\bpartition\s+by\b/gi, level, specOpen + 1, specEnd);
  const o = findAt(code, depth, /\border\s+by\b/gi, level, specOpen + 1, specEnd);
  const f = findAt(code, depth, /\b(?:rows|range)\b/gi, level, (o ?? p)?.index ?? specOpen + 1, specEnd);
  const stop = (...xs) => Math.min(...xs.filter((n) => n !== undefined && n !== null));

  const partition = p
    ? splitAt(code, depth, p.index + p[0].length, stop(o?.index, f?.index, specEnd), level)
      .map(([a, b]) => code.slice(a, b).trim())
    : [];
  const order = o
    ? splitAt(code, depth, o.index + o[0].length, stop(f?.index, specEnd), level).map(([a, b]) => {
      const text = code.slice(a, b).trim();
      const dir = text.match(/\s+(asc|desc)$/i);
      return { expr: dir ? text.slice(0, dir.index).trim() : text, desc: !!dir && dir[1].toLowerCase() === "desc" };
    })
    : [];
  const frame = f
    ? parseFrame(code.slice(f.index, specEnd), spec)
    // The standard default: with an order by, everything up to the current row
    // and its peers; without one, the whole partition.
    : { mode: "range", start: { at: -Infinity }, end: { at: order.length ? 0 : Infinity } };

  const rawArgs = splitAt(code, depth, open + 1, argEnd, depth[open] + 1).map(([a, b]) => code.slice(a, b).trim());
  const star = rawArgs.length === 1 && rawArgs[0] === "*";
  // lag/lead/ntile/nth_value take a literal count after the value expression;
  // only the value expression becomes a column.
  const counted = rawArgs.slice(1).map((t) => {
    if (/^-?\d+$/.test(t)) return Number(t);
    if (/^null$/i.test(t)) return null;
    if (/^'([^']*)'$/.test(t)) return t.slice(1, -1);
    throw new Error(explain(`${fn}() takes a literal after its value, not an expression.`, {
      Expected: "a whole number, a quoted text value, or null",
      Received: JSON.stringify(t),
      Source: `${fn}(${rawArgs.join(", ")}) over (...)`,
      Fix: `write the offset as a number, e.g. ${fn}(x, 1)`,
    }));
  });
  const args = star || !rawArgs.length ? [] : [rawArgs[0]];

  if (ignoreNulls && !NULLABLE.includes(fn)) {
    throw new Error(explain(`${fn}() has no nulls to ignore.`, {
      Expected: `ignore nulls only applies to ${NULLABLE.join(", ")}`,
      Received: `${fn}(...) ignore nulls over (...)`,
      Cause: `${fn}() skips nulls already, the way every aggregate does`,
      Source: `the ${named} column in this query`,
      Fix: `drop "ignore nulls"`,
    }));
  }

  return {
    fn,
    alias: named,
    end: specEnd + 1,
    ignoreNulls,
    star,
    args,
    counted,
    partition,
    order,
    frame,
    span: [from, to],
    hidden: {
      args: args.map((_, j) => `__w${index}a${j}`),
      partition: partition.map((_, j) => `__w${index}p${j}`),
      order: order.map((_, j) => `__w${index}o${j}`),
    },
  };
};

export const rewriteWindows = (code) => {
  const depth = topLevel(code);
  const outsideStrings = code.replace(/'[^']*'/g, "''");
  // Cheap exit: no over( and no qualify outside a string means nothing to lift.
  if (!/\bover\s*\(/i.test(outsideStrings) && !/\bqualify\b/i.test(outsideStrings))
    return { sql: code, windows: [], qualify: null, limit: null, offset: 0 };

  const select = findAt(code, depth, /\bselect\b/gi, 0);
  if (!select) {
    throw new Error(explain(`A window function needs a select statement around it.`, {
      Received: code.trim().slice(0, 60),
      Source: "this query",
      Fix: "put the over(...) clause in the select list of a select statement",
    }));
  }
  const listStart = select.index + select[0].length;
  const tail = findAt(
    code,
    depth,
    /\b(?:from|where|group|having|qualify|order|limit|offset|union|into)\b/gi,
    0,
    listStart,
  );
  const listEnd = tail ? tail.index : code.length;

  if (/^\s*(?:distinct|top\b)/i.test(code.slice(listStart, listEnd))) {
    throw new Error(explain(`A window function cannot share a select list with distinct or top.`, {
      Received: code.slice(listStart, listEnd).trim().slice(0, 60),
      Cause: "both change which rows exist, and a window is defined over the rows that do",
      Source: "the select list of this query",
      Fix: "compute the window in its own query sheet, then select distinct from that",
    }));
  }

  // qualify is where `row_number() over (...) = 1` belongs, and the condition
  // cannot name a column the select list does not have, so a window written
  // there is lifted into a hidden column of its own and the condition is
  // pointed at that. The clause itself never reaches the engine.
  const qual = findAt(code, depth, /\bqualify\b/gi, 0, listEnd);
  const qualStop = qual && findAt(code, depth, /\b(?:order|limit|offset)\b/gi, 0, qual.index + qual[0].length);
  const qualFrom = qual ? qual.index + qual[0].length : 0;
  const qualTo = qual ? (qualStop ? qualStop.index : code.length) : 0;

  // Everything outside the select list and the qualify clause: only row_number()
  // works there unlifted.
  const blank = (from, to) => " ".repeat(Math.max(0, to - from));
  const outside = code.slice(0, listStart) + blank(listStart, listEnd) +
    (qual ? code.slice(listEnd, qual.index) + blank(qual.index, qualTo) + code.slice(qualTo) : code.slice(listEnd));
  const buried = outside.replace(/'[^']*'/g, "''").match(/([A-Za-z_][A-Za-z0-9_]*)\s*\([^()]*\)\s*over\s*\(/i);
  if (buried && buried[1].toLowerCase() !== "row_number") {
    throw new Error(explain(`A window function only works in the outermost select list.`, {
      Received: buried[0].trim(),
      Cause: "windows are computed after the engine returns its rows, so a nested one would see the wrong rows",
      Source: "a subquery or clause of this query",
      Fix: "move that select into its own query sheet and reference it with @query:",
    }));
  }

  const windows = [];
  const edits = [];
  // Lifts every window written at this depth out of [from, to), leaving the
  // text with each one replaced by the hidden column that will hold its answer.
  const lift = (from, to) => {
    const swaps = [];
    for (const call of code.slice(from, to).matchAll(/[A-Za-z_][A-Za-z0-9_]*\s*\(/g)) {
      const at = from + call.index;
      if (swaps.some(([a, b]) => at >= a && at < b)) continue;
      if (depth[at] !== depth[from]) continue;
      const w = parseCall(code, depth, at, to, windows.length, `__q${windows.length}`);
      if (!w) continue;
      windows.push(w);
      swaps.push([at, w.end, w.alias]);
    }
    return {
      swaps,
      text: swaps
        .sort((x, y) => y[0] - x[0])
        .reduce((t, [a, b, name]) => t.slice(0, a - from) + name + t.slice(b - from), code.slice(from, to)),
    };
  };

  for (const [a, b] of splitAt(code, depth, listStart, listEnd, 0)) {
    const w = parseCall(code, depth, a, b, windows.length);
    if (w) {
      windows.push(w);
      edits.push([a, b, ` null as ${w.alias} `]);
      continue;
    }
    // A window wrapped in an expression cannot be lifted: the item's other
    // operands are consumed by the same expression, and computing it after the
    // engine has returned would need them as columns nobody asked for. Only
    // row_number() survives being left to AlaSQL, so name the rest.
    const stray = code.slice(a, b).replace(/'[^']*'/g, "''").match(
      /([A-Za-z_][A-Za-z0-9_]*)\s*\([^()]*\)\s*over\s*\(/i,
    );
    if (stray && stray[1].toLowerCase() !== "row_number") {
      throw new Error(explain(`A window function has to be a select item on its own.`, {
        Expected: `${stray[1]}(...) over (...) as some_name`,
        Received: code.slice(a, b).trim(),
        Source: "the select list of this query",
        Fix: "give the window a column of its own, then do the arithmetic in a query sheet that reads this one",
      }));
    }
  }

  let qualify = null;
  if (qual) {
    qualify = lift(qualFrom, qualTo).text.replace(/;\s*$/, "").trim();
    if (!qualify) {
      throw new Error(explain(`qualify needs a condition after it.`, {
        Expected: "qualify <condition>, e.g. qualify recency = 1",
        Received: code.slice(qual.index, qualTo).trim(),
        Source: "the qualify clause of this query",
        Fix: "name the window column and the value it must have",
      }));
    }
    if (!windows.length) {
      throw new Error(explain(`qualify filters on a window function, and this query has none.`, {
        Received: code.slice(qual.index, qualTo).trim().slice(0, 60),
        Cause: "qualify runs after the windows are computed; with no window it can only repeat what where already did",
        Source: "the qualify clause of this query",
        Fix: "write it as a where clause instead",
      }));
    }
    edits.push([qual.index, qualTo, " "]);
  }
  if (!windows.length) return { sql: code, windows: [], qualify: null, limit: null, offset: 0 };

  // Two columns with one name: AlaSQL keeps whichever it wrote last and the
  // window silently overwrites the other. Name the collision instead.
  const taken = new Map();
  for (const [a, b] of splitAt(code, depth, listStart, listEnd, 0)) {
    const item = code.slice(a, b).trim();
    const named = item.match(/\bas\s+["'`[]?([A-Za-z_][A-Za-z0-9_]*)/i) ??
      item.match(/^(?:[A-Za-z_][A-Za-z0-9_]*\.)?([A-Za-z_][A-Za-z0-9_]*)$/);
    if (named) taken.set(named[1].toLowerCase(), (taken.get(named[1].toLowerCase()) ?? 0) + 1);
  }
  for (const w of windows) {
    if (taken.get(w.alias.toLowerCase()) > 1) {
      throw new Error(explain(`Two columns in this query are both named "${w.alias}".`, {
        Cause: "the window is written into that column after the engine runs, so it would overwrite the other one",
        Source: "the select list of this query",
        Fix: `rename one of them, e.g. ${w.alias}_window`,
      }));
    }
  }

  const added = [
    ...windows.filter((w) => LIFTED.test(w.alias)).map((w) => `null as ${w.alias}`),
    ...windows.flatMap((w) => [
      ...w.args.map((expr, j) => `${expr} as ${w.hidden.args[j]}`),
      ...w.partition.map((expr, j) => `${expr} as ${w.hidden.partition[j]}`),
      ...w.order.map(({ expr }, j) => `${expr} as ${w.hidden.order[j]}`),
    ]),
  ];
  edits.push([listEnd, listEnd, added.length ? `, ${added.join(", ")} ` : " "]);

  // A window is computed over every row the query produced, so the row cap has
  // to come off before the engine applies it and go back on afterwards.
  let limit = null, offset = 0;
  const lim = findAt(code, depth, /\blimit\b/gi, 0, listEnd);
  if (lim) {
    const rest = code.slice(lim.index).match(/^limit\s+(\d+)(?:\s+offset\s+(\d+))?\s*;?\s*$/i);
    if (!rest) {
      throw new Error(explain(`I cannot read the row limit on a query that uses a window function.`, {
        Expected: "limit <n>, or limit <n> offset <m>, at the very end",
        Received: code.slice(lim.index).trim(),
        Source: "the end of this query",
        Fix: "write the limit as a plain number at the end of the query",
      }));
    }
    limit = Number(rest[1]);
    offset = rest[2] ? Number(rest[2]) : 0;
    edits.push([lim.index, code.length, " "]);
  }

  let sql = code;
  for (const [a, b, text] of edits.sort((x, y) => y[0] - x[0])) sql = sql.slice(0, a) + text + sql.slice(b);
  return { sql, windows, qualify, limit, offset };
};

// Nulls sort last ascending and first descending, matching Postgres.
const winCompare = (a, b) => {
  const [na, nb] = [a === null || a === undefined, b === null || b === undefined];
  if (na || nb) return na && nb ? 0 : na ? 1 : -1;
  if (typeof a === "number" && typeof b === "number") return a - b;
  const [x, y] = [String(a), String(b)];
  return x < y ? -1 : x > y ? 1 : 0;
};

const winNum = (fn, v) => {
  if (absent(v)) return null;
  const n = typeof v === "string" ? Number(v) : v instanceof Date ? v.getTime() : v;
  if (typeof n !== "number" || !Number.isFinite(n)) {
    throw new Error(explain(`${fn}() over a window received a value it cannot add up.`, {
      Expected: "a number or a blank",
      Received: `${typeof v} ${JSON.stringify(v)}`,
      Source: `the ${fn}(...) over (...) column in this query`,
      Fix: "filter the non-numeric rows out, or use min()/max() which compare instead of add",
    }));
  }
  return n;
};

// Classical additive decomposition: y = trend + seasonal + what is left. Every
// row of a partition answers off the whole series, so it is computed once and
// kept against the `ord` array, which applyWindows allocates fresh per window
// and per partition -- so no row walks the series again, and the three
// functions of one query each walk it once.
// Nothing here can check that the series is evenly spaced -- it must be one row
// per step of the cycle, in order, with no step missing.
const decomposed = new WeakMap();

const decompose = (w, rows, ord) => {
  const hit = decomposed.get(ord);
  if (hit) return hit;
  const size = ord.length;
  const period = w.counted.length ? w.counted[0] : null;
  if (!Number.isInteger(period) || period < 2) {
    throw new Error(explain(`${w.fn}() needs a whole number of steps in one cycle.`, {
      Expected: "a whole number of at least 2, e.g. trend(visits, 12) for monthly rows over a year",
      Received: JSON.stringify(period),
      Source: `the ${w.alias} column in this query`,
      Fix: `write ${w.fn}(<column>, 12) over (order by <the column that steps>)`,
    }));
  }
  if (size < period * 2) {
    throw new Error(explain(`${w.fn}() needs two whole cycles to tell a season from a trend.`, {
      Expected: `at least ${period * 2} rows in a partition, two cycles of ${period}`,
      Received: `${size} rows`,
      Source: `the ${w.alias} column in this query`,
      Fix: "shorten the period, widen the partition, or drop the decomposition",
    }));
  }

  // A blank cell is the normal state of a spreadsheet column, so a blank y is a
  // null trend and a null deseasonalized rather than a refusal. What is refused
  // is a step of the cycle no trend row covers at all: averaging nothing is a
  // NaN, and a NaN rides every row downstream saying nothing about why.
  const y = ord.map((i) => winNum(w.fn, rows[i][w.hidden.args[0]]));
  // The centred moving average: an even period has no middle row, so it is the
  // 2 x period average -- half weight on each end point of period + 1 rows.
  const half = Math.floor(period / 2);
  const even = period % 2 === 0;
  const trend = new Array(size).fill(null);
  for (let p = half; p + half < size; p++) {
    let total = 0;
    for (let q = p - half; q <= p + half; q++) {
      if (y[q] === null) {
        total = null;
        break;
      }
      total += even && (q === p - half || q === p + half) ? y[q] / 2 : y[q];
    }
    if (total !== null) trend[p] = total / period;
  }

  const sums = new Array(period).fill(0), counts = new Array(period).fill(0);
  for (let p = 0; p < size; p++) {
    if (trend[p] === null) continue;
    sums[p % period] += y[p] - trend[p];
    counts[p % period]++;
  }
  const means = sums.map((total, phase) => {
    if (!counts[phase]) {
      throw new Error(explain(`${w.fn}() found no row to read step ${phase + 1} of the cycle from.`, {
        Expected: `a value at every one of the ${period} steps of a cycle, on a row the trend covers`,
        Received: `${counts.filter((n) => n).length} of ${period} steps`,
        Source: `the ${w.alias} column in this query`,
        Fix: "fill the blanks in that column, or widen the partition so another cycle covers the step",
      }));
    }
    return total / counts[phase];
  });
  // Centred so one cycle of the pattern sums to zero: what it moves is the
  // level, and the level belongs to the trend.
  const centre = means.reduce((a, b) => a + b, 0) / period;
  const seasonal = means.map((m) => m - centre);

  const answer = { trend, seasonal, y };
  decomposed.set(ord, answer);
  return answer;
};

const frameBounds = (frame, pos, peer, size) => {
  const edge = ({ at }, fallback) => at === -Infinity ? 0 : at === Infinity ? size - 1 : at === 0 ? fallback : pos + at;
  // A range frame moves to the edge of the peer group; a rows frame counts rows.
  const lo = frame.mode === "range" && frame.start.at === 0 ? peer.first : edge(frame.start, pos);
  const hi = frame.mode === "range" && frame.end.at === 0 ? peer.last : edge(frame.end, pos);
  return [Math.max(0, lo), Math.min(size - 1, hi)];
};

const winValue = (w, rows, ord, pos) => {
  const size = ord.length;
  const at = (p) => rows[ord[p]];
  const arg = (p) => (w.hidden.args.length ? at(p)[w.hidden.args[0]] : null);
  const sameOrder = (a, b) => w.hidden.order.every((k) => winCompare(at(a)[k], at(b)[k]) === 0);

  // Peer group: the run of rows with the same order-by value. With no order by
  // every row in the partition is a peer, which is what makes rank() all 1s.
  let first = pos, last = pos;
  if (!w.order.length) [first, last] = [0, size - 1];
  else {
    while (first > 0 && sameOrder(first - 1, pos)) first--;
    while (last < size - 1 && sameOrder(last + 1, pos)) last++;
  }

  if (w.fn === "row_number") return pos + 1;
  if (w.fn === "rank") return first + 1;
  if (w.fn === "dense_rank") {
    let n = 1;
    for (let i = 1; i <= first; i++) if (!sameOrder(i, i - 1)) n++;
    return n;
  }
  if (w.fn === "percent_rank") return size === 1 ? 0 : first / (size - 1);
  if (w.fn === "cume_dist") return (last + 1) / size;
  if (w.fn === "ntile") {
    const n = w.counted.length ? w.counted[0] : Number(w.args[0]);
    if (!Number.isInteger(n) || n < 1) {
      throw new Error(explain(`ntile() needs a whole number of buckets.`, {
        Expected: "a positive whole number, e.g. ntile(4)",
        Received: JSON.stringify(w.counted[0] ?? w.args[0] ?? null),
        Source: `the ${w.alias} column in this query`,
        Fix: "write ntile(4) over (...) for quartiles",
      }));
    }
    const big = size % n, small = Math.floor(size / n);
    const cut = big * (small + 1);
    return pos < cut ? Math.floor(pos / (small + 1)) + 1 : big + Math.floor((pos - cut) / small) + 1;
  }
  const missing = (v) => v === null || v === undefined;
  if (OFFSET.includes(w.fn)) {
    const back = w.fn === "lag";
    let want = w.counted.length ? Number(w.counted[0]) : 1;
    if (!w.ignoreNulls) {
      const p = pos + (back ? -want : want);
      return p >= 0 && p < size ? arg(p) : (w.counted.length > 1 ? w.counted[1] : null);
    }
    // ignore nulls: step over the rows that have no value, which is what makes
    // lag() a forward fill and last_value() an as-of read.
    for (let p = pos + (back ? -1 : 1); p >= 0 && p < size; p += back ? -1 : 1) {
      if (missing(arg(p))) continue;
      if (--want === 0) return arg(p);
    }
    return w.counted.length > 1 ? w.counted[1] : null;
  }
  if (DECOMPOSE.includes(w.fn)) {
    const { trend, seasonal, y } = decompose(w, rows, ord);
    if (w.fn === "trend") return trend[pos];
    const s = seasonal[pos % seasonal.length];
    if (w.fn === "seasonal") return s;
    return y[pos] === null ? null : y[pos] - s;
  }

  const [lo, hi] = frameBounds(w.frame, pos, { first, last }, size);
  if (hi < lo) return w.fn === "count" ? 0 : null;
  if (w.fn === "count") {
    if (w.star) return hi - lo + 1;
    let n = 0;
    for (let p = lo; p <= hi; p++) if (arg(p) !== null && arg(p) !== undefined) n++;
    return n;
  }
  if (["first_value", "last_value", "nth_value"].includes(w.fn)) {
    const n = w.fn === "nth_value" ? (w.counted.length ? w.counted[0] : 1) : 1;
    const step = w.fn === "last_value" ? -1 : 1;
    let want = n;
    for (let p = w.fn === "last_value" ? hi : lo; p >= lo && p <= hi; p += step) {
      if (w.ignoreNulls && missing(arg(p))) continue;
      if (--want === 0) return arg(p);
    }
    return null;
  }
  if (w.fn === "min" || w.fn === "max") {
    let best;
    for (let p = lo; p <= hi; p++) {
      const v = arg(p);
      if (v === null || v === undefined) continue;
      const keep = w.fn === "min" ? winCompare(v, best) < 0 : winCompare(v, best) > 0;
      if (best === undefined || keep) best = v;
    }
    return best ?? null;
  }
  const xs = [];
  for (let p = lo; p <= hi; p++) {
    const n = winNum(w.fn, arg(p));
    if (n !== null) xs.push(n);
  }
  if (!xs.length) return null;
  const total = xs.reduce((a, b) => a + b, 0);
  if (w.fn === "sum") return total;
  if (w.fn === "avg") return total / xs.length;
  if (xs.length < 2) return null;
  const m = total / xs.length;
  return Math.sqrt(xs.reduce((a, b) => a + (b - m) ** 2, 0) / (xs.length - 1));
};

export const applyWindows = ({ columns, data }, { windows, qualify, limit, offset }, run) => {
  for (const w of windows) {
    const groups = new Map();
    data.forEach((row, i) => {
      const key = JSON.stringify(w.hidden.partition.map((k) => row[k] ?? null));
      if (!groups.has(key)) groups.set(key, []);
      groups.get(key).push(i);
    });
    for (const idxs of groups.values()) {
      // Stable: the engine's own row order breaks a tie, so repeated runs agree.
      const ord = [...idxs].sort((a, b) => {
        for (let j = 0; j < w.order.length; j++) {
          const c = winCompare(data[a][w.hidden.order[j]], data[b][w.hidden.order[j]]) * (w.order[j].desc ? -1 : 1);
          if (c) return c;
        }
        return a - b;
      });
      for (let pos = 0; pos < ord.length; pos++) data[ord[pos]][w.alias] = winValue(w, data, ord, pos);
    }
  }
  for (const row of data) for (const key of Object.keys(row)) if (HIDDEN.test(key)) delete row[key];

  // The engine evaluates its own predicate over the finished rows: a hand-rolled
  // expression evaluator here would be a second SQL dialect to keep in step.
  let kept = data;
  if (qualify) {
    // AlaSQL reads an unknown column as undefined, so `qualify nope = 1` is
    // false for every row and the answer is an empty sheet rather than an error.
    const known = new Set([...columns.map((col) => col.columnid), ...windows.map((w) => w.alias)]);
    const text = qualify.replace(/'[^']*'/g, "''");
    for (const m of text.matchAll(/[A-Za-z_][A-Za-z0-9_]*/g)) {
      const word = m[0];
      const before = text[m.index - 1], after = text.slice(m.index + word.length);
      if (before === "." || /^\s*\(/.test(after)) continue;
      if (QUALIFY_WORDS.test(word) || known.has(word)) continue;
      const hit = nearest(word, [...known]);
      throw new Error(explain(`The qualify condition names "${word}", which this query does not return.`, {
        ...(hit ? { "Did you mean": hit } : { Available: [...known].join(", ") || "(no columns)" }),
        Source: `qualify ${qualify}`,
        Fix: hit ? `write ${hit} instead` : "the condition may only name columns the select list produces",
      }));
    }
    try {
      kept = run(`select * from ? where ${qualify}`, [data]);
    } catch (err) {
      throw new Error(explain(`I could not apply this qualify condition.`, {
        Received: qualify,
        Cause: err instanceof Error ? err.message.split("\n")[0] : String(err),
        Source: "the qualify clause of this query",
        Fix: "the condition may only name columns the query returns, including the window ones",
      }));
    }
  }
  for (const row of kept) for (const key of Object.keys(row)) if (LIFTED.test(key)) delete row[key];
  return {
    columns: columns.filter((col) => !HIDDEN.test(col.columnid) && !LIFTED.test(col.columnid)),
    data: limit === null || limit === undefined ? kept : kept.slice(offset, offset + limit),
  };
};

// --- result types
//
// A query column used to be typed by its name alone: whatever column of that
// name some loaded sheet declared, or text. So `cast(price as string) as price`
// still read usd, `count(*) as n` read text, and every sheet downstream of that
// query inherited the lie. A type is a property of the select item, so it is
// read off the select item.
//
// This is WINDOW_TYPES for the rest of the select list, and null means the same
// thing here as it does there: whatever its argument already was, which is what
// makes sum(amount_usd) usd and round(avg(price), 2) usd as well.
const SELECT_TYPES = {
  count: "int",
  sum: null,
  avg: null,
  min: null,
  max: null,
  round: null,
  min_text: "text",
  max_text: "text",
  ols: "json",
  logit: "json",
  ols_predict: "num",
  logit_predict: "num",
  kmeans: "json",
  kmeans_assign: "int",
  sample_uniform: "num",
  sample_normal: "num",
  sample_triangular: "num",
};

// A cast is the one expression whose type is stated rather than inferred — but
// only where AlaSQL performs the cast. `cast(x as text)`, `as bool` and `as json`
// pass the value through untouched, and `cast('2026-01-02' as date)` answers the
// string "26.01.01", so stating a type for any of those would be this bug again
// in a new place. They are absent, which lands them in the fallback.
const CAST_TYPES = {
  string: "text",
  varchar: "text",
  char: "text",
  int: "int",
  integer: "int",
  smallint: "int",
  bigint: "int",
  number: "num",
  float: "num",
  double: "num",
  decimal: "num",
  numeric: "num",
  boolean: "bool",
};

// How deep a select item is peeled before the type stops being worth chasing.
const SELECT_DEPTH = 8;

// Brackets are how AlaSQL quotes a name its parser will not take bare, and
// `[total]` is the same column as `total`. Every pass here that reads a name out
// of the query's own text reads it through this, or a quoted column is a
// different column to that pass than the bare one: a select item that loses the
// type hint it would have carried, an unpivot that wraps a second bracket round
// a name that already had one.
const bare = (text) => text.trim().replace(/^\[(.*)\]$/, "$1").trim();

const itemType = (expr, nameToType) => {
  let text = expr.trim();
  let averaged = false;
  for (let i = 0; i < SELECT_DEPTH; i++) {
    if (/^'(?:[^']|'')*'$/.test(text)) return "text";
    if (/^-?\d+(?:\.\d+)?$/.test(text)) return "num";
    const plain = bare(text).match(/^(?:[A-Za-z_][A-Za-z0-9_]*\s*\.\s*)?([A-Za-z_][A-Za-z0-9_]*)$/);
    // An average of whole numbers is not a whole number. Every other function
    // that follows its argument hands the type back untouched.
    if (plain) return averaged && nameToType[plain[1]] === "int" ? "num" : nameToType[plain[1]];
    const head = text.match(/^([A-Za-z_][A-Za-z0-9_]*)\s*\(/);
    if (!head) return undefined;
    const depth = topLevel(text);
    const open = head[0].length - 1;
    // The call has to be the whole item: `sum(a) + 1` is arithmetic, not a sum.
    if (closeAt(text, depth, open) !== text.length - 1) return undefined;
    const fn = head[1].toLowerCase();
    const args = splitAt(text, depth, open + 1, text.length - 1, depth[open] + 1)
      .map(([a, b]) => text.slice(a, b).trim());
    if (fn === "cast") {
      const as = findAt(text, depth, /\bas\b/gi, depth[open] + 1, open + 1, text.length - 1);
      return as ? CAST_TYPES[text.slice(as.index + 2, text.length - 1).trim().toLowerCase()] : undefined;
    }
    if (!(fn in SELECT_TYPES)) return undefined;
    if (SELECT_TYPES[fn]) return SELECT_TYPES[fn];
    if (!args.length) return undefined;
    averaged = averaged || fn === "avg";
    text = args[0];
  }
  return undefined;
};

/** The type each top-level select item produces, keyed by the name its column
 * will carry: its alias, or the column's own name.
 *
 * An item this cannot type is left out rather than guessed at, and the caller
 * falls back to the source column of that name, which is all typing did before
 * this existed. A type is a hint about an answer the engine already computed, so
 * an expression nobody anticipated costs the hint and never the answer.
 */
export const selectTypes = (code, nameToType) => {
  const depth = topLevel(code);
  const select = findAt(code, depth, /\bselect\b/gi, 0);
  if (!select) return {};
  const listStart = select.index + select[0].length;
  const tail = findAt(
    code,
    depth,
    /\b(?:from|where|group|having|qualify|order|limit|offset|union|into)\b/gi,
    0,
    listStart,
  );
  const out = {};
  for (const [a, b] of splitAt(code, depth, listStart, tail ? tail.index : code.length, 0)) {
    // The first `as` written at the item's own depth. The one inside
    // `cast(x as int)` sits a bracket deeper and is not an alias.
    const as = findAt(code, depth, /\bas\s+["'`[]?([A-Za-z_][A-Za-z0-9_]*)/gi, 0, a, b);
    const item = code.slice(a, b).trim();
    const name = as ? as[1] : bare(item).match(/^(?:[A-Za-z_][A-Za-z0-9_]*\s*\.\s*)?([A-Za-z_][A-Za-z0-9_]*)$/)?.[1];
    if (!name) continue;
    const type = itemType(as ? code.slice(a, as.index) : item, nameToType);
    if (type) out[name] = type;
  }
  return out;
};

// --- pivot and unpivot
//
// AlaSQL's own `pivot` works, with one trap: an in-list of quoted strings
// matches nothing and returns zero rows instead of failing. `unpivot` is worse
// — it drops every column that is not being unpivoted, and the value column too
// once you name it in the select list — so it never reaches the engine either.
// Both run before the window pass, on SQL that scanRefs has already rewritten.

export const checkPivot = (code) => {
  const m = code.match(/\bpivot\s*\([\s\S]*?\bin\s*\(([^()]*)\)/i);
  if (!m || !m[1].includes("'")) return;
  throw new Error(explain(`A pivot in-list takes column names, not quoted text.`, {
    Received: m[1].trim(),
    Cause: "AlaSQL matches a quoted value against no column at all and answers with zero rows rather than failing",
    Source: "the pivot clause in this query",
    Fix: "write the names bare or in brackets, e.g. in ([jan], [feb])",
  }));
};

export const rewriteUnpivot = (code, columnsOf) => {
  if (!/\bunpivot\b/i.test(code.replace(/'[^']*'/g, "''"))) return code;
  const depth = topLevel(code);
  const re = /SHEET\('([^']+)'\)\s*(?:as\s+)?([A-Za-z_][A-Za-z0-9_]*\s+)?unpivot\s*\(/gi;
  const edits = [];
  for (let m; (m = re.exec(code));) {
    const open = m.index + m[0].length - 1;
    const close = closeParen(code, depth, open);
    const spec = code.slice(open + 1, close);
    const ident = "[A-Za-z_][A-Za-z0-9_]*|\\[[^\\]]*\\]";
    const parsed = spec.match(new RegExp(`^\\s*(${ident})\\s+for\\s+(${ident})\\s+in\\s*\\(([\\s\\S]+)\\)\\s*$`, "i"));
    if (!parsed) {
      throw new Error(explain(`I cannot read this unpivot clause.`, {
        Expected: "unpivot (<value column> for <name column> in (col, col, ...))",
        Received: `unpivot (${spec.trim()})`,
        Source: "the from clause of this query",
        Fix: "e.g. unpivot (amount for month in (jan, feb, mar))",
      }));
    }
    const [value, name] = [bare(parsed[1]), bare(parsed[2])];
    // The ident pattern above admits a bracketed name, whose content is anything
    // but a `]` -- a space, a quote, another `[` -- and both of these are
    // spliced straight back out inside brackets below. The in-list beside them
    // is already held to a bare column name, so all three names in one clause
    // obey the one rule rather than two.
    for (const [what, col] of [["value column", value], ["name column", name]]) {
      if (/^[A-Za-z_][A-Za-z0-9_]*$/.test(col)) continue;
      throw new Error(explain(`An unpivot's ${what} takes a column name.`, {
        Expected: "a column name in letters, digits and underscores, e.g. amount",
        Received: col,
        Source: `unpivot (${spec.trim()})`,
        Fix: `write the ${what} as a bare name, e.g. unpivot (amount for month in (jan, feb))`,
      }));
    }
    const wide = parsed[3].split(",").map(bare);
    for (const col of wide) {
      if (/^[A-Za-z_][A-Za-z0-9_]*$/.test(col)) continue;
      throw new Error(explain(`An unpivot in-list takes column names, not values.`, {
        Received: col,
        Source: `unpivot (${spec.trim()})`,
        Fix: "name the wide columns themselves, e.g. in (jan, feb, mar)",
      }));
    }
    const id = m[1];
    const known = (columnsOf[id] ?? []).map((col) => col.name);
    for (const col of wide) {
      if (known.includes(col)) continue;
      const hit = nearest(col, known);
      throw new Error(explain(`The sheet @${id} has no column named "${col}" to unpivot.`, {
        ...(hit ? { "Did you mean": hit } : { Available: known.join(", ") || "(no columns)" }),
        Source: `unpivot (${spec.trim()})`,
        Fix: hit ? `write ${hit} instead` : "check the column names in the sheet's type row",
      }));
    }
    const keys = known.filter((col) => !wide.includes(col));
    const alias = (m[2] ?? "").trim() || `unpivoted`;
    const branches = wide.map((col) =>
      `select ${
        [...keys.map((k) => `[${k}]`), `'${col}' as [${name}]`, `[${col}] as [${value}]`].join(", ")
      } from SHEET('${id}')`
    );
    edits.push([m.index, close + 1, `(${branches.join(" union all ")}) ${alias}`]);
  }
  if (!edits.length) {
    throw new Error(explain(`unpivot reads a sheet, not a subquery.`, {
      Expected: "@type:doc_id unpivot (<value> for <name> in (col, ...))",
      Received: code.trim().slice(0, 80),
      Cause: "the wide columns are read from the sheet's own type row, which a subquery does not have",
      Source: "the from clause of this query",
      Fix: "materialize the subquery as its own query sheet, then unpivot that",
    }));
  }
  let out = code;
  for (const [a, b, text] of edits.sort((x, y) => y[0] - x[0])) out = out.slice(0, a) + text + out.slice(b);
  return out;
};

/** `min(x)` and `max(x)` over a text column, pointed at the two aggregates that
 * can answer them.
 *
 * AlaSQL compiles min() and max() inline, restricted to numbers, bigints and
 * dates, and turns a text value into undefined -- and the compiler never
 * consults alasql.aggr for those two names, so a UDF cannot replace them.
 * min_text()/max_text() are ours and compare as text; this pass aims the call at
 * them whenever the argument is a column the loaded sheets type as text.
 *
 * Rewriting the call is the fix available to us. src/alasql.mjs is a minified
 * bundle `deno task vendor` regenerates from deno.json, so a patch to the engine
 * would not survive the next re-vendor.
 *
 * Only a bare column name, and only one that every sheet holding it types as
 * text. An expression, a name nothing loaded holds, and a name typed two ways
 * are all left alone and land on checkResultColumns()'s message, which still
 * names min_text() as the way out.
 *
 * Two calls it must not touch. One inside a string literal is not a call at all,
 * which is what the depth array says. A window is: `min(x) over (...)` is
 * computed by applyWindows, whose comparison already handles text, and
 * rewriteWindows finds it by the name this pass would have changed.
 *
 * An unaliased call is renamed with its column: `min(code)` answers under
 * `min_text(code)` rather than `MIN(code)`. selectTypes reads the author's own
 * text, before this pass runs, so the result type is the same either way.
 */
export const MAX_EXTREMES = 100;

export const rewriteExtremes = (code, colsOf) => {
  const text = new Map();
  for (const cols of Object.values(colsOf ?? {})) {
    for (const col of cols) {
      const is = textType(col.type);
      text.set(col.name, text.has(col.name) ? text.get(col.name) && is : is);
    }
  }
  if (![...text.values()].some(Boolean)) return code;

  // A name the query invents for itself. `select min(a) from (select n as a ...)`
  // is a number wearing the name of somebody's text column, and comparing it as
  // text answers "10" for a minimum of 9 -- the silent wrong answer this whole
  // pass exists to remove, put back one level down. Scope is not something a
  // regex can see, so an aliased name is simply not rewritten.
  const aliased = new Set(
    [...code.matchAll(/\bas\s+["'`[]?([A-Za-z_][A-Za-z0-9_]*)/gi)].map((m) => m[1]),
  );

  const depth = topLevel(code);
  const edits = [];
  const re = /\b(min|max)\s*\(/gi;
  let seen = 0;
  for (let m; (m = re.exec(code));) {
    // Bounded like every other loop here, and the message carries the counter.
    // Each unclosed call below costs a scan to the end of the statement, so
    // without this a body of nothing but `min(` is quadratic -- 80KB took 1.5s
    // on the machine this was written on, and BODY_CAP is 1MiB.
    if (++seen > MAX_EXTREMES) {
      throw new Error(explain(`This query calls min() or max() more than ${MAX_EXTREMES} times.`, {
        Limit: `${MAX_EXTREMES} min() and max() calls in one statement`,
        Received: `at least ${seen}`,
        Source: "the select list of this query",
        Fix: "split the query, or aggregate in a subquery first",
      }));
    }
    const open = m.index + m[0].length - 1;
    if (depth[open] < 0) continue;
    const close = closeAt(code, depth, open);
    // Unbalanced, so this is not SQL the engine will accept either. Stop rather
    // than scan to the end for every call after it, and let the engine be the
    // one that points at the character.
    if (close < 0) return code;
    if (/^\s*over\b/i.test(code.slice(close + 1))) continue;
    const arg = bare(code.slice(open + 1, close)).replace(/^[A-Za-z_][A-Za-z0-9_]*\s*\.\s*/, "");
    if (!/^[A-Za-z_][A-Za-z0-9_]*$/.test(arg) || !text.get(arg) || aliased.has(arg)) continue;
    // Lowercase whatever the author wrote: register() defines min_text and
    // MIN_TEXT and nothing between, so `MIN(` used to be rewritten to a
    // `MIN_text` that does not exist and died as a raw TypeError.
    edits.push([m.index, m.index + m[1].length, `${m[1].toLowerCase()}_text`]);
  }
  let out = code;
  for (const [a, b, name] of edits.sort((x, y) => y[0] - x[0])) out = out.slice(0, a) + name + out.slice(b);
  return out;
};

// --- charts
//
// A chart is a sheet: a source ref, a kind, the two columns to plot, and
// optionally the column that splits them into series. Both engines build the
// same query out of that, so the picture the page draws and the rows the server
// exports are the same answer.

// One of the columns a generated statement names, quoted for the engine.
// `total`, `store` and `class` are AlaSQL keywords that will not parse as a bare
// identifier, so a column named one of them used to reach the reader as a parse
// error with a caret into SQL nobody wrote. Brackets are what AlaSQL quotes a
// name with, and a name holding a `]` is refused below rather than quoted, which
// is what keeps every statement built out of this well formed. `whose` is the
// thing that named the column -- a chart, a cohort table -- because the refusal
// is read beside the settings it is about.
const chartIdent = (whose, what, value) => {
  // Typed, not coerced: /^[A-Za-z_]\w*$/.test(NaN) reads the string "NaN" and
  // passes, which would splice a bare NaN into the select list.
  if (typeof value === "string" && /^[A-Za-z_][A-Za-z0-9_]*$/.test(value)) return `[${value}]`;
  throw new Error(explain(`A ${whose}'s ${what} has to be a column name.`, {
    Expected: "a plain column name, e.g. month",
    Received: show(value),
    Source: `this ${whose}'s settings`,
    Fix: `pick a column from the sheet the ${whose} reads`,
  }));
};

// Only what a query can reference: the page refuses any other prefix while
// loading, and a statement that runs on the server but not in the page is worse
// than one refused in both.
const writtenFrom = (whose, source) => {
  if (!/^@(?:table|query):[A-Za-z0-9_-]+$/.test(source ?? "")) {
    throw new Error(explain(`A ${whose} reads one table or query sheet.`, {
      Expected: "@table:doc_id or @query:doc_id",
      Received: show(source ?? null),
      Source: `this ${whose}'s settings`,
      Fix: `build the ${whose} from a table or query sheet, e.g. @table:orders`,
    }));
  }
};

// Every way a chart may be drawn, and the one list of them: `kindSpec` in
// src/Main.elm is this list on the other side of the wire, and browser_test.ts
// fails when the two stop agreeing. A kind nobody draws used to render as a
// line and say nothing, so a typo survived in the document forever.
//
// `box` is the one kind that changes the query. Every other chart reads one row
// per point and asks the page to draw it; a box is five numbers about the rows
// that share an x, and there is no way to carry five numbers on a point whose
// whole shape is one. So it aggregates here, where the sheet's own rows still
// are, rather than in a second reader on the page that the server's export and
// the MCP read would not have.
export const CHART_KINDS = ["line", "bar", "area", "scatter", "kpi", "box"];

// The quartiles a box is drawn from, in the order it draws them. Written down
// once because the select list and the page's reader have to name the same five
// columns, and a sixth would otherwise mean editing two lists that look
// unrelated.
const BOX_QUANTILES = [["q1", 0.25], ["med", 0.5], ["q3", 0.75]];

export const chartSql = ({ source, kind = "line", x, y, y2 = "", series = "" }) => {
  writtenFrom("chart", source);
  // A chart that was never given a kind is a line, which is what the page's own
  // decoder defaults to. Anything else that is not on the list is refused.
  if (!CHART_KINDS.includes(kind)) {
    // show(), never JSON.stringify: a chart document can hold a megabyte in that
    // field, and stringifying it twice is a two-megabyte message in the error
    // log. show() shortens, and says "number NaN" where JSON says "null".
    const meant = typeof kind === "string" ? nearest(kind, CHART_KINDS) : undefined;
    throw new Error(explain(`That is not a kind of chart.`, {
      Expected: CHART_KINDS.join(", "),
      Received: show(kind),
      "Did you mean": meant,
      Source: "this chart's settings",
      Fix: meant ? `set the kind to ${meant}` : `set the kind to one of ${CHART_KINDS.join(", ")}`,
    }));
  }
  const across = chartIdent("chart", "x column", x);
  const up = chartIdent("chart", "y column", y);
  // A box already splits its rows -- the five numbers per x are the split -- so
  // a second way to split them is a question with two answers. Refused by name
  // rather than ignored: a series box drawn as a plain one is a picture of rows
  // somebody thinks are separate.
  if (kind === "box") {
    for (const [what, value] of [["series column", series], ["second y column", y2]]) {
      if (value !== "") {
        throw new Error(explain(`A box chart is already the spread of its rows, so it takes no ${what}.`, {
          Expected: "a box chart with an x column and a y column and nothing else",
          Received: `${what} ${show(value)}`,
          Source: "this chart's settings",
          Fix: `clear the ${what}, or pick a kind that draws one`,
        }));
      }
    }
    const spread = BOX_QUANTILES.map(([name, at]) => `percentile(array(${up}), ${at}) as ${name}`).join(", ");
    // A blank cell is dropped, the way every other kind of chart drops a row
    // whose y does not read as a number. It is filtered here and not left to the
    // aggregate because `percentile` refuses a null outright: one blank cell
    // anywhere in the column took down the whole chart, every group of it,
    // including the groups that had nothing wrong with them -- and a blank cell
    // is the normal state of a spreadsheet, not an error in one. A group whose
    // every cell is blank has no rows left and simply does not appear, which is
    // what "nothing to draw here" looks like on every other kind.
    return `select ${across} as x, min(${up}) as lo, ${spread}, max(${up}) as hi from ${source} where ${up} is not null group by ${across} order by 1`;
  }
  // A tile is one number and its change, so there is no second scale to put a
  // second column on. Refused rather than dropped, for the reason every other
  // unread field here is: a document holding a y2 nobody draws is a chart whose
  // settings lie about what is on screen.
  if (y2 !== "" && kind === "kpi") {
    throw new Error(explain(`A kpi tile draws one number, so it has no second scale.`, {
      Expected: "a kpi with one y column",
      Received: `second y column ${show(y2)}`,
      Source: "this chart's settings",
      Fix: "clear the second y column, or draw this as a line",
    }));
  }
  // The second scale rides the same row as the first: one statement, one pass
  // over the source, and two columns the page gives two axes to.
  const plot = y2 === ""
    ? `${across} as x, ${up} as y`
    : `${across} as x, ${up} as y, ${chartIdent("chart", "second y column", y2)} as y2`;
  // Ordered by the x column, so the line is drawn in the order it is read and
  // two runs of the same chart agree; by the series first when there is one, so
  // each series arrives whole and in that same order. Absent or blank is the one
  // unnamed series every chart drew before there was more than one: clearing the
  // box in the page writes "", and a chart that broke when you emptied a field
  // you never filled is worse than one series. Anything else goes through
  // chartIdent, so a series held as a number -- or as a null somebody hand-wrote
  // into the document -- is refused by name the way an x column is rather than
  // quietly drawing one series.
  //
  // The series' position is counted and not typed: it is the third column of a
  // chart with one scale and the fourth of a chart with two, and `order by 3` on
  // the second of those sorts by the second scale's values.
  return series === ""
    ? `select ${plot} from ${source} order by 1`
    : `select ${plot}, ${chartIdent("chart", "series column", series)} as series from ${source} order by ${
      y2 === "" ? 3 : 4
    }, 1`;
};

// --- cohorts
//
// A cohort table, written once. `chartSql` runs on every read of a chart sheet,
// because a chart keeps its settings and nothing else; this runs at creation
// instead -- the text it answers becomes an ordinary query sheet's `code`, which
// its owner then edits, and there is no cohort sheet type for anything to build
// it from a second time.

// How much of `date_trunc`'s ISO string names the cohort. ISO 8601 writes a year
// in four characters, a month in seven and a day in ten, and a quarter and a
// week truncate to a day. One entry per unit `UNITS` lists, and these keys are
// what a grain is checked against rather than that list: a unit added there with
// no width here is refused by name rather than reaching `substr` as an undefined
// length.
const COHORT_LABEL = { year: 4, quarter: 10, month: 7, week: 10, day: 10, hour: 13, minute: 16, second: 19 };

const refuseTaken = (whose, fields) => {
  for (const [what, column, taken] of fields) {
    if (taken.includes(column)) {
      throw new Error(explain(`A ${whose}'s ${what} has to be a column it does not already name.`, {
        Expected: `a ${what} other than ${taken.join(", ")}`,
        Received: show(column),
        Source: `this ${whose}'s settings`,
        Fix: `rename that column in the sheet, or pick a different ${what}`,
      }));
    }
  }
};

export const cohortSql = ({ source, date, key, value = "", grain }) => {
  writtenFrom("cohort table", source);
  if (!Object.hasOwn(COHORT_LABEL, grain)) {
    const grains = Object.keys(COHORT_LABEL);
    const meant = typeof grain === "string" ? nearest(grain, grains) : undefined;
    throw new Error(explain(`That is not a period to group a cohort by.`, {
      Expected: grains.join(", "),
      Received: show(grain),
      "Did you mean": meant,
      Source: "this cohort table's settings",
      Fix: meant ? `group the cohort by ${meant}` : `group the cohort by one of ${grains.join(", ")}`,
    }));
  }
  const when = chartIdent("cohort table", "date column", date);
  const who = chartIdent("cohort table", "key column", key);
  // A cohort table with no value column counts its keys and nothing else, the
  // way a chart with no series draws one. Blank is what the page writes for a
  // sheet holding no money column, and a sheet that broke over an empty field
  // nobody filled is worse than a table of counts.
  const amount = value === "" ? "" : chartIdent("cohort table", "value column", value);
  const period = `${grain}_no`;
  // The names this statement gives columns of its own. A source column spelled
  // one of them lands in the generated SQL twice, and AlaSQL answers rows rather
  // than refusing: a key named `cohort` is joined against the truncated first
  // date and matches nothing, so the table comes back empty; one named
  // `<grain>_no` overwrites the period number in every row; and a value summed
  // off the key or the date column is text AlaSQL drops, so the field is missing
  // from every row rather than zero. Which collisions corrupt and which happen
  // to survive is AlaSQL's own alias shadowing, which promises nothing, so all
  // of them are refused. A blank value is no column and collides with none of
  // them, because `chartIdent` has already refused an empty date and key.
  const named = ["cohort", "first_seen", "active", period];
  refuseTaken("cohort table", [
    ["date column", date, named],
    ["key column", key, named],
    ["value column", value, [...named, date, key]],
  ]);
  // `min_text` and not `min`: AlaSQL's `min` drops text and a date column reaches
  // the engine as the ISO text it is stored as, so the first period a key appears
  // in would be null for every key.
  const first =
    `select ${who}, date_trunc('${grain}', first_seen) as cohort from (select ${who}, min_text(${when}) as first_seen from ${source} group by ${who})`;
  const carried = amount === "" ? "" : `, o.${amount} as ${amount}`;
  const each = `select substr(c.cohort, 1, ${
    COHORT_LABEL[grain]
  }) as cohort, date_diff('${grain}', c.cohort, o.${when}) as ${period}, o.${who} as ${who}${carried} from ${source} o join (${first}) c on c.${who} = o.${who}`;
  const measures = amount === ""
    ? ""
    : `, round(sum(${amount}), 2) as ${amount}, round(sum(${amount}) / count(distinct ${who}), 2) as [${value}_per_active]`;
  return `select cohort, ${period}, count(distinct ${who}) as active${measures} from (${each}) group by cohort, ${period} order by cohort, ${period}`;
};

// --- scores
//
// Recency, frequency and monetary scores per key, written once the way a cohort
// table is. This is quantile scoring and not clustering, which is `kmeansSql`'s
// job: no distances, no centroids and no combined segment label. Each score is
// `ntile` over one measure, and the
// highest bucket is the best: the most recent, the most frequent, the most
// spent. Recency orders by the last date seen and never by `now()`, so the two
// hosts answer the same scores on different days. `applyWindows` splits a tie
// in the order the rows arrive, which the outer `order by` makes key order.

// Percentiles are the finest grade anybody names. Past them a score is a row
// rank, which `row_number()` already answers.
const RFM_BUCKETS_MAX = 100;

export const rfmSql = ({ source, date, key, value, buckets }) => {
  writtenFrom("score table", source);
  if (!Number.isInteger(buckets) || buckets < 2 || buckets > RFM_BUCKETS_MAX) {
    throw new Error(explain(`A score table's buckets have to be a whole number from 2 to ${RFM_BUCKETS_MAX}.`, {
      Expected: `a whole number from 2 to ${RFM_BUCKETS_MAX}, e.g. 5`,
      Received: show(buckets ?? null),
      Source: "this score table's settings",
      Fix: "score into 5 buckets, the usual RFM scale",
    }));
  }
  const when = chartIdent("score table", "date column", date);
  const who = chartIdent("score table", "key column", key);
  const amount = chartIdent("score table", "value column", value);
  // A source column spelled like an output name lands in the generated SQL
  // twice, and AlaSQL's alias shadowing decides which one a row keeps.
  const named = ["last_seen", "orders", "r", "f", "m"];
  refuseTaken("score table", [
    ["date column", date, named],
    ["key column", key, [...named, date]],
    ["value column", value, [...named, date, key]],
  ]);
  // `max_text` and not `max`: AlaSQL's `max` drops text, and a date reaches the
  // engine as its ISO text.
  const each =
    `select ${who}, max_text(${when}) as last_seen, count(*) as orders, round(sum(${amount}), 2) as ${amount} from ${source} group by ${who}`;
  const score = (measure, name) => `ntile(${buckets}) over (order by ${measure}) as ${name}`;
  return `select ${who}, last_seen, orders, ${amount}, ${score("last_seen", "r")}, ${score("orders", "f")}, ${
    score(amount, "m")
  } from (${each}) order by ${who}`;
};

// --- segments
//
// A k-means segment table, written once the way a cohort table is: every source
// row's key and clustered columns, and the 1-based segment `kmeans_assign`
// reads back off one `kmeans` over the whole source. Scaling stays the
// author's: the statement divides nothing, so a column in dollars outweighs one
// in years until the owner rescales it in the code.

// Past this a segmentation is a list nobody reads as segments, and every pass
// costs points times k.
const KMEANS_K_MAX = 20;
// With KMEANS_POINTS in register(), this bounds one pass of `kmeans`: points
// times k times dimensions.
export const KMEANS_DIMS = 12;

export const kmeansSql = ({ source, key, columns, k }) => {
  writtenFrom("segment table", source);
  if (!Number.isInteger(k) || k < 2 || k > KMEANS_K_MAX) {
    throw new Error(explain(`A segment table's k has to be a whole number from 2 to ${KMEANS_K_MAX}.`, {
      Expected: `a whole number of segments from 2 to ${KMEANS_K_MAX}, e.g. 3`,
      Received: show(k ?? null),
      Source: "this segment table's settings",
      Fix: "ask for the number of segments you mean to read, e.g. 3",
    }));
  }
  if (!Array.isArray(columns) || !columns.length || columns.length > KMEANS_DIMS) {
    throw new Error(explain(`A segment table clusters on 1 to ${KMEANS_DIMS} columns.`, {
      Expected: `a list of 1 to ${KMEANS_DIMS} column names, e.g. ["lat", "lon"]`,
      Received: show(columns ?? null),
      Source: "this segment table's settings",
      Fix: "list the numeric columns that tell the segments apart",
    }));
  }
  const who = chartIdent("segment table", "key column", key);
  const on = columns.map((column) => chartIdent("segment table", "clustered column", column));
  // A source column spelled like the output name lands in the generated SQL
  // twice, and AlaSQL's alias shadowing decides which one a row keeps.
  refuseTaken("segment table", [
    ["key column", key, ["segment"]],
    ...columns.map((column, i) => ["clustered column", column, ["segment", key, ...columns.slice(0, i)]]),
  ]);
  const point = on.map((column) => `s.${column}`).join(", ");
  return `select s.${who}, ${point}, kmeans_assign(m.centroids, ${point}) as segment from ${source} s, (select kmeans(${k}, ${
    on.map((column) => `array(${column})`).join(", ")
  }) as centroids from ${source}) m order by segment, s.${who}`;
};

// --- resolving a query's sheet references
//
// Both engines do the same three things before AlaSQL sees anything: load every
// sheet the query names, check what only a loaded sheet can be checked against,
// and lift the windows out. Where the sheets come from is the only real
// difference — a database and an access check on the server, a library entry or
// an automerge document in the page — so that is the part each one passes in.

/** A sheet's rows keyed by column name, which is the shape a query reads. */
export const toRecords = ([cols, ...rows]) =>
  rows.map((row) => Object.fromEntries(Object.values(cols).map((c) => [c.name, row[c.key]])));

/** Load every sheet a query names, in the order it names them.
 *
 * `fetch(id)` returns one as `[cols, ...rows]`. `onLoad(id, rows)` runs after
 * each, which is where the server spends its row budget; the page has no budget.
 * `describing` skips the column-type check, because a sheet whose cells are wrong
 * is exactly the sheet `describe` exists to inspect. `stages`, when given,
 * collects one timed row per load; a plain run passes none.
 */
export const loadRefs = async (ids, { path, describing, fetch, onLoad, stages }) => {
  const docs = {}, colsOf = {};
  for (const id of ids) {
    if (docs[id]) continue;
    checkRefPath(path, id);
    await timed(stages, `load @${id}`, null, async () => {
      const sheet = await fetch(id);
      colsOf[id] = Object.values(sheet[0]);
      docs[id] = toRecords(sheet);
      await onLoad(id, docs[id]);
      // Only table sheets: a query column keeps its source column's declared type,
      // so `cast(price as string) as price` would trip a check meant for a bad cell.
      if (!describing && id.startsWith("table:")) checkColumnTypes(id, colsOf[id], docs[id]);
      return docs[id];
    }, (rows) => rows.length);
  }
  return { docs, colsOf };
};

/** Everything the engine cannot be trusted with, in the order it has to happen:
 * a cell reference needs its sheet loaded to be checked, unpivot needs its column
 * names, and a window has to be lifted out of whatever those two produce.
 *
 * The extremes pass reads the author's own call, so it runs before either
 * rewrite touches the text -- and before the window pass, which would otherwise
 * be handed a `min_text` it does not know by that name.
 */
export const planQuery = (sql, cells, docs, colsOf) => {
  checkCells(cells, docs, colsOf);
  checkJoinRows(sql, docs);
  checkPivot(sql);
  return rewriteWindows(rewriteUnpivot(rewriteExtremes(sql, colsOf), colsOf));
};

// --- registration

export const register = (alasql) => {
  const fn = alasql.fn, aggr = alasql.aggr;

  // AlaSQL ships these uppercase only, so `select var(x)` fails with
  // "alasql.fn.var is not a function". Alias the lowercase spellings.
  for (const name of ["VAR", "VARP", "STDEV", "STDEVP", "STDDEV", "STD", "MEDIAN", "QUART", "QUART2", "QUART3"])
    if (aggr[name]) aggr[name.toLowerCase()] = aggr[name];

  // Aggregates: (value, accumulator, stage) with stage 1 init, 2 accumulate, 3 finalize.
  const collect = (finish) => (v, acc, stage) => {
    if (stage === 1) return v === null || v === undefined ? [] : [v];
    if (stage === 2) return (v === null || v === undefined ? acc : (acc.push(v), acc));
    return finish(acc);
  };

  // AlaSQL compiles min()/max() inline, restricted to numbers, bigints and
  // dates, and turns a text value into undefined: `min(code)` does not return
  // the first code, it drops the column out of the result entirely. The compiler
  // never consults alasql.aggr for those two names, so they cannot be replaced —
  // these compare as text, and checkResultColumns points a dropped min() here.
  const extreme = (keep) => (v, acc, stage) => {
    if (stage === 1) return v === null || v === undefined ? undefined : String(v);
    if (stage === 2) {
      if (v === null || v === undefined) return acc;
      const val = String(v);
      return acc === undefined || keep(val, acc) ? val : acc;
    }
    return acc === undefined ? null : acc;
  };
  aggr.min_text = aggr.MIN_TEXT = extreme((val, acc) => val < acc);
  aggr.max_text = aggr.MAX_TEXT = extreme((val, acc) => val > acc);

  aggr.array_agg = aggr.ARRAY_AGG = collect((acc) => acc);
  aggr.mode = aggr.MODE = collect((acc) => {
    const counts = new Map();
    for (const v of acc) counts.set(v, (counts.get(v) ?? 0) + 1);
    let best, top = -1;
    // Ties resolve to the first value seen, so repeated runs agree.
    for (const [v, n] of counts) if (n > top) [best, top] = [v, n];
    return best ?? null;
  });

  // Scalars over arrays. Two-argument aggregates are not expressible in
  // AlaSQL's single-value protocol, so these pair with array(): corr(array(x), array(y)).
  fn.percentile = (xs, p) => {
    const v = nums("percentile", 1, xs);
    if (typeof p !== "number" || p < 0 || p > 1) {
      throw fail(
        "percentile() argument 2",
        "a fraction between 0 and 1",
        show(p),
        "use 0.5 for the median, 0.95 for p95",
      );
    }
    if (!v.length) return null;
    return quantile(v, p);
  };
  fn.corr = (xs, ys) => {
    const [x, y] = pair("corr", xs, ys), { sxy, sxx, syy } = fit(x, y);
    return sxx === 0 || syy === 0 ? null : sxy / Math.sqrt(sxx * syy);
  };
  fn.regr_slope = (xs, ys) => {
    const [x, y] = pair("regr_slope", xs, ys), { sxy, sxx } = fit(x, y);
    return sxx === 0 ? null : sxy / sxx;
  };
  fn.regr_intercept = (xs, ys) => {
    const [x, y] = pair("regr_intercept", xs, ys), { mx, my, sxy, sxx } = fit(x, y);
    return sxx === 0 ? null : my - (sxy / sxx) * mx;
  };
  fn.r2 = (xs, ys) => {
    const c = fn.corr(xs, ys);
    return c === null ? null : c * c;
  };

  fn.regr_predict = (xs, ys, at) => {
    const [x, y] = pair("regr_predict", xs, ys), { mx, my, sxy, sxx } = fit(x, y);
    const a = num("regr_predict", 3, at);
    return sxx === 0 ? null : my + (sxy / sxx) * (a - mx);
  };
  // The spread of the points around the line, in the units of y. A slope with no
  // standard error beside it is a number nobody can argue with, which is worse
  // than one nobody can use.
  fn.regr_stderr = (xs, ys) => {
    const [x, y] = pair("regr_stderr", xs, ys), { mx, my, sxy, sxx } = fit(x, y);
    if (sxx === 0 || x.length < 3) return null;
    const b = sxy / sxx, a = my - b * mx;
    let ss = 0;
    for (let i = 0; i < x.length; i++) ss += (y[i] - (a + b * x[i])) ** 2;
    return Math.sqrt(ss / (x.length - 2));
  };

  // Curve fitting by the transform that straightens the curve: a log on y for
  // exponential decay (a well, a half-life), a log on both for a power law (a
  // learning curve). A value at or below zero has no logarithm, so it is refused
  // by name rather than dropped, which would bend the fit silently.
  const curve = (name, xs, ys, at, logX) => {
    const [x0, y0] = pair(name, xs, ys);
    const positive = (arg, v) => {
      if (v <= 0) {
        throw fail(
          `${name}() argument ${arg}`,
          "only values above zero",
          show(v),
          "a log curve has no value at zero: filter those rows out first",
        );
      }
      return Math.log(v);
    };
    const x = logX ? x0.map((v) => positive(1, v)) : x0;
    const y = y0.map((v) => positive(2, v));
    const { mx, my, sxy, sxx } = fit(x, y);
    if (sxx === 0) return null;
    const a = num(name, 3, at);
    return Math.exp(my + (sxy / sxx) * ((logX ? positive(3, a) : a) - mx));
  };
  fn.fit_exponential = (xs, ys, at) => curve("fit_exponential", xs, ys, at, false);
  fn.fit_power = (xs, ys, at) => curve("fit_power", xs, ys, at, true);

  // Arps hyperbolic decline, q(t) = qi / (1 + b*Di*t)^(1/b), which is the curve a
  // well actually follows and the one every reserve report is written in. The
  // shape parameter b sits in the exponent, so no log straightens this one: it is
  // fit by nonlinear least squares instead. Levenberg-Marquardt, because the
  // damping shrinks a step that lands where the curve has no value rather than
  // taking it, which is where plain Gauss-Newton walks off.
  const HYPERBOLIC_STEPS = 200;
  // b is the shape: 0 is exponential decline, 1 is harmonic, and larger is a
  // flatter tail. Past 2 the curve integrates to infinite cumulative production,
  // so a fit that wants to go there is one this family will not answer -- not a
  // statement about whether the points decline, which they may do perfectly well.
  const B_MAX = 2;
  // The fit walks every point four times per step -- the residual and three
  // Jacobian columns -- so its cost is points times steps, and only the steps
  // were bounded: a million pairs out of one sheet was seconds of blocked,
  // uninterruptible event loop from one query. The linear fits need no such
  // bound; they are a single pass. Two orders of magnitude above any decline
  // anybody reads by hand, which is a monthly rate over a well's life.
  const HYPERBOLIC_POINTS = 5000;
  // A step this small beside the parameter it moves has stopped saying anything,
  // whatever the damping does next.
  const LM_STOP = 1e-10;
  // The b column of the Jacobian is a 0/0 limit at b = 0 -- the exponential
  // member of the family, and where this fit starts. A forward difference crosses
  // that point without a series expansion for it.
  const LM_DIFF = 1e-7;
  const arps = (qi, di, b, t) => qi * Math.exp(b === 0 ? -di * t : -Math.log1p(b * di * t) / b);
  // Gaussian elimination with partial pivoting over the damped normal equations.
  // null is a matrix singular even under the damping, which the caller answers by
  // damping harder.
  const solve3 = (a, rhs) => {
    const m = a.map((row, i) => [...row, rhs[i]]);
    for (let c = 0; c < 3; c++) {
      let piv = c;
      for (let r = c + 1; r < 3; r++) if (Math.abs(m[r][c]) > Math.abs(m[piv][c])) piv = r;
      [m[c], m[piv]] = [m[piv], m[c]];
      if (m[c][c] === 0) return null;
      for (let r = c + 1; r < 3; r++) {
        const f = m[r][c] / m[c][c];
        for (let k = c; k < 4; k++) m[r][k] -= f * m[c][k];
      }
    }
    const out = [0, 0, 0];
    for (let r = 2; r >= 0; r--) {
      let s = m[r][3];
      for (let k = r + 1; k < 3; k++) s -= m[r][k] * out[k];
      out[r] = s / m[r][r];
    }
    return out.every(Number.isFinite) ? out : null;
  };
  fn.fit_hyperbolic = (xs, ys, at) => {
    const [x0, y0] = pair("fit_hyperbolic", xs, ys);
    // Three parameters need three points: two of them are fit exactly by every b.
    if (x0.length < 3)
      throw fail("fit_hyperbolic()", "at least 3 pairs", `${x0.length}`, "widen the query so more rows match");
    if (x0.length > HYPERBOLIC_POINTS) {
      throw fail(
        "fit_hyperbolic()",
        `at most ${HYPERBOLIC_POINTS} pairs`,
        `${x0.length}`,
        "aggregate the rate to one point per period first, e.g. group by month",
      );
    }
    for (const v of y0) {
      if (v <= 0) {
        throw fail(
          "fit_hyperbolic() argument 2",
          "only values above zero",
          show(v),
          "a decline curve has no rate at or below zero: filter those rows out first",
        );
      }
    }
    // The step, the damping and LM_STOP all read in the units of the columns, so
    // the same curve in barrels and in cubic feet, or over months and over unix
    // seconds, was a different search each time: one settled, one ran out of
    // steps, one never moved b off the exponential seed. Fit the shape in a unit
    // box instead and undo the scaling in the answer. A spread throws a
    // RangeError past about a hundred thousand arguments and a query loads up to
    // MAX_QUERY_ROWS points, so the three extremes are taken in one pass.
    let scale = y0[0], t0 = x0[0], tn = x0[0];
    for (let i = 1; i < x0.length; i++) {
      if (y0[i] > scale) scale = y0[i];
      if (x0[i] < t0) t0 = x0[i];
      if (x0[i] > tn) tn = x0[i];
    }
    // Every x the same is a column with no time in it, which the exponential
    // seed below answers null on; the span only has to be non-zero to get there.
    const span = tn - t0 || 1;
    const x = x0.map((v) => (v - t0) / span);
    const y = y0.map((v) => v / scale);
    // The exponential fit is the b = 0 member of the same family, so it is both
    // the starting point and the answer for a rate that declines straight.
    const { mx, my, sxy, sxx } = fit(x, y.map(Math.log));
    if (sxx === 0) return null;
    const slope = sxy / sxx;
    let p = [Math.exp(my - slope * mx), -slope, 0];
    const cost = (guess) => {
      let sum = 0;
      for (let i = 0; i < x.length; i++) sum += (arps(guess[0], guess[1], guess[2], x[i]) - y[i]) ** 2;
      // Where the curve has no value the step is not one to take, at any damping.
      return Number.isFinite(sum) ? sum : Infinity;
    };
    let best = cost(p), lambda = 1e-3, steps = 0, climbing = false;
    for (;;) {
      if (++steps > HYPERBOLIC_STEPS) {
        throw new Error(explain(`fit_hyperbolic() did not settle on a curve.`, {
          Limit: `${HYPERBOLIC_STEPS} least-squares steps`,
          Received: `${HYPERBOLIC_STEPS} steps, still moving at b ${p[2].toPrecision(4)}`,
          Source: "the points handed to fit_hyperbolic()",
          Fix: "read fit_exponential() or fit_power() on the same points to see the shape they have",
        }));
      }
      const base = x.map((t) => arps(p[0], p[1], p[2], t));
      const cols = p.map((v, j) => {
        const h = LM_DIFF * (Math.abs(v) || 1);
        const bumped = [...p];
        bumped[j] = v + h;
        return x.map((t, i) => (arps(bumped[0], bumped[1], bumped[2], t) - base[i]) / h);
      });
      const dot = (r, c) => {
        let sum = 0;
        for (let i = 0; i < x.length; i++) sum += cols[r][i] * cols[c][i];
        return sum;
      };
      // Damping is added to the diagonal rather than scaling it, because a
      // parameter the curve is flat in has a diagonal of zero — b on a rate that
      // does not move — and scaling zero leaves the matrix singular forever.
      const diag = [dot(0, 0), dot(1, 1), dot(2, 2)];
      const damp = lambda * (Math.max(...diag) || 1);
      const g = [0, 1, 2].map((r) => {
        let sum = 0;
        for (let i = 0; i < x.length; i++) sum += cols[r][i] * (y[i] - base[i]);
        return sum;
      });
      // b is at one end of its range and the fit wants to leave. Hold it there
      // and solve the other two on their own: a step whose b component is thrown
      // away afterwards is not the step the other two needed, and the fit crawls
      // a millionth at a time instead of converging.
      const bStep = g[2] / (diag[2] || 1);
      const held = (p[2] === 0 && bStep < 0) || (p[2] === B_MAX && bStep > 0);
      // Whether the settled fit is pinned at the top of the range and the
      // descent still pushes past it — read after the loop, where the fit is the
      // one it kept rather than a trial step that overshot and came back.
      climbing = p[2] === B_MAX && bStep > LM_STOP;
      const normal = [0, 1, 2].map((r) =>
        [0, 1, 2].map((c) => (held && (r === 2 || c === 2) ? (r === c ? 1 : 0) : r === c ? diag[r] + damp : dot(r, c)))
      );
      const d = solve3(normal, held ? [g[0], g[1], 0] : g);
      if (!d) {
        lambda *= 10;
        continue;
      }
      if (d.every((v, j) => Math.abs(v) <= LM_STOP * (Math.abs(p[j]) + LM_STOP))) break;
      const next = p.map((v, j) => v + d[j]);
      // Both ends of b's range stop the step rather than refusing it: zero is the
      // exponential member of the family and B_MAX is a curve that never runs
      // out, and a trial step that overshoots either one comes back.
      next[2] = Math.min(Math.max(next[2], 0), B_MAX);
      const s = cost(next);
      if (s < best) {
        p = next;
        best = s;
        lambda = Math.max(lambda / 10, 1e-12);
      } else { lambda *= 10; }
    }
    if (climbing) {
      // What actually happened, and not "these points do not decline": a fit
      // pinned at B_MAX wanting more b is a tail that flattens faster than the
      // family's bound allows, which an ordinary shale decline reaches. The old
      // headline sent whoever read it looking for a rate column that rises,
      // which was never their problem, and called a flattening tail steepening.
      throw new Error(explain(`fit_hyperbolic() could not settle on a decline exponent inside its range.`, {
        Expected: `a best fit with b between 0 and ${B_MAX}`,
        Received: `a fit pinned at b ${B_MAX} whose tail is still flattening there`,
        Source: "the points handed to fit_hyperbolic()",
        Fix:
          `b past ${B_MAX} integrates to infinite cumulative production, so it is not fit here: read fit_exponential() on the same points, or fit the later rows on their own`,
      }));
    }
    const a = num("fit_hyperbolic", 3, at);
    const answer = arps(p[0], p[1], p[2], (a - t0) / span) * scale;
    if (!Number.isFinite(answer)) {
      // b = 0 is the exponential member and has no asymptote, so there is no x to
      // name: -1 / (0 * di) is -Infinity, a bound every x already clears. That
      // one overflowed instead.
      const [expected, why] = p[2] === 0
        ? ["an x where the fitted rate is still a number", "the rate overflows there"]
        : [
          `an x above ${(t0 + span * (-1 / (p[2] * p[1]))).toPrecision(4)}, where the fitted curve begins`,
          "the curve is vertical there",
        ];
      throw fail(
        "fit_hyperbolic() argument 3",
        expected,
        show(a),
        `${why}: predict inside the ${t0} to ${tn} the points cover`,
      );
    }
    return answer;
  };

  // A distribution on an input, sampled without Math.random: the same call has
  // to answer the same number on the server and in the page, and a sheet whose
  // numbers move on every reload is a sheet nobody can check. The seed is a
  // hash of the whole call, the function's own name included, so one `trial`
  // column draws an independent-looking value per distribution and per
  // parameter set and still draws the same one tomorrow.
  //
  // Only sample_normal reads a transcendental. ECMAScript pins sqrt to IEEE 754
  // and leaves log and cos implementation-defined, so a normal draw is bit-equal
  // wherever one engine runs both hosts and may differ in the last ulp between a
  // browser and the server; uniform and triangular are exact everywhere.
  //
  // The hash reads the call one value at a time: kmeans() hands it every
  // coordinate it clusters, and one string of all of them is a copy of the sheet.
  const seedOf = (name, args) => {
    // FNV-1a, 32 bits: Math.imul is the multiply that wraps the way the hash is
    // defined to, rather than the one that loses the low bits to a double.
    let h = 0x811c9dc5;
    const feed = (text) => {
      for (const ch of text) h = Math.imul(h ^ ch.codePointAt(0), 0x01000193) >>> 0;
    };
    feed(`${name}(`);
    for (const [i, a] of args.entries()) feed(i ? `,${a}` : `${a}`);
    feed(")");
    return h;
  };
  // mulberry32: one multiply-xor round per draw, every step on a uint32.
  const mulberry32 = (seed) => {
    let a = seed >>> 0;
    return () => {
      a = (a + 0x6d2b79f5) >>> 0;
      let t = Math.imul(a ^ (a >>> 15), 1 | a);
      t = (t + Math.imul(t ^ (t >>> 7), 61 | t)) ^ t;
      return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
    };
  };
  // A range whose bounds are the wrong way round is a typo, and a mode outside
  // it is a triangle with no apex. Both are refused by name: a sampler that
  // quietly answers NaN puts it in a percentile and every number downstream.
  const range = (name, lo, hi) => {
    if (lo > hi) {
      throw fail(
        `${name}() argument 2`,
        `a low bound at or below the high bound ${hi}`,
        show(lo),
        "swap the two bounds",
      );
    }
  };
  // Two finite arguments a mulberry32 fraction is multiplied against or added
  // to can still overflow a double -- (hi - lo) past 1.8e308, or (mu + sd *
  // factor) past it the same way -- and 0 times that Infinity is a NaN, not an
  // Infinity, on whichever draw lands on a fraction of exactly zero. Checked on
  // the draw itself rather than the bounds, because a wide range and a seed
  // that never overflows it is not this sampler's problem to refuse.
  const drawn = (name, v, a, b) => {
    if (!Number.isFinite(v)) {
      throw fail(
        `${name}()`,
        "a draw that fits in a finite number",
        `${a} and ${b} drew ${show(v)}`,
        "scale the inputs down before sampling",
      );
    }
    return v;
  };
  fn.sample_uniform = (seed, lo, hi) => {
    const s = num("sample_uniform", 1, seed);
    const a = num("sample_uniform", 2, lo), b = num("sample_uniform", 3, hi);
    range("sample_uniform", a, b);
    return drawn("sample_uniform", a + (b - a) * mulberry32(seedOf("sample_uniform", [s, a, b]))(), a, b);
  };
  fn.sample_normal = (seed, mu, sd) => {
    const s = num("sample_normal", 1, seed);
    const m = num("sample_normal", 2, mu), d = num("sample_normal", 3, sd);
    if (d < 0) {
      throw fail(
        "sample_normal() argument 3",
        "a standard deviation at or above zero",
        show(d),
        "a spread is a distance: use 0 for an input nobody is unsure about",
      );
    }
    const draw = mulberry32(seedOf("sample_normal", [s, m, d]));
    // Box-Muller, over 1 - u rather than u: mulberry32 answers [0, 1) and
    // log(0) is -Infinity, which would write one Infinity per 4 billion draws.
    const v = m + d * Math.sqrt(-2 * Math.log(1 - draw())) * Math.cos(2 * Math.PI * draw());
    return drawn("sample_normal", v, m, d);
  };
  fn.sample_triangular = (seed, lo, mode, hi) => {
    const s = num("sample_triangular", 1, seed);
    const a = num("sample_triangular", 2, lo), c = num("sample_triangular", 3, mode);
    const b = num("sample_triangular", 4, hi);
    range("sample_triangular", a, b);
    if (c < a || c > b) {
      throw fail(
        "sample_triangular() argument 3",
        `a mode between ${a} and ${b}`,
        show(c),
        "the most likely value lies inside the range, not outside it",
      );
    }
    // One value is the whole distribution, and the inverse CDF divides by the
    // width to find it.
    if (a === b) return a;
    const u = mulberry32(seedOf("sample_triangular", [s, a, c, b]))();
    const at = (c - a) / (b - a);
    const v = u < at ? a + Math.sqrt(u * (b - a) * (c - a)) : b - Math.sqrt((1 - u) * (b - a) * (b - c));
    return drawn("sample_triangular", v, a, b);
  };

  // Multiple regression: a plane rather than a line. AlaSQL's aggregate protocol
  // takes one value, so the columns arrive as arrays the way corr() takes them --
  // ols(array(y), array(x1), array(x2)) -- and the answer is the coefficient
  // array [b0, b1, ...] that ols_predict() reads back one row at a time.
  //
  // One call walks the points once per pair of terms, and logit() pays that again
  // on every step, so the points and the terms are both bounded here.
  const OLS_POINTS = 5000;
  const OLS_TERMS = 12;
  // Every predictor is scaled to a root-mean-square of one and the normal
  // equations are divided through by their own total weight, so Cauchy-Schwarz
  // holds every entry in [-1, 1] and a pivot this small is a column the ones
  // before it already span -- whatever units the sheet happens to hold.
  const OLS_SINGULAR = 1e-10;
  const design = (name, ys, xss) => {
    const y = nums(name, 1, ys);
    if (!xss.length) {
      throw fail(
        `${name}()`,
        "at least one predictor column",
        "only the response",
        `add one, e.g. ${name}(array(y), array(x))`,
      );
    }
    if (xss.length > OLS_TERMS) {
      throw fail(
        `${name}()`,
        `at most ${OLS_TERMS} predictor columns`,
        `${xss.length}`,
        "fit the predictors that move the answer and drop the rest",
      );
    }
    const cols = xss.map((xs, j) => nums(name, j + 2, xs));
    for (const [j, x] of cols.entries()) {
      if (x.length !== y.length) {
        throw fail(
          `${name}()`,
          "every array the same length",
          `${y.length} values in argument 1 and ${x.length} in argument ${j + 2}`,
          "aggregate every column over the same rows",
        );
      }
    }
    if (y.length > OLS_POINTS) {
      throw fail(
        `${name}()`,
        `at most ${OLS_POINTS} points`,
        `${y.length}`,
        "aggregate the rows to one point per period first, e.g. group by month",
      );
    }
    // Fewer points than coefficients is a system with infinitely many answers,
    // and the one that came back would be whichever the pivoting happened on.
    if (y.length < cols.length + 1) {
      throw fail(
        `${name}()`,
        `at least one point per coefficient: ${cols.length + 1}, one per predictor and one intercept`,
        `${y.length} points`,
        "widen the query so more rows match, or fit fewer predictors",
      );
    }
    const scale = cols.map((x) => {
      // Squaring a raw value first overflows past 1e154 and underflows past
      // 1e-162, so a column of ordinary numbers in an extreme unit (nanoseconds
      // since epoch, say) squared to Infinity or to 0 and then reached the
      // divide as a zero column -- a real predictor refused as one the other
      // columns already span. Dividing by the largest magnitude first keeps
      // every squared term in [0, 1], so the sum cannot overflow, and multiplying
      // the root back out by that same magnitude cannot overflow either.
      let peak = 0;
      for (const v of x) if (Math.abs(v) > peak) peak = Math.abs(v);
      // A column of nothing but zeros has no scale to take out. It reaches the
      // solve as it is and comes back named as the column nothing identifies.
      if (peak === 0) return 1;
      let sum = 0;
      for (const v of x) sum += (v / peak) ** 2;
      return peak * Math.sqrt(sum / x.length);
    });
    return { y, cols: cols.map((x, j) => x.map((v) => v / scale[j])), scale };
  };
  // Column 0 of the design matrix is the intercept, which is all ones.
  const term = (cols, j, i) => (j === 0 ? 1 : cols[j - 1][i]);
  // The normal equations X'WX b = X'v, both sides divided through by the total
  // weight rather than by the point count: logit() reweights by p(1-p), and every
  // weight shrinking together is a fit running away rather than a column going
  // collinear, which is what an unweighted divisor turned the matrix into.
  const equations = (cols, w, v) => {
    const n = w.length, p = cols.length + 1, a = [], rhs = [];
    let total = 0;
    for (const x of w) total += x;
    for (let r = 0; r < p; r++) {
      const row = [];
      for (let c = 0; c < p; c++) {
        let s = 0;
        for (let i = 0; i < n; i++) s += w[i] * term(cols, r, i) * term(cols, c, i);
        row.push(s / total);
      }
      let s = 0;
      for (let i = 0; i < n; i++) s += term(cols, r, i) * v[i];
      a.push(row);
      rhs.push(s / total);
    }
    return [a, rhs];
  };
  // Gaussian elimination with partial pivoting. A failure carries the column it
  // stopped on, which is the predictor the columns before it reproduce.
  const gauss = (a, rhs) => {
    const p = rhs.length, m = a.map((row, i) => [...row, rhs[i]]);
    for (let c = 0; c < p; c++) {
      let piv = c;
      for (let r = c + 1; r < p; r++) if (Math.abs(m[r][c]) > Math.abs(m[piv][c])) piv = r;
      [m[c], m[piv]] = [m[piv], m[c]];
      if (Math.abs(m[c][c]) <= OLS_SINGULAR) return { singular: c };
      for (let r = c + 1; r < p; r++) {
        const f = m[r][c] / m[c][c];
        for (let k = c; k <= p; k++) m[r][k] -= f * m[c][k];
      }
    }
    const out = new Array(p).fill(0);
    for (let r = p - 1; r >= 0; r--) {
      let s = m[r][p];
      for (let k = r + 1; k < p; k++) s -= m[r][k] * out[k];
      out[r] = s / m[r][r];
    }
    // Every pivot cleared the tolerance and the answer still overflowed: there is
    // no column to name, and the refusal does not invent one.
    return out.every(Number.isFinite) ? { out } : { singular: 0 };
  };
  const singular = (name, c) =>
    c === 0
      ? fail(
        `${name}()`,
        "a system with one answer",
        "a singular one",
        "vary the predictor columns, or fit fewer of them",
      )
      : fail(
        `${name}()`,
        "predictor columns no other column reproduces",
        `argument ${c + 1}, which the columns before it already span`,
        "drop that column, or the one it repeats",
      );
  fn.ols = (ys, ...xss) => {
    const { y, cols, scale } = design("ols", ys, xss);
    const solved = gauss(...equations(cols, y.map(() => 1), y));
    if (!solved.out) throw singular("ols", solved.singular);
    // The predictors were solved scaled, so their coefficients come back scaled.
    // The intercept is in the units of y already.
    return solved.out.map((b, j) => (j === 0 ? b : b / scale[j - 1]));
  };
  const linear = (name, coefs, ats) => {
    // nums() reads a column somebody aggregated with array(), and its fix says
    // so. These coefficients are a fit's own answer instead, so a value that is
    // not one is refused here rather than sent off to wrap a number in array().
    if (!Array.isArray(coefs)) {
      throw fail(
        `${name}() argument 1`,
        "the coefficient array a fit answered",
        show(coefs),
        `fit in a subquery and read its column here, e.g. ${name}(m.coefs, x)`,
      );
    }
    const b = nums(name, 1, coefs);
    if (b.length !== ats.length + 1) {
      throw fail(
        `${name}()`,
        `one more coefficient than predictor values: ${ats.length + 1} for ${ats.length}`,
        `${b.length} coefficients`,
        "hand it the whole array the fit answered, and one value per predictor in the order they were fit",
      );
    }
    let sum = b[0];
    for (const [j, a] of ats.entries()) sum += b[j + 1] * num(name, j + 2, a);
    return sum;
  };
  fn.ols_predict = (coefs, ...ats) => linear("ols_predict", coefs, ats);

  // Logistic regression, for an outcome that happened or did not. Iteratively
  // reweighted least squares: the same normal equations, reweighted by p(1-p) and
  // solved again until the step stops moving them.
  const LOGIT_STEPS = 50;
  // A step this small beside the coefficient it moves has stopped saying
  // anything, the way LM_STOP reads for the decline fit.
  const LOGIT_STOP = 1e-10;
  // A residual under this on every point is a fit that has separated the two
  // outcomes rather than one that has settled.
  const LOGIT_FIT = 1e-8;
  fn.logit = (ys, ...xss) => {
    const { y, cols, scale } = design("logit", ys, xss);
    for (const v of y) {
      if (v !== 0 && v !== 1) {
        throw fail(
          "logit() argument 1",
          "only 0 and 1",
          show(v),
          "write the outcome as a 0/1 column, e.g. case when status = 'won' then 1 else 0 end",
        );
      }
    }
    // A boundary the predictors draw exactly is a likelihood with no maximum:
    // the coefficients run off to infinity and every further step still improves
    // the fit. It arrives two ways -- every point fitted exactly, and the weights
    // of every point but the tied ones collapsing until the reweighted matrix is
    // singular -- and both are this one refusal, because the coefficients a
    // bounded run hands back read as enormous effects rather than as a column
    // that gives the answer away.
    const separated = () =>
      fail(
        "logit()",
        "two outcomes the predictors do not separate",
        "a boundary that separates them exactly",
        "drop the predictor that already decides the outcome, or fit the rows where the two outcomes overlap",
      );
    let b = new Array(cols.length + 1).fill(0);
    for (let steps = 1;; steps++) {
      if (steps > LOGIT_STEPS) {
        throw new Error(explain(`logit() did not settle on a fit.`, {
          Limit: `${LOGIT_STEPS} reweighted least-squares steps`,
          Received: `${LOGIT_STEPS} steps, still moving`,
          Source: "the points handed to logit()",
          Fix: "fit fewer predictors, or check whether one of them already decides the outcome",
        }));
      }
      const w = [], resid = [];
      let exact = true;
      for (let i = 0; i < y.length; i++) {
        let eta = b[0];
        for (let j = 0; j < cols.length; j++) eta += b[j + 1] * cols[j][i];
        const p = 1 / (1 + Math.exp(-eta));
        w.push(p * (1 - p));
        resid.push(y[i] - p);
        if (Math.abs(y[i] - p) > LOGIT_FIT) exact = false;
      }
      if (exact) throw separated();
      const solved = gauss(...equations(cols, w, resid));
      // b starts at zero, so every p is a half and the first step weighs every
      // point the same: its matrix is the one ols() solves, and a singular one
      // there is a column another column reproduces. A singular one after it is
      // the weights collapsing, which is the fit running away.
      if (!solved.out) throw steps === 1 ? singular("logit", solved.singular) : separated();
      b = b.map((v, j) => v + solved.out[j]);
      if (solved.out.every((d, j) => Math.abs(d) <= LOGIT_STOP * (Math.abs(b[j]) + LOGIT_STOP))) break;
    }
    return b.map((v, j) => (j === 0 ? v : v / scale[j - 1]));
  };
  // exp() of a large negative number is 0 and of a large positive one is
  // Infinity, so both ends answer a probability rather than a NaN.
  fn.logit_predict = (coefs, ...ats) => 1 / (1 + Math.exp(-linear("logit_predict", coefs, ats)));

  // Segmentation: kmeans(k, array(x1), array(x2), ...) answers k centroids and
  // kmeans_assign(centroids, x1, x2, ...) reads back each row's 1-based cluster,
  // the way ols() and ols_predict() split a fit from its reading.
  //
  // Distance is raw Euclidean, so a column in dollars outweighs one in years:
  // scale the inputs first. Scaling here would make kmeans_assign carry the
  // scale beside the centroids.
  //
  // Start points are k-means++, drawn from mulberry32 the way the samplers draw,
  // and every step after is +, -, *, / and comparisons, which ECMAScript pins to
  // IEEE 754: both hosts answer bit for bit.
  const KMEANS_STEPS = 100;
  // design() validates a response beside its predictors and scales them, which
  // a clustering has no use for. With KMEANS_DIMS this bounds the cost design()
  // bounds.
  const KMEANS_POINTS = 5000;
  const closest = (point, centroids) => {
    let best = 0, far = Infinity;
    for (let j = 0; j < centroids.length; j++) {
      let d = 0;
      for (let c = 0; c < point.length; c++) d += (point[c] - centroids[j][c]) ** 2;
      // Strict: a tie goes to the lower index.
      if (d < far) [best, far] = [j, d];
    }
    return [best, far];
  };
  fn.kmeans = (k_, ...xss) => {
    const k = num("kmeans", 1, k_);
    if (!Number.isInteger(k) || k < 2 || k > KMEANS_K_MAX) {
      throw fail(
        "kmeans() argument 1",
        `a whole number of clusters from 2 to ${KMEANS_K_MAX}`,
        show(k_),
        "ask for the number of segments you mean to read, e.g. kmeans(3, array(x), array(y))",
      );
    }
    if (!xss.length)
      throw fail("kmeans()", "at least one column to cluster on", "only k", "add one, e.g. kmeans(3, array(x))");
    if (xss.length > KMEANS_DIMS) {
      throw fail(
        "kmeans()",
        `at most ${KMEANS_DIMS} columns to cluster on`,
        `${xss.length}`,
        "cluster on the columns that tell the segments apart and drop the rest",
      );
    }
    const cols = xss.map((xs, j) => nums("kmeans", j + 2, xs));
    const n = cols[0].length;
    for (const [j, x] of cols.entries()) {
      if (x.length !== n) {
        throw fail(
          "kmeans()",
          "every array the same length",
          `${n} values in argument 2 and ${x.length} in argument ${j + 2}`,
          "aggregate every column over the same rows",
        );
      }
    }
    if (n > KMEANS_POINTS) {
      throw fail(
        "kmeans()",
        `at most ${KMEANS_POINTS} points`,
        `${n}`,
        "cluster a sample, or aggregate the rows to one point per customer first",
      );
    }
    const points = Array.from({ length: n }, (_, i) => cols.map((x) => x[i]));
    const distinct = new Set(points.map((p) => p.join(","))).size;
    if (distinct < k) {
      throw fail(
        "kmeans()",
        `at least ${k} distinct points for ${k} clusters`,
        `${distinct} distinct points`,
        "ask for fewer clusters, or widen the query so more rows match",
      );
    }
    // Every centroid is a mean of points, so it stays inside their box, and no
    // distance inside the box is past its diagonal. A finite diagonal is every
    // distance finite, and a mean summed as offsets from the low corner cannot
    // overflow either.
    const lo = cols.map((x) => x.reduce((a, b) => (b < a ? b : a))),
      hi = cols.map((x) => x.reduce((a, b) => (b > a ? b : a)));
    let diagonal = 0;
    for (let c = 0; c < cols.length; c++) diagonal += (hi[c] - lo[c]) ** 2;
    if (!Number.isFinite(diagonal)) {
      throw fail(
        "kmeans()",
        "points whose squared distances fit in a finite number",
        `a squared spread of ${show(diagonal)} across the columns`,
        "scale the inputs down first, e.g. divide by the column's largest value",
      );
    }
    const draw = mulberry32(seedOf("kmeans", [k, ...cols.flat()]));
    // k-means++: each start point is drawn in proportion to its squared distance
    // from the nearest one already drawn. A drawn point has weight zero, so no
    // start point repeats while the weights sum to a finite number above zero.
    // A sum that overflows or underflows can draw a repeat, and the empty
    // cluster it leaves takes a new point below.
    const centroids = [[...points[Math.floor(draw() * n)]]];
    const weight = points.map((p) => closest(p, centroids)[1]);
    while (centroids.length < k) {
      let total = 0;
      for (const w of weight) total += w;
      const target = draw() * total;
      let at = 0;
      for (let sum = weight[0]; sum <= target && at < n - 1;) sum += weight[++at];
      centroids.push([...points[at]]);
      for (let i = 0; i < n; i++) {
        const d = closest(points[i], [centroids[centroids.length - 1]])[1];
        if (d < weight[i]) weight[i] = d;
      }
    }
    const assigned = new Array(n).fill(-1), own = new Array(n);
    for (let steps = 1;; steps++) {
      let moved = 0;
      const counts = new Array(k).fill(0);
      for (let i = 0; i < n; i++) {
        const [j, d] = closest(points[i], centroids);
        if (assigned[i] !== j) moved++;
        assigned[i] = j;
        own[i] = d;
        counts[j]++;
      }
      const empty = counts.filter((c) => !c).length;
      if (!moved && !empty) break;
      if (steps >= KMEANS_STEPS) {
        throw new Error(explain(`kmeans() did not settle on ${k} clusters.`, {
          Limit: `${KMEANS_STEPS} assignment passes`,
          Received: `${steps} passes, the last moving ${moved} points and leaving ${empty} clusters empty`,
          Source: "the points handed to kmeans()",
          Fix: "ask for fewer clusters, or scale the columns so no one of them decides every distance",
        }));
      }
      const sums = Array.from({ length: k }, () => new Array(cols.length).fill(0));
      for (let i = 0; i < n; i++) for (let c = 0; c < cols.length; c++) sums[assigned[i]][c] += points[i][c] - lo[c];
      for (let j = 0; j < k; j++) if (counts[j]) centroids[j] = sums[j].map((s, c) => lo[c] + s / counts[j]);
      // An empty cluster takes the point farthest from its own centroid, lowest
      // index on a tie. With k distinct points some point sits off every
      // centroid, and a point taken is at distance zero, so the next empty
      // cluster takes another.
      for (let j = 0; j < k; j++) {
        if (counts[j]) continue;
        let at = 0;
        for (let i = 1; i < n; i++) if (own[i] > own[at]) at = i;
        centroids[j] = [...points[at]];
        own[at] = 0;
      }
    }
    // Sorted by their coordinates, so a cluster's number does not hang on which
    // start point happened to be drawn first.
    return centroids.sort((a, b) => {
      for (let c = 0; c < a.length; c++) if (a[c] !== b[c]) return a[c] - b[c];
      return 0;
    });
  };
  fn.kmeans_assign = (centroids, ...xs) => {
    if (!xs.length) {
      throw fail(
        "kmeans_assign()",
        "one point value per clustered column after the centroids",
        "only the centroids",
        "add them, e.g. kmeans_assign(m.centroids, x, y)",
      );
    }
    const shape = `a non-empty array of at most ${KMEANS_K_MAX} centroids, each an array of ${xs.length} numbers`;
    // kmeans() never answers more than KMEANS_K_MAX centroids, so a longer array
    // is a column passed by mistake, and every row would walk all of it.
    if (
      !Array.isArray(centroids) || !centroids.length || centroids.length > KMEANS_K_MAX ||
      !centroids.every(Array.isArray)
    ) {
      throw fail(
        "kmeans_assign() argument 1",
        shape,
        show(centroids),
        "cluster in a subquery and read its column here, e.g. kmeans_assign(m.centroids, x, y)",
      );
    }
    const at = centroids.map((c) => nums("kmeans_assign", 1, c));
    for (const [j, c] of at.entries()) {
      if (c.length !== xs.length) {
        throw fail(
          "kmeans_assign()",
          shape,
          `centroid ${j + 1} of ${c.length} numbers beside ${xs.length} point values`,
          "hand it one value per column, in the order kmeans() clustered them",
        );
      }
    }
    const point = xs.map((x, c) => num("kmeans_assign", c + 2, x));
    const [j, d] = closest(point, at);
    // kmeans() checks its own box, but a row read back can sit far outside it.
    // Two squared distances that both overflow tie at Infinity, and the tie
    // would name centroid 1 whichever is nearer.
    if (!Number.isFinite(d)) {
      throw fail(
        "kmeans_assign()",
        "a point whose squared distance to some centroid fits in a finite number",
        `${show(point)}, whose squared distance to every centroid overflows`,
        "scale the point the way the clustered columns were scaled",
      );
    }
    return j + 1;
  };

  // Median absolute deviation, and the outlier score built on it. 1.4826 scales
  // a MAD to the standard deviation of a normal sample, so robust_z reads on the
  // same scale as a z-score — except that the outlier being measured cannot move
  // the ruler, which is exactly what a z-score gets wrong on the day that matters.
  const middle = (xs) => {
    const m = quantile(xs, 0.5);
    return [m, quantile(xs.map((v) => Math.abs(v - m)), 0.5)];
  };
  fn.mad = (xs) => {
    const v = nums("mad", 1, xs);
    return v.length ? middle(v)[1] : null;
  };
  fn.robust_z = (v, xs) => {
    const n = num("robust_z", 1, v), x = nums("robust_z", 2, xs);
    if (!x.length) return null;
    const [m, d] = middle(x);
    // Half the sample identical: there is no spread to score against, and 0/0
    // would read as "perfectly normal" for a value that is nothing of the kind.
    return d === 0 ? null : (n - m) / (1.4826 * d);
  };

  // Welch's two-sample t-test, which assumes neither equal sizes nor a shared
  // variance, because the version that does is the one that quietly reports a
  // difference that is not there. Returns the two-sided p-value.
  fn.t_test = (as, bs) => {
    const a = nums("t_test", 1, as), b = nums("t_test", 2, bs);
    for (const [i, s] of [[1, a], [2, b]]) {
      if (s.length < 2)
        throw fail(`t_test() argument ${i}`, "at least 2 values", `${s.length}`, "widen the query so more rows match");
    }
    const [va, vb] = [variance(a) / a.length, variance(b) / b.length];
    if (va + vb === 0) return null;
    const t = (mean(a) - mean(b)) / Math.sqrt(va + vb);
    const df = (va + vb) ** 2 / (va ** 2 / (a.length - 1) + vb ** 2 / (b.length - 1));
    return tTail(Math.abs(t), df);
  };

  // The mean's confidence interval, t-based, so eight rows widen it the way they
  // should instead of reporting the precision of eight hundred.
  const interval = (name, xs, level, sign) => {
    const v = nums(name, 1, xs);
    if (typeof level !== "number" || level <= 0 || level >= 1) {
      throw fail(
        `${name}() argument 2`,
        "a confidence level between 0 and 1",
        show(level),
        "use 0.95 for a 95% interval",
      );
    }
    if (v.length < 2)
      throw fail(`${name}()`, "at least 2 values", `${v.length}`, "widen the query so more rows match");
    return mean(v) + sign * tCrit(level, v.length - 1) * Math.sqrt(variance(v) / v.length);
  };
  fn.ci_low = (xs, level) => interval("ci_low", xs, level, -1);
  fn.ci_high = (xs, level) => interval("ci_high", xs, level, 1);

  // The standard-SQL histogram bin. Below the range is bucket 0 and above it is
  // n+1, so the tails stay visible instead of being folded into the end bars,
  // which is the one thing a histogram must never do.
  fn.width_bucket = (v, lo, hi, count) => {
    const x = num("width_bucket", 1, v), a = num("width_bucket", 2, lo), b = num("width_bucket", 3, hi);
    const n = num("width_bucket", 4, count);
    if (!Number.isInteger(n) || n < 1) {
      throw fail(
        "width_bucket() argument 4",
        "a whole bucket count of 1 or more",
        show(count),
        "e.g. width_bucket(lead_days, 0, 30, 6)",
      );
    }
    if (a >= b) {
      throw fail(
        "width_bucket()",
        "a low end below the high end",
        `${a} and ${b}`,
        "swap arguments 2 and 3, or widen the range",
      );
    }
    if (x < a) return 0;
    if (x >= b) return n + 1;
    return Math.floor(((x - a) / (b - a)) * n) + 1;
  };

  // Regex. regexp_like already works in AlaSQL, so it is left alone.
  const re = (name, pattern, flags) => {
    try {
      return new RegExp(str(name, "pattern", pattern), flags);
    } catch (e) {
      throw fail(
        `${name}() pattern`,
        "a valid regular expression",
        `${show(pattern)} (${e.message})`,
        "escape the special characters",
      );
    }
  };
  fn.regexp_replace = (s, pattern, replacement, flags = "g") =>
    str("regexp_replace", 1, s).replace(re("regexp_replace", pattern, flags), str("regexp_replace", 3, replacement));
  fn.regexp_extract = (s, pattern, group = 0) => {
    const m = str("regexp_extract", 1, s).match(re("regexp_extract", pattern));
    return m ? (m[group] ?? null) : null;
  };
  fn.regexp_split = (s, pattern) => str("regexp_split", 1, s).split(re("regexp_split", pattern));

  // Fuzzy matching, for entity dedupe.
  fn.levenshtein = (a, b) => levenshtein(str("levenshtein", 1, a), str("levenshtein", 2, b));
  fn.similarity = (a, b) => jaccard(trigrams(str("similarity", 1, a)), trigrams(str("similarity", 2, b)));
  fn.token_set_ratio = (a, b) => jaccard(tokens(str("token_set_ratio", 1, a)), tokens(str("token_set_ratio", 2, b)));
  fn.soundex = (s) => soundex(str("soundex", 1, s));

  // Dates, all UTC.
  fn.date_trunc = (u, ts) => truncate(unit("date_trunc", u), date("date_trunc", ts)).toISOString();
  fn.date_add = (u, n, ts) => {
    if (typeof n !== "number")
      throw fail("date_add() argument 2", "a number of units", show(n), "e.g. date_add('day', 7, created_at)");
    return shift(unit("date_add", u), n, date("date_add", ts)).toISOString();
  };
  fn.date_diff = (u, a, b) => diff(unit("date_diff", u), date("date_diff", a), date("date_diff", b));
  fn.iso_week = (ts) => {
    const d = truncate("day", date("iso_week", ts));
    // Thursday of this ISO week decides which year the week belongs to.
    d.setUTCDate(d.getUTCDate() + 3 - ((d.getUTCDay() + 6) % 7));
    const jan4 = new Date(Date.UTC(d.getUTCFullYear(), 0, 4));
    return 1 + Math.round((d.getTime() - jan4.getTime()) / (7 * DAY) + ((jan4.getUTCDay() + 6) % 7) / 7);
  };
  fn.fiscal_year = (ts, start) => {
    const { year, month, start: s } = fiscal("fiscal_year", ts, start);
    return s === 1 || month < s - 1 ? year : year + 1;
  };
  fn.fiscal_quarter = (ts, start) => Math.floor(fiscal("fiscal_quarter", ts, start).into / 3) + 1;
  fn.fiscal_period = (ts, start) => fiscal("fiscal_period", ts, start).into + 1;

  // Great-circle distance in kilometres. AlaSQL ships no trigonometry at all, and
  // "how far apart are these two rows" is the only thing anyone wants it for, so
  // this is one function rather than six primitives to compose wrongly.
  fn.haversine_km = (lat1, lon1, lat2, lon2) => {
    const rad = [lat1, lon1, lat2, lon2].map((v, i) => {
      const n = absent(v) ? null : typeof v === "string" ? Number(v) : v;
      if (typeof n !== "number" || !Number.isFinite(n)) {
        throw fail(
          `haversine_km() argument ${i + 1}`,
          "a finite latitude or longitude in degrees",
          show(v),
          "drop the rows with no coordinates in a where clause",
        );
      }
      const limit = i % 2 === 0 ? 90 : 180;
      if (Math.abs(n) > limit) {
        throw fail(
          `haversine_km() argument ${i + 1}`,
          `${i % 2 === 0 ? "a latitude" : "a longitude"} between -${limit} and ${limit}`,
          show(v),
          i % 2 === 0 ? "the arguments are (lat, lon, lat, lon); check the order" : "check the column's units",
        );
      }
      return (n * Math.PI) / 180;
    });
    const [a1, o1, a2, o2] = rad;
    const h = Math.sin((a2 - a1) / 2) ** 2 + Math.cos(a1) * Math.cos(a2) * Math.sin((o2 - o1) / 2) ** 2;
    // 6371.0088 km is the IUGG mean Earth radius.
    return 2 * 6371.0088 * Math.asin(Math.min(1, Math.sqrt(h)));
  };

  const degrees = (name, arg, v, kind) => {
    const n = num(name, arg, v), limit = kind === "latitude" ? 90 : 180;
    if (Math.abs(n) > limit) {
      throw fail(
        `${name}() argument ${arg}`,
        `a ${kind} in degrees, between -${limit} and ${limit}`,
        show(v),
        kind === "latitude" ? "latitude comes first; check the order" : "check the column's units",
      );
    }
    return n;
  };

  // A polygon is a JSON array of [lat, lon] pairs — the shape a cell can hold and
  // a query can build. The ring closes itself, so the last point need not repeat
  // the first.
  const ring = (name, arg, value) => {
    let raw = value;
    if (typeof raw === "string") {
      try {
        raw = JSON.parse(raw);
      } catch {
        throw fail(
          `${name}() argument ${arg}`,
          "a JSON array of [lat, lon] pairs",
          show(value),
          `e.g. '[[40.7,-74.0],[40.8,-74.0],[40.8,-73.9]]'`,
        );
      }
    }
    if (!Array.isArray(raw) || raw.length < 3) {
      throw fail(
        `${name}() argument ${arg}`,
        "at least 3 [lat, lon] pairs",
        show(value),
        "a polygon needs three corners to enclose anything",
      );
    }
    // Math.max(...lons) below throws a RangeError past about a hundred thousand
    // arguments, which says nothing about the polygon. Refuse it by size first.
    if (raw.length > 10000) {
      throw fail(
        `${name}() argument ${arg}`,
        "at most 10000 points in a polygon",
        `${raw.length} points`,
        "simplify the ring before storing it, or split it into parts",
      );
    }
    const pts = raw.map((p, i) => {
      if (!Array.isArray(p) || p.length < 2) {
        throw fail(
          `${name}() argument ${arg}, point ${i + 1}`,
          "a [lat, lon] pair",
          show(p),
          "each point is a two-element array, latitude first",
        );
      }
      return [
        degrees(name, `${arg}, point ${i + 1}`, p[0], "latitude"),
        degrees(name, `${arg}, point ${i + 1}`, p[1], "longitude"),
      ];
    });
    const lons = pts.map(([, lon]) => lon);
    // A ring wider than half the world is one that wraps the antimeridian, and
    // every formula below would read it inside out. Refuse it rather than answer.
    if (Math.max(...lons) - Math.min(...lons) > 180) {
      throw fail(
        `${name}() argument ${arg}`,
        "a polygon that does not cross the antimeridian",
        `longitudes from ${Math.min(...lons)} to ${Math.max(...lons)}`,
        "split the polygon at 180 degrees and add the two halves",
      );
    }
    return pts;
  };

  // Ray casting. Exact for the polygons a sheet holds — a parcel, a zoning
  // district, a delivery zone — and a point exactly on the edge falls on one
  // side of it, consistently, rather than on both.
  fn.point_in_polygon = (lat, lon, polygon) => {
    const y = degrees("point_in_polygon", 1, lat, "latitude"), x = degrees("point_in_polygon", 2, lon, "longitude");
    const pts = ring("point_in_polygon", 3, polygon);
    let inside = false;
    for (let i = 0, j = pts.length - 1; i < pts.length; j = i++) {
      const [yi, xi] = pts[i], [yj, xj] = pts[j];
      if ((yi > y) !== (yj > y) && x < ((xj - xi) * (y - yi)) / (yj - yi) + xi) inside = !inside;
    }
    return inside;
  };

  // Spherical, not planar: a county-sized polygon read as flat is wrong by more
  // than the decision it is feeding.
  fn.polygon_area_km2 = (polygon) => {
    const pts = ring("polygon_area_km2", 1, polygon), rad = Math.PI / 180;
    let total = 0;
    for (let i = 0, j = pts.length - 1; i < pts.length; j = i++) {
      const [lat1, lon1] = pts[j], [lat2, lon2] = pts[i];
      total += (lon2 - lon1) * rad * (2 + Math.sin(lat1 * rad) + Math.sin(lat2 * rad));
    }
    return Math.abs((total * 6371.0088 * 6371.0088) / 2);
  };

  // Initial bearing, degrees clockwise from north. It is the direction you leave
  // in, not the one you arrive on: a great circle turns as it goes.
  fn.bearing_deg = (lat1, lon1, lat2, lon2) => {
    const rad = Math.PI / 180;
    const a1 = degrees("bearing_deg", 1, lat1, "latitude") * rad;
    const o1 = degrees("bearing_deg", 2, lon1, "longitude") * rad;
    const a2 = degrees("bearing_deg", 3, lat2, "latitude") * rad;
    const o2 = degrees("bearing_deg", 4, lon2, "longitude") * rad;
    const y = Math.sin(o2 - o1) * Math.cos(a2);
    const x = Math.cos(a1) * Math.sin(a2) - Math.sin(a1) * Math.cos(a2) * Math.cos(o2 - o1);
    return (((Math.atan2(y, x) / rad) % 360) + 360) % 360;
  };

  // Geohash: the cheap spatial bucket. Two points sharing a prefix are near each
  // other, so `group by geohash(lat, lon, 5)` is a hotspot map without a spatial
  // index. The cells are fixed, which is what makes two runs agree.
  const GEOHASH32 = "0123456789bcdefghjkmnpqrstuvwxyz";
  fn.geohash = (lat, lon, precision) => {
    const y = degrees("geohash", 1, lat, "latitude"), x = degrees("geohash", 2, lon, "longitude");
    const p = num("geohash", 3, precision);
    if (!Number.isInteger(p) || p < 1 || p > 12) {
      throw fail(
        "geohash() argument 3",
        "a whole precision between 1 and 12",
        show(precision),
        "5 is about 5km across, 7 about 150m",
      );
    }
    const lats = [-90, 90], lons = [-180, 180];
    let hash = "", bits = 0, ch = 0, even = true;
    while (hash.length < p) {
      const [range, v] = even ? [lons, x] : [lats, y];
      const mid = (range[0] + range[1]) / 2;
      if (v >= mid) ch = ch * 2 + 1, range[0] = mid;
      else ch = ch * 2, range[1] = mid;
      even = !even;
      if (++bits === 5) hash += GEOHASH32[ch], bits = 0, ch = 0;
    }
    return hash;
  };

  fn.business_days = (a, b) => {
    let [from, to] = [truncate("day", date("business_days", a)), truncate("day", date("business_days", b))];
    const sign = from <= to ? 1 : -1;
    if (sign < 0) [from, to] = [to, from];
    let n = 0;
    for (let d = from; d < to; d = new Date(d.getTime() + DAY)) {
      const w = d.getUTCDay();
      if (w !== 0 && w !== 6) n++;
    }
    return n * sign;
  };

  // JSON.
  fn.json_extract = (value, path) => {
    const p = str("json_extract", 2, path);
    let cur = typeof value === "string" ? JSON.parse(value) : value;
    for (const key of p.replace(/^\$\.?/, "").split(".").filter(Boolean)) {
      if (cur === null || cur === undefined) return null;
      const idx = key.match(/^(.*)\[(\d+)\]$/);
      cur = idx ? cur[idx[1] || 0]?.[Number(idx[2])] : cur[key];
    }
    return cur ?? null;
  };
  fn.to_json = (v) => JSON.stringify(v ?? null);

  // A sheet reference in an expression position: `where x = @table:cfg` reaches
  // the engine as a call to a function that does not exist, and "alasql.fn.SHEET
  // is not a function" says nothing about the query. A sheet is not a value.
  fn.SHEET = (id) => {
    throw new Error(explain(`A sheet reference cannot be used as a single value.`, {
      Received: `@${id}`,
      Expected: `@${id}.<column>, which reads one value out of a one-row sheet`,
      Source: "an expression in this query",
      Fix: `name the column, e.g. @${id}.amount, or read the sheet in the from clause instead`,
    }));
  };

  // From-functions. AlaSQL's own range() yields empty objects and unnest() throws.
  alasql.from.UNNEST = (arr, _opts, cb, idx, query) => {
    if (!Array.isArray(arr)) throw fail("unnest()", "an array", show(arr), "pass a json array column");
    let res = arr.map((
      value,
    ) => (value !== null && typeof value === "object" && !Array.isArray(value) ? value : { value }));
    if (cb) res = cb(res, idx, query);
    return res;
  };
  // Date spine, so gaps become zero rows instead of missing rows.
  alasql.from.SERIES = (start, stop, cb, idx, query) => {
    const [from, to] = [date("series", start), date("series", stop)];
    const res = [];
    for (let d = from; d <= to; d = shift("day", 1, d)) {
      if (res.length > 100000)
        throw fail("series()", "at most 100000 days", `${from.toISOString()}..${to.toISOString()}`, "narrow the range");
      res.push({ date: d.toISOString() });
    }
    return cb ? cb(res, idx, query) : res;
  };

  return alasql;
};
