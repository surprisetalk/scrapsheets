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
export const show = (v) =>
  v === null
    ? "null"
    : v === undefined
    ? "nothing"
    : typeof v === "number" && !Number.isFinite(v)
    ? `number ${v}`
    : `${typeof v} ${JSON.stringify(v)}`;

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
// that guesses.

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

// The column types a cell has to be a number to hold. Exported because the
// tests assert what this pass promises, and a second copy of the list would
// drift from the one the check reads.
export const NUMERIC_TYPES = ["num", "int", "float", "usd", "percentage"];

export const checkColumnTypes = (id, cols, rows) => {
  for (const col of cols) {
    if (!NUMERIC_TYPES.includes(col.type)) continue;
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
      throw new Error(explain(`Column "${col.name}" of @${id} holds a value its type does not allow.`, {
        Expected: `${col.type}, so a number or a blank`,
        Received: `${typeof v} ${JSON.stringify(v)}`,
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

const MAX_QUERY_ROWS = 200_000;
export const MAX_QUERY_MS = 15_000;

export const checkQueryRows = (total, id) => {
  if (total <= MAX_QUERY_ROWS) return total;
  throw new Error(explain(`This query loads more rows than one run is allowed.`, {
    Received: `${total} rows, the last of them from @${id}`,
    Limit: `${MAX_QUERY_ROWS} rows across every @sheet in one query`,
    Source: "the @sheet refs in this query",
    Fix: "filter the large sheet in its own query sheet, then reference that instead",
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
const KEYWORD = /^(from|where|group|order|having|limit|offset|join|on|and|or|union|into)$/i;

export const checkResultColumns = (cols, rows, known = [], code = "") => {
  if (!rows.length) return;
  const aliased = new Set(
    [...code.matchAll(/\bas\s+["'`[]?([A-Za-z_][A-Za-z0-9_]*)/gi)].map((m) => m[1]),
  );
  // Every min()/max() in the query, by the name its column will carry: the alias
  // if it has one, otherwise AlaSQL's own MIN(expr) spelling.
  const extremes = new Set(
    [...code.matchAll(/\b(min|max)\s*\(\s*([^()]*?)\s*\)(?:\s+(?:as\s+)?["'`[]?([A-Za-z_][A-Za-z0-9_]*))?/gi)]
      .flatMap(([, fn, arg, as]) => [
        `${fn.toUpperCase()}(${arg})`,
        ...(as && !KEYWORD.test(as) ? [as] : []),
      ]),
  );
  for (const { columnid } of cols) {
    if (known.includes(columnid)) continue;
    // Strictly undefined, never null: `select null as x` is a real answer.
    if (!rows.every((row) => row[columnid] === undefined)) continue;
    if (extremes.has(columnid)) {
      throw new Error(explain(`min() and max() cannot compare the text in "${columnid}".`, {
        Received: `"${columnid}" is empty in all ${rows.length} rows`,
        Cause: "AlaSQL computes min() and max() over numbers and dates only, and drops a text value",
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

// --- window functions
//
// AlaSQL parses `over (partition by ...)` and then computes it wrong: `sum(x)
// over (partition by k)` comes back 0, and only row_number() is right. So a
// window never reaches the engine. rewriteWindows() lifts each one out of the
// top-level select list, leaves `null as <alias>` where it stood, and appends
// the plain columns the window reads; applyWindows() computes it over the rows
// the engine returns and drops those columns again.

// The type each window produces. null means "whatever its argument already was".
export const WINDOW_TYPES = {
  row_number: "int",
  rank: "int",
  dense_rank: "int",
  ntile: "int",
  count: "int",
  percent_rank: "num",
  cume_dist: "num",
  sum: "num",
  avg: "num",
  stddev: "num",
  lag: null,
  lead: null,
  first_value: null,
  last_value: null,
  nth_value: null,
  min: null,
  max: null,
};

// Ranking and offset functions read the whole partition; a frame never applies.
const OFFSET = ["lag", "lead"];
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

const itemType = (expr, nameToType) => {
  let text = expr.trim();
  let averaged = false;
  for (let i = 0; i < SELECT_DEPTH; i++) {
    if (/^'(?:[^']|'')*'$/.test(text)) return "text";
    if (/^-?\d+(?:\.\d+)?$/.test(text)) return "num";
    const plain = text.match(/^(?:[A-Za-z_][A-Za-z0-9_]*\s*\.\s*)?([A-Za-z_][A-Za-z0-9_]*)$/);
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
    const name = as ? as[1] : item.match(/^(?:[A-Za-z_][A-Za-z0-9_]*\s*\.\s*)?([A-Za-z_][A-Za-z0-9_]*)$/)?.[1];
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

const bare = (text) => text.trim().replace(/^\[(.*)\]$/, "$1").trim();

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

// --- charts
//
// A chart is a sheet: a source ref, a kind, and the two columns to plot. Both
// engines build the same query out of that, so the picture the page draws and
// the rows the server exports are the same answer.

const chartIdent = (what, value) => {
  if (/^[A-Za-z_][A-Za-z0-9_]*$/.test(value ?? "")) return value;
  throw new Error(explain(`A chart's ${what} has to be a column name.`, {
    Expected: "a plain column name, e.g. month",
    Received: JSON.stringify(value ?? null),
    Source: "this chart sheet's settings",
    Fix: "pick a column from the sheet the chart reads",
  }));
};

export const chartSql = ({ source, x, y }) => {
  // Only what a query can reference: the page refuses any other prefix while
  // loading, and a chart that runs on the server but not in the page is worse
  // than one that is refused in both.
  if (!/^@(?:table|query):[A-Za-z0-9_-]+$/.test(source ?? "")) {
    throw new Error(explain(`A chart reads one table or query sheet.`, {
      Expected: "@table:doc_id or @query:doc_id",
      Received: JSON.stringify(source ?? null),
      Source: "this chart sheet's settings",
      Fix: "set the source to a table or query sheet, e.g. @query:budget-burn",
    }));
  }
  // Ordered by the x column, so the line is drawn in the order it is read and
  // two runs of the same chart agree.
  return `select ${chartIdent("x column", x)} as x, ${chartIdent("y column", y)} as y from ${source} order by 1`;
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
 * is exactly the sheet `describe` exists to inspect.
 */
export const loadRefs = async (ids, { path, describing, fetch, onLoad }) => {
  const docs = {}, colsOf = {};
  for (const id of ids) {
    if (docs[id]) continue;
    checkRefPath(path, id);
    const sheet = await fetch(id);
    colsOf[id] = Object.values(sheet[0]);
    docs[id] = toRecords(sheet);
    await onLoad(id, docs[id]);
    // Only table sheets: a query column keeps its source column's declared type,
    // so `cast(price as string) as price` would trip a check meant for a bad cell.
    if (!describing && id.startsWith("table:")) checkColumnTypes(id, colsOf[id], docs[id]);
  }
  return { docs, colsOf };
};

/** Everything the engine cannot be trusted with, in the order it has to happen:
 * a cell reference needs its sheet loaded to be checked, unpivot needs its column
 * names, and a window has to be lifted out of whatever those two produce.
 */
export const planQuery = (sql, cells, docs, colsOf) => {
  checkCells(cells, docs, colsOf);
  checkPivot(sql);
  return rewriteWindows(rewriteUnpivot(sql, colsOf));
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
