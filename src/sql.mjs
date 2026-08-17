// Shared by both AlaSQL engines: the server imports npm:alasql (main.ts), the
// page loads it from a CDN <script> (index.html). Both call register() with
// their own instance, so a query behaves the same wherever it runs.

// --- errors

const fail = (what, expected, received, fix) =>
  new Error([
    `${what} received ${received}.`,
    ``,
    `  Expected: ${expected}`,
    `  Fix:      ${fix}`,
  ].join("\n"));

const show = (v) => v === null ? "null" : v === undefined ? "nothing" : `${typeof v} ${JSON.stringify(v)}`;

const str = (fn, arg, v) => {
  if (typeof v !== "string")
    throw fail(`${fn}() argument ${arg}`, "a text value", show(v), `cast it with cast(x as string)`);
  return v;
};

const nums = (fn, arg, v) => {
  if (!Array.isArray(v)) {
    throw fail(
      `${fn}() argument ${arg}`,
      "an array of numbers",
      show(v),
      `build one with array(x), e.g. ${fn}(array(x), array(y))`,
    );
  }
  return v.map((n) => {
    const f = typeof n === "string" ? Number(n) : n;
    if (typeof f !== "number" || !Number.isFinite(f))
      throw fail(`${fn}() argument ${arg}`, "only finite numbers", show(n), "filter the nulls out with a where clause");
    return f;
  });
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
      "check the column's type row",
    );
  }
  return d;
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

// --- @sheet references
//
// One scanner for both engines, so `@type:doc_id` means the same thing on the
// server and in the page. The colon is required, which is what keeps AlaSQL's
// own `@params` variable from being mistaken for a sheet reference.
export const scanRefs = (code) => {
  let out = "", inStr = false;
  const ids = [];
  for (let i = 0; i < code.length; i++) {
    const ch = code[i];
    if (ch === "'") inStr = !inStr;
    const ref = !inStr && ch === "@" ? code.slice(i).match(/^@[a-z-]+:[A-Za-z0-9._-]+/)?.[0] : undefined;
    if (ref) {
      ids.push(ref.slice(1));
      out += `SHEET('${ref.slice(1)}')`;
      i += ref.length - 1;
    } else { out += ch; }
  }
  return { sql: out, ids };
};

export const MAX_REF_DEPTH = 8;

// Bounds the @query -> @query chain. A cycle is reported as the path that closes
// it, because "nested too deeply" sends you looking for the wrong problem.
export const checkRefPath = (path, id) => {
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

// --- error formatting

const nearest = (name, known) => {
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
export const checkResultColumns = (cols, rows, known = [], code = "") => {
  if (!rows.length) return;
  const aliased = new Set(
    [...code.matchAll(/\bas\s+["'`[]?([A-Za-z_][A-Za-z0-9_]*)/gi)].map((m) => m[1]),
  );
  for (const { columnid } of cols) {
    if (!/^[A-Za-z_][A-Za-z0-9_]*$/.test(columnid) || known.includes(columnid)) continue;
    if (aliased.has(columnid)) continue;
    // Strictly undefined, never null: `select null as x` is a real answer.
    if (!rows.every((row) => row[columnid] === undefined)) continue;
    const hit = nearest(columnid, known);
    throw new Error(
      [
        `No column named "${columnid}".`,
        ``,
        hit
          ? `  Did you mean: ${hit}`
          : `  Available:    ${known.join(", ") || "(the referenced sheets have no columns)"}`,
        `  Source:       ${known.length ? "the sheets this query references" : "no @sheet is referenced"}`,
        `  Fix:          ${
          hit ? `rename "${columnid}" to "${hit}"` : "check the column names in the sheet's type row"
        }`,
      ].join("\n"),
    );
  }
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
