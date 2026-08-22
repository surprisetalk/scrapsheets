# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Scrapsheets is a web-based spreadsheet application that combines traditional spreadsheet functionality with modern web
technologies. It uses a hybrid architecture with:

- **Backend**: Deno-based server using Hono framework (main.ts)
- **Frontend**: Elm single-page application (src/Main.elm)
- **Database**: PostgreSQL with schema defined in schema/db.sql
- **Real-time collaboration**: Automerge CRDT for document synchronization (see https://automerge.org/llms-full.txt)
- **Data storage**: File-based automerge documents in data/automerge/

## Important Files

- `main.ts` - Main server application and API routes
- `src/Main.elm` - Frontend application
- `src/examples.mjs` - Bundled example datasets and queries (imported by both the page and the server seeder)
- `src/page.mjs` - The parts of `src/index.html` that are functions of their input rather than of the browser: the
  library merge, the sheet thumbnail, the four things the page does with an HTTP response, and `sheets()` — the page's
  half of the query engine, which resolves `@type:doc_id` refs and recurses through referenced queries. Extracted so
  they can be tested, since nothing boots `index.html` itself
- `schema/db.sql` - Declarative database schema (schema only, no data: `pg-schema-diff` diffs a live DB against it)
- `examples.sql` - Shop catalogue of query templates (applied by `seed()` on first request)
- `deno.json` - Dependencies and import map
- `src/index.html` - Frontend HTML entry point
- `vendor.ts` - Rebuilds the vendored browser bundles in `src/` (automerge, automerge-repo, alasql)
- `data/automerge/` - Document storage directory

## Development Commands

### Building and Running

- **Build frontend**: `deno task build` (copies src/* to dist and runs elm make)
- **Development server**: `deno task dev`
- **Run all tests**: `deno task test` (or `deno test --allow-all`). No test drives a browser. The suite is five files,
  and which one a failure belongs in is usually obvious:
  - `main_test.ts` — the server: auth, sync and roles, shop and Stripe, `POST /query`, the `src/sql.mjs` UDFs, net-http
    polling, alerts and digests, MCP, export. One `Deno.test` full of prose-headed blocks against in-process PGlite.
  - `examples_test.ts` — every bundled sheet, run through **both** engines (`npm:alasql` and the vendored
    `src/alasql.mjs` the page loads) and compared row for row. A UDF registered in one and not the other, or a vendored
    bundle built from a different alasql, fails here rather than in somebody's browser.
  - `page_test.ts` — the page itself, booted under jsdom: the compiled Elm in `dist/index.js` initializes, renders, and
    answers clicks, with the library fed in through `library()` from `src/page.mjs` — the same function `index.html`
    calls, not a copy written for the test. Also covers that module on its own. Covers the gallery strip, sorting by
    clicking a header, hiding a column, chart SVG, dashboard tiles, `?embed=1`, the tutorial and the shortcut sheet. It
    always runs `deno task build` first, so it can never pass against a stale bundle. deno-dom is not enough for this —
    it has no `replaceData` on a text node, which is how Elm's virtual-dom patches text in place.
  - `browser_test.ts` — despite the name, no browser: dist builds, index.html wires the WASM and the import map, every
    root-absolute asset is in `_redirects`, every name index.html imports is actually exported, nothing reaches a CDN,
    and the built bundles still export what the page boots from. Nothing boots `index.html`, so its
    `<script type="module">` body is sliced out and piped over stdin to `deno lint` with `no-undef` and
    `no-unused-vars`. That is real scope analysis — module bindings, block scope, hoisting — rather than the regex it
    replaced, which matched call shapes only and so could not see a value use like `PARSERS[ct]`. A free identifier or a
    dead import fails here instead of shipping as a blank page; it is what caught `app` being referenced from the
    fatal-initialization `catch` that its `try` had scoped it out of. `BROWSER_GLOBALS` in that file is the whole
    allowlist — the seven names the page reaches for that Deno's own global scope lacks, `Elm` among them, from the
    classic `<script src="/index.js">`. Everything else it touches, `fetch` and `localStorage` and `WebSocket`, Deno
    already defines, so a new browser API has to be added there on purpose.
  - `tests/MainTest.elm` via `elm_test.ts` — pure Elm: selection and navigation, sort and filter, clipboard parsing,
    column stats, `docDecoder`, `chartPoints`.
- **Re-vendor browser bundles**: `deno task vendor` (after bumping the versions at the top of `vendor.ts`; alasql tracks
  `deno.json`)
- **Elm review**: `deno task review`. Runs clean with zero suppressions; keep it that way.
- **Watch and build**: `watch src { try { cp -vu src/* dist ; elm make src/Main.elm --debug --output=dist/index.js } }`

### Elm Commands (via deno.json imports)

- **Format Elm code**: `deno run -A npm:elm-format`
- **Run Elm tests**: `deno run -A npm:elm-test`
- **Elm review**: `deno task review` (also runs in CI)

### Database Setup

- Initialize database schema: `psql < schema/db.sql`
- Migrate an existing database: edit `schema/db.sql`, then `deno task db:plan` to read the generated migration and
  `deno task db:apply` to run it. Both load `.env` (`DATABASE_URL` required; `TEMP_DATABASE_URL` optional for plan).
  `schema/db.sql` is the desired state; there are no migration files. DML (backfills) is never generated — splice it in
  with `--insert-statement 'index=<n> statement="..."'`. `db:apply` passes `--allow-hazards INDEX_BUILD` (every plan
  containing a `CREATE INDEX CONCURRENTLY` trips the gate, and concurrent builds cost CPU without locking writes) and
  `--skip-confirm-prompt` (the prompt needs a TTY). `db:plan` is therefore the only review step — read it before you
  apply, especially for anything pg-schema-diff does _not_ label a hazard: `add column ... generated always as identity`
  rewrites the whole table under an access exclusive lock and carries no warning.
- Default connection: `postgresql://postgres@127.0.0.1:5434/postgres`, but **`.env` may point `DATABASE_URL` somewhere
  else entirely — check it before running `db:apply`**, which now applies without a confirmation prompt
- Tests run against an in-process PGlite over a local pg-gateway on port 5434 (no real Postgres needed)
- External-DB DSNs are encrypted at the application level (AES-GCM via DSN_ENCRYPTION_KEY) before storage

## Architecture Overview

### Backend Architecture (main.ts)

- **Framework**: Hono web framework with JWT middleware
- **Database**: PostgreSQL via postgresjs (pool capped at one connection: the test gateway is a single PGlite session)
- **Real-time sync**: official automerge NodeWSServerAdapter behind a small ws-shim over Hono's upgradeWebSocket;
  per-document access is enforced in the /library/sync message path (`syncRole`), not just sharePolicy. `syncRole`
  returns owner/editor/viewer or null, covering membership, purchase-derived access, `sheet.public`, and share-link
  JWTs; a viewer's frames are inspected with `Automerge.decodeSyncMessage` and rejected if they carry changes
- **Authentication**: JWT-based with email verification via Resend (plain fetch, RESEND_API_KEY)
- **Document types**: table, query, net-hook, net-http, net-socket, portal, alert, chart, codex-*
- **Seeding**: a lazy-once middleware runs `seed()` (examples.sql + src/examples.mjs datasets) on the first request;
  idempotent via `on conflict (doc_id) do update`
- **net-http polling**: `pollNetOnce` scans net-http sheets every 15s, fetches due URLs through the safeFetch SSRF
  guard, and appends bodies to the `net` table. A failed fetch lands as a row too, shaped by `fetchFailure()`: status,
  the URL actually fetched after redirects, content type, a body snippet, and a `repro` curl line that names the header
  keys the sheet sent but never their values. `/proxy` returns the same shape
- **Webhook ingest**: `POST /net/:id` rejects an unknown sheet (404), a non-net sheet (400) and a body over
  `NET_BODY_CAP` (413), each naming what it received. Signature verification does not exist yet
- **Alerts**: an `alert` sheet is `{ code, to, interval }` and fires when its query returns a row, so the condition is
  the where clause and there is no second expression language. `pollAlertOnce` scans them every 15s and runs each
  through `POST /query` **as its owner** (`createJwt(created_by)`), so an alert can never read a sheet its owner cannot.
  Every run whose answer differs from the last one lands in `net` with `method = 'ALERT'` — status, row count,
  `added`/`removed` against the run before, the rows it matched up to `ALERT_ROWS`, and what the delivery did — which
  makes the alert's history a sheet you can query, and makes both the de-dupe and the diff survive a restart, because
  the previous run is read back out of that log. Past `ALERT_ROWS` the run records why it could not diff rather than a
  number it cannot stand behind. `POST /sell` refuses an alert: a copy would mail the seller's address on the buyer's
  timer
- **Charts**: a `chart` sheet is `{ source, kind, x, y }`. `chartSql()` in `src/sql.mjs` turns that into one query both
  engines build identically, so the SVG the page draws and the rows `GET /sheet/chart:abc` and `/export/chart:abc.csv`
  return are the same answer. Only a column name reaches the SQL — anything else is refused by name rather than
  concatenated in — and the page draws it with `elm/svg`, baseline pinned to zero unless the data goes below it,
  dropping a row whose y is not a number rather than reading it as zero. The `svg`/`png` buttons serialize the SVG
  already on screen instead of drawing it again, so a saved chart cannot drift from the shown one
- **Embeds and dashboards**: `?embed=1` on any sheet's own URL renders that sheet with no chrome — same page, same
  access rules, so an embed grants nothing a link would not. A `dashboard` sheet is `{ tiles: ["@chart:a", ...] }` and
  lays those out as a grid of embeds, so nothing in it knows how to draw a chart or a table: the sheet it names already
  does. One page per tile is the cost, and a dashboard cannot be a tile on a dashboard because that nests forever
- **Fork**: the page's `forkDoc` port copies any sheet you can open — a bundled example included — into a new automerge
  document carrying `forked_from`, registers it, and navigates to it. The toolbar reads `forked_from` back out of the
  document, so lineage travels with the sheet rather than living in one browser's localStorage
- **MCP server**: POST /mcp/:id is a hand-rolled JSON-RPC 2.0 endpoint (initialize, tools/list, tools/call) with tools
  read_sheet, write_cells, query_sheet, list_sheets; :id is the default sheet scope

### Key Backend Features

- **Sheet system**: Polymorphic documents identified by `type:doc_id` format
- **Query engine**: SQL execution via AlaSQL for cross-sheet queries using `@sheet_id` syntax. `src/sql.mjs` is shared
  by both engines (server `npm:alasql`, page `/alasql.mjs`): `register()` adds the UDFs AlaSQL lacks — aggregates,
  regression, fuzzy matching, UTC date arithmetic including `fiscal_year`/`fiscal_quarter`/`fiscal_period` — inference
  (`t_test()` is Welch's, `ci_low()`/`ci_high()` the mean's t-based interval, both over one Student-t CDF), robust
  statistics (`mad()`, `robust_z()`, so the outlier cannot widen the ruler it is measured against), curve fitting by log
  transform (`fit_exponential()`, `fit_power()`), `width_bucket()` (which is why a histogram needs no chart kind),
  geometry (`point_in_polygon()` — usable as a join predicate — `polygon_area_km2()` spherical, `bearing_deg()`,
  `geohash()`), `haversine_km()` for great-circle distance (AlaSQL ships no trigonometry at all),
  `min_text()`/`max_text()` because AlaSQL's own `min()`/`max()` are compiled inline over numbers and dates and drop a
  text value, so `min(code)` returns nothing rather than the first code — `checkResultColumns()` names that case and
  points at the replacements. `scanRefs()` is the one `@type:doc_id` scanner — `@type:doc_id.column` is a **cell**, one
  value out of a one-row sheet, rewritten to a scalar subquery so nothing is spliced in as a literal, and `checkCells()`
  refuses a sheet that does not hold exactly one row. `checkResultColumns()` turns AlaSQL's silent undefined column into
  an error. `nearest()` backs every "did you mean": unknown columns and unresolved `@sheet` refs in both engines. The
  server passes sheet rows through `params[0]` so `alasql.from.SHEET` stays request-scoped. **Both engines resolve a
  query through the same three functions in `src/sql.mjs`**: `toRecords()` keys a sheet's rows by column name,
  `loadRefs()` walks the refs — cycle bound, fetch, type check, and the row budget the server spends through its
  `onLoad` — and `planQuery()` runs the four pre-engine passes in the one order that works. Where a sheet comes from is
  the only real difference, and it is an argument: `sheet()` with its access check on the server, a library entry or an
  automerge document in the page. `describing` is why `describe` still answers on a sheet whose cells fail the type
  check, in both places. Two AlaSQL behaviours shape how every query sheet is written: a `group by` expression is
  evaluated against an **empty row**, so a UDF named there receives nothing and the call has to move into a subquery;
  and an exception thrown from a function while a subquery in the from clause is computed is **discarded**, surfacing as
  "Cannot read properties of null (reading 'data')" — `formatQueryError()` replaces that one, because the message it
  destroyed is unrecoverable
- **Window functions**: AlaSQL parses `over (partition by ...)` and computes it wrong (`sum(x) over (...)` came back 0),
  so a window never reaches it. `rewriteWindows()` in `src/sql.mjs` lifts each one out of the **top-level** select list,
  leaves `null as <alias>` where it stood and appends the plain columns it reads as `__w<n>[apo]<n>`; `applyWindows()`
  computes it over the rows the engine returned and drops those columns again. Both engines call the pair, so a window
  means the same thing in the page and in `POST /query`. Ranking, offset and aggregate functions with `rows`/`range`
  frames; the default frame is peer-correct `range unbounded preceding to current row`. A trailing `limit` is stripped
  before the engine runs and re-applied after, because a window is defined over every row the query produced. A window
  that is not a select item of its own — wrapped in an expression, or inside a subquery — is refused by name: returning
  zeros for it is the bug this replaced. `WINDOW_TYPES` gives each alias its result type on the server; the page takes
  it from the query sheet's own `cols`. `ignore nulls` on `lag`/`lead`/`first_value`/`last_value`/`nth_value` steps over
  the rows that have no value, which is what makes `last_value(x) ignore nulls` a forward fill; asking any other
  function to ignore nulls is refused, because it already does. `qualify <condition>` rides the same pass: a window
  named in the condition is lifted into a `__q<n>` column, computed with the rest, filtered on by the engine itself,
  then dropped. It is what makes an as-of join one statement —
  `join ... on p.day <= t.traded_on qualify row_number() over (partition by
t.trade_id order by p.day desc) = 1` —
  instead of two sheets, and an unknown column in the condition is named rather than answered with an empty sheet
- **Pivot and unpivot**: `pivot` is AlaSQL's own and correct, except that a quoted in-list matches nothing and answers
  with zero rows, which `checkPivot()` refuses. `unpivot` is ours: AlaSQL drops every column it is not unpivoting, so
  `rewriteUnpivot()` expands it into the `union all` it means, reading the wide column names off the source sheet's own
  type row. That is why the source has to be a `@sheet` and a subquery is refused
- **Schema introspection**: `describe @table:abc` is intercepted by `describeRef()` before the engine sees it, in both
  engines — inside `executeSql` on the server and inside `runSql` in the page — and answers with
  column/type/rows/nulls/sample. It is the one statement that still works on a sheet whose cells fail the type check,
  because that is the sheet you need to inspect
- **Cost guards**: `checkQueryRows()` caps the rows a single query may load across every `@sheet` (`MAX_QUERY_ROWS`);
  that is the guard that actually stops a runaway, since a single-threaded engine cannot be preempted. `MAX_QUERY_MS`
  only bounds how long the caller waits — the work itself still finishes
- **Type mismatch**: `checkColumnTypes()` runs on each **table** sheet as it loads, in both engines, and rejects a
  non-numeric value in a `num`/`int`/`float`/`usd`/`percentage` column, naming the row, the declared type and the value.
  Without it a `sum()` over a column holding "n/a" was quietly wrong. Query sheets are exempt: their column types are
  the source column's, so `cast(price as string) as price` would trip a check meant for a bad cell
- **Error shape**: `explain(headline, fields)` in `src/sql.mjs` is the one formatter for the aligned
  expected/received/source/fix block
- **Sharing**: `GET/POST/DELETE /library/:id/share` (owner-only, by email + role), `POST /library/:id/public`, and
  `POST /library/:id/link` which mints a viewer-scoped JWT. The link rides the sync socket's existing `?auth=`
  parameter, so there is one read path rather than two
- **Marketplace checkout**: `POST /buy/:id` fulfills `$0` listings immediately. A positive `sell_price` creates a Stripe
  Checkout Session (`STRIPE_SECRET_KEY`) and returns `{ checkout_url }`. `POST /stripe` verifies `stripe-signature`
  (`STRIPE_WEBHOOK_SECRET`) and fulfills `checkout.session.completed` with `payment_status=paid`. Checkout is card-only.
  Money lands on the platform account; Connect payouts are not wired.
- **Reads and export**: `GET /sheet/:id` is the stable JSON read for every sheet type. `GET /export/:id.<format>` is the
  download path, one route over the `EXPORTS` table: `csv`, `json`, `ndjson`, `md`, `ics`. All go through `sheet()`, so
  they inherit `assertSheetAccess` (membership, purchase, and `public`), pagination, and query-sheet recursion. Export
  asks for 100000 rows so a net or query sheet is not truncated at the default 50. The name-keyed formats (`json`,
  `ndjson`) refuse a sheet with two columns of the same name rather than overwrite one; `ics` needs a
  `date`/`timestamp`/`create` column and says so when there is none
- **CSV import**: `POST /import/csv` rejects a row whose field count does not match the header, naming the line, both
  counts, the raw text and the column it stops at. Type inference requires every non-blank value to parse: at the old
  80% threshold the other fifth became `NaN` silently
- **Deterministic reads**: `/library` had no `order by` at all and `/shop` ordered by a non-unique `name`, so paging
  either repeated and skipped rows. Both now carry a unique tiebreaker, as the `net` read already did
- **Marketplace**: Buy/sell sheets with pricing system
- **Real-time data**: WebSocket portals for live data (time, stock prices)
- **Database codex**: Connect external PostgreSQL databases

### Frontend Architecture (src/Main.elm)

- **Architecture**: Elm Architecture (Model-Update-View)
- **Document types**: Library, Shop, Tab (table), Query, NetHook, NetHttp, NetSocket. Remaining server types (portal,
  template, codex-_) decode to `Unviewable typ`, which the view reports as an error naming the type and a query that can
  read the sheet. Give a type a real view by replacing its `Unviewable` branch in `docDecoder`.
- **Default library**: client-side (localStorage) system entries merge bundled examples from `src/examples.mjs`
  (datasets, reference/crosswalk tables and example queries), 7 live portals, and the tutorial sheet; system ids skip
  `repo.find` in `changeId`.
  `deno eval "const m = await import('./src/examples.mjs'); console.log(m.DATASETS.length,
  Object.keys(m.EXAMPLES).length, JSON.stringify(m.EXAMPLES).length)"`
  prints the counts and the byte size, which has to stay well under the ~5 MB localStorage budget. Reference tables
  carry the `reference` tag and joinable spines `dataset`, the only handle the flat library gives for telling them
  apart. Thirty-odd end-to-end pipelines from the Demo Gallery ship as sheets tagged `demo` plus a domain tag, spanning
  twenty-two domains — finance, healthcare, legal, real estate, manufacturing, government, retail, insurance, logistics,
  hr, education, nonprofit, household, music, sports, transport, science, agriculture and the rest — each a seeded
  source table plus one or two query sheets. **`todo.md`'s Demo Gallery is the index: the checked entries name their
  sheets, and that is the list to read rather than this one.** Their data is invented; their shapes are not. `store` and
  `class` are AlaSQL keywords, so no column may be named either: `select store from ...` will not parse.
  `table:assumptions` is the one-row settings sheet those demos read their parameters from, so changing one cell changes
  four sheets. `examples_test.ts` runs **every** bundled query in both engines, resolving `@query:` refs itself, and
  calls `checkResultColumns()` on each result — a typo in a bundled example used to read as a sheet of blanks. Two
  AlaSQL limits shape how the demo SQL is written: a UDF cannot be named in a `group by` (bin or bucket in a subquery,
  then group by the column), and `min()`/`max()` drop a date held as text (`min_text()`/`max_text()`)
- **Library gallery**: `viewGallery` puts a strip above the library table naming every `demo`-tagged query sheet as a
  link and every tag as a filter chip. It reads `model.library`, so a new demo needs no code change
- **Cross-sheet queries in the browser**: `sheets(alasql, shelf, find)` in `src/page.mjs` returns the `runSql` that
  rewrites `@type:doc_id` to `SHEET('id')` and pre-loads each doc before AlaSQL runs — the pre-load has to happen first
  because finding a document is async and an AlaSQL from-function is not. It then applies the cell, unpivot and window
  passes in the same order the server does; `@query:` refs recurse **through `runSql` itself**, so a window inside a
  referenced query is computed rather than handed to AlaSQL. Bounded by `checkRefPath`, which reports a cycle as the
  path that closes it (`a -> b -> a`) and caps depth at `MAX_REF_DEPTH`. Only two things come from the browser and both
  are arguments: `shelf()` is the library map (`Library.get`) and `find(doc_id)` is `repo.find`, which is what lets
  `page_test.ts` drive the whole thing with neither
- **UI chrome**: keyboard shortcut sheet (Ctrl/⌘+/ or "?"), library sparkline thumbnails (computed JS-side into
  localStorage entries), five-step first-run tutorial (localStorage `scrapsheets-tutorial`, -1 = dismissed)
- **Real-time sync**: Ports for Automerge integration
- **UI**: Table-based interface with cell editing, selection, and statistics
- **No runtime CDN**: every asset the page loads is served by us. `deno task vendor` uses esbuild to inline each
  package's dependencies into `src/automerge.mjs`, `src/automerge-repo*.mjs` and `src/alasql.mjs`, and fails the build
  if a bundle would still fetch something. A CDN in the request path also meant root-relative URLs inside third-party
  bundles (`/npm/...`, `/sm/....map`) resolving against our origin and hitting the `/*` catch-all.
- **Automerge loading**: `src/index.html` maps `@automerge/automerge` to the vendored `/automerge.mjs` and calls
  `initializeWasm(fetch("/automerge.wasm"))`. Automerge stays _external_ in the repo bundles so all three share that one
  initialized copy; bundling it into each would give the browser three, only one with WASM.
- **AlaSQL parity**: the page imports `/alasql.mjs`, bundled from the same bare `alasql` specifier the server uses, so
  both engines resolve one version through `deno.lock`. It used to be a CDN `<script>` on a floating `@4` tag.

### Key Frontend Features

- **Live editing**: In-place cell editing with type-aware rendering
- **Table UX**: multi-column sort (click a header to sort, shift-click to add a key; descending flips the comparator per
  key rather than reversing the list, so ties keep their order), column hide via the filter popover with "show all" in
  the filter bar, row insert/duplicate (`Ctrl/⌘+Enter`, `Ctrl/⌘+Shift+Enter`) and fill-down (`Ctrl/⌘+D`). A hidden
  column keeps its x coordinate and only stops rendering — navigation steps over it via `skipHidden`. Filtering the
  column array instead would shift every selection index in the file
- **Statistics**: Real-time column statistics for Number/Usd (numeric), Text (descriptive), Date/Timestamp (first, last,
  span, gaps) and Boolean (true/false/blank); other column types get no stats. A totals row sums numeric columns over
  the rows actually on screen, so it respects the active filter
- **Query interface**: Embedded SQL editor for query sheets
- **Type system**: Rich type system including USD, links, images, forms

### Database Schema (schema/db.sql)

#### Core Tables

- **usr**: User accounts with identity, name, email (citext), password, and `stripe_customer_id`
- **sheet**: Central document table with polymorphic sheet_id format (`type:doc_id`)
  - Types: template, table, net-hook, net-http, net-socket, query, portal, alert, chart, codex-*
  - Marketplace fields: sell_id, sell_type, sell_price, buy_id, buy_price
  - Document data: row_0 (jsonb), name, tags (text[])
  - `public boolean`: anonymous read through `syncRole`
- **sheet_usr**: Many-to-many permissions between sheets and users, with `role` (owner/editor/viewer)
- **db**: External database connections (DSN storage for codex sheets)
- **net**: Webhook data storage for net-\* sheets, and the run log for `alert` sheets, which is why the sheet_id check
  allows both prefixes. The read projects `created_at, body, method, req_headers, query_params` — the last three
  appended, so existing column positions and `select body from @net-hook:x` still hold. postgresjs returns jsonb as raw
  JSON text, so those cells are strings; read them with `json_extract()`. `meta` is what the run itself cost:
  `{status, ms, bytes}` for a net-http poll, `{ms}` for an alert run, `{bytes}` for a webhook delivery. `net_id`
  identity PK plus an index on `(sheet_id, created_at desc)`; the read orders by both, without which paging a log
  repeats and skips rows. `trimNet()` keeps the newest `NET_KEEP` rows per sheet and runs behind every write, so the log
  is bounded — a sheet that must keep everything writes to a table, which is never trimmed
- **payment**: Marketplace transactions (buyer, seller, sell_id, buyer sheet_id, amount, Stripe session)

#### Key Schema Features

- **Generated sheet_id**: Computed as `type || ':' || doc_id` (e.g., "table:abc123")
- **Marketplace system**: sell_id generated from md5(doc_id||created_by), prevents selling and buying same sheet
- **Type constraints**: Enforced sheet types with check constraints
- **citext extension**: Case-insensitive email handling
