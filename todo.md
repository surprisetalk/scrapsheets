# Scrapsheets — todo

> Programmable data OS: every table is a database, every query is a table, every sheet is an API.

A queue, not a history. Every item is a capability someone gains, and carries the instructions to build it. Finished
items are deleted — what shipped is described in `claude.md`. Anything below that turns out to need research goes to
**Research** first and comes back as an item, or does not come back.

---

## Query engine

The single biggest gap. Most of the Demo Gallery dies here first.

- [ ] **A money column names its currency.** `usd` is the only money type in `COLUMN_TYPES`; there is no `eur` column to
      refuse, so an EUR column is spelled `num` today and the refusal this item wants has nothing to fire on.
  1. A parametric `money:XXX` family in `COLUMN_TYPES`, matched by prefix the way `enum:` is in `knownType()`, checked
     against `table:currencies`' codes, with `usd` kept as a read-only alias of `money:USD`. Three language-boundary
     copies move with it (`Type` in `main.ts`, `columnTypes`/`typeAliases` and the wildcard-free `spec` in `Main.elm`)
     and `browser_test.ts` holds them equal.
  2. Only then: addition across two codes is an error naming both, and says the fix is an explicit rate (a join, not an
     operator). `itemType` peels function calls only and never operators, so this is a new walk over the select list,
     not an extension of an existing pass; decide where it lives before starting.

- [ ] **A timestamp means the same thing in two timezones.** Stored values are naive, rendered naive, and the zone is
      guessed at both ends.
  1. Store UTC; `date`, `time`, `datetime` and `timestamptz` are four distinct column types, not one.
  2. Render in the viewer's zone, which is a per-user setting and not the browser's guess.
  3. `table:timezones` ships the offsets; the transition dates are what the arithmetic needs and are still open.

- [ ] **A forty-sheet chain does not re-run on every keystroke.** Nothing is cached, so the editor's debounce is the
      only thing between a chain and the CPU.
  1. Cache a query sheet's result keyed by its code plus the version of every sheet it reads.
  2. An explicit refresh, and an automatic one when a dependency's version changes — the dependency list is what
     `scanRefs()` already returns.
  3. Recompute only the sheets downstream of what changed.

- [ ] **A million-row join does not need a million rows in the tab.** `MAX_QUERY_ROWS` refuses the work instead, which
      is the right guard and the wrong answer.
  1. Chunked execution on the server for anything over the cap, with the page reading pages of the result.
  2. `checkQueryRows()` keeps its message and gains "or run it on the server", with the route that does.

- [ ] **A codex filter runs on the far database.** The whole table is transferred and then filtered, which is the
      difference between a demo and a product on any real table.
  1. Push the where clause into the DSN query for `codex-db`, and only what is provably safe to push.
  2. `describe` already reads the remote schema; the pushdown uses the same read.

- [ ] **You name an expression once and use it in five sheets.** Every demo repeats the same case statement. A cell ref
      (`@table:snips.discount`) already answers a _value_ from a one-row sheet; it cannot carry an expression.
  1. Decide the sheet first: a `snippet` type fails the check constraint in `schema/db.sql` and needs `db:apply` in the
     order the map describes, plus the `Template`/`Sheet` unions, `docDecoder`, and the two-prefix guards in
     `src/page.mjs` and `completionRef`. The no-schema version is a `using @table:x` clause over an ordinary table of
     `name`/`expr` rows.
  2. Expansion runs **before `scanRefs`**, not inside `planQuery()`: a snippet's text may itself hold `@refs`, and
     `scanRefs` is what collects the ids `loadRefs` fetches. Both hosts call it at the same point, and the snippet sheet
     is fetched through the same loader `loadRefs` is handed.
  3. Its bound is its own, not `MAX_REF_DEPTH`: `checkRefPath` bounds the host's recursion through query sheets, not
     textual expansion. A cycle is reported as the path that closes it, in the same shape.
  4. This is the seam Scrapscript eventually replaces.

---

## Types & validation

- [ ] **A cell can hold the things a spreadsheet holds.** Six types are missing and each blocks a whole class of sheet.
  1. Percent, ratio and basis points — display against stored value handled once.
  2. Duration and interval, for hours worked, dwell time, cycle time.
  3. Enum and multi-select, with a defined option list, and the list may come from another sheet.
  4. Reference: a cell pointing at a row in another sheet, a real foreign key rather than a string id.
  5. Attachment: files, images and PDFs per cell.
  6. Unit of measure: quantity plus unit with conversion, so lbs and kg cannot silently add.
  7. Geo: point, polygon and address. `point_in_polygon()` and `polygon_area_km2()` already take the shapes.

- [ ] **A bad row is refused at the boundary, not coerced.** CSV import rejects; `POST /net/:id` and cell edits do not,
      and nothing is quarantined.
  1. Column constraints: not-null, unique, range, regex, allowed values, referential integrity.
  2. `POST /net/:id` and cell edits run the same check the CSV importer runs.
  3. A refused row lands in a dead-letter sheet with the reason, rather than being dropped — inspectable, requeueable,
     and the same sheet the action queue uses.

- [ ] **A column can be an expression over its own row.** Every derived value is a whole query sheet today.
  1. A computed column on a table sheet, evaluated by the same engine the query sheets use.
  2. Column-level defaults and generated values ride the same field: created-at, row hash, sequence.

---

## Table UX

The unglamorous spreadsheet niceties. Their absence is what makes people leave.

- [ ] **A number reads the way your locale writes it.** A per-column `format` ships beside `decimals` — grouped and
      scientific, out of the column's panel — and percent and currency are the `percentage` and `usd` types. The
      separator is always `,` and the point always `.`.
  1. Locale-aware separators from a per-user setting rather than the browser's guess: one more per-user field, read
     where `formatNumber` reads the format, so the cell, the stats row and the totals row change together. There is no
     per-user settings row today, so this is a `schema/db.sql` change first.
  2. A custom mask only after a second real use asks for one: a mask is a parser, and `formatNumber` is the one place a
     number becomes text.

- [ ] **You colour a cell by a rule over its row.** A numeric column is shaded by its own values — a colour scale or
      data bars, chosen in the column's panel beside the format select, stored with the arrangement as the view field
      `shade`.
  1. A rule that is an expression over the whole row waits on computed columns: there is nowhere yet for a per-row
     expression to be written or evaluated.
  2. Icon sets: a `Shade` constructor and a `shadeSpec` row, drawn in the same `div.shade` wrapper.

- [ ] **You group rows and see subtotals without writing SQL.** `group by` exists only in a query sheet. Interleaved
      group-header rows break the identity "display row `n` is `sortedRows[n-1]`" that `displayYToDocY`, `selectedRows`,
      `libraryIdAtRow`, `tableBounds` and about twelve write paths assume, so this is two items in order.
  1. First, on its own: `displayYToDocY` answers `Maybe Int` and every caller refuses a write onto a row that is not a
     document row by name.
  2. Then collapsible groups with subtotals over the rows on screen, the way the totals row already respects the filter.
     The bounded alternative that needs neither: a second footer `tr` per group below the table, grouped by one chosen
     column, off `columnTotal`'s fold.
  3. A pivot UI over the same machinery — AlaSQL's `pivot` is correct once `checkPivot()` has had its say.

- [ ] **A very large sheet scrolls.** Every row renders.
  1. Virtualized rendering.
  2. Server-side pagination behind it, for sheets too big to send at all.

- [ ] **A cell can carry a note.** A `json` cell holding a flat array of numbers draws as a sparkline now; notes do not
      exist, and the row has nowhere to put one.
  1. Decide row identity first. A note belongs to a row, and a row here has no id: `data` is a positional array that
     `rowSplices`, `rowDeletions` and the `move` patch all address by index, so an insert above a noted row moves the
     note to its neighbour.
  2. Then decide where the note lives. It cannot be an extra field on the row object: `named()` in `main.ts` builds
     every read and every export as `cols.map((col) => [col.name, row[col.key]])`, so a field no column names is dropped
     on `GET /sheet`, on every export and on every MCP read — the note would be visible only to a browser with the
     document open.
  3. Only then the view: a marker on the cell, the note on hover or on a shortcut, and a `DocMsg` so undo, the viewer
     refusal and the sync path are the ones already written.

---

## Charts & dashboards

- [ ] **A geo column draws a map.** Nothing renders geometry.
  1. Point maps and choropleths from a geo column.
  2. Boundary datasets as sheets — counties, tracts, ZCTAs, districts — are the other half and live in **Inventory**.

---

## Ingest — net-http

- [ ] **A feed behind OAuth works.** A feed is polled with a GET, a POST or a PUT and a templated body, and a static key
      out of the secret store is the only credential it can carry.
  1. OAuth authorization code flow plus automatic refresh, on top of the secret store: the refresh token is a secret
     like any other, and the access token is written back beside it rather than into the document.
  2. Static egress IP, which many enterprise sources require before they will talk at all.

- [ ] **A connector is added without a code change.** Every SaaS source under **Inventory** is otherwise a pull request.
  1. Most of a connector definition already exists on a net-http sheet: `url`, `method`, `body`, headers,
     `page_by`/`page_param`/`page_path`, `mode`/`key`/`rows_path`, and a secret per header. What a definition adds is a
     name, one set of those fields per endpoint, and auth.
  2. So it waits on the OAuth item above: auth is the only part a net-http sheet cannot already express, and a
     definition written before it has a hole exactly where every real connector needs a token.
  3. Then a `connector` sheet holding the definition, applied by writing the net-http documents it describes — nothing
     new polls, and `pollNetOnce` stays the one poller.
  4. Google Sheets, Airtable and Notion by hand first: they are the migration path in and the OAuth shape everything
     else reuses.
  5. Bidirectional sync is the same definition read the other way, and waits on **Actions & write-back**.

- [ ] **The response is parsed, not stored as a blob.** CSV, TSV, NDJSON, gzip, zip, XML, RSS, Atom, HTML and Parquet
      land as the JSON they mean, read by the type the answer declares and decoded by the charset it declares. XLSX and
      PDF are still one cell. A text format is one more entry in `BODY_PARSERS` and one more branch in `readFeedBody`; a
      binary one also needs the text decode skipped, the way `how === "parquet"` does.
  1. XLSX: blocked, and the blocker is the pin. `npm:xlsx@0.18.5` is the last version SheetJS published to npm — check
     with `npm view xlsx dist-tags` — and its read path is exactly what `claude.md` means by "written with and never
     read with". Decide first: a newer SheetJS from the vendor's own registry, or a different reader. Do not call
     `XLSX.read` on the pinned one.
  2. PDF table extraction, because half of government data ships as PDF. An HTML table already reads, so a PDF whose
     tables a converter can turn into HTML is the cheap half; a scanned one is not.
  3. A gzipped Parquet body: the gzip sniff answers `json` or `csv` off the first character and never `parquet`, so one
     is still one cell. Sniff `PAR1` there before the bracket test.

---

## Ingest — net-hook & forms

- [ ] **A form on the internet writes rows into a sheet.** A net-hook sheet takes JSON from something that can sign;
      nothing renders a form.
  1. Generate a real form from the column types, with validation and a thank-you page.
  2. File uploads land in an attachment column.
  3. Filters drop events that do not match a predicate before they reach the table.
  4. Response templating: choose the status and body, so the sheet can answer a callback synchronously.

- [ ] **You forward an email or a text and it becomes rows.** Neither address exists.
  1. An email-in address per sheet; body and attachments become a row.
  2. Inbound SMS on the same path, through the Twilio-shaped webhook the signature work already covers.

---

## Codex — databases

- [ ] **You connect the database you actually have.** Postgres only.
  1. MySQL, SQLite, SQL Server, DuckDB/Parquet, BigQuery, Snowflake, Redshift, ClickHouse, MongoDB, Athena.
  2. SSH tunnel and TLS options, which is what every enterprise connection needs before it connects.

- [ ] **You pick a table without writing SQL first.** The first screen is an empty query box.
  1. Schema browser and table picker, off the same read `describe` uses.
  2. Sampling for preview: never `select *` a billion-row table to draw a thumbnail.

- [ ] **A codex cannot write unless you said so.** The session is set read-only in `codexTables`, and that is the only
      statement this server ever sends to a codex database: `GET /codex/:id` reads `information_schema` and nothing in
      it. There is no arbitrary-SQL path, so there is nothing to grant with and nothing to mask on.
  1. First a codex query path — which is "You pick a table without writing SQL first" and the pushdown item under
     **Query engine**.
  2. Then an explicit write grant per connection, off by default, and row and column masking over that path.

- [ ] **A big table syncs by delta.** The whole table moves every time.
  1. Incremental sync with a watermark column.
  2. CDC or logical replication for sources that support it.

---

## Scheduling & runs

The runner is in **Now**. These are what the Demo Gallery needs on top of it.

- [ ] **A schedule can mean what you meant.** An interval in seconds is the only trigger.
  1. Cron, interval, and "on upstream change" — three kinds, not one.
  2. Timezone- and DST-aware, so "9am local on business days" means it. `table:timezones` ships the offsets.
  3. Business-day and fiscal-calendar triggers: third business day after month end. `business_days()` and
     `fiscal_period()` already do the arithmetic.

- [ ] **You can backfill a schedule over a historical date range.** A net-http or alert sheet only ever runs forward
      from the moment it was made. `POST /library/:id/run` runs one poll now, and the `{{cursor}}` watermark off the
      previous run's row is the seam a backfill rides — but it is one opaque value, not a range.
  1. Decide how a range is expressed before building: a `cursor` override on `POST /library/:id/run`,
     `{{from}}`/`{{to}}` template variables, or a `POST /library/:id/backfill` that loops the cursor N times under a
     bound. That decision is the item.

---

## Alerts & notifications

- [ ] **An alert can fire on "outside its usual band".** The condition is the query's where clause, or a row added or
      removed since the run before; a band is neither.
  1. An anomaly band needs the forecasting work under **Stats & modeling**, and waits for it.

- [ ] **An alert reaches you on a phone.** Email, a webhook url and a Teams channel ship; nothing reaches a device that
      is not reading mail.
  1. SMS and push, each a sender beside `sendAlertUrl` chosen off the destination the same way.
  2. Each needs an account somewhere -- a carrier, a push service -- so each is its own item once one of them is picked.
     Teams needed none: an Incoming Webhook is a url the customer makes in their own channel and pastes into `to`, which
     is why it shipped as one host arm and a `KEY_SHAPES` row rather than as an integration.

---

## Actions & write-back

The missing other half: sheets that do something, not just show something.

- [ ] **A sheet can send something.** Every pipeline ends in a table.
  1. An HTTP action: POST or PUT to an external API with a templated body, through the `safeFetch` SSRF guard.
  2. Built-in actions: send email, send SMS, post to Slack, create a calendar event, write to a codex table.
  3. Per-row actions: run once for each row matching a predicate, exactly once.

- [ ] **Nothing is sent that you did not see first.** An action with no dry run is a mail merge with no preview.
  1. Dry-run mode showing exactly what would be sent.
  2. An approval gate before a batch executes.
  3. Rate limits and blast-radius caps: refuse to email 40,000 people by accident, and say so.

- [ ] **A failed action is retried and then kept.** Nothing survives a failure.
  1. An action queue with retries, backoff and idempotency keys, sharing the dead-letter sheet from **Types &
     validation**.
  2. Who ran what, against which rows, with which payload, goes in the one audit log under **Permissions & governance**
     rather than a second one beside it.

---

## Lineage, tests & freshness

- [ ] **You are warned before a rename breaks a dependent.** `library:lineage` is the graph: one row per sheet and the
      sheet it depends on, off the live document through `scanRefs()`, with the columns each dependent names of that
      sheet in its `columns` column.
  1. The rename and delete verbs in the column's own panel are the door; the read is `GET /library/lineage` filtered to
     the rows whose `depends_on` is this sheet.
  2. All three states are a warning, not just the first: a row naming the column in `columns`, a row whose `columns` is
     `*` and so reads every column of it, and a row whose `columns` is `?`, which is nothing anybody could check. Name
     each dependent by its `name` and let the typist confirm; a silent write past a `?` is the guess this column exists
     to never make.
- [ ] **A sheet states what must be true of it.** Nothing is asserted.
  1. Assertions: not-null, unique, accepted values, row-count range, freshness bound, referential integrity.
  2. Results land in the run log, and failing rows are quarantined rather than passed silently.
  3. Row-level provenance: which source and which run produced this row.

---

## Stats & modeling

The Excel add-in market lives here.

- [ ] **You can solve for an input.** No goal seek, no solver. A UDF receives evaluated values and a window receives
      rows the engine already returned, so neither can re-evaluate an expression; the open decision is how the objective
      is named (a snippet ref, a query re-run with a bound parameter, or a Scrapscript lambda — none exists).
  1. Buildable now with no engine change: a sweep demo in `src/examples.mjs` — candidates from `SERIES` or
     `@table:trials`, the objective per candidate, and `qualify row_number() over (order by abs(y - target)) = 1` to
     pick the closest, the shape `query:asof-price` already uses.
  2. Real goal seek and constrained optimization wait on the decision above.

- [ ] **A clustering table writes itself.** You open a table or query sheet and the palette writes the segmentation SQL
      for it, the way it already writes a cohort table.
  1. `cohortSql` in `src/sql.mjs` is the precedent: a config object in, one validated SQL string out, a bad field
     refused by name, and `newDoc` in `src/index.html` the one caller, because Elm cannot import that module.
  2. The palette guesses the columns the way the cohort command does, and offers nothing over a sheet it cannot guess
     from.

---

## Geospatial

- [ ] **An address becomes a point.** Nothing geocodes.
  1. Geocoding and reverse geocoding, with a match-confidence score.
  2. Address normalization and dedupe, which is the hard part of every property and customer dataset.

- [ ] **A nearest point is found without measuring every pair.** `point_in_polygon()` works as a join predicate and
      within-distance is `haversine_km(...) <= n`.
  1. The sorted-input join under **Research** is the machinery; this item is the geodesy on top of it.
  2. Drive-time distance, and something that reprojects — neither exists.

---

## AI & MCP

- [ ] **A column can be filled by a model.** Nothing calls one.
  1. Classify, extract, summarize or translate per row, cached by row hash so a re-run is free.
  2. Document to table: PDF, invoice or contract into structured rows.
  3. Entity resolution by embedding, to match "Acme Corp." to "ACME CORPORATION" across sheets.
  4. Cost caps and token budgets per sheet and per user, enforced **before** the spend.

- [ ] **You ask for a query in words and read the SQL before it runs.** There is no natural-language path.
  1. Schema-aware generation off the same read `describe` uses.
  2. The generated SQL is shown for review, never run unseen.
  3. A prompt eval sheet — test cases and scores — is a normal sheet, and is how this stays honest.

---

## Reports & export

- [ ] **A report arrives looking like a report.** `csv`, `json`, `ndjson`, `md`, `ics`, `xlsx` and `parquet` ship
      through one route; the workbook carries values, a number format per column and widths, and no cell styles, because
      the SheetJS community edition drops them on write, and the parquet file types each column once because a parquet
      column is homogeneous.
  1. PDF with a print layout: headers, page breaks, title page.
  2. Prose plus live sheet embeds, so the narrative regenerates with the numbers.
  3. Scheduled delivery, emailed with the file attached — the runner in **Now** is what it rides.

---

## Permissions & governance

- [ ] **You grant access to a group, not to twelve addresses.** `sheet_usr` is per user.
  1. Teams, groups and org accounts.
  2. SSO, SAML and SCIM, which is table stakes for any org-sized customer.
  3. Ownership transfer and offboarding: what happens to sheets when someone leaves.

---

## Search, shop & discovery

- [ ] **You find a sheet by what is in it.** Search is over the library table on screen.
  1. Global search across sheet names, column names and cell contents.
  2. Semantic search across a library: find the sheet, not the filename.

- [ ] **The shop can be browsed.** Name, price, license, type and tags are columns of `GET /shop`, the column panel is
      the facet, and the page asks for the whole catalogue.
  1. You filter the shop by category, by source and by update cadence. None of the three is a field anywhere: they need
     columns on `sheet` in `schema/db.sql`, a backfill for the seeded listings, and the deploy-before-constraint order
     the map describes, before `GET /shop` can answer or filter them.
  2. Collections — curated bundles of related sheets.
  3. "Used by N sheets" as the trust signal, plus buyer ratings. `POST /shop/:sell_id/review` is already taken — it is
     the operator's `keep` or `takedown` on a report — so a rating needs a route and a table of its own, and a listing
     column that averages them.

- [ ] **You see a dataset before you buy it.** There is no preview. `license` ships and is enforced at listing time.
  1. A free first-N-rows sample for a `table` listing, as a public route beside `GET /shop`. Decided: it cannot call
     `sheet()` (that hard-calls `assertSheetAccess` with a `usr_id` a public route has none of), so it reads the
     document through `docData` and `named()`; it re-scans only the rows it returns against `KEY_SHAPES` and
     `PII_SHAPES` and refuses with a 409 that names nothing, because `assertNoKeys` ran once at `POST /sell/:id` and the
     document has synced since; it records an audit row with a null `usr_id` and `via: "public"`. Open, and the reason
     it is not built: a public door that spends the seller's own sheet budget through `spend()` lets anyone starve that
     sheet, and a door that does not spend breaks the one-budget invariant. Decide which before starting. `query`
     listings need a `Context` with a user to run and stay out.
  2. A changelog per dataset, and provenance on every published one: source URL, fetch date, transformation chain.

---

## Marketplace economics

Stripe Checkout ships platform-side; Connect payouts are the one piece missing.

- [ ] **A seller gets paid.** Money lands on the platform account and stays there.
  1. Stripe Connect onboarding and payouts.
  2. Tax and VAT handling, plus invoices and receipts.

- [ ] **A feed can be sold as a subscription.** One-off purchase is the only model, which is the wrong one for anything
      that keeps updating.
  1. Recurring price for a dataset that keeps updating.
  2. Usage-based pricing and metering: per query, per row, per API call.
  3. Tiered and free plans: free up to a row cap, paid above.
  4. Trials, coupons and refunds.

- [ ] **A seller can see what is selling.** There is no dashboard.
  1. Revenue, subscribers, churn and per-sheet analytics.
  2. License enforcement: what a buyer may do with a purchased dataset, and what happens on cancellation.
  3. Private and org-only listings, bundles, and referral credit.

---

## Collaboration

- [ ] **You see who else is in the sheet.** Not blocked anywhere: the vendored `automerge-repo` bundle exports
      `Presence` (heartbeat, TTL, `broadcast`, peer states) over `DocHandle.broadcast`, and the sync path in `main.ts`
      relays an ephemeral frame — `carriesChanges` is false for it, so a viewer's is not refused, and `syncRole` still
      gates it.
  1. An active-user list in the sheet header first: a `Presence` started in `selectDoc` and stopped where `watched` is,
     broadcasting `model.auth.email` (a client can claim any name; say so), one port pair, `model.peers`, chips in
     `viewToolbar`.
  2. Cursor presence after, throttled to 200ms or more: every frame goes through the server's serialized sync queue.
  3. The cost is the harness: `fakeHandle` has no `broadcast`, and `glue()` builds one repo, so a presence test needs
     two connected `realRepo`s or a server-side relay test in `main_test.ts` plus an Elm rendering test in
     `page_test.ts`.

- [ ] **You can argue about a cell in the cell.** There is nowhere to put a comment.
  1. Threaded comments on individual cells.

- [ ] **You can go back.** Automerge stores the full history and nothing reads it. The vendored bundle exports
      `getHistory`, `view`, `getHeads`, `topoHistoryTraversal` and `diff`.
  1. A timeline and a read-only past state first: `historyLoad`/`historyLoaded` and `historyView`/`historyShown` ports
     modelled on `preflight`, using `getHistory(doc).map(h => h.change)` (never `.snapshot`, which rebuilds from scratch
     per entry) and `view(doc, [hash])`. The past doc lives in a new `model.history`, **not** `docSelected` — reusing it
     rebuilds the whole `Sheet` and leaves `changeDoc` live, so a keystroke on a past version would write the live
     document. `glue_harness.ts` `deps` must list the new imports, and `import { x as y }` is refused there, so pick
     names that do not collide in `index.html`'s module scope. Tests need `realRepo: true`; `fakeHandle` has no history.
  2. A visual diff between two versions: `diff` answers automerge patches, and rendering a patch list as a cell-level
     diff is its own design.
  3. Named snapshots, and rollback — a write, in its own item.
  4. A conflict-resolution view, for the merges Automerge cannot decide.

---

## Offline & mobile

- [ ] **The app works on a phone and on a plane.** It installs and its shell opens offline now —
      `src/manifest.webmanifest` and `src/sw.js` — and then still assumes a mouse and a connection for the data.
  1. Responsive touch-friendly cell editing and swipe navigation.
  2. IndexedDB-first sync — Automerge already uses it, so this is optimisation rather than new machinery. The shell
     already opens offline out of `src/sw.js`; this is the data half.

---

## Developer surface

- [ ] **A sheet can live in git.** Everything is clicked.
  1. A text definition of a sheet and its pipeline, checked in.
  2. A CLI: push and pull CSVs, run queries, tail run logs.
  3. Thin typed client SDKs over the REST API.

- [ ] **You can change a pipeline without breaking the live one.** Every edit is live.
  1. Staging copies: clone a sheet, change the query, review the diff, promote.
  2. Branch and merge a sheet — Automerge makes this genuinely possible and nobody else can offer it.
  3. A sandbox: fake webhook deliveries and dry-run schedules.
- [ ] **You can run it yourself.** There is no self-host path.
  1. A docker image, for the customers who cannot send data anywhere.
  2. Workspace export: a zip of every sheet the account owns. There is no zip writer in the repo (only the reader
     `zipMembers`/`zipData` and `crc32`); a stored-only writer round-tripped through that reader is the test. One
     `<sheet_id>.csv` per table through `EXPORTS.csv.render` plus a `manifest.json` (name, type, tags, license, column
     types), and a non-table sheet's settings as JSON, since its value is its document and not its run log. Bounded by
     `USER_SHEETS_MAX` and a byte cap. **Not** `/export/workspace.zip`: that path collides with the `EXPORTS` route
     regex.
  3. Workspace import, its own item: N documents, N `sheet` and `sheet_usr` rows atomically, `assertSheetsQuota` over
     the whole batch, `assertRoom` per sheet, and a decision on `@ref`s that name doc_ids which no longer exist.

---

## Navigation & workspace UX

- [ ] **A library of hundreds of sheets is navigable.** Favourites, bulk trash and bulk tag are in; it is still one flat
      list.
  1. You select many library rows and move them into a folder together. Folders do not exist yet: a folder is a field on
     the library entry this browser stores, `library()` in `src/page.mjs` merges it, and the library table gains a
     column for it before any verb can move a sheet into one.
  2. You select many library rows and share them together. One server call per sheet, through `POST /library/:id/share`,
     so it needs a bounded fan-out and one refusal that names the sheets it could not share rather than a per-sheet
     error nobody reads.
- [ ] **Everything is reachable without a mouse or a screen.** Ctrl/⌘+K opens a palette over every sheet and every
      runnable shortcut, every modal is a labelled `role="dialog"`, every icon-only button and every placeholder-only
      input carries an `aria-label`, and the table is a named `role="grid"`.
  1. One modal at a time: settings, shortcuts and the palette can all be open at once today, each `aria-modal`. Close
     the others in `ShortcutsToggle`, `PaletteToggle` and the `showSettings` route branch.
  2. Trap focus inside an open modal and restore it to whatever opened it on close.
  3. Give the `.grab` row handle and the `.grip` column resizer keyboard equivalents, so reorder and resize are not
     mouse-only.
  4. Put `gridcell` and `aria-selected` on the cells, so a reader hears which cell the selection is on.
  5. Point the palette at its highlighted row with `aria-activedescendant`, since `selected` is a model index and
     nothing in the DOM says which row it is.

---

## Performance & scale

- [ ] **A large sheet does not choke the tab.** Automerge holds the whole document in memory.
  1. Document size limits, compaction and history pruning, with a graceful path for large tables.
  2. Columnar storage for bulk data, with Automerge kept for collaborative editing.
  3. Cold row archiving: keep history without keeping it hot.

- [ ] **A long query does not freeze the UI.** The engine is single-threaded and cannot be preempted, which is why
      `MAX_QUERY_ROWS` is the real guard.
  1. Background computation with progress.
  2. Per-sheet resource metering — rows, bytes, compute, fetches — visible before the limit hits.

---

## Go-to-market

- [ ] **Somebody outside this repo has used it.** No launch has happened.
  1. Post a compelling demo and say you are looking for angels. Skip the deck.
  2. File real GitHub issues, publish the demo plus a blog post, rally contributors.
  3. Sit with Clark, Kirk and Jake; convert findings into issues.
  4. Reach out to Ellen Chisa, intro via Brandon, lead with the demo.

- [ ] **The path from signup to paid is instrumented.** `GET /status` grades one usage condition and it does not page.
  1. Define signup → first sheet → paid, and instrument each step.
  2. Test paid acquisition on Reddit and adjacent communities against that funnel.
  3. Demo-first deck at /scrapland once the dev launch lands.
  4. Pitch spreadsheet and data podcasts; become the face of data cleaning and curation.

- [ ] **The shop has something nobody else has.** It has the seeded examples.
  1. Seed the public-entity spines: orgs, people, parcels, colors, songs.
  2. Build the giant local-events table as the flagship public dataset.

---

## Inventory

Raw lists, mined by later passes rather than worked top to bottom. Nothing here is a task until it comes out as one.

### Demo Gallery

End-to-end use cases written as sheet pipelines. **`src/examples.mjs` is the index** — the sheets tagged `demo` are what
ships, run by `examples_test.ts` in both engines. Every unbuilt one dies at the same place, the ingest half, because a
seeded table is a feed nobody has connected yet. That is what **Now** and **Ingest** are for; when a pipeline is worth
building, it comes out of here as one item naming the feed it needs.

- [ ] **Flagship**: restaurant-group weekly P&L, fund 13F drift, contractor WIP schedule, municipal budget watchdog, the
      solo consultant's whole business in six sheets. Four of the five already ship their query half.
- [ ] **Finance, trading and insurance**: three-statement forecast, commission calculator, covenant monitor, FX
      exposure, overhead allocation, ASC 606, AR aging, earnout tracking, pairs trade, options screener, crypto
      treasury, earnings calendar, alt-data backtest, catastrophe exposure, producer commission reconciliation, claims
      leakage, rate filing comparison.
- [ ] **Healthcare, legal and real estate**: hospital price transparency, PS&R reconciliation, drug shortage exposure,
      trial competitive map, infection surveillance, prior-auth turnaround, wRVU productivity, docket watch, realization
      analysis, entity and lien monitoring, patent landscape, discovery cost, property tax appeal, construction draw,
      short-term rental pricing, deal underwriting, CAM reconciliation.
- [ ] **Operations**: construction (RFI log, material escalation, weather-at-risk, equipment utilization, certified
      payroll, punch list), manufacturing (OEE, BOM roll-up, supplier scorecard, preventive maintenance, takt planning),
      logistics (landed cost, carrier scorecard, container ETA, safety stock, cold chain, tariff impact).
- [ ] **Commerce and hospitality**: competitor repricing, settlement reconciliation, inventory allocation, return abuse,
      assortment gap, promo lift, recipe costing, hourly labor, multi-unit rollup, theoretical vs actual usage, health
      inspection watch, hotel pace.
- [ ] **Public and civic**: budget burn rate, permit backlog, campaign finance network, lobbying vs votes, records
      request tracker, grant subrecipient monitoring, effort reporting, student risk, enrollment funnel, district
      spending vs outcomes, course demand, grant pipeline, 990 benchmarking, outcome reporting, restricted funds,
      volunteer scheduling.
- [ ] **Energy, agriculture, climate**: day-ahead spread, solar performance ratio, demand response settlement, utility
      bill audit, carbon inventory, irrigation scheduling, grain hedge ratio, crop insurance documentation, livestock
      rations, CSA pick list, air quality, water sampling compliance, facility emissions, wildfire and flood risk, waste
      diversion.
- [ ] **Media, sport, science and personal**: story data pipeline, FOIA tracker, newsletter analytics, ad pacing, beat
      entity database, tour routing, setlist analytics, sync licensing, merch inventory, film budget variance, player
      efficiency, athlete load, youth league scheduling, ticket pricing, recruiting board, literature watch, instrument
      QC, variant annotation, field survey, reproducible analysis artifact, job search tracker, renovation bids,
      collection catalog, event planning, fantasy league.
- [ ] **Go-to-market functions**: blended CAC, SERP tracking, multi-touch attribution, influencer ROI, content calendar,
      pipeline hygiene, territory planning, lead scoring, renewal risk, deal desk, partner revenue, applicant funnel,
      headcount plan, certification dates, onboarding checklists, engagement survey, incident metrics, CVE exposure,
      DORA metrics, on-call fairness, MCP ticket triage, feature flag cleanup, hours-of-service, flight ops, charter
      quoting, transit reliability.

### Datasets

The shop inventory the Demo Gallery implies. Most are public, most are ugly, and cleaning them is the product. Each
lands as a sheet with a stated source, license, cadence and provenance. Roughly forty reference tables already ship —
`src/examples.mjs` is the list, not this one.

- [ ] **Spines**: organizations, people, places, parcels and buildings, products, events, securities, songs, colors.
      `table:countries`, `table:us-states`, `table:airports`, `table:seaports`, `table:exchanges` and
      `table:gs1-prefixes` are the parts that ship; every geometry and every identifier crosswalk is open.
- [ ] **Reference and crosswalks**: calendars, timezone transitions, FX rates, ZIP↔county↔CBSA↔tract, NAICS/SIC/GICS
      detail codes, SOC and O*NET detail. The top level of each already ships; the detail and the mappings do not.
- [ ] **Economy and government**: BLS, BEA, FRED and Treasury, Census, Federal Register, USAspending and SAM,
      Grants.gov, FEC, Congress, lobbying, IRS 990, state business registries, sanctions and PEP lists.
- [ ] **Finance and markets**: EDGAR filings with XBRL, equity prices with splits and dividends, options chains, futures
      and commodities, crypto, short interest and insider trades, earnings calendar, bankruptcies, FDIC and NCUA call
      reports.
- [ ] **Health**: CMS (NPI, compare, cost reports, fee schedules, Part D), hospital price transparency, FDA, trials, the
      detailed code sets (ICD-10, HCPCS, LOINC, RxNorm, SNOMED — note CPT needs a license), CDC, genomics references.
- [ ] **Weather, climate and hazard**: NWS/NOAA, historical normals, storm tracks, FEMA, wildfire, air quality, water,
      earthquakes.
- [ ] **Energy and environment**: EIA, ISO/RTO LMP feeds, solar resource, state oil and gas, emission factors beyond the
      EPA set, EPA enforcement.
- [ ] **Property, trade and logistics**: assessor rolls and deeds, zoning, building permits, rent and price indices,
      HUD, construction cost indices, prevailing wage, OSHA; HTS duty rates, trade flows, vessel positions, port
      throughput, fuel surcharge, FMCSA, flight movements, freight rate indices.
- [ ] **Retail, food, agriculture, education and legal**: USDA prices and NASS, FoodData Central, restaurant
      inspections, product taxonomy, trend indices; soil survey, NDVI, RMA, livestock reports; IPEDS, College Scorecard,
      districts, assessments, H-1B and PERM, job postings, licenses; dockets and opinions, patents and trademarks, UCC,
      enforcement actions, statutes.
- [ ] **Media and technology**: news corpus, Wikidata extracts, sports schedules and box scores, betting odds, box
      office, setlists; NVD/OSV/KEV feeds, package registries, cloud SKU prices, DNS and CT logs, status pages.

- [ ] **The machinery that makes a dataset sellable.** Without it none of the above can be listed honestly.
  1. A dataset manifest per sheet: source URL, license, attribution requirement, cadence, owner, refresh status.
  2. A redistribution flag, checked before a listing goes live.
  3. Versioned publishing: buyers pin a version, a changelog explains each release.
  4. Shared normalization conventions — column names, date formats, code sets — across every shop dataset.
  5. The datasets themselves defined as Scrapsheets pipelines, which is the dogfood.

### Codex — SaaS connectors

Ordered roughly by how many demos each unblocks. Each is the same work; the item that matters is the last one.

- [ ] Accounting (QuickBooks, Xero, NetSuite, Sage, SAP, Dynamics); payments and banking (Stripe, Square, Plaid,
      Mercury, Ramp, Brex, PayPal, Adyen); commerce (Shopify, Amazon SP-API, WooCommerce, BigCommerce, Etsy, eBay).
- [ ] CRM (Salesforce, HubSpot, Pipedrive, Close, Attio); support (Zendesk, Intercom, Front, Help Scout); product and
      project (Jira, Linear, Asana, Monday, ClickUp, Notion, Airtable, Trello).
- [ ] Dev (GitHub, GitLab, Sentry, Datadog, PagerDuty, CircleCI, Vercel, Cloudflare); marketing and ads (Google, Meta,
      LinkedIn, TikTok, GA4, Search Console, Klaviyo, Mailchimp, Braze); analytics (Segment, Mixpanel, Amplitude,
      PostHog).
- [ ] HR and payroll (Gusto, Rippling, ADP, Workday, BambooHR, Greenhouse, Lever, Ashby); comms and calendar (Slack,
      Discord, Gmail, Outlook, Google Calendar, Zoom, Calendly, Twilio, DocuSign); vertical systems (Procore,
      ServiceTitan, Toast, Lightspeed, Epic/FHIR, Availity, Clio, MINDBODY, Shipstation).

---

## Research

Each ends in an item above, or in a decision to drop it.

- [ ] **TUI spreadsheet prior art**: xleak, vex-tui, CacTui (github.com/bgreenwell/xleak, CodeOne45/vex-tui,
      vkobinski/CacTui) for interaction ideas.
- [ ] **Competitive teardown**: what Ultorg, Rowboat, Excel add-ons, Wolfram, Airtable, Retool, GSuite and Linear/Jira
      each do that Scrapsheets should absorb. Include the data-virtualization end — Snowflake, Redshift, Denodo, MDM
      tools, the human "data steward" workflow — and say where Scrapsheets fits.
- [ ] **Template galleries and vertical niches**: airtable.com/universe, sourcetable.com/excel-templates,
      smartsheet.com/solutions. Decide which categories seed the shop, and which verticals have no good tool at all.
- [ ] **Excel add-in market**: which add-ins actually earn (@RISK, Crystal Ball, XLSTAT, JMP, Minitab, Kutools,
      Ablebits, Power Query/Pivot) and which are replaceable by a sheet.
- [ ] **Connector economics**: what a codex connector needs for NetSuite, SAP, Dynamics, Salesforce, HubSpot, Jira and
      Epic; and what a firehose integration actually costs from Refinitiv, Bloomberg, IEX, Tiingo, Twelve Data,
      Sportradar, FlightAware.
- [ ] **Who buys this**: interview controllers and FP&A on accruals, overhead allocation, revenue recognition, variance
      analysis and commission calc — which are just net-http plus query? Then the recurring-reconciliation verticals
      (Medicare PS&R, insurance IBNR, construction percent-complete, FAR/CAS, university grants), and the third-party
      data finance teams want. Pick one beachhead: data vendors selling to hedge funds, mid-size food distributors,
      RIAs, restaurant-group controllers.
- [ ] **Data vendors as sellers**: small and mid-size feed providers lacking distribution — crypto, alt-data, news,
      weather, sports, logistics, real estate — who could sell portal sheets.
- [ ] **Spreadsheet influencers**: ExcelIsFun, Leila Gharani, Kevin Stratvert, Chandoo, Excel Campus; podcasts
      Spreadsheet Radio, MyExcelOnline, Humans of Data.
- [ ] **Read lexega.com/blog/how-lexega-turns-sql-into-signals**: the SQL-into-signals framing may map onto query
      sheets.
- [ ] **A join over sorted inputs does not build every pair first**, which was an item under **Query engine** and came
      back here. `qualify` _is_ `rewriteWindows` — the pairs are built in AlaSQL's from clause before `applyWindows`
      runs, and `checkJoinRows` refuses past `MAX_JOIN_ROWS` up front because the engine cannot be stopped once it
      starts. Escaping the pair build means leaving the from clause, and the only hook is a from-function like
      `from.SHEET`, which is deliberately written per host three times (`main.ts`, `src/page.mjs`, `examples_test.ts`).
      Answer before it comes back: can one shared `ASOF(...)` take the loaded rows through the one existing host
      difference; is the surface a string-typed from-function or a pattern `planQuery` recognises; how does
      `checkJoinRows` learn not to charge a merge; and does nearest-neighbour by distance share any machinery with as-of
      by time, which the old item asserted and nothing tested.

- [ ] **Exact decimal money**, which was an item under **Query engine** and came back here. A `Decimal` carried as a
      string cannot be summed by the engine at all: AlaSQL compiles `SUM` as a first-class `aggregatorid`
      (`aggregatorid=="SUM"` in `src/alasql.mjs`), the same trap `min`/`max` hit, so `alasql.aggr.sum` is never
      consulted and a UDF cannot replace it. Decide between patching the vendored bundle, rewriting `sum()` over a
      decimal column in a pre-engine pass the way `rewriteWindows()` rewrites a window, and keeping floats with the
      error bound written down. `round2` in `src/Main.elm` now rounds rather than truncating, which was the cent this
      was losing on the way to the screen; the arithmetic under it is still a double. Measured: the `sum()` rename is a
      copy of `rewriteExtremes` (`grep -o 'aggregatorid=="[A-Z_]*"' src/alasql.mjs` confirms `SUM` is inlined like
      `MIN`), but the type that marks the column is the cost — it cannot be `numeric: true` (`checkColumnTypes` would
      `Number()` it) and cannot be `json.type: "string"` (`TEXT_TYPES` would sweep it into `min_text`, which compares
      `"9" > "10"`), so it is a third category beside both, three language-boundary copies, and a new guard because an
      un-rewritten `sum` over decimal text answers a number rather than the `undefined` `checkResultColumns` catches.
      Comes back as two items under **Query engine** — the column first, exact `sum` second — or does not come back.

---

## Strategic notes

**The flywheel**: marketplace payments attract template creators → the MCP server makes sheets AI-accessible →
Scrapscript is the moat no one can replicate → pipelines make sheets self-updating → sheet-as-API makes every sheet a
microservice.

**Where it stands**: the foundation, MCP and Stripe Checkout are done platform-side. Connect payouts are the one piece
of the marketplace still missing. Everything else compounds on top.

**The unique position**: Scrapsheets is not Google Sheets and not Airtable. It is a programmable data OS where every
table is a queryable database, every query result is a shareable table, every portal is a live data stream, every sheet
is an API, and every formula is a content-addressable program.
