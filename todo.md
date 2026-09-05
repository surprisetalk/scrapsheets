# Scrapsheets — todo

> Programmable data OS: every table is a database, every query is a table, every sheet is an API.

A queue, not a history. Every item is a capability someone gains, and carries the instructions to build it. Finished
items are deleted — what shipped is described in `claude.md`. Anything below that turns out to need research goes to
**Research** first and comes back as an item, or does not come back.

---

## Now

The next things to build, in this order.

---

## Query engine

The single biggest gap. Most of the Demo Gallery dies here first.

- [ ] **A currency cannot be added to another currency by accident.** A `usd` column and an `eur` column sum today, and
      the answer is a number with no meaning.
  1. Money is value plus ISO 4217 code; `table:currencies` already ships the codes and minor units.
  2. Addition across two codes is an error naming both, and says the fix is an explicit rate.
  3. A rate is a join, not an operator: the error points at it rather than inventing one.

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

- [ ] **You can see which step of a query is slow.** There is no profile, so a slow sheet is a guess.
  1. `explain <query>` beside the existing `describe <ref>`, intercepted the same way in both engines.
  2. Rows in and out per stage, and the milliseconds each took.

- [ ] **The editor suggests the sheets and columns you actually have.** `nearest()` names a typo after the fact; there
      is nothing before it.
  1. Autocomplete `@type:doc_id` from the library, and columns from the sheet once the ref resolves.
  2. Same source as `describe`, so a suggestion cannot disagree with the schema.

- [ ] **You name an expression once and use it in five sheets.** Every demo repeats the same case statement.
  1. A `snippet` sheet holding named expressions; `planQuery()` expands them before the engine runs.
  2. Expansion is textual and bounded like `MAX_REF_DEPTH`, and a cycle is reported as the path that closes it.
  3. This is the seam Scrapscript eventually replaces.

- [ ] **A join over sorted inputs does not build every pair first.** `qualify` covers the as-of join and
      `haversine_km(...) <= n` covers within-distance, but both build the pairs before the filter, and AlaSQL cannot
      parse `lateral` at all. Three questions share one missing piece — top-N-per-group, as-of, and nearest-neighbour —
      so it is written once.
  1. A join that walks two inputs already in order, keyed on time for an as-of and on distance for a nearest.
  2. It is what **Geospatial**'s nearest-neighbour item waits on, and the only reason that item is separate is the
     geodesy around it.

- [ ] **`min()` over text answers.** AlaSQL compiles `min`/`max` inline over numbers and dates and drops a text value,
      so `min(code)` returns nothing. `min_text()`/`max_text()` are the workaround and `checkResultColumns()` names the
      case.
  1. The real fix is upstream in AlaSQL, so decide: patch the vendored bundle, or keep the workaround forever and say so
     in the message.

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

- [ ] **You drag a row where it belongs, and drag a series down.** Insert, delete and duplicate ship, and a column
      drags to a new position; a row does not, and fill-down is plain today.
  1. Row drag-reorder, on the drag state `ColumnMoveStart`/`ColumnMoveEnd` already established for a column.
  2. Drag-fill that continues dates, numbers and simple patterns rather than repeating the cell.

- [ ] **A number looks like the number it is.** There is no formatting layer at all.
  1. Decimals, thousands separators, currency symbol, percent, scientific, custom mask — per column.
  2. Locale-aware, from a per-user setting rather than the browser's guess.

- [ ] **A cell can be coloured by a rule.** Nothing conditions on value today.
  1. Colour scales, data bars, icon sets and rule-based cell colour, per column.
  2. The rule is an expression over the row, the same one computed columns take.

- [ ] **You group rows and see subtotals without writing SQL.** `group by` exists only in a query sheet.
  1. Collapsible groups with subtotals over the rows on screen, the way the totals row already respects the filter.
  2. A pivot UI over the same machinery — AlaSQL's `pivot` is correct once `checkPivot()` has had its say.

- [ ] **You clean a column without writing SQL.** Binning and forward-fill are done in SQL (`width_bucket()`,
      `last_value(x) ignore nulls`); the rest are UI verbs and do not exist.
  1. Trim, dedupe rows, split column, text-to-columns, change case, remove blanks.
  2. Fuzzy dedupe: cluster near-duplicate rows and merge with a chosen survivor. `fuzzy` matching already ships as a
     UDF, so this is the UI over it.

- [ ] **A very large sheet scrolls.** Every row renders.
  1. Virtualized rendering.
  2. Server-side pagination behind it, for sheets too big to send at all.

- [ ] **A cell can carry a note and a sparkline.** Neither exists.
  1. Cell notes, distinct from the threaded comments below.
  2. Mini-charts in a cell, drawn the way the library thumbnails already are.

---

## Charts & dashboards

- [ ] **The chart set covers what people plot.** Line and bar ship, baseline pinned to zero, non-numeric y dropped.
      Histogram needs no kind — `width_bucket()` bins and a bar chart draws it.
  1. Stacked bar, area, scatter, box plot.
  2. Dual axis and a secondary series.
  3. Heatmap and matrix, because cohort grids and loss triangles read naturally that way.

- [ ] **A time axis behaves like time.** The x axis is ordinal today, so a gap in the data is a gap in nothing.
  1. Date axis with real spacing, explicit gap handling, and downsampling for a long series.
  2. Annotations: mark a release, a price change, a storm on the axis.

- [ ] **A single number gets its own tile.** A dashboard of one-row charts is the workaround.
  1. A KPI tile: value, delta against a prior period, and a sparkline.
  2. It is a `chart` kind, not a new sheet type.

- [ ] **A geo column draws a map.** Nothing renders geometry.
  1. Point maps and choropleths from a geo column.
  2. Boundary datasets as sheets — counties, tracts, ZCTAs, districts — are the other half and live in **Inventory**.

---

## Ingest — net-http

- [ ] **A feed that needs more than one GET works.** One unauthenticated GET per interval is the whole of it.
  1. POST and PUT with a templated body.
  2. OAuth authorization code flow plus automatic refresh, on top of the secret store.
  3. Static egress IP, which many enterprise sources require before they will talk at all.

- [ ] **The response is parsed, not stored as a blob.** JSON lands as one cell.
  1. Parsers: JSON path, CSV/TSV, NDJSON, XML, RSS/Atom, HTML with CSS selectors, XLSX, Parquet.
  2. Archives: zip and gzip, including "the one CSV inside this daily zip".
  3. PDF table extraction, because half of government data ships as PDF.
  4. Payload mapping is the same job on the `net-hook` side: JSON path to column, so a webhook lands as typed rows.

- [ ] **A feed that answers in pages is read to the end.** Conditional requests and the since-last-run cursor ship; one
      poll is still one request, so a paged API delivers its first page forever.
  1. Pagination: page, offset, cursor and Link-header, each with a stop condition, and the page count bounded so a feed
     that never says "last" is a failure row rather than a loop.
  2. Write mode per sheet: append, replace, or upsert by key. Append is what every net sheet does today.

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

- [ ] **A codex cannot write unless you said so.** The session is set read-only, which is the right default and not a
      permission.
  1. An explicit write grant per connection, off by default.
  2. Row and column masking, so a subset of a sensitive table can be exposed.

- [ ] **A big table syncs by delta.** The whole table moves every time.
  1. Incremental sync with a watermark column.
  2. CDC or logical replication for sources that support it.

- [ ] **A rotated credential does not break live sheets.** There is no rotation path.
  1. Two DSNs during a rollover, tried in order — the `secret` table already has this shape.
  2. The codex sheet itself already has freshness: `GET /codex/:id` writes a `method = 'CODEX'` run and
     `library:freshness` grades it. The **downstream** half is what is missing — a query sheet selecting from
     `@codex-db:x` says nothing about the connection under it, so a dead credential reads as an empty result.

---

## Scheduling & runs

The runner is in **Now**. These are what the Demo Gallery needs on top of it.

- [ ] **A schedule can mean what you meant.** An interval in seconds is the only trigger.
  1. Cron, interval, and "on upstream change" — three kinds, not one.
  2. Timezone- and DST-aware, so "9am local on business days" means it. `table:timezones` ships the offsets.
  3. Business-day and fiscal-calendar triggers: third business day after month end. `business_days()` and
     `fiscal_period()` already do the arithmetic.

- [ ] **You can run it now, pause it, and see when it runs next.** Nothing is visible or controllable.
  1. Run now, pause, disable, with the next-run time on the sheet.
  2. Backfill: run a schedule over a historical date range.

- [ ] **Sheets run in dependency order.** Each runs on its own timer, so a downstream sheet can run before its source.
  1. A DAG derived from the `@sheet` refs `scanRefs()` already returns.
  2. A cycle is refused as the path that closes it, exactly as `checkRefPath` reports one.

- [ ] **A runaway pipeline hits a limit with a clear message.** Nothing is metered per user.
  1. Run budgets and quotas per user and per sheet.
  2. A manual approval step, so a pipeline can pause for a human before a write.

---

## Alerts & notifications

- [ ] **An alert can fire on a change, not only on a row.** The condition is the query's where clause, which covers
      threshold and change but not "outside its usual band".
  1. Change conditions read the previous run out of the log the alert already writes.
  2. An anomaly band needs the forecasting work under **Stats & modeling**, and waits for it.

- [ ] **An alert reaches you where you are.** Email ships, through the Resend key signup already uses.
  1. SMS, Slack, Discord, Teams, webhook, push.
  2. A refused delivery is recorded on the alert and retried next interval, which is already the rule for email.

- [ ] **You can silence an alert without deleting it.** There is no acknowledgement of any kind.
  1. Snooze, acknowledge, escalate.
  2. Subscribe to a sheet: be told when a sheet you follow changes, without owning it.

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
  2. Who ran what, against which rows, with which payload, goes in the one audit log under **Permissions &
     governance** rather than a second one beside it.

---

## Lineage, tests & freshness

- [ ] **You can see what feeds a sheet and what it feeds.** The refs are known and never shown.
  1. A dependency graph view off `scanRefs()`.
  2. Impact analysis: what breaks if this column is renamed or removed.
  3. Warn dependents before a schema change lands.

- [ ] **A sheet states what must be true of it.** Nothing is asserted.
  1. Assertions: not-null, unique, accepted values, row-count range, freshness bound, referential integrity.
  2. Results land in the run log, and failing rows are quarantined rather than passed silently.
  3. Row-level provenance: which source and which run produced this row.

---

## Stats & modeling

The Excel add-in market lives here.

- [ ] **A regression answers more than one shape.** `regr_slope()`, `regr_intercept()`, `r2()`, `corr()`,
      `regr_predict()` and `regr_stderr()` ship.
  1. Multiple regression, logistic regression, and per-row residuals.

- [ ] **A seasonal series can be forecast.** `regr_predict()` is the straight line and `fit_exponential()` the
      log-linear one.
  1. Seasonal decomposition, which needs a series-to-series function — neither the aggregate protocol nor the window
     pass can express one today, so that is the actual work.

- [ ] **You can put a distribution on an input and read percentiles out.** The @RISK slot, and nothing occupies it.
  1. Monte Carlo: distributions on input cells, sampled outputs, percentile results.
  2. Sensitivity and tornado analysis: which input moves the output most.
  3. A scenario manager: named sets of assumptions compared side by side. `table:assumptions` is the one-row sheet the
     demos already read parameters from, so this generalises it.

- [ ] **You can solve for an input.** No goal seek, no solver.
  1. Goal seek over one cell, then constrained optimization over a sheet.

- [ ] **A cohort table writes itself.** `query:cohort-retention` and `query:cohort-grid` are the worked SQL — derive the
      cohort from the first order, then pivot.
  1. A helper that writes that SQL from a source, a date column and a key.
  2. Clustering and segmentation is the same shape of helper over a different verb.

- [ ] **A hyperbolic decline curve fits.** `fit_exponential()` and `fit_power()` fit by the transform that straightens
      the curve and refuse a value at or below zero by name.
  1. Nonlinear least squares, which is what hyperbolic decline actually needs.

---

## Geospatial

- [ ] **An address becomes a point.** Nothing geocodes.
  1. Geocoding and reverse geocoding, with a match-confidence score.
  2. Address normalization and dedupe, which is the hard part of every property and customer dataset.

- [ ] **A nearest point is found without measuring every pair.** `point_in_polygon()` works as a join predicate and
      within-distance is `haversine_km(...) <= n`.
  1. The sorted-input join under **Query engine** is the machinery; this item is the geodesy on top of it.
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

- [ ] **An agent can be trusted with write access.** MCP writes carry the caller's whole authority.
  1. Scoped tokens and per-tool permissions.
  2. Agent writes go in the one audit log under **Permissions & governance**.
  3. Sheets exposed as MCP resources and prompts, not only tools.

---

## Reports & export

- [ ] **A report arrives looking like a report.** `csv`, `json`, `ndjson`, `md` and `ics` ship through one route.
  1. XLSX with formatting, and Parquet.
  2. PDF with a print layout: headers, page breaks, title page.
  3. Prose plus live sheet embeds, so the narrative regenerates with the numbers.
  4. Scheduled delivery, emailed with the file attached — the runner in **Now** is what it rides.

---

## Permissions & governance

- [ ] **A column can be hidden from someone who can read the sheet.** Access is per sheet.
  1. Row-level and column-level permissions.
  2. PII tagging: mark a column sensitive, masked by default in shares and embeds.

- [ ] **You grant access to a group, not to twelve addresses.** `sheet_usr` is per user.
  1. Teams, groups and org accounts.
  2. SSO, SAML and SCIM, which is table stakes for any org-sized customer.
  3. Ownership transfer and offboarding: what happens to sheets when someone leaves.

- [ ] **You cannot publish a secret by accident.** Nothing is scanned.
  1. Refuse to publish a sheet containing an API key.
  2. Warn before a dataset with personal data goes public.
  3. Retention policies, legal hold, whole-workspace backup and restore, and region pinning are the rest of this row,
     and each is its own item once one customer asks.

---

## Search, shop & discovery

- [ ] **You find a sheet by what is in it.** Search is over the library table on screen.
  1. Global search across sheet names, column names and cell contents.
  2. Semantic search across a library: find the sheet, not the filename.

- [ ] **The shop can be browsed.** It is one flat list ordered by name.
  1. Facets: category, source, update cadence, license, price.
  2. Tags and collections — curated bundles of related sheets.
  3. "Used by N sheets" as the trust signal, plus ratings and reviews.

- [ ] **You see a dataset before you buy it.** There is no preview.
  1. A free first-N-rows sample sheet for every paid dataset.
  2. A changelog per dataset, and provenance on every published one: source URL, license, fetch date, transformation
     chain.

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

## Sheet as an API

---

## Collaboration

- [ ] **You see who else is in the sheet.** Automerge syncs the document and nothing else.
  1. Cursor presence with coloured indicators.
  2. An active-user list in the sheet header, and presence in the library so you can see which sheets are live.

- [ ] **You can argue about a cell in the cell.** There is nowhere to put a comment.
  1. Threaded comments on individual cells.

- [ ] **You can go back.** Automerge stores the full history and nothing reads it.
  1. A timeline slider showing document state over time.
  2. A visual diff between any two versions.
  3. Named snapshots, and rollback to any historical state.
  4. A conflict-resolution view, for the merges Automerge cannot decide.

---

## Offline & mobile

- [ ] **The app works on a phone and on a plane.** The layout assumes a mouse and a connection.
  1. Responsive touch-friendly cell editing and swipe navigation.
  2. A PWA manifest, installable, with offline support.
  3. IndexedDB-first sync — Automerge already uses it, so this is optimisation rather than new machinery.

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
  2. Workspace export and import, stated loudly as a feature rather than buried.

---

## Navigation & workspace UX

- [ ] **A library of hundreds of sheets is navigable.** It is one flat list.
  1. Folders, workspaces and favourites.
  2. Bulk operations: multi-select, move, tag, delete, share.
  3. Recently viewed, and back/forward.

- [ ] **Everything is reachable without a mouse or a screen.** Ctrl/⌘+K opens a palette over every sheet and every
      runnable shortcut; nothing below it is done.
  1. Full keyboard-only operation, screen reader support, contrast and focus order.

- [ ] **Deleting a sheet is undoable.** It is not.
  1. Trash and restore.

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

## Trust, safety & abuse

---

## Scrapscript

The moat, and the reason the formula work below is shaped the way it is.

- [ ] **A formula is a content-addressed program.** Nothing exists. The `Lang` union that named `Scrapscript` and
      `Formula` was deleted — it decoded five strings and read none of them, and a constructor is not a plan. A query
      sheet's `lang` is now a plain string, and `querify()` refuses anything but `"sql"` by name, which is the one place
      a second language has to be admitted.
  1. Compile the Scrapscript interpreter to WASM for browser execution.
  2. Admit `"scrapscript"` in `querify()` and in `queryDoc`'s switch in `src/index.html`, and run programs against sheet
     data. Both engines have to learn it together or a sheet means two things.
  3. `=#` in a cell triggers Scrapscript evaluation; `=` triggers the formula mode below.
  4. Cross-sheet references by hash rather than by the fragile `@sheet_id` string.
  5. Sell and share self-contained Scrapscript functions as composable sheet utilities — the saved-snippets item under
     **Query engine** is the seam this replaces.

- [ ] **`=A1 + B1` works.** There is no formula evaluation at all.
  1. A parser behind a `"formula"` lang, admitted in both engines the way the item above admits Scrapscript: arithmetic,
     `SUM`, `AVERAGE`, `COUNT`, `MIN`, `MAX`.
  2. Cross-sheet references: `=@table:abc123.A1`. The cell-reference rewrite in `scanRefs()` is the same idea in SQL.
  3. Dependency tracking with a topological sort and cycle detection, reported as the path that closes it.
  4. Reactive recalculation when a referenced cell changes.

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
- [ ] **A connector is added without a code change.** Every one above is otherwise a pull request.
  1. A declarative connector definition — auth, endpoints, pagination, schema — that users and sellers write.
  2. Google Sheets, Airtable and Notion are the three to build by hand first, because they are the migration path in and
     the OAuth shape everything else reuses.
  3. Bidirectional sync — writing Scrapsheets data back — is the same definition read the other way.

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

- [ ] **Exact decimal money**, which was an item under **Query engine** and came back here. A `Decimal` carried as a
      string cannot be summed by the engine at all: AlaSQL compiles `SUM` as a first-class `aggregatorid`
      (`aggregatorid=="SUM"` in `src/alasql.mjs`), the same trap `min`/`max` hit, so `alasql.aggr.sum` is never
      consulted and a UDF cannot replace it. Decide between patching the vendored bundle, rewriting `sum()` over a
      decimal column in a pre-engine pass the way `rewriteWindows()` rewrites a window, and keeping floats with the
      error bound written down. `round2` in `src/Main.elm` now rounds rather than truncating, which was the cent this
      was losing on the way to the screen; the arithmetic under it is still a double. Comes back as an item under
      **Query engine**, or does not come back.

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
