<!-- TODO: maybe restart this project as something more whimsical and personal like kinopio? -->

# Scrapsheets — todo

> Programmable data OS: every table is a database, every query is a table, every sheet is an API.

A queue, not a history. Each item is a capability someone gains, then the steps that build it. When an item is finished,
delete it: `claude.md` describes what shipped. A goal that is not code is a condition in `GET /status`, not an item
here.

---

## Next batch

- [ ] A rule you set on a column is checked by the same engine in the page and on the server.
  1. Bug fix, no design. `checkBreaks` in `Main.elm` copies `checkConstraints`. The copies drifted once already (a
     numeric string under `min` and `max`, fixed on the server in run 14), and `unique` compares by `E.encode` there.
  2. Add a `checkColumn` port in `src/index.html` that runs `checkConstraints` and answers `checkLoaded`; hold the
     panel's write until the answer lands. Delete `checkBreaks`.
  3. Give the check patch an undo entry.

---

## Ready, needs a design

Each feature here needs its design approved before the build.

- [ ] You import a money column, JPY has no decimals, and a conversion is a function.
  1. Accept `money:XXX` in `/import/csv?types=`; `inferType` never guesses it.
  2. An ISO exponent per code replaces the fixed 2 places (JPY 0, BWP 3).
  3. Extend `rejectMixedCurrency` to `where`, `case`, `sum(a + b)` and join keys.
  4. Add `convert(amount, from, to, rate)`.

- [ ] You use snippets from several tables, from a query sheet, and with completion in the editor.
  1. Accept `using @table:a, @table:b`; refuse a name two tables define, naming both.
  2. Allow `using @query:x`, requiring `name` and `expr` in its answer.
  3. Complete `{{` in the editor from the snippet table's `name` column through `columnsFor`.
  4. Arguments (`{{f(x)}}`) wait for a real snippet that needs one.

- [ ] A cell edit tells you at once when it breaks its column's check, and the import and webhook doors enforce it.
  1. Run `checkConstraints` over the edited row in `changeDoc` as advice only: a socket edit is never refused.
  2. Call it in `readImport` and `POST /net/:id`; a bad row fails the whole batch, never skipped.
  3. Row-count range, freshness bound and referential integrity each become one `CHECK_KEYS` key when picked up.

- [ ] You query a connected database's filtered table with a ref in a query sheet, on the server and in the page.
  1. Design the ref syntax first: `scanRefs` reads `@type:id.column` as a scalar cell, so a table and its filters need
     their own form. Write it as example records.
  2. Make `sheet()` answer a `codex-db` id through `codexConnect` and the preview statement, editor-only through
     `assertSheetEditor`, rows capped at `CODEX_PREVIEW_MAX`.
  3. Answer `assertNoKeys`, `library:lineage` and one `record()` row per query for the new ref.
  4. Run it in the page through `sheets()` too, and add the pair to `examples_test.ts`.

- [ ] A multi-select cell offers checkboxes, its options can come from another sheet, and every tool reads it as a list.
  1. Edit: a `viewEditCell` branch for `Multi` draws one checkbox per option and writes through `cellValue`.
  2. Import: the preview offers `multi:` for a column of comma-separated repeats.
  3. API: `GET /openapi/:id` writes `{ type: array, items: { enum: options } }` for the column.
  4. Query: `has_pick(cell, 'red')` in `src/sql.mjs`, so a where clause tests membership.
  5. Options from another sheet (`multi:@table:x.col`) wait for a sheet that needs them.

- [ ] You see who else is in the sheet by name, and where their cursor is.
  1. Stamp each ephemeral frame server-side in `/library/sync` with the sender's display name, never the email.
  2. Derive the colour from that identity, so one person keeps one colour.
  3. Broadcast the focus cell on a `cell` channel, throttled to 200 ms or more, and outline it in the peer's colour.

- [ ] You promote a fork and learn what it breaks, and branch a pipeline.
  1. Before promote writes, call `lineageFor` on the source and `dependents` on the columns the old query returned and
     the new one does not; hold the write in `model.pending` as a rename does.
  2. Add `forked_heads` so promote also catches non-code edits to the source.
  3. Branch and merge a query through automerge `clone` and `merge`. A sandbox of fake deliveries comes after.
  4. Extend promote to alert, chart and net-http sheets.

- [ ] You name a snapshot of a sheet and go back to it from the history modal.
  1. Decided: the names live in this browser's store under `Stored`, keyed by sheet id and change hash, like folders.
     They do not sync and are not in `library.zip`.
  2. The history modal lists named versions first, and a name field on the picked version.
  3. A conflict view, for the merges automerge cannot decide, comes after and needs its own design.

- [ ] You restore a past version's rows from the history modal, and Ctrl/⌘+Z takes it back.
  1. Design. Data: `rollbackPatches(now, past)` in `src/page.mjs` takes two `data` arrays and answers patches:
     `[{ action: "set", path: [3, "k2"], value: "40" }, { action: "del", path: [5, "k1"] }, { action: "splice", path:
     [], value: [7, 2] }]`.
     One `set` or `del` per cell that differs, then one tail `splice` for the row count (a delete, or an insert of the
     past rows whole).
  2. Flow: (1) You pick a version in the history modal. (2) The past table shows, with a `restore these rows` button.
     The button is hidden for a viewer. (3) The click sends `historyRestore { id, hash }`. (4) The glue answers
     `historyRestorable { id, data: { hash, forward, backward } }`, where `forward = rollbackPatches(now, past)` and
     `backward = rollbackPatches(past, now)`. (5) Elm sends `forward` through `changeDoc`, pushes
     `{ forward,
     backward }` as one `UndoEntry`, and closes the modal.
  3. Rows only. `data[0]` is never written: that would rewrite declared types and the arrangement, and skip the rename
     and delete warning.
  4. Failure modes, each through `catchy` to `model.error`:
     - Column keys differ: "Expected the version's columns to match today's. Received: k4 \"notes\" added since. Source:
       history restore. Fix: add or remove the columns by hand, then restore."
     - Past `ROLLBACK_PATCHES_MAX` (10000): "Expected at most 10000 cell changes. Received: 12408. Source: history
       restore. Fix: restore a nearer version."
     - No difference: "That version's rows equal today's. Source: history restore. Fix: pick another version."
  5. A visual diff of the picked version against now comes after, from the same comparison.

- [ ] You group rows and see subtotals without writing SQL.
  1. Decided: the chosen column is model-only state, lost on reload. Tables only: a query has no totals row today. A cap
     on groups, with an "N more" row.
  2. One footer `tr` per group under the table, in the `Tab` branch of `viewTableFooter`, from the fold `columnTotal`
     already does, over the drawn rows. A "subtotal by this column" button in the column panel.
  3. Collapsible groups after that.
  4. A pivot UI over the same machinery. AlaSQL's `pivot` is correct once `checkPivot()` has checked it.

---

## Blocked on you

### Query engine

- [ ] You see timestamps in your own zone and numbers with your locale's separators, and a timestamp means the same
      instant to every reader.
  1. Decide first: the zone and the locale are per-user settings, and no per-user settings row exists. That means a
     `schema/db.sql` change, applied in the order `claude.md` gives.
  2. Store UTC. Make `date`, `time`, `datetime` and `timestamptz` four types in `COLUMN_TYPES`, with the three
     language-boundary copies.
  3. Render in the viewer's zone, read from that setting, never from the browser's guess. Do the arithmetic through
     `Intl.DateTimeFormat` with `timeZone`. `table:timezones` holds the offsets and does not hold the transition dates.
  4. Read the locale where `formatNumber` reads the format, so the cell, the stats row and the totals row change
     together. Today the separator is always `,` and the point is always `.`.
  5. Add a custom mask only after a second real use asks for one.

### Types & validation

- [ ] A cell holds a reference, a file, or a quantity with its unit.
  1. Reference: a cell that holds another sheet's key, checked on write. It waits on row identity (see the cell-note
     item).
  2. Attachment: waits on a file store, and none exists. Decide the store first.
  3. Unit of measure: a quantity plus a unit. Adding lbs to kg is refused the same way two currencies are, so it comes
     after the money item.

- [ ] You add a column that is an expression over its own row, and colour a cell by a rule over that row.
  1. Decided: the glue computes on each `docChanged` and overlays read-only values before sort, filter, shade and stats
     read them. A row whose expression throws refuses the read and names the row and the column.
     - Decide first: the type, declared on the column and checked, or inferred through `selectTypes`.
  2. A computed column is an `expr` field on the column object. `src/sql.mjs` evaluates it in both hosts through AlaSQL,
     one `select (<expr>) as [v] from ?` per computed column, zipped back by row index. It is never stored.
  3. An expression refuses refs, subqueries and `SHEET(`, `AGGREGATES` calls, `over`, `?` parameters, another computed
     column, and `VOLATILE` calls. `namesIn()` checks that every name is a stored column of the same sheet, with
     `nearest()`.
  4. Coerce through `checkColumnTypes` before evaluating, or `"3" + "2"` concatenates. `GET /sheet/:id` does not run
     that check today, so adding it makes a sheet with one bad numeric cell refuse `GET`.
  5. Every read computes: `sheet()` into `named()`, and `loadRefs` in both hosts, which gains an engine argument. A
     server query must not compute twice. `GET /sheet/:id` gains the `MAX_QUERY_MS` race, and caps on computed columns
     per sheet and on expression length.
  6. Every write refuses a computed column: `POST /sheet/:id`, `write_cells`, paste, fill-down, the cleaning verbs and
     split. OpenAPI marks it `readOnly`. `atRisk` warns when a rename or delete takes a column an expression names.
  7. A colour rule over the whole row is the arrangement's `shade`, driven by a per-row expression, after step 6.
  8. Defaults and generated values (created-at, row hash, sequence) are their own item on the same field.

### Table UX

- [ ] You write a note on a cell and discuss it in a thread.
  1. Decide row identity first. `data` is a positional array that `rowSplices`, `rowDeletions` and the `move` patch
     address by index, so an insert above a noted row moves the note to its neighbour.
  2. Then decide where the note lives. Not an extra field on the row object: `named()` drops it from every read.
  3. The view: a marker on the cell, the note on hover or on a shortcut, and a `DocMsg`, so undo, the viewer refusal and
     sync use the paths already written.
  4. A thread is a list of notes, each with a parent and an author, stored in the same place.

### Navigation & workspace UX

- [ ] You find a sheet by what is in its cells.
  1. Cell contents need every document. Decide first whether that is a server route that spends each sheet's budget.
  2. Semantic search after that.

- [ ] Search folds case correctly: "istanbul" finds "İstanbul".
  1. Case folding goes through JS `toLowerCase`, which turns "İ" into "i" plus a combining dot. A fix needs
     normalization, which Elm lacks. Build it only after a real search breaks.

### Geo

- [ ] You turn addresses into points, draw them on a map, and find the nearest one.
  1. Decide the geocoder first. It is an outside service with an account and a usage policy. Its key goes in the secret
     store.
  2. A geo column type: point, polygon, address. `point_in_polygon()` and `polygon_area_km2()` already take these
     shapes.
  3. Geocode and reverse geocode, with a match-confidence column. For address dedupe, reuse `soundex` and `similarity`.
  4. A map chart kind: one `CHART_KINDS` row and its `kindSpec` copy. Boundary datasets are shop tables.
  5. Nearest point without measuring every pair. Answer first: can one shared `ASOF(...)` from-function cross the host
     difference (`from.SHEET` is written three times), and how does `checkJoinRows` learn not to charge a merge?
  6. Drive time and reprojection. Each needs an outside service.

### Ingest

- [ ] You connect a feed behind OAuth, and a SaaS source without a code change.
  1. The OAuth authorization-code flow with automatic refresh, built on the secret store. The refresh token is a secret,
     and the access token is written back beside it, never into the document.
  2. A static egress IP. Deno Deploy has none, so decide on a proxy before promising one.
  3. A `connector` sheet holds a definition: a name, one set of net-http fields per endpoint, and auth. Applying it
     writes the net-http documents it describes. `pollNetOnce` stays the one poller.
  4. Build Google Sheets, Airtable and Notion first, then the rest by how many demos each one unblocks: accounting,
     payments, commerce, CRM, support, project, dev, ads, HR, comms, vertical.
  5. Bidirectional sync reads the same definition in the other direction, and waits on the actions item.

- [ ] You poll an XLSX or a PDF and get rows, not one cell.
  1. XLSX is blocked by the pin. `npm:xlsx@0.18.5` is the last SheetJS version on npm. Decide first: a newer SheetJS
     from the vendor's registry, or a different reader. Do not call `XLSX.read` on the pinned version.
  2. PDF: a PDF whose tables a converter turns into HTML reads through `htmlDelimited()`. A scanned PDF does not.

- [ ] You post a form, forward an email or send a text, and it becomes a row.
  1. Decide the inbound mail and SMS providers first. Each is an account, and its webhook signature scheme is part of
     the choice.
  2. A form generated from the column types, served per net-hook sheet, with a thank-you page. It runs the bad-row
     check. A form cannot sign, so it needs `rateLimit(callerIp)` plus a spend.
  3. A filter: a predicate on the sheet drops an event before it is stored.
  4. Response templating: the sheet picks the status and the body.
  5. Email-in and SMS-in, one address per sheet, on the same path. File uploads wait on the attachment type.

### Codex — databases

- [ ] You write to the database you connected, sync it incrementally, and connect more engines.
  1. Decide first the masking model: a write grant per connection, off by default, with row and column masking.
  2. Incremental sync by a watermark column. CDC or logical replication after that.
  3. More engines, one at a time, each with its own driver and `cannotConnect()` mapping: MySQL, SQLite, SQL Server,
     DuckDB, BigQuery, Snowflake, Redshift, ClickHouse, MongoDB, Athena. Decide the first.
  4. SSH tunnel and TLS options.

### Alerts

- [ ] An alert reaches your phone.
  1. Web push first. Decide the signing library, and spike it: `npm:web-push` has a reported Deno failure;
     `jsr:@negrel/webpush` sends around `safeFetch`; `web-push-neo` exposes the request details so `safeFetch` can send
     them (the suggestion).
  2. Subscriptions are a new `schema/db.sql` table (`usr_id`, unique `endpoint`, `p256dh`, `auth`, `failures`), pruned
     on a 404 or 410. Allowlist the push services' hosts as well as `safeFetch`.
  3. VAPID keys are server environment variables you set in production. Missing keys follow the `RESEND_API_KEY`
     pattern. `to: push` means the alert owner's devices, and the run row writes `push`, never an endpoint.
  4. `src/sw.js` gains `push` and `notificationclick`. `clients` may join `BROWSER_GLOBALS`, and the `sync_test.ts`
     worker tests need `registration.showNotification` on their `self`.
  5. SMS needs a carrier account. It is a sender beside `sendAlertUrl`, picked by the destination.

### Actions & write-back

- [ ] A sheet sends something, you see what it will send first, and a failure is retried and kept.
  1. Decide first: an `action` sheet type is a `schema/db.sql` check-constraint change.
  2. Dry run first. An action sheet shows exactly what it would send, one row per request, and sends nothing.
  3. An HTTP action through `safeFetch`, with a templated body that resolves `{{secret:name}}`.
  4. An approval gate before a batch. A blast-radius cap refuses past N recipients, beside `sendWithinQuota`.
  5. Per row, exactly once: an idempotency key per row, a queue with bounded retries and backoff, and the dead-letter
     sheet from the bad-row item.
  6. Built-in actions: email, Slack, a calendar event, and a codex write. Who ran what goes in `audit` through
     `record()`.

### Stats & modeling

- [ ] You solve for an input with goal seek or a constrained optimizer.
  1. Decide first how the objective is named: a snippet ref, a query re-run with a bound parameter, or a Scrapscript
     lambda (none exists yet).
  2. Build goal seek on the chosen form, then add constraints.

### AI & MCP

- [ ] A model fills a column, and you ask for a query in words and read the SQL before it runs.
  1. Decide first where the key lives and how the cost cap works: a secret per account, and a `spend()`-style token
     budget checked before the call.
  2. Classify, extract, summarize or translate per row, cached by row hash so a re-run costs nothing.
  3. Words to SQL, using the schema that `describe` reads. The SQL is shown for review and never runs unseen.
  4. Document to table. Entity resolution by embedding. A prompt eval sheet is an ordinary sheet.

### Reports & export

- [ ] A report arrives as a PDF, on a schedule.
  1. Decide the PDF writer first.
  2. A print layout: headers, page breaks, a title page.
  3. Prose with live sheet embeds: a dashboard sheet's print render.
  4. Scheduled delivery is an alert with an attachment, bounded by `sendWithinQuota`.

### Permissions & governance

- [ ] You grant a group access, sign in with your company's SSO, and hand your sheets over when you leave.
  1. Decide the schema first: a `grp` table and a `grp_usr` table, with a group arm in `sheet_usr`.
  2. Teams, groups and org accounts.
  3. SSO, SAML and SCIM, through established libraries.
  4. Ownership transfer and offboarding.

### Shop & marketplace

- [ ] You browse the shop by category, preview a dataset before you buy it, and see where it came from.
  1. Category, source and cadence: each needs a column on `sheet`, a backfill for the seeded listings, and the
     deploy-before-constraint order.
  2. A free first-N-rows sample of a `table` listing, on a public route beside `GET /shop`. Decide first: spend the
     seller's budget or not; store the `personal` claim or scan `KEY_SHAPES` only; how to bound `audit` growth from a
     public route.
  3. Collections: curated bundles of related sheets.
  4. "Used by N sheets", and buyer ratings on a route and table of their own.
  5. A manifest per dataset: source URL, license, attribution, cadence, owner, refresh state, a redistribution flag, and
     pinned versions with a changelog.

- [ ] You buy a cleaned public dataset nobody else sells.
  1. Decide first which dataset to build first: the spines (orgs, people, parcels), the local-events table, or a
     crosswalk.
  2. Shared normalization: column names, date formats and code sets, the same across every shop dataset.
  3. Each dataset is a net-http pipeline with its source, license and cadence stated.
  4. Then by demand: government, markets, health, hazard, energy, property and trade, retail and ag, education, legal,
     tech.

- [ ] A seller gets paid, sells a feed by subscription, and sees what sells.
  1. Stripe Connect onboarding and payouts need your Stripe account setup. Today the money stays on the platform.
  2. Tax and VAT, invoices and receipts.
  3. A recurring price, then usage-based metering, then tiers, trials, coupons and refunds.
  4. A seller dashboard: revenue, subscribers, churn and per-sheet analytics.
  5. License enforcement on cancellation. Private and org-only listings, bundles and referral credit.

### Offline & mobile

- [ ] You edit a sheet on a phone, and on a plane.
  1. Decide first which touch gestures replace the keyboard verbs.
  2. Responsive, touch-friendly cell editing and swipe navigation.
  3. IndexedDB-first data sync. Automerge already uses IndexedDB, and `src/sw.js` already opens the shell offline.

### Developer surface

- [ ] A sheet lives in git, and you drive it from a CLI.
  1. The text definition is the workspace zip's `manifest.json` plus each sheet's settings JSON.
  2. A CLI: push and pull CSVs, run queries, tail run logs. Decide first the auth: a `scrapsheets-key` reaches only
     `/sheet/:id`, `/openapi/:id` and `/mcp/:id`. A CSV push suggests `POST /sheet/:id` taking `text/csv` through
     `readImport()`.
  3. Thin typed client SDKs, generated from `GET /openapi/:id`.

- [ ] You run it yourself, and you move your workspace in.
  1. A docker image, for customers who cannot send data anywhere. Decide first whether self-hosting is a product.
  2. Workspace import of the `library.zip` shape. Decide first: the body cap (one exempt path at `WORKSPACE_BYTES_MAX`),
     ids and ref rewriting, feeds paused and alerts snoozed on arrival, `enum:` types, what the zip loses (column keys,
     the arrangement, null against `""`, JSON object cells, secrets, webhooks, shares), and refusing portal, template
     and purchased sheets.
  3. Validate everything, create the N documents, then insert every `sheet` and `sheet_usr` row in one transaction,
     under `assertSheetsQuota` with an `adding` count and `assertRoom` per sheet. On failure, delete the documents.

### Performance & scale

- [ ] A large sheet scrolls, and a million-row join runs.
  1. Virtualized rendering. Decided: past 500 drawn rows, rows take a fixed height and clip wrapped text, and only the
     window plus the focus row render. Design first: the window floor (jsdom reports a zero viewport) and how the focus
     cell stays rendered for `follow`.
  2. Run a query in a Worker, with progress.
  3. Chunked execution on the server past the cap, with the page reading pages of the result.
  4. Document size limits, compaction and history pruning. Columnar storage for bulk data, and cold row archiving.
  5. Per-sheet metering (rows, bytes, compute, fetches), visible before a limit hits.

### Demo gallery

- [ ] You open a demo pipeline for your line of work, and it runs on live data.
  1. Decide first which flagship to connect first. Every unbuilt demo dies at its feed.
  2. Flagships: the restaurant-group weekly P&L, fund 13F drift, the contractor WIP schedule, the municipal budget
     watchdog, and the solo consultant's business in six sheets.
  3. Then by field: finance, health, legal, real estate, operations, commerce, civic, energy, go-to-market.
