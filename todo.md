# Scrapsheets — todo

> Programmable data OS: every table is a database, every query is a table, every sheet is an API.

A queue, not a history. Each item is a capability someone gains, then the steps that build it. When an item is finished,
delete it: `claude.md` describes what shipped. A goal that is not code is a condition in `GET /status`, not an item
here.

---

## Ready

Decision: scrapsheets is in the Platforms bucket (you, 31 Oct). Each feature builds its recommended design, with
`REVIEW:` notes for the owner.

- [ ] You branch a query, edit it freely, and merge it back, and you promote alert, chart and net-http sheets too.
  1. Branch with automerge `clone` of the source handle's document: store the clone as the fork's own document, so
     `merge` can carry both histories. A bundled source still takes the JSON copy.
  2. Merge with `Automerge.merge(source, fork)` in `promoteDoc`; replace the `data[0].code` write and the `forked_heads`
     key check. Show the keys that conflicted before the write.
  3. Make `forkDoc` and `promoteDoc` accept `alert`, `chart` and `net-http`: drop the `query:` prefix tests, and write
     `forked_code` as the sheet's own text field.
  4. Teach `promoteColumns` the columns of an alert (its query's), a chart (its source's) and a net-http feed (its
     stored rows' names).
  5. Test each type with a real-automerge fork, as the query test in `sync_test.ts` does.
  6. Skip the lineage warning for a fork of a bundled query: the glue refuses that promote anyway.

- [ ] You resolve the merges automerge cannot decide in a conflict view.
  1. Design first: which conflicts `getConflicts` reports for a cell, and how the history modal shows them.

- [ ] You pivot rows by one column and columns by another, in the UI.
  1. A pivot panel (row, column and value columns) over the same `subtotalGroups` fold. AlaSQL's `pivot` is correct once
     `checkPivot()` has checked it.

- [ ] You open a demo pipeline for your line of work, and it runs on live data.
  1. Decided (you, 6 Nov): the municipal budget watchdog first, on a Socrata open-data portal (public JSON, no key). The
     static half ships: `cityCheckbook` and the budget-burn queries in `src/examples.mjs`.
  2. A bundled sheet cannot poll, since `pollNetOnce` reads only `sheet` rows. Have `seed()` write one public `net-http`
     sheet, owned by the sentinel, polling a Socrata `resource/<id>.json` with `page_by: offset` and `mode: replace`.
     Pick a dataset with monthly appropriation and spend by department.
  3. Point the demo queries at that sheet, and check how the page resolves a public `@net-http:` ref (`sheets()`'s
     loader); the server resolves it through `sheet()`.
  4. The feed's liveness condition in `GET /status` then covers the demo with no new condition.
  5. Then the restaurant-group weekly P&L, fund 13F drift, the contractor WIP schedule, and the solo consultant's
     business in six sheets.
  6. Then by field: finance, health, legal, real estate, operations, commerce, civic, energy, go-to-market.

- [ ] You see timestamps in your own zone and numbers with your locale's separators, and a timestamp means the same
      instant to every reader.
  1. Decided (you, 6 Nov): two nullable columns, `usr.zone` (IANA name) and `usr.locale` (BCP 47). A null reads as UTC
     and `en-US`. Alternative: a `usr_setting` table; it adds a noun before a third setting exists. You run `db:apply`
     before the deploy.
  2. Store UTC. Make `date`, `time`, `datetime` and `timestamptz` four types in `COLUMN_TYPES`, with the three
     language-boundary copies.
  3. Render in the viewer's zone, read from that setting, never from the browser's guess. Do the arithmetic through
     `Intl.DateTimeFormat` with `timeZone`. `table:timezones` holds the offsets and does not hold the transition dates.
  4. Read the locale where `formatNumber` reads the format, so the cell, the stats row and the totals row change
     together. Today the separator is always `,` and the point is always `.`.
  5. Add a custom mask only after a second real use asks for one.

---

## Blocked on you

### Waits on a real use

Each entry below waits for a sheet that needs it. Tell me when one does.

- [ ] You write a snippet that takes arguments, like `{{f(x)}}`.
  1. Parse `{{name(a, b)}}` in `expand()`, split on top-level commas outside strings, bounded by `SNIPPET_ARGS_MAX`.
  2. A third column `args` names the parameters; a call with another count is refused, naming both counts.
  3. Substitute each parameter as a whole word outside strings, each argument in parentheses, before the recursive
     expand. Completion inserts `name(` for a snippet with `args`.

- [ ] A multi column takes its options from another sheet.
  1. `multi:@table:<id>.<column>`; the page draws that column's distinct values as the checkboxes.
  2. Decide first how `checkColumnTypes` learns the options on the server without a read per write.

- [ ] A table refuses a write that breaks a rule beyond one cell: a row-count range, a freshness bound, a reference.
  1. A table's `data[0]` is its column array, so a sheet-level check needs a root key, `check`, beside `type` and
     `data`. A row-count min cannot hold, since deletes arrive over sync, which refuses nothing; a max repeats
     `assertRoom` per sheet. Build it only for a sheet that needs one.
  2. Freshness bound and referential integrity wait on row identity (see the cell-note entry).

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
  1. XLSX: SheetJS 0.20.3 carries no known advisory, so `XLSX.read` is now allowed. Decide first how a workbook with
     many sheets picks one, like `rows_path`.
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
  1. Waits (you, 6 Nov): build only once you can set VAPID keys in Deno Deploy, so status does not sit red. Web push
     first. Decide the signing library, and spike it: `npm:web-push` has a reported Deno failure; `jsr:@negrel/webpush`
     sends around `safeFetch`; `web-push-neo` exposes the request details so `safeFetch` can send them (the suggestion).
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
