# Scrapsheets — todo

> Programmable data OS: every table is a database, every query is a table, every sheet is an API.

A queue, not a history. Each item is a capability someone gains. It lists the decisions it waits on first, then the
steps that build it. When an item is finished, delete it: `claude.md` describes what shipped. A goal that is not code is
a condition in `GET /status`, not an item here.

---

## Query engine

- [ ] You write a money column in a currency other than dollars, and an addition of two currencies is refused.
  1. Decide first:
     - Where the code list lives. `src/sql.mjs` imports nothing, so either it exports a `CURRENCY_CODES` list that a
       test holds equal to `table:currencies`, or it checks only the `^money:[A-Z]{3}$` shape.
     - Whether `usd` becomes a read-only alias now. If it does, it leaves `CANONICAL_TYPES`, and that breaks `pgType`,
       the shop listing column, `/import/csv?types=usd`, the "usd" name in `spec` and about 20 `main_test.ts` fixtures.
     - What `formatNumber` prints for a currency whose symbol it does not know. Today it hard-codes `$`.
  2. Replace every `NUMERIC_TYPES.includes(...)` with a family predicate: `checkColumnTypes` in `src/sql.mjs`, and the
     xlsx, parquet and MCP casts in `main.ts`. Do this before you add the family. Otherwise a `money:EUR` column skips
     the numeric check and exports as a parquet STRING. The `pct` alias already had this bug.
  3. Add a parametric `money:XXX` family to `COLUMN_TYPES`, matched by prefix the way `knownType()` matches `enum:`. The
     three language-boundary copies move with it: add a `` `money:${string}` `` arm to `Type` in `main.ts`, and update
     `columnTypes`, `typeAliases` and `spec` in `Main.elm`. `browser_test.ts` reads only the `| "x"` arms of the union,
     so widen its regex. If you do not, the guard covers less than it seems to.
  4. An addition across two codes is an error that names both, with the fix "join a rate table". `itemType` peels only
     function calls, so this check is a new walk over the select list. Put it in `planQuery()` beside
     `rewriteExtremes()`.
  5. Exact sums: decide between floats with the error bound written down, and a decimal `sum()`. AlaSQL inlines `SUM` as
     an `aggregatorid` (`grep -o 'aggregatorid=="[A-Z_]*"' src/alasql.mjs`), so a decimal sum means a
     `rewriteExtremes`-style rename plus a third type category beside numeric and text. That category cannot be
     `numeric: true`, because `checkColumnTypes` calls `Number()` on it. It cannot be text either, because `min_text`
     compares `"9" > "10"`. A sum that is not rewritten answers a number, not the `undefined` that `checkResultColumns`
     catches, so it also needs a new guard.

- [ ] You see a timestamp in your own timezone, and it means the same instant to every reader.
  1. Decide first: the zone is a per-user setting, and no per-user settings row exists. That means a `schema/db.sql`
     change. The locale item under **Table UX** uses the same row.
  2. Store UTC. Make `date`, `time`, `datetime` and `timestamptz` four types in `COLUMN_TYPES`, with the three
     language-boundary copies.
  3. Render in the viewer's zone, read from that setting, never from the browser's guess. Do the arithmetic through
     `Intl.DateTimeFormat` with `timeZone` (date arithmetic is never hand-rolled). `table:timezones` holds the offsets
     and does not hold the transition dates.

- [ ] You name an expression once and use it in five sheets.
  1. Decide first:
     - The reference syntax. A bare name collides with columns, with `namesIn()` and with `KEYWORD`, so it needs a sigil
       or a call form.
     - The sheet. One option is a `snippet` type. It fails the check constraint, needs `db:apply` in the order
       `claude.md` gives, and touches the `Template`/`Sheet` unions, `docDecoder`, and the two-prefix guards in
       `src/page.mjs` and `completionRef`. The other option is a `using @table:x` clause over an ordinary table of
       `name`/`expr` rows. `using` is already a join keyword in `KEYWORD`, so the parser must require `@` after it and
       strip the clause before `scanRefs` runs.
  2. Add one async `expandSnippets(code, fetch)` in `src/sql.mjs`. Call it before `scanRefs` in `executeSql`
     (`main.ts`), in `runSql` (`src/page.mjs`) and in the `examples_test.ts` replay. Give it the same loader that
     `loadRefs` gets.
  3. Also expand before the scan in `lineage()`, and before the MCP key-scope guard in `query_sheet`. If you do not, a
     snippet body that holds `@other` gets past a key scoped to the snippet sheet.
  4. Give it its own bound, not `MAX_REF_DEPTH`. Report a cycle as the path that closes it, in the shape `checkRefPath`
     uses.
  5. Scrapscript eventually replaces this seam.

---

## Types & validation

- [ ] A cell holds a ratio, a multi-select, a reference, a file, or a quantity with its unit.
  1. Build one type per item, cheapest first. Each type is one `COLUMN_TYPES` row, the three language-boundary copies
     and a `browser_test.ts` pair.
  2. Ratio and basis points: display only, through `formatNumber`, over the stored number. `percentage` already ships.
  3. Multi-select: a `json` array checked against the `enum:` list. An option list from another sheet is an `@ref` that
     the check reads.
  4. Reference: a cell that holds another sheet's key, checked on write. It waits on row identity (see the cell-note
     item).
  5. Attachment: waits on a file store, and none exists. Decide the store first.
  6. Unit of measure: a quantity plus a unit. Adding lbs to kg is refused the same way two currencies are, so it comes
     after the money item.
- [ ] A bad row is refused at every door and kept in a dead-letter sheet, and a sheet states what must be true of it.
  1. Decide first where a constraint lives. The suggestion is the column object in `data[0]`, beside `type`, which is
     where the arrangement already lives. Read it leniently, the way `viewDecoder` reads.
  2. Constraints: not-null, unique, range, regex, allowed values, row-count range, freshness bound, referential
     integrity.
  3. `POST /net/:id`, `POST /sheet/:id`, `write_cells` and a cell edit run the same check that `readImport()` runs,
     through `checkColumnTypes`.
  4. A refused row goes to a computed dead-letter sheet, with the reason, and it can be requeued. Register the sheet in
     the three places `claude.md` names for a computed sheet.
  5. Row provenance: which source and which run wrote the row. For a net sheet this is `net.net_id`.

- [ ] You add a column that is an expression over its own row.
  1. A computed column on a table sheet is an `expr` field on the column object. `src/sql.mjs` evaluates it in both
     hosts, and it is never stored.
  2. Defaults and generated values use the same field: created-at, row hash, sequence.

---

## Table UX

- [ ] You read a number with your locale's separators. Today the separator is always `,` and the point is always `.`.
  1. Add a per-user locale field, in the same settings row as the timezone item, so this starts in `schema/db.sql`. Read
     it where `formatNumber` reads the format, so the cell, the stats row and the totals row change together.
  2. Add a custom mask only after a second real use asks for one. A mask is a parser, and `formatNumber` is the one
     place a number becomes text.

- [ ] You colour a cell by a rule over its whole row.
  1. A rule over the whole row comes after the computed-column item, because that item gives a per-row expression a
     place to live.
- [ ] You group rows and see subtotals without writing SQL.
  1. Add collapsible groups, with subtotals over the rows on screen, the same way the totals row respects the filter. A
     smaller alternative needs neither: one footer `tr` per group under the table, grouped by one chosen column, from
     the fold `columnTotal` already does.
  2. A pivot UI over the same machinery. AlaSQL's `pivot` is correct once `checkPivot()` has checked it.
- [ ] You write a note on a cell and discuss it in a thread.
  1. Decide row identity first. A note belongs to a row, and a row here has no id. `data` is a positional array that
     `rowSplices`, `rowDeletions` and the `move` patch address by index, so an insert above a noted row moves the note
     to its neighbour.
  2. Then decide where the note lives. It cannot be an extra field on the row object: `named()` builds every read and
     export from the columns, so `GET /sheet`, every export and every MCP read drop the field.
  3. The view: a marker on the cell, the note on hover or on a shortcut, and a `DocMsg`, so undo, the viewer refusal and
     sync use the paths already written.
  4. A thread is a list of notes, each with a parent and an author, stored in the same place.

---

## Geo

- [ ] You turn addresses into points, draw them on a map, and find the nearest one.
  1. Decide the geocoder first. It is an outside service with an account and a usage policy. Its key goes in the secret
     store.
  2. A geo column type: point, polygon, address. `point_in_polygon()` and `polygon_area_km2()` already take these
     shapes.
  3. Geocode and reverse geocode, with a match-confidence column. For address dedupe, reuse the soundex and `similarity`
     that `nearDuplicates` uses.
  4. A map chart kind for points and choropleths: one `CHART_KINDS` row and its `kindSpec` copy. Boundary datasets
     (counties, tracts, ZCTAs, districts) are shop tables.
  5. Nearest point without measuring every pair. Answer these questions first:
     - Can one shared `ASOF(...)` from-function take the loaded rows through the one host difference? `from.SHEET` is
       written per host three times: `main.ts`, `src/page.mjs` and `examples_test.ts`.
     - How does `checkJoinRows` learn not to charge a merge?
     - Does nearest-by-distance share any machinery with as-of by time?
  6. Drive time and reprojection. Neither exists, and each one needs an outside service.

---

## Ingest

- [ ] You connect a feed behind OAuth, and a SaaS source without a code change.
  1. The OAuth authorization-code flow with automatic refresh, built on the secret store. The refresh token is a secret,
     and the access token is written back beside it, never into the document.
  2. A static egress IP. Deno Deploy has none, so decide on a proxy before promising one.
  3. A `connector` sheet holds a definition: a name, one set of net-http fields per endpoint, and auth. Applying it
     writes the net-http documents it describes. Nothing new polls, and `pollNetOnce` stays the one poller.
  4. Build Google Sheets, Airtable and Notion first. They are the migration path in, and every connector after them
     reuses their OAuth shape.
  5. Then the rest, ordered by how many demos each one unblocks:
     - accounting: QuickBooks, Xero, NetSuite
     - payments: Stripe, Square, Plaid, Mercury
     - commerce: Shopify, Amazon SP-API
     - CRM: Salesforce, HubSpot
     - support: Zendesk, Intercom
     - project: Jira, Linear, Asana
     - dev: GitHub, Sentry, PagerDuty
     - ads and analytics: Google Ads, Meta, GA4, Segment
     - HR: Gusto, Rippling, Greenhouse
     - comms: Slack, Gmail, Google Calendar, Twilio
     - vertical: Procore, Toast, Epic/FHIR, Clio
  6. Bidirectional sync reads the same definition in the other direction, and waits on the actions item.

- [ ] You poll an XLSX or a PDF and get rows, not one cell.
  1. XLSX is blocked by the pin. `npm:xlsx@0.18.5` is the last SheetJS version on npm (`npm view xlsx dist-tags`), and
     its read path is never used here. Decide first: a newer SheetJS from the vendor's registry, or a different reader.
     Do not call `XLSX.read` on the pinned version.
  2. PDF: a PDF whose tables a converter turns into HTML reads through `htmlDelimited()`. A scanned PDF does not.

- [ ] You post a form, forward an email or send a text, and it becomes a row.
  1. Decide the inbound mail and SMS providers first. Each one is an account, and its webhook signature scheme is part
     of the choice.
  2. A form generated from the column types, served per net-hook sheet, with a thank-you page. It runs the check from
     the bad-row item. A form cannot sign, so it needs `rateLimit(callerIp)` plus a spend.
  3. A filter: a predicate on the sheet drops an event before it is stored.
  4. Response templating: the sheet picks the status and the body, so it can answer a callback synchronously.
  5. Email-in and SMS-in, one address per sheet, on the same path.
  6. File uploads wait on the attachment type.

---

## Codex — databases

- [ ] You connect the database you actually have, browse its tables, and read only the rows you filter.
  1. A codex query path. Today the only statements are the read-only session, `information_schema` and the preview's
     `limit`. Push down only a where clause that is provably safe.
  2. A write grant per connection, off by default, with row and column masking over that path.
  3. Incremental sync by a watermark column. CDC or logical replication after that.
  4. More engines, one at a time, each with its own driver and its own `cannotConnect()` mapping: MySQL, SQLite, SQL
     Server, DuckDB, BigQuery, Snowflake, Redshift, ClickHouse, MongoDB, Athena.
  5. SSH tunnel and TLS options.

---

## Alerts

- [ ] An alert reaches your phone.
  1. Web push first. It needs no vendor account: VAPID keys are secrets, each user has a subscription, and `src/sw.js`
     receives the push. Use an established library for the signing.
  2. SMS needs a carrier account. Choose one before this step becomes its own item. It is a sender beside
     `sendAlertUrl`, picked by the destination.

---

## Actions & write-back

- [ ] A sheet sends something, you see what it will send first, and a failure is retried and kept.
  1. Dry run first. An action sheet shows exactly what it would send, one row per request, and sends nothing.
  2. An HTTP action through `safeFetch`, with a templated body that resolves `{{secret:name}}` the way net-http does.
  3. An approval gate before a batch. A blast-radius cap refuses past N recipients and names the count, beside
     `sendWithinQuota`.
  4. Per row, exactly once: an idempotency key per row (its hash), a queue with bounded retries and backoff, and the
     dead-letter sheet from the bad-row item.
  5. Built-in actions: email (Resend ships), Slack (`sendAlertUrl` ships), a calendar event, and a codex write, which
     waits on the write grant.
  6. Who ran what against which rows goes in `audit` through `record()`. There is no second log.

---

## Stats & modeling

- [ ] You solve for an input with goal seek or a constrained optimizer.
  1. Decide first how the objective is named. A UDF gets evaluated values, and a window gets rows the engine already
     returned, so neither can re-evaluate an expression. The candidates are a snippet ref, a query re-run with a bound
     parameter, or a Scrapscript lambda (none exists yet).
  2. The bundled sweep demo already shows the answer by search. Build goal seek on the chosen form, then add
     constraints.

---

## AI & MCP

- [ ] A model fills a column, and you ask for a query in words and read the SQL before it runs.
  1. Decide first where the key lives and how the cost cap works. The key is a secret per account. The cap is a
     `spend()`-style budget on tokens, checked before the call.
  2. Classify, extract, summarize or translate per row, cached by row hash so a re-run costs nothing.
  3. Words to SQL, using the schema that `describe` reads. The SQL is shown for review and never runs unseen.
  4. Document to table (PDF, invoice, contract). Entity resolution by embedding.
  5. A prompt eval sheet, holding test cases and scores, is an ordinary sheet.

---

## Reports & export

- [ ] A report arrives as a PDF, on a schedule.
  1. Decide the PDF writer first, and say which one and why.
  2. A print layout: headers, page breaks, a title page.
  3. Prose with live sheet embeds. A dashboard sheet already embeds sheets, so this is its print render.
  4. Scheduled delivery is an alert with an attachment, bounded by `sendWithinQuota`.

---

## Permissions & governance

- [ ] You grant a group access, sign in with your company's SSO, and hand your sheets over when you leave.
  1. Decide the schema first: a `grp` table and a `grp_usr` table, with a group arm in `sheet_usr`. This is a
     `schema/db.sql` change, applied in the order `claude.md` gives.
  2. Teams, groups and org accounts.
  3. SSO, SAML and SCIM, through established libraries. Auth is never hand-rolled.
  4. Ownership transfer and offboarding.

---

## Shop & marketplace

- [ ] You browse the shop by category, preview a dataset before you buy it, and see where it came from.
  1. Category, source and cadence. None of the three is a field anywhere. Each needs a column on `sheet`, a backfill for
     the seeded listings, and the deploy-before-constraint order.
  2. A free first-N-rows sample of a `table` listing, on a public route beside `GET /shop`. It reads through `docData`
     and `named()`, re-scans only the rows it returns, refuses with a 409 that names nothing, and records audit with a
     null `usr_id` and `via: "public"`. Decide first:
     - Spend or not. Spending the seller's budget lets anybody starve the seller's sheet. Not spending breaks the
       one-budget invariant.
     - The `personal` claim. `POST /sell/:id` does not store it, so a `personal: true` listing would answer 409 to every
       sample. Store it (a schema column), or scan `KEY_SHAPES` only.
     - Audit growth. `audit` is never trimmed, and a public route writes a row on every hit.
  3. Collections: curated bundles of related sheets.
  4. "Used by N sheets", and buyer ratings on a route and table of their own. `POST /shop/:sell_id/review` is the
     operator's keep-or-takedown.
  5. A manifest per dataset: source URL, license, attribution, cadence, owner and refresh state. A redistribution flag
     is checked at `POST /sell/:id`. Publishing is versioned: a buyer pins a version, and a changelog explains each
     release.

- [ ] You buy a cleaned public dataset nobody else sells.
  1. Shared normalization first: column names, date formats and code sets, the same across every shop dataset.
  2. Each dataset is a net-http pipeline with its source, license and cadence stated. That is the dogfood.
  3. Build first: the spines (orgs, people, parcels, colors, songs) and the giant local-events table. Then the geometry
     and identifier crosswalks (ZIP↔county↔CBSA↔tract, NAICS/SIC/GICS detail, SOC and O*NET detail, timezone
     transitions, FX rates).
  4. Then by demand:
     - government: BLS, BEA, FRED, Treasury, Census, Federal Register, USAspending, SAM, FEC, IRS 990, sanctions
     - markets: EDGAR/XBRL, prices with splits, options, futures, crypto, FDIC call reports
     - health: CMS, NPI, price transparency, FDA, trials, ICD-10, HCPCS, LOINC, RxNorm (CPT needs a license)
     - hazard: NOAA, FEMA, wildfire, air, water, quakes
     - energy: EIA, ISO LMP, EPA
     - property and trade: assessor rolls, permits, HTS, FMCSA, port throughput
     - retail and ag: USDA, NASS, inspections
     - education: IPEDS, Scorecard
     - legal: dockets, patents, UCC
     - tech: NVD, OSV, KEV, package registries, cloud SKU prices

- [ ] A seller gets paid, sells a feed by subscription, and sees what sells.
  1. Stripe Connect onboarding and payouts. Today the money lands on the platform account and stays there.
  2. Tax and VAT, invoices and receipts.
  3. A recurring price for a dataset that keeps updating, then usage-based metering (per query, row or call), then free
     and paid tiers, trials, coupons and refunds.
  4. A seller dashboard: revenue, subscribers, churn and per-sheet analytics.
  5. License enforcement on cancellation. Private and org-only listings, bundles and referral credit.

---

## Collaboration

- [ ] You see who else is in the sheet.
  1. Decide first what a peer broadcasts. `model.auth.email` shows the editor's address to every viewer of a shared
     sheet, and a client can claim any name.
  2. Fix the harness first. `fakeHandle.on` ignores the event name and has no `broadcast`, so a `Presence` receives
     `change` payloads and `start()` throws in every glue test. Teach it event names and add `broadcast`. Two
     `realRepo`s built with `network: []` cannot reach each other.
  3. Check that `main.ts` relays an ephemeral frame. Only `carriesChanges` has been read.
  4. The vendored bundle exports `Presence` (`start({ initialState, heartbeatMs, peerTtlMs })`). Start it in
     `selectDoc`, call `stop()` where `watched` is cleared, add one port pair, bound `model.peers`, and show chips in
     `viewToolbar`.
  5. Cursor presence after that, throttled to 200ms or more. Every frame goes through the server's serialized sync
     queue.

- [ ] You go back to a past version of a sheet.
  1. A visual diff between two versions. `diff` answers automerge patches, and rendering them as a cell-level diff is
     its own design.
  2. Named snapshots, then rollback, which is a write.
  3. A conflict view, for the merges automerge cannot decide.

---

## Offline & mobile

- [ ] You edit a sheet on a phone, and on a plane.
  1. Responsive, touch-friendly cell editing and swipe navigation.
  2. IndexedDB-first data sync. Automerge already uses IndexedDB, and `src/sw.js` already opens the shell offline, so
     this is the data half.

---

## Developer surface

- [ ] A sheet lives in git, and you drive it from a CLI.
  1. The text definition is the workspace zip's `manifest.json` plus each sheet's settings JSON.
  2. A CLI: push and pull CSVs, run queries, tail run logs.
  3. Thin typed client SDKs, generated from `GET /openapi/:id`.

- [ ] You change a pipeline without breaking the live one.
  1. A staging copy: fork the sheet, change the query, review the diff, promote.
  2. Branch and merge a sheet. Automerge makes this possible.
  3. A sandbox: fake webhook deliveries and dry-run schedules.

- [ ] You run it yourself, and you move your workspace in.
  1. A docker image, for customers who cannot send data anywhere.
  2. Workspace import. N documents and N `sheet` and `sheet_usr` rows are written atomically. `assertSheetsQuota` runs
     over the whole batch and `assertRoom` runs per sheet. Decide what an `@ref` to a doc_id that no longer exists
     means.

---

## Navigation & workspace UX

- [ ] You find a sheet by what is in it.
  1. Global search over sheet names, column names and cell contents. Semantic search after that.

---

## Performance & scale

- [ ] A large sheet scrolls, a forty-sheet chain does not re-run on every keystroke, and a million-row join runs.
  1. Virtualized rendering. Today every row renders.
  2. A nested `@query:` answer already comes from the page cache in `sheets(..., heads)` until a document it read
     changes heads. What is left: an open query refreshes when a dependency's heads change with no keystroke, and only
     what is downstream of the changed document runs again. Subscribe to `change` on the handle of each document the
     open query read, drop only the cache keys that read it, and call `runQuery` once. Today nothing subscribes: the
     next keystroke or reopen reads the new heads.
  3. Run a query in a Worker, with progress. The engine cannot be preempted, which is why `MAX_QUERY_ROWS` is the real
     guard.
  4. Chunked execution on the server past the cap, with the page reading pages of the result. `checkQueryRows()` keeps
     its message and adds the route.
  5. Document size limits, compaction and history pruning. Columnar storage for bulk data, and cold row archiving.
  6. Per-sheet metering (rows, bytes, compute, fetches), visible before a limit hits.

---

## Demo gallery

- [ ] You open a demo pipeline for your line of work, and it runs on live data.
  1. `src/examples.mjs` is the index. `examples_test.ts` runs the sheets tagged `demo`. Every unbuilt demo dies at the
     same place: the feed, because a seeded table is a feed nobody has connected. Build a demo as one item that names
     the feed it needs.
  2. Flagships first: the restaurant-group weekly P&L, fund 13F drift, the contractor WIP schedule, the municipal budget
     watchdog, and the solo consultant's business in six sheets. Four of the five already ship their query half.
  3. Then by field:
     - finance: three-statement forecast, covenant monitor, AR aging, FX exposure, ASC 606
     - health: price transparency, PS&R reconciliation
     - legal: docket watch
     - real estate: property tax appeal, deal underwriting
     - operations: OEE, landed cost, safety stock
     - commerce: settlement reconciliation, recipe costing
     - civic: budget burn, permit backlog
     - energy: solar performance ratio, utility bill audit
     - go-to-market: blended CAC, pipeline hygiene, DORA metrics
