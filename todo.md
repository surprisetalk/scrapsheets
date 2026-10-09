<!-- TODO: maybe restart this project as something more whimsical and personal like kinopio? -->

# Scrapsheets — todo

> Programmable data OS: every table is a database, every query is a table, every sheet is an API.

A queue, not a history. Each item is a capability someone gains. It lists the decisions it waits on first, then the
steps that build it. When an item is finished, delete it: `claude.md` describes what shipped. A goal that is not code is
a condition in `GET /status`, not an item here.

---

## Status

- [ ] snitch's daily report shows scrapsheets red while a usage goal is below 1.0, and the 15-minute alarm still pages
      only on an outage.
  1. Decide first: `status.yml` and snitch both run `deno task status`, and they want different answers. The suggestion:
     the workflow's last step becomes `curl -sS --fail-with-body --max-time 30 https://api.sheets.scrap.land/status`, so
     the HTTP code stays the paging verdict. Tradeoff: the workflow loses the "at least 13 conditions" guard, so an
     empty 200 no longer pages. A `jq 'length >= 13'` step keeps it.
  2. The `status` task in `deno.json` exits nonzero when any condition is below 1.0, `REPORTED_ONLY` included. It still
     prints every grade first, and it still fails on an answer that is not 2xx.
  3. Rewrite the paragraph under the commands in `readme.md`, the `deno task status` and `REPORTED_ONLY` lines in
     `claude.md`, and the step comment in `status.yml`. The task fails on every condition. The 503 from `GET /status`
     pages.
  4. `GET /status` and `REPORTED_ONLY` stay as they are.
  - Why: on 2026-10-09 the task exited 0 with five usage goals at 0, so snitch listed scrapsheets as passing. snitch
    mails one report a day, so a red goal there is not an alarm that gets muted.

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

- [ ] You see timestamps in your own zone and numbers with your locale's separators, and a timestamp means the same
      instant to every reader.
  1. Decide first: the zone and the locale are per-user settings, and no per-user settings row exists. That means a
     `schema/db.sql` change, applied in the order `claude.md` gives.
  2. Store UTC. Make `date`, `time`, `datetime` and `timestamptz` four types in `COLUMN_TYPES`, with the three
     language-boundary copies.
  3. Render in the viewer's zone, read from that setting, never from the browser's guess. Do the arithmetic through
     `Intl.DateTimeFormat` with `timeZone` (date arithmetic is never hand-rolled). `table:timezones` holds the offsets
     and does not hold the transition dates.
  4. Read the locale where `formatNumber` reads the format, so the cell, the stats row and the totals row change
     together. Today the separator is always `,` and the point is always `.`.
  5. Add a custom mask only after a second real use asks for one. A mask is a parser, and `formatNumber` is the one
     place a number becomes text.

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

- [ ] A cell holds a multi-select, a reference, a file, or a quantity with its unit.
  1. Build one type per item, cheapest first. Each type is one `COLUMN_TYPES` row and the three language-boundary
     copies. `browser_test.ts` reads each copy whole, so it needs no new code.
  2. Multi-select: a JSON array checked against an option list. Decide first the spelling (`multi:a,b` is the
     suggestion) and where it is refused (every write door, beside the constraints item's `enum:` check). Every grid
     write stores a string today (`SheetWrite` encodes `E.string`), and `POST /sheet/:id` refuses an array value. An
     option list from another sheet is an `@ref`, and it is not cheap: `checkColumnTypes` is synchronous and cannot load
     a sheet, so it needs `loadRefs`, lineage, the MCP key-scope guard and a port.
  3. Reference: a cell that holds another sheet's key, checked on write. It waits on row identity (see the cell-note
     item).
  4. Attachment: waits on a file store, and none exists. Decide the store first.
  5. Unit of measure: a quantity plus a unit. Adding lbs to kg is refused the same way two currencies are, so it comes
     after the money item.
- [ ] A bad row is refused at every door and kept in a dead-letter sheet, and a sheet states what must be true of it.
  1. Decide first:
     - The shape. The suggestion is one nested `check` object on the column in `data[0]`, beside `type`, read leniently
       the way `viewDecoder` reads. One reader, and no collision with the arrangement's fields.
     - A constraint the current rows already break. The suggestion is to refuse adding it, in the page.
     - Regex. Defer it, or allow a restricted syntax with length caps. Nothing bounds a catastrophic pattern today:
       `MAX_QUERY_MS` bounds how long the caller waits, not how long the event loop burns.
     - The dead letter's store. `net` is wrong: its check constraint admits no table id, `trimNet` keeps `NET_KEEP` rows
       across every account's rejects, and the error log never stores values. `audit` is wrong: it is never trimmed, and
       a refused request writes nothing. A new `reject` table is a `schema/db.sql` change.
  2. Constraints: not-null, unique, range, regex, allowed values, row-count range, freshness bound, referential
     integrity. Allowed values are the `enum:` options. Their membership is checked nowhere today: not in
     `checkColumnTypes`, not at `POST /sheet/:id`, not in `write_cells`.
  3. A `checkConstraints` in `src/sql.mjs`, beside `checkColumnTypes`, runs at `POST /sheet/:id` and in `write_cells`
     over the rows as they will be after the write. The glue's `changeDoc` runs the same function for a cell edit. That
     check is advisory: the server cannot refuse a socket edit, because the editor's copy already holds the change and a
     dropped frame wedges sync. `readImport()` always makes a new sheet, so it has nothing to enforce. `POST /net/:id`
     is out of scope: a net sheet has no columns.
  4. A refused batch stays all-or-nothing. It goes to a computed dead-letter sheet with the reason. A requeue only
     appends, because rows are positional and an overwrite's target may have moved. Register the sheet in the three
     places `claude.md` names for a computed sheet.
  5. Row provenance: which source and which run wrote the row. For a net sheet this is `net.net_id`.

- [ ] You add a column that is an expression over its own row, and colour a cell by a rule over that row.
  1. Decide first:
     - What the grid shows. Elm decodes cells straight from the document, so a computed cell renders blank. Either the
       glue computes on each `docChanged` and overlays read-only values before sort, filter, shade and stats read them,
       or the grid shows the expression's text.
     - A row whose expression throws: refuse the read and name the row, or a null cell.
     - The type: declared on the column and checked, or inferred through `selectTypes`.
  2. A computed column is an `expr` field on the column object. `src/sql.mjs` evaluates it in both hosts through AlaSQL,
     one `select (<expr>) as [v] from ?` per computed column, zipped back by row index. It is never stored.
  3. An expression refuses refs, subqueries and `SHEET(`, `AGGREGATES` calls, `over`, `?` parameters, another computed
     column, and `VOLATILE` calls (the page's `http()` would fetch once per row per render). `namesIn()` checks that
     every name is a stored column of the same sheet, with `nearest()`. Its bracket pattern admits identifier characters
     only.
  4. Coerce through `checkColumnTypes` before evaluating, or `"3" + "2"` concatenates. `GET /sheet/:id` does not run
     that check today, so adding it makes a sheet with one bad numeric cell refuse `GET`.
  5. Every read computes: `sheet()` into `named()` (`GET /sheet/:id`, every export, every MCP read, `library.zip`), and
     `loadRefs` in both hosts, which gains an engine argument in `main.ts`, `src/page.mjs` and the `examples_test.ts`
     replay. A server query must not compute twice. `GET /sheet/:id` gains the `MAX_QUERY_MS` race, and a cap on
     computed columns per sheet and on expression length.
  6. Every write refuses a computed column: `POST /sheet/:id` drops it from the required names; `write_cells`, paste,
     fill-down, the cleaning verbs and split refuse it; OpenAPI marks it `readOnly`. `atRisk` warns when a rename or
     delete takes a column an expression names.
  7. A colour rule over the whole row is the arrangement's `shade`, driven by a per-row expression, after step 6.
  8. Defaults and generated values (created-at, row hash, sequence) are their own item on the same field.

---

## Table UX

- [ ] You group rows and see subtotals without writing SQL.
  1. Decide first:
     - Where the chosen column lives. A column-level `group` field in the arrangement takes the `shade` path
       (`colViewFields`, `viewOf`, `pruneView`, `viewPatches`, `onScreenView`) and is written by `arrange`. Model-only
       state is half the code and is lost on reload. A sheet-level field cannot live on a table: its patches are rooted
       at `data`.
     - A cap on groups, with an "N more" row. A high-cardinality column is one footer row per row.
     - Tables only. A query has no totals row today.
  2. One footer `tr` per group under the table, in the `Tab` branch of `viewTableFooter`, from the fold `columnTotal`
     already does, over the drawn rows the totals row reads. A "subtotal by this column" button in the column panel.
  3. Collapsible groups after that.
  4. A pivot UI over the same machinery. AlaSQL's `pivot` is correct once `checkPivot()` has checked it.
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
  1. A filtered preview, decided: owners and editors only, until masking exists. `GET /codex/:id/preview` takes up to
     five AND-ed `where=<column>.<op>.<value>`; a codex column never holds a dot, because the route already refuses one.
     The ops are `eq neq lt lte gt gte isnull notnull`, plus `like ilike` on text. The column passes by exact membership
     in `codexTables()`, the op comes from the list, and the value is a bound parameter. A value that will not cast
     (SQLSTATE class 22) or an op its type lacks (class 42) is a 400 caught inside `read`. Never the 502 and the failed
     `codexRun` row, which `POLL_OK` grades. The page UI comes after, then a `@codex-db:` query path.
  2. A write grant per connection, off by default, with row and column masking over that path.
  3. Incremental sync by a watermark column. CDC or logical replication after that.
  4. More engines, one at a time, each with its own driver and its own `cannotConnect()` mapping: MySQL, SQLite, SQL
     Server, DuckDB, BigQuery, Snowflake, Redshift, ClickHouse, MongoDB, Athena.
  5. SSH tunnel and TLS options.

---

## Alerts

- [ ] An alert reaches your phone.
  1. Web push first. It needs no vendor account. Decide first the signing library, and spike it:
     - `npm:web-push` has a reported Deno failure in `generateRequestDetails`.
     - `jsr:@negrel/webpush` sends through the global `fetch`, around `safeFetch`.
     - `web-push-neo` exposes the request details, so `safeFetch` can send them. This is the suggestion.
  2. Subscriptions are a new `schema/db.sql` table (`usr_id`, unique `endpoint`, `p256dh`, `auth`, `failures`), pruned
     on a 404 or 410. The endpoint is a URL the browser supplies: allowlist the push services' hosts as well as
     `safeFetch`.
  3. VAPID keys are server environment variables. Missing keys follow the `RESEND_API_KEY` pattern: nothing was sent.
     `to: push` means the alert owner's devices, and the run row writes `push`, never an endpoint. The payload is the
     headline and a link. `sendWithinQuota` counts one delivery per run.
  4. `src/sw.js` gains `push` and `notificationclick`. `browser_test.ts` lints it (`clients` may join
     `BROWSER_GLOBALS`), and the `sync_test.ts` worker tests need `registration.showNotification` on their `self`.
  5. SMS needs a carrier account. Choose one before this step becomes its own item. It is a sender beside
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
  1. Decide first what a peer broadcasts. The suggestion: nothing identifying now (a per-tab colour and a count), and an
     identity the server stamps on each frame later. `model.auth.email` would show the editor's address to every viewer,
     public readers included. A `usr.name` is claimed by the client, and nothing writes it yet.
  2. Fix the harness first. `fakeHandle.on` ignores the event name and has no `broadcast`, so a `Presence` receives
     `change` payloads and `start()` throws in every glue test. Teach it event names and add `broadcast`. Two
     `realRepo`s built with `network: []` cannot reach each other.
  3. `main.ts` relays an ephemeral frame, by reading: `carriesChanges` passes it, and automerge-repo relays it to every
     peer on the document with no size or rate limit. Add a byte cap and a rate cap on `type === "ephemeral"` in the
     socket handler, with a step in `main_test.ts`'s real-socket block.
  4. The vendored bundle exports `Presence` (`start({ initialState, heartbeatMs, peerTtlMs })`). Start it in
     `selectDoc`, call `stop()` where `watched` is cleared, add one port pair, bound `model.peers`, and show chips in
     `viewToolbar`.
  5. Cursor presence after that, throttled to 200ms or more. Every frame goes through the server's serialized sync
     queue.

- [ ] You go back to a past version of a sheet.
  1. Rollback first, rows only. A pure `rollbackPatches(now, past)` in `src/page.mjs` emits a `set` or `del` per cell
     that differs (path `[y, key]`) and one tail `splice` for the row count. Its mirror is the undo entry, so one
     automerge change goes through `changeDoc` and Ctrl/⌘+Z takes it back. It refuses when the column keys differ, and
     past a named patch bound. Writing `data[0]` back would rewrite declared types and the arrangement, and would skip
     the rename and delete warning.
  2. A visual diff of the picked version against now, from the same comparison.
  3. Named snapshots. Decide first where the names live. A root `snapshots` list needs a glue write path outside
     `d.data`; the other places are this browser's store and a server table.
  4. A conflict view, for the merges automerge cannot decide.

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
  2. A CLI: push and pull CSVs, run queries, tail run logs. Decide first the auth. A `scrapsheets-key` reaches only
     `/sheet/:id`, `/openapi/:id` and `/mcp/:id`. Everything else needs a JWT from `POST /login`. A CSV push suggests
     `POST /sheet/:id` taking `text/csv` through `readImport()`, so one CSV reader stays. Test it with a stubbed `fetch`
     in its own file.
  3. Thin typed client SDKs, generated from `GET /openapi/:id`.

- [ ] You change a pipeline without breaking the live one.
  1. A staging copy: fork the sheet, change the query, review the diff, promote. `forkDoc` is `repo.create` of a copy,
     so the fork shares no history and `merge` cannot promote it. Promote copies `code` back to the source. `forkDoc`
     records `forked_code` and `forked_heads`, so promote warns when the source changed since the fork. A bundled source
     is refused. The rename and delete machinery (`lineageFor`, `dependents`) warns when the new query drops a column
     another sheet reads.
  2. Branch and merge a sheet through automerge's `clone` and `merge`. This also merges the data and the arrangement.
  3. A sandbox: fake webhook deliveries and dry-run schedules.

- [ ] You run it yourself, and you move your workspace in.
  1. A docker image, for customers who cannot send data anywhere.
  2. Workspace import of the `library.zip` shape. Decide first:
     - The body cap. `bodyLimit` caps every body at `BODY_CAP`, so no table larger than that comes back in through any
       door. The suggestion is one exempt path, capped at `WORKSPACE_BYTES_MAX`.
     - Ids. `automerge.create` mints them, so imported sheets get new ones. Refs are rewritten in query and alert
       `code`, chart `source` and dashboard `tiles`, by a scanner shaped like `scanRefs`. A ref to a sheet the zip does
       not hold is imported and listed in the answer.
     - On arrival: feeds paused, alerts snoozed. A paused alert that never ran counts as overdue after two hours, and
       past `OVERDUE_MAX` that pages the operator.
     - Types. `importTypes` admits only `CANONICAL_TYPES`, which refuses `enum:`. The suggestion is any `knownType()`
       spelling, as written.
     - What the zip loses: column keys, the arrangement, null against `""`, JSON object cells (`csvCell` writes
       `[object Object]`), secrets, webhooks and shares. Extend the manifest first, or import what is there.
     - Refuse portal, template and purchased sheets. Importing a purchase would launder its license.
  3. Validate everything, create the N documents, then insert every `sheet` and `sheet_usr` row in one transaction.
     `assertSheetsQuota` runs on that transaction with an `adding` count, and `assertRoom` runs per sheet. On failure,
     delete the documents already created.

---

## Navigation & workspace UX

- [ ] You search the library and match only what you see: "true", "rows" or "null" does not find every sheet.
  1. `matchesSearch` reads every cell through the lenient `string`, so the star's `{id, on}`, the thumb's
     `{kind, cols, rows, spark}` and a `NULL` match. Add one fact to `spec`: whether a type's cell draws its text (not
     `Star`, `Thumb`, `Trash`, `Restore` or `Delete`). Pass the columns to `matchesSearch`, and read only the cells
     whose type draws text. Every table then searches what it draws.
  2. Case folding goes through JS `toLowerCase`, which turns "İ" into "i" plus a combining dot, so "istanbul" misses
     "İstanbul". A fix needs normalization, which Elm lacks. Add it only after a real search breaks.

- [ ] You open settings on a sheet you only read, and it shows what you can do there, not an error.
  1. The settings panel asks for the member list on open. A public reader or a buyer gets a 403 from
     `GET /library/:id/share`, and the banner shows it. Decide first how the page learns that it holds no share: from
     that 403 (the panel hides the member list and the share form, and says why in place), or from a role the server
     sends with the document.

- [ ] You find a sheet by what is in it.
  1. Column names first: a bounded `names` list in `docThumb` (`src/page.mjs`). `matchesSearch` already reads thumb
     values, so library rows match, and the palette's `matches` list adds it. A stored thumb gains names when its sheet
     is next opened.
  2. Cell contents need every document. Decide first whether that is a server route that spends each sheet's budget.
  3. Semantic search after that.

---

## Performance & scale

- [ ] A large sheet scrolls, and a million-row join runs.
  1. Virtualized rendering. Today every row renders. Decide first:
     - The row height. Text wraps without limit today; only child media is capped.
     - The threshold. Tests expect more than 190 rendered rows.
     - A window floor. jsdom reports a zero viewport.
     - The focus cell. `follow` reads the cells `followIds` marks by id, so a focus row outside the window reads as an
       undrawn row and never scrolls into view. Keep it rendered or measured.
  2. Run a query in a Worker, with progress. The engine cannot be preempted, which is why `MAX_QUERY_ROWS` is the real
     guard.
  3. Chunked execution on the server past the cap, with the page reading pages of the result. `checkQueryRows()` keeps
     its message and adds the route.
  4. Document size limits, compaction and history pruning. Columnar storage for bulk data, and cold row archiving.
  5. Per-sheet metering (rows, bytes, compute, fetches), visible before a limit hits.

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
