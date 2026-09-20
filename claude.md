# CLAUDE.md

Guidance for Claude Code working in this repository.

This file is the **map**: what each file is, what each command does, what the schema holds, and the invariants a change
must not break. It is deliberately not the reasons. Every "why is it written this way" lives in the comment above the
code it is about — three copies of a reason means two of them are eventually wrong about code that changed. When you
need the reason, read the comment.

## Project Overview

Scrapsheets is a programmable data OS shaped like a spreadsheet: every table is a queryable database, every query result
is a shareable table, every sheet is an API.

- **Backend**: Deno + Hono (`main.ts`)
- **Frontend**: Elm single-page app (`src/Main.elm`) glued by `src/index.html`
- **Database**: PostgreSQL, declarative schema in `schema/db.sql`
- **Real-time**: Automerge CRDT (https://automerge.org/llms-full.txt), documents in `data/automerge/`

## Files

| Path               | What it is                                                                                  |
| ------------------ | ------------------------------------------------------------------------------------------- |
| `main.ts`          | The whole server: routes, sync, polling, alerts, status, MCP                                |
| `src/Main.elm`     | The whole frontend: model, update, view                                                     |
| `src/index.html`   | The glue Elm cannot do: ports, the automerge repo, sockets, `fetch`                         |
| `src/page.mjs`     | The parts of `index.html` that are functions of their input, so they can be tested          |
| `src/sql.mjs`      | The query engine both sides share: UDFs, ref resolution, the pre-engine passes, `explain()` |
| `src/examples.mjs` | Bundled datasets, reference tables and demo queries — **the index of what ships**           |
| `src/portals.mjs`  | The live demo feeds: `{ name, ms, init, tick }`. The one list of portal names               |
| `src/sw.js`        | The service worker: the app shell out of a cache, so an installed app opens with no network |
| `schema/db.sql`    | Desired schema, no data. `pg-schema-diff` diffs a live DB against it; no migration files    |
| `examples.sql`     | Shop catalogue of query templates, applied by `seed()` on first request                     |
| `vendor.ts`        | Rebuilds the vendored browser bundles in `src/` (automerge, automerge-repo, alasql)         |
| `deno.json`        | Tasks, dependencies, import map                                                             |

## Commands

- `deno task build` — copy `src/*` to `dist`, then `elm make`
- `deno task dev` — build, then serve `dist`
- `deno task test` — the whole suite. **Not** `deno test --allow-all`: the task is the one place `JWT_SECRET`,
  `TOKEN_SECRET` and `DSN_ENCRYPTION_KEY` are set, and `main.ts` refuses to load without all three. It builds `dist`
  once, runs the files in parallel, and fails past ten seconds of wall time, which is the rule that the suite gets fixed
  before a feature is added. It type checks **once, for every test file at a time, alongside the run** rather than
  letting `deno test` do it: that checks in each worker it starts, and eight of those at once cost more than the
  checking does — measured at about a second of the ten. The check and the run are awaited together and either one
  failing fails the task, so nothing is traded away for it. Time one file with `deno test --allow-all <file>`, and check
  `top` first: a build job on the same machine makes every number here a lie
- `deno task review` — elm-review. Runs clean with zero suppressions; keep it that way
- `deno task status` — print every graded condition from the deployed `GET /status`, exit nonzero if any is below 1.0.
  `.github/workflows/status.yml` runs it on a 15-minute cron; the failure email is the alarm
- `deno task vendor` — re-vendor the browser bundles after bumping the versions at the top of `vendor.ts`
- `deno task db:plan` / `db:apply` — read the generated migration, then run it. **Check `.env` first: `DATABASE_URL` may
  point at production**, and `db:apply` no longer prompts. Its allow list is `INDEX_BUILD,INDEX_DROPPED`, so a migration
  that drops a constraint (a primary key move) is refused for `ACQUIRES_ACCESS_EXCLUSIVE_LOCK`; run the same
  `pg-schema-diff apply` by hand with that hazard added for the one migration rather than widening the task
- `deno run -A npm:elm-format --yes src/Main.elm` — format Elm
- Watch: `watch src { try { cp -vu src/* dist ; elm make src/Main.elm --debug --output=dist/index.js } }`

Local Postgres default is `postgresql://postgres@127.0.0.1:5434/postgres`. Tests need no real Postgres: they run against
in-process PGlite behind a pg-gateway on that port. Schema changes go in `schema/db.sql`; DML is never generated, splice
it in with `--insert-statement`. **Deploy the code before a check constraint on a column `seed()` writes.** Postgres
checks an `insert ... on conflict do update`'s proposed row before it looks for the conflict, so the running seed must
already propose the column or every request fails on the constraint until the deploy lands. `license` on `sheet` took
production down this way: backfill the rows, deploy, then `db:apply`. The opposite order for a new table the code writes
on every request: `audit` had to exist before the code that inserts into it was deployed. **Schema first for the `db`
primary key move** (`sheet_id` to `db_id`, with `created_at`): the new `GET /codex/:id` orders by columns the old table
does not have, so code-first refuses every codex read until `db:apply` lands, while schema-first costs only a
`POST /codex-db/:id` in the window, because the old upsert's `on conflict (sheet_id)` target is gone.

## Tests

Eight files, and two harness modules beside them. Which one a failure belongs in is usually obvious.

**Why there are eight.** `deno test --parallel` runs files side by side and not tests, so the suite costs whatever its
slowest file costs — every split below was made because one file had become that. The two jsdom harnesses were split
apart first, then each was split again, and the halves are kept **even in the thing that is actually expensive**: a boot
or a `glue()`, which is Elm's first paint into a jsdom. Counted rather than eyeballed, with
`grep -c "await boot(" page_test.ts library_test.ts` and the same for `glue_test.ts` and `sync_test.ts`. Adding a file
is not free — each one is another process, another module graph — so split only when one file is the critical path, and
rebalance rather than pile onto whichever file the test seems to belong to.

- `main_test.ts` — the server. One `Deno.test` of named `t.step`s against in-process PGlite: auth, sync and roles, shop
  and Stripe, `POST /query`, the `src/sql.mjs` UDFs, net-http polling, socket reports, alerts and digests, MCP, export.
  Steps run in order against one database, so a step still depends on what ran before it. What steps buy is a name in
  the failure and every later step still running — **not** isolation, and not `--filter`, which matches test names and
  not step names. A **second** PGlite behind a second gateway on `127.0.0.1:5435` is the codex sheets' external database
  — a second instance and not a second address onto the first, because the gateway hands every connection onto one
  PGlite session and the codex connection sets that session read only, which refused the next insert anywhere in the
  suite. It is **cloned from the first rather than booted**: `dumpDataDir` on the main instance before the schema is
  applied is a few tens of milliseconds and `loadDataDir` is a quarter of a second, where booting a second Postgres from
  nothing is another second and a half of WebAssembly competing with the first for cores. Taken before the schema on
  purpose — a codex database holding our own tables would have `codexTables()` listing them — and built on the first
  connection to it rather than at startup, since most of this suite never opens a codex sheet. A DSN that must fail
  names a loopback port nobody listens on, never a hostname: the suite does no DNS.

  `request()` — the helper for calls that are meant to succeed — clears `rateLimitBuckets` first. Every request here
  arrives from one address, which no real client does, so that bucket is shared by the whole run and empties over it; it
  refills on the wall clock, so whether a step passed depended on how slow the suite had been up to that point, and
  making the suite faster is what surfaced it. The limiter has its own steps, which set the bucket they are about by
  hand and go straight to `app.request`, so clearing here cannot hide what they assert.

  **This file is the suite's critical path** and the one that stays whole. Splitting it was built and measured on
  2026-09-17 and thrown away; what follows is so nobody spends the afternoon again. Its later half really is independent
  — the last thirty-four steps pass with the first thirty-four skipped — and two files of thirty-four ran green on their
  own. Three things killed it:

  - `deno test --parallel` runs test files as **workers of one process**, so `Deno.env` is shared between them. A second
    file naming its own database in `DATABASE_URL` names it for every other file too, both halves open the one PGlite,
    and their protocol frames interleave until a parse fails. Giving it a `deno test` process of its own fixes that and
    costs the process.
  - Both processes then share `data/automerge` on disk, which `main.ts` puts beside `Deno.mainModule`. The sync step
    fails with a 404 perhaps one run in five.
  - And it bought about half a second. The second process pays its own PGlite (~1.6s) and module load (~0.7s), which is
    most of the ~3.5s of steps it took off the first.

  The pool is not the blocker it looks like: `max: Deno.env.get("DATABASE_URL") ? 10 : 1` reads the presence of that
  variable as "a real Postgres", so a second file setting it would get ten connections onto one PGlite session — but a
  `DATABASE_POSTGRES_POOL` escape hatch fixed that in one line, and was reverted with the rest. The codex steps'
  `5434`/`5435` literals move mechanically; what does not move is the storage directory or the cost.
- `examples_test.ts` — every bundled sheet through **both** engines (`npm:alasql` and the vendored `src/alasql.mjs` the
  page loads), compared row for row.
- `page_test.ts`, `library_test.ts`, `glue_test.ts` and `sync_test.ts` — the page under jsdom, through two harnesses,
  two files each. `page_test.ts` is the table and the query sheet: how they render, sort, arrange and take the keyboard.
  `library_test.ts` is the library itself, the sheets opened from it (a feed, an alert, a chart, a dashboard), the
  palette over them, and the parts of `src/page.mjs` that need no page at all. `glue_test.ts` is what the glue does to a
  document; `sync_test.ts` is what arrives from outside it — a CSV chosen or dropped, a socket report, a fork, a real
  automerge document taking every patch shape, the row and column verbs, and `src/sw.js`. `glue_harness.ts` holds the
  `glue()` the last two share. `page_harness.ts` is what all four share — the compiled Elm, the window installed as
  globals, `boot`, `until`, the page-side query engine — and is a module rather than a test file so that neither
  registers the other's tests by importing it. `boot` runs the compiled Elm in `dist/index.js` with every port answered
  by hand and the library fed in through `library()`; reach for it for anything about what the page renders, and for
  `rendered()` — the one booted page shared across tests, the library, for the tests that only read what it painted —
  when nothing in the test writes to the model. `glue` runs `src/index.html`'s own `<script type="module">` over the
  same jsdom — its imports rewritten to a destructure, `initializeWasm` and the storage stubbed, the websocket adapter
  genuine, `fetch` and `WebSocket` recorded and answered by the test — so `changeDoc`, `arrangeDoc`, `applyPatches`,
  `Views`, the query re-run guard, the share requests, CSV import, `newDoc`, fork and the socket-health report are the
  real ones; reach for it for anything about what the glue does. Both harnesses count a settle off a mutation observer,
  not by serializing the body per frame, which was most of what a settle cost. `docs` hands it a synced document, which
  is where a write is watched: the handle holds the test's own object. `realRepo` swaps the stub repo for automerge
  itself — slower, and the only way to find out whether a patch means the same thing to a real document as it does to a
  plain object. Both harnesses drive animation frames off the event loop rather than jsdom's ~16ms clock: a settle waits
  for the page to go quiet, not for real time, and that clock was most of this file's wall time. Anything that does wait
  on a real timer — the query debounce, a file being read — asks `settle(ms)` for it by name, and `until()` is the
  bounded poll for the ones where the wait is for something to happen; a flat `settle(ms)` is only for proving that
  something did **not**. Refuses a `dist` older than `src` rather than building one: `deno task test` builds once before
  any file runs, so the files can run in parallel without a compiler racing a reader of its output. deno-dom is not
  enough — it has no `replaceData` on a text node. It also runs `src/sw.js` over a hand-made `self`, `caches` and
  `fetch`, which is the only way to take the network away from a service worker. Dead ends, measured, so nobody spends
  the afternoon again: `--optimize` shrinks `dist/index.js` by a few percent and moves nothing; the flat `settle(ms)`
  sleeps left prove something did **not** happen and cannot be shortened; what a `boot` costs is Elm's first paint into
  jsdom, so the levers are fewer boots or another file.
- `browser_test.ts` — no browser: dist is fresh, `index.html` wires the WASM and the import map, every root-absolute
  asset is in `_redirects`, every imported name is exported, nothing reaches a CDN. `index.html`'s
  `<script type="module">` body is piped to `deno lint` for real scope analysis. `BROWSER_GLOBALS` is the whole
  allowlist of names Deno's global scope lacks. `src/sw.js` is linted the same way, its `SHELL` list is held equal to
  `_redirects` in both directions, and `PAGE_BY`/`pageBy` is one more of the language-boundary copies it reads as source
  text.
- `tests/MainTest.elm` via `elm_test.ts` — pure Elm: selection and navigation, sort and filter, clipboard parsing,
  column stats, `docDecoder`, `chartPoints`, `chartBoxes`, `chartSpan`, `legendLayout`, and the `similarity`/`soundex`
  pair `main_test.ts` asserts the same pairs against.

## Invariants

A change that breaks one of these is a bug even if the suite is green.

- **One identity.** `usr_id` is a string in every handler, decided in the one middleware that reads `jwtPayload.sub`. A
  share-link token carries no `sub` and is refused with 403 before any route runs, so no handler ever sees an anonymous
  caller.
- **One refusal shape.** Every 4xx/5xx this server raises is one `bad(status, headline, fields)` call carrying expected
  / received / source / fix. Three guards sit at the boundary so no handler has to remember: a NUL in a path or query
  string is refused before routing, every body is capped by `bodyLimit` at `BODY_CAP`, and every JSON body is read
  through `jsonBody()`, which refuses what is not one object. `cselect()` refuses a `limit` or `offset` that is not a
  count, and `docData()` refuses a claimed document with no rows. The few surviving `throw new HTTPException` are
  passthroughs of an `explain()` block `src/sql.mjs` already built. The error log is written after the response and not
  awaited; `errorLogged()` is how a test waits for it instead of sleeping. `Received` goes through `show()`, never
  `JSON.stringify` — `explain()` drops an undefined field, so a stringified `undefined` silently loses the line.
- **No refusal is an oracle.** A signature rejection never prints the secret, the expected digest, or how close a guess
  was.
- **One engine, two hosts.** `src/sql.mjs` is shared by the server (`npm:alasql`) and the page (`/alasql.mjs`).
  `toRecords()`, `loadRefs()` and `planQuery()` are the three functions both call; where a sheet comes from is the only
  difference, and it is an argument. A query must mean the same thing in both.
- **Rows are keyed by column name at every boundary.** `GET /sheet/:id`, `POST /sheet/:id`, every export and every MCP
  tool. `col.key` is the document's own spelling and never leaves it. A sheet with two columns of one name is refused on
  read and on export, and the CSV importer and `nameClash()` refuse to create one.
- **Every jsonb write goes through `sql.json(...)`**, never `JSON.stringify`: postgresjs serializes jsonb itself, and a
  pre-stringified value lands as a jsonb _string_.
- **Every cast out of jsonb is guarded**, and guarded _inside_ a `case` rather than beside it with `and` — Postgres does
  not promise to evaluate `and` left to right.
- **Every bounded map goes through `bound(map, max)`**, and every loop, retry and recursion has a bound whose message
  carries the counter.
- **One budget per sheet, spent through `spend()`.** `hookBucket()` is the bucket and
  `spend(sheet_id, what, rows, bytes, fix)` is the one check-and-charge, synchronous so concurrent callers cannot all
  pass it. Every door into a document sheet spends one unit: a webhook delivery, a socket report, a whole read through
  `sheet()` (`GET /sheet`, an export, an MCP read; the computed sheets are free), and an append whatever it carries,
  with its bytes as the volume. It is taken after the access or signature check, so a refused request spends nothing,
  and its refusal is a 429, which `app.onError` does not log.
- **One account is bounded across its sheets.** `rateLimit()` runs once more on the account, in `accountBuckets` rather
  than the address map so address churn cannot evict it; `assertSheetsQuota()` caps the sheets an account owns at claim,
  import and purchase; `assertRoom()` caps a sheet's rows at the engine's own `MAX_QUERY_ROWS` at import and append;
  `sendWithinQuota()` caps an account's alert deliveries a day, email and url alike, counted off the run log. Fetches
  need no count: a feed polls at most once a minute and a poll reads at most `PAGE_MAX` pages, so the sheets cap bounds
  them. A refusal that changed what an account keeps or sends says "quota" where `GET /status` reads it, the error log's
  413s and the alert run's delivery line; a 429 is shed unlogged and is not counted, by design.
- **`POLL_OK` / `ALERT_OK` / `RUN_OF` / `RUN_OK` / `pauseSwitch` have one definition each.** `GET /status` and
  `library:freshness` both read them from there. Two hand-copied copies had already drifted. `pauseSwitch` is the odd
  one -- it reads a document rather than a column, because a paused sheet writes no run for SQL to read -- and its two
  callers read its `null` differently on purpose: `freshness()` reports the unknown as unknown, because naming the sheet
  is its job, and `status()` grades it as running, because an unreadable document must never excuse a dead feed.
- **The status check grades, never maximizes.** 1.0 is the minimum pass, `grade()` floors, and a condition that cannot
  compute throws by name.
- **Secrets never reach a document.** Sheet secrets are referenced from a net-http header as `{{secret:name}}` and
  resolved at fetch time into a separate object, so the automerge document sync hands every viewer keeps the reference.
  A publish is scanned for one that got in anyway: `assertNoKeys()` refuses `POST /library/:id/public` and a priced
  `POST /sell/:id` when a cell matches a shape in `KEY_SHAPES`, naming the column and the 1-based row, never the value.
  Both routes check the owner before they scan -- the refusal names what a private document holds -- and the scan is
  bounded by `KEY_SCAN_CELLS` and `KEY_SCAN_BYTES` both, because one synced cell may hold a megabyte. **A scan that
  could not run is not a scan that passed**: the sheets with no document of their own are named (the computed ids, and
  the `codex-` prefix), and a document that will not load refuses the publish by name rather than skipping it.
  `PII_SHAPES` -- an email address, a phone number, a US social security number, and a run of digits `luhn()` says is a
  card -- is scanned in the same pass under the same bounds, and refused by the same two routes unless the body carries
  `personal: true`, a claim a credential has no equivalent of. `library:lineage` is in the skip list beside the other
  computed ids.
- **One spelling per fact.** `API_BASE` in `src/page.mjs` is the only API host, handed to Elm through flags and to
  `index.html` by import. `PORTALS` in `src/portals.mjs` is the only portal list. `Stored` in `index.html` is the only
  `localStorage` key prefix. `spec` in `Main.elm` is the only per-column-type table, and it has no wildcard, so a new
  type fails to compile. `CHART_KINDS` in `src/sql.mjs` is the only list of ways a chart is drawn: `chartSql` refuses
  one that is not on it, `kindSpec` in `Main.elm` is the copy the language boundary forces, and `browser_test.ts` fails
  when the two disagree — it compares the two as sets, so a kind added to both needs no edit there. `NET_METHODS` in
  `main.ts` is the only list of verbs a feed is polled with, `netMethods` in `Main.elm` is its copy, and the same test
  fails on the same drift.
- **A column type is one word everywhere.** `COLUMN_TYPES` in `src/sql.mjs` is the list. Its entries are either a type
  or an `as` alias of one; `CANONICAL_TYPES` is the half anything may write, `NUMERIC_TYPES` and `JSON_TYPES` derive
  through `canonicalType()`, and `main.ts`'s `Type` union plus `columnTypes`/`typeAliases` in `Main.elm` are the copies
  a language boundary forces — `browser_test.ts` reads all three as source text, in both directions, and fails when any
  drifts. An alias is read and never written, so an old `pct` column still loads, still queries and is still checked,
  and no new one is stored. A spelling outside the list is refused by `checkColumnTypes` rather than skipped: skipping
  it is how a percent column stopped being checked at all.
- **A column's declared type is never rewritten.** `col.raw` is the document's own spelling, the way `col.key` is, and a
  header write patches one field (`[0, x, "name"]`) rather than replacing the column object. Replacing it made a rename
  re-encode the type beside the name.
- **No runtime CDN.** Everything the page loads is served by us; `deno task vendor` fails the build if a bundle would
  still fetch something. Automerge stays external in the repo bundles so all three share one WASM-initialized copy.

## Backend map (`main.ts`)

One file on purpose — the header comment says why splitting was measured and rejected. The `// ---` sections are the
navigation, in file order:

`refusals` · `secrets & crypto` · `webhook signing` · `types` · `sheet & query core` · `database` · `app & middleware` ·
`seeding` · `automerge sync` · `live portals` · `public routes` · `status` · `delivery signatures` · `delivery budgets`
· `net-http polling` · `alerts` · `authenticated routes` · `freshness` · `sharing` · `secrets` · `import/export` ·
`codex (external databases)` · `mcp`

- **Sheet types** (the check constraint in `schema/db.sql` is the list): `template`, `table`, `net-hook`, `net-http`,
  `net-socket`, `query`, `portal`, `alert`, `chart`, `dashboard`, and `codex-*`. A sheet id is `type:doc_id`.
- **Computed sheets**: `library:freshness`, `library:lineage`, `library:audit`, `net-hook:errors` and `net-hook:reports`
  answer through `sheet()` without an automerge document, so they page, export and can be selected from a query like any
  other sheet. The operator is `isOperator()`: whoever reads `net-hook:errors`, which `OPERATOR_EMAIL` is granted at
  seed time.
- **Auth**: JWT middleware; a per-sheet API key (`scrapsheets-key`) is scoped by a path check _before_ routing, so no
  handler has to remember to ask. Email through Resend. A key is minted under one of `API_KEY_NAMES` -- `api` writes,
  `api-read` reads -- and `/mcp/:id` is in its path scope; see **MCP**.
- **Sync**: official automerge `NodeWSServerAdapter` behind a ws-shim over Hono's `upgradeWebSocket`. Per-document
  access is `syncRole` in the message path, not just `sharePolicy`; a viewer's frames are decoded and rejected if they
  carry changes.
- **Webhook ingest**: `POST /net/:id`, always signed (`scrapsheets-signature: t=…,v2=…`, or a Stripe/GitHub/Shopify
  scheme chosen by the sheet's stored secret name). Replay is refused by the unique index `net_hook_signature_idx` on
  the digest that actually verified.
- **Socket health**: nothing server-side opens a `net-socket` sheet's socket, so a browser with the tab open is the only
  witness and `POST /library/:id/socket` is how it says so. Two states — `connected` and `error`, never a close, because
  `changeId()` closes the socket on every navigation. `library:freshness` admits the sheet only once it has a `SOCKET`
  run, so a socket nobody has watched is absent rather than "never run" forever.
- **Alerts**: an alert's `when` is `rows` (the query returned a row; the default), `added` or `removed` (the answer
  gained or lost a row since the run before). `status` is the verdict and `delivery` what was done, so no reader learns
  a new word; a change condition's first run is a silent baseline and a run past `ALERT_ROWS` under one is an `error`
  row. `ALERT_WHEN` in `main.ts` and `whenSpec` in `Main.elm` are the two copies a language boundary forces. A `to`
  matching `^https?://` is delivered by `sendAlertUrl` through `safeFetch` instead of mailed — `{text}` to
  hooks.slack.com, `{content}` to discord.com/api/webhooks, `{sheet, name, rows}` to anything else — refused posts
  recorded and retried like a refused email, `sendWithinQuota` counting one the same as the other, and a digest over a
  url refused at config read. The run row keeps a url's **host** and never its path: the path is the whole of a
  webhook's authorization, and every viewer of the sheet reads that row. **An alert is silenced, not deleted, by
  `snoozed_until`.** It is an ISO timestamp in the alert document's `data[0]`, read against the run's own clock, so one
  in the past is over without anything clearing it, and one the regex and `Date.parse` cannot both read is an error run
  naming the field rather than a quiet no-snooze. A run inside the snooze is decided and recorded as any other -- the
  verdict in `status`, the rows in `matched`, so the baseline `added` and `removed` diff against still moves -- and its
  `delivery` is `snoozed`, chosen below the no-destination line and above the digest branch: an alert nobody gave a
  destination still reads as broken rather than silenced, and neither door mails a snoozed one. `sendWithinQuota` counts
  only what was sent, `ALERT_OK` admits `snoozed` beside `sent` and the digest hold so a day of silence is not a day of
  outage, and a snoozed run is `stuck`, so the run after it is not de-duped away as an answer somebody has already read.
  `snoozedUntil` on `Main.elm`'s `Alert` is the page's copy and `isoStamp` is the one spelling of the stamp: the chip
  asks `Time.now` at the click and writes now plus a day, and `viewAlert` decides whether the snooze still holds by
  comparing the two stamps as text, because ISO sorts the way the calendar does -- so a cell the poller refuses draws as
  whatever it says, with the `unsnooze` chip that clears it.
- **Polling**: `pollNetOnce` and `pollAlertOnce` on a 15-second tick; a sheet's `method` is one of `NET_METHODS` and its
  `body` is templated where a header already was — `{{secret:name}}` out of the secret table and `{{cursor}}` the same
  watermark the cursor parameter carries, both kept as the sheet spells them in the failure row's repro, and a GET
  carrying a body refused by `netRequest` while that unresolved text is still what is held — safeFetch is handed the
  resolved one, and a size measured there is an oracle on the secret. Every row records the verb it went out with, and
  the watermark rides every row a poll writes, failures among them: one bad poll that dropped it asked the feed for all
  of history again. The validators do not travel with it — a 304 moves the row the validator came with, and a failure
  row does not hold that body. Conditional requests, per-host `Retry-After`, bounded retries, and a `net` row per run —
  including quiet ones, because a healthy quiet alert and a dead timer otherwise write the same nothing. A run's row
  carries `meta.shape`, the columns the body answered with and the JSON type of each (`shapeOf`); a run whose shape
  differs from the run before keeps its rows and carries `meta.shape_change` naming what was added, dropped and retyped
  (`shapeChange`), and `POLL_OK` grades it as failed, once, so freshness and the status alarm hear it through the path
  every other failure takes. A good run's body is its idempotency key: its digest rides `meta.sig`, the slot a
  delivery's signature takes, so `net_hook_signature_idx` refuses the same body twice and `netRow` moves the row it
  matched to now, marked `repeated`. A query over a net-http sheet reads only the runs `POLL_OK` grades (`sheet()` adds
  it when `path_` is non-empty); the sheet view and the export keep the whole log, failures among them, so one bad poll
  cannot empty what is built downstream. A sheet's `page_by` is one of `PAGE_BY` (`page`, `offset`, `cursor`, `link`)
  and a poll reads every page into one body: the arrays concatenated, so `shapeOf`, the digest, `BODY_CAP` and every
  reader downstream see what a one-page feed hands them. `page_param` names the query parameter the number, the offset
  or the next cursor rides — a `link` feed names the whole url itself, and `page_path` says where in the answer the next
  cursor sits. `pageConfig()` reads and refuses the three fields by name the way `netRequest` refuses a method;
  `pageRows()` is the one page parser (a top-level array, or under `cursor` and `link` the one array-valued key of the
  envelope; two arrays is a guess and is refused naming both, because `{warnings: [], items: [...]}` read as no rows at
  all under a green run row) and `nextPage()` the one stop condition. A name before the last of `page_path` that page
  one does not hold is a wrong path and a failure row, not a one-page feed; a later page dropping the envelope is the
  feed's own way of saying there is no next. Page one carries the number or the offset, so the validators, the
  `{{cursor}}` watermark and the host holdoff still ride the first request alone; a cursor and a Link header only arrive
  with an answer. The walk is bounded by `PAGE_MAX` and by `BODY_CAP` checked as the pages sum, a `link` next page must
  be on the origin the sheet names (scheme and port included), and `page_param` naming the sheet's own `cursor` is
  refused because paging overwrites it on every request. **The sheet keeps nothing from a poll that failed part way**: a
  429 or a 5xx on any page is the one `later()` retry path for the whole poll, every other refusal throws into the
  catch, and the next scheduled poll starts at page one. The failure row names the page that broke, so its repro replays
  that request. The pre-flight makes one request whatever `page_by` says. **A sheet's `mode` says what a good run does
  to the runs before it.** It is one of `NET_MODES` (`append`, `replace`, `upsert`), a sheet without one appends the
  whole log the way every feed did, and `storeConfig()` reads and refuses it beside `pageConfig()` off the same document
  and before a request goes out -- with `key`, the dotted path into one row an `upsert` supersedes by, which that mode
  needs and every other refuses, and `rows_path`, the dotted path to the array of rows in an envelope, which
  `pageRows()` honours before its one-array guess so a feed answering `data` beside `included` names which one is the
  answer. `NET_PATH` is the one spelling all three dotted paths are checked against, and `atNames()` the one walk along
  them, answering nothing where the path runs out so each caller refuses in its own words. `netRow()` takes the
  `Storing` beside the meta and does the insert and what it supersedes in one transaction: `replace` deletes the sheet's
  earlier `POLL_OK` rows, `upsert` writes the sorted distinct key values as `meta.keys` (`rowKeys()`, reading the array
  itself or the one at `rows_path`, refusing a row holding nothing at the key by its position rather than skipping it,
  and refusing a number past `Number.MAX_SAFE_INTEGER` -- JSON has no integer past 2^53, so `JSON.parse` has already
  folded two distinct ids into one float before this server sees them, which is the merge upsert exists to prevent) and
  deletes the earlier rows whose `meta->'keys'` overlap them, guarded inside a `case` on `jsonb_typeof`. **Only a 2xx
  supersedes anything**: a retry row and a caught refusal are handed `null`, so one bad poll cannot empty the sheet, and
  the failure rows stay under `replace` because `POLL_OK` never counted them -- as does a run `POLL_OK` grades on
  `shape_change`, which a query over the sheet does not read either. The run log stays the unit -- the key says which
  earlier runs this one replaces, not which rows -- and `trimNet` and the digest dedupe are unchanged, so a repeated
  body still moves its row to now and that moved row is the one `replace` keeps. **A 304 keeps every field the row it
  moves already held**, `meta.sig` and `meta.keys` among them, and overwrites only the three a not-modified answer
  changes: a quiet tick that rebuilt the row's meta out of the validators alone left a row that no longer said what it
  had answered for, and the next real run superseded nothing.
- **A body is parsed by the type it declares.** `BODY_PARSERS` is the list -- `text/csv`, `text/tab-separated-values`,
  `application/x-ndjson`, `application/jsonl`, `application/gzip`, `application/x-gzip`, `application/zip`,
  `application/x-zip-compressed`, `application/xml`, `text/xml`, `application/rss+xml`, `application/atom+xml`,
  `text/html`, `application/xhtml+xml` -- and `readFeedBody` is the one reader both doors call: `pollNetOnce` on every
  page it reads, and `POST /net/:id` after the signature. A type on none of the list is stored as the text it arrived
  as, which is what a JSON feed hands us already, and a body that did not answer 2xx is never parsed. A CSV and a TSV go
  through `parseDelimited`, the text-to-`{cols, rows}` core lifted out of `readImport()` -- the importer is a thin
  wrapper over it, so a feed's digits are numbers, its blanks are nulls and its ragged line is refused by number exactly
  as an uploaded file's are, and `source` is what every one of those refusals names the line in: the uploaded file
  there, the url here. `assertRoom` stayed behind in `readImport`: the row quota is a sheet's, and only that door makes
  a sheet, so a feed's rows are bounded by `BODY_CAP` alone the way a JSON feed's always were. The rows are stored keyed
  by column name; `col.key` is the document's own spelling and stays with the importer, the one door that writes a
  document. NDJSON is one JSON value a line, a blank line no record and a line that will not parse a refusal naming it.
  `expand()` is the one bounded decompressor and both compressed doors go through it: bytes are fed to
  `DecompressionStream` `EXPAND_SLICE` at a time and read back through `readBody`, so a bomb is refused holding one
  slice's expansion past the cap rather than the gigabyte it writes; handed the whole of a bomb at once the decompressor
  answers all of it in one chunk, which is the cap spent after the memory is gone. It has one home rather than two
  copies because it is a subtlety that only shows up under attack; it slices **before** the stream exists so `pull`
  cannot throw, and it refuses a non-`Uint8Array` argument as ours -- a `TypeError` raised inside `pull` was caught by
  the decompressor's own catch and reported as the host having sent a bad body. What came out of a gzip is read by its
  first character -- a bracket or a brace is JSON, everything else a CSV -- because nothing in an answer says what a
  gzip holds. **`BODY_CAP` is spent on three different numbers**, and no one of them stands in for another: the bytes
  that arrived, what a gzip decompressed to, and what the body _means_ -- a file names its columns once and the rows it
  means name them on every row, so a few kilobytes on the wire is megabytes in the column. The poller's cross-page sum
  counts that third number too, what `readFeedBody` answered and not the wire bytes a page carried: three gzip pages
  each under the cap decompressed summed to twice it while their wire bytes stayed at kilobytes. Every parsed body is
  stored as the JSON text it means -- the array of rows for every format that holds one, and the document itself for a
  generic XML body -- so `shapeOf`, `meta.sig`, `pageRows`, the export and every query downstream see exactly what a
  JSON feed hands them, and a body its own declared type cannot parse is that poll's failure row or that delivery's 400,
  never a stored blob. `BODY_DEPTH_MAX` bounds how many containers one body nests, because the only other thing stopping
  a zip quine is a table two screens away holding no `zip` row.
- **A NUL byte is refused where a body becomes the text a row stores.** Postgres text cannot hold one and the column is
  text because a body is a body, so `readFeedBody` asks its own answer for one as its last act, beside the `BODY_CAP`
  refusal and on the same `meant` and the same `source` -- which is what makes it one rule for both doors rather than
  two copies that drift: `POST /net/:id` gets the 400 it always answered, and the poller, which cannot answer 400 at
  all, gets the throw as a failure row through `pollNetSheet`'s catch, storable because the refusal names an offset and
  never quotes the byte. It is asked of what is stored and not of what arrived: a gzip body is full of NULs and what is
  stored is the rows that came out of it, written by `JSON.stringify`, which spells the byte as six characters that
  store like any other six. The branch it exists for is the unparsed one -- a type on none of `BODY_PARSERS`,
  `application/json` among them -- which used to return the decoded bytes before any check ran, so the byte reached
  `netRow`'s insert and the sheet's own log recorded a driver's words under a status-0 row. It is now one more branch of
  the one chain every other type takes, on `reading === undefined`, so it spends `BODY_CAP` too: a decode that writes
  U+FFFD for invalid UTF-8 means more bytes than arrived, and neither door's wire check can stand in for that. The `zip`
  block is the one early return left, and a `.json` member is refused by `jsonMeant` before it reaches either check,
  since JSON has no unescaped control character -- which is also why a zipped `.json` member is the one body `BODY_CAP`
  does not bound, as it was before this check moved.
- **A type this server guessed is checked; a type the sender declared is not.** `jsonMeant()` is the line between them.
  A body that declares `application/json` is taken at its word and stored as it arrived -- the NUL check at the end of
  this same reader has a better refusal for the one byte Postgres cannot hold than "not JSON" does, and it is the
  sender's claim either way -- but what came out of a gzip and what a zip member's _name_ says are this server's own
  guesses, and an unchecked guess stored an HTML error page verbatim under a green run row. So both go through
  `JSON.parse` and the text that arrived is what is stored: re-serialising it would move the digest on `meta.sig` and a
  repeated body would stop being recognised as one.
- **XML is read by a library, and read twice.** `npm:fast-xml-parser` is the tokenizer: an XML reader is what "never
  hand-roll anything that parses" is about, and this one expands no custom entity and refuses an external one, so the
  billion-laughs body is stored as the text `&lol2;` rather than as a gigabyte and an XXE never reaches the filesystem.
  It also refuses `__proto__`, `constructor` and `prototype` as element names itself. `xmlDoc()` runs `XMLValidator`
  **before** `XMLParser`, because the reader takes `<a><b></a>` as `{a:{b:""}}` without raising and a half-read document
  stored under a green run is the blob this whole list exists to stop; the refusal carries the line, and the column only
  where there is one, since an empty body answers no column and a stringified `undefined` reads as a place in the
  document. The parser's own bounds -- a nesting depth, a declared external entity -- are a throw it raises after the
  validator has called the document well formed, and its own words are the message: naming one of those causes sent the
  owner flattening a document one level deep. Text stays text (`parseTagValue: false`): `checkColumnTypes()` is the one
  place a cell becomes what its column says, and coercing here reads an id of `007` as 7 and one past 2^53 as a number
  the feed never sent. Attributes are kept under an `@` prefix, because half of Atom's payload lives in them --
  `<link href=...>` carries the url. A namespace prefix is dropped (`removeNSPrefix`), which is what makes `<a:entry>`
  an entry and a prefixed element addressable at all, since `rows_path` is checked against `NET_PATH` and a colon is not
  in it.
- **A markup body is decoded the way it says to decode it.** `markupText()` is the only place a body's own encoding is
  read, because XML and HTML are the only formats here that state one -- a CSV or an NDJSON body is still decoded as
  UTF-8 whatever its answer said. The order is the one the web reads these in: a byte-order mark or the NUL beside the
  opening `<` first, because a sixteen-bit document cannot be sniffed for its own declaration as UTF-8 and one was
  refused as "not XML" on every poll forever; then the answer's own `charset` parameter (`CHARSET_PARAM`), which is what
  lets a Latin-1 page carrying no `<meta charset>` be read at all rather than refused on its first accent; then a regex
  over the first `MARKUP_HEAD_BYTES` -- `XML_ENCODING` for the prolog's `encoding="..."`, `HTML_CHARSET` for a
  `<meta charset>` -- both ASCII by definition and so readable before the first byte that is not; then UTF-8. The decode
  is **fatal**: Latin-1 RSS and Latin-1 HTML are both still common, and a bare UTF-8 decode turned every accented
  character into U+FFFD and stored that as a cell, which nothing downstream can tell from a character the feed actually
  sent. A label this runtime has no decoder for is its own refusal. The in-document scan cuts HTML comments and script
  bodies out of the head first, because this runs before there is a parser that could know a `<meta charset>` written
  inside a `<script>` string is not a declaration -- one there, ahead of the real tag, decoded a UTF-8 page as Latin-1
  and stored the mojibake.
- **RSS and Atom answer their rows; generic XML answers its document.** The two feed formats name the element a row sits
  in, so there is nothing to guess: `xmlRows()` walks the parsed document depth-first with each node's children pushed
  reversed -- which is what makes a stack hand them back in document order, where breadth-first returned a nested feed's
  rows before the ones written above them -- and collects every `<item>` (RSS) or `<entry>` (Atom) wherever it sits, RSS
  2.0 putting them under `<channel>` and RSS 1.0 directly under the root. A row is taken whole and never walked into, so
  an `<item>` inside an `<item>` is that row's data and not a second row; a row that is not an object is refused by its
  position, because a text-only `<item>hello</item>` stored as a row made `shapeOf` answer null for the whole run and
  left the run after it with no shape to be compared against. `isArray` holds both names to a list whatever the count,
  so a feed answering one row and a feed answering ten answer the same shape: collapsing the single one to an object is
  how a reader breaks on the quiet day rather than on the day it was written. The walk is bounded by `XML_NODES_MAX`,
  which counts nodes and says so -- it pushes no scalar, since spending the budget on every string in the document made
  the counter mean something other than its own refusal. **`xmlFeedRoot()` refuses a body that is not the feed it says
  it is**, against `XML_FEED_ROOTS`: a well-formed HTML holding page served with a feed content-type -- what a provider
  behind a 200-ing proxy answers -- parsed, held no `<item>`, and stored `[]` under a green run row with no shape and so
  no `shape_change` either, which is a sheet empty forever and a status check that never said why, and under
  `mode: replace` the one run that did get graded took every earlier row with it. A generic `application/xml` body names
  no row element, so what it means is the whole document and `rows_path` is how a sheet says where the rows sit in it --
  which is exactly what a JSON feed answering an envelope already does.
- **`rows_path` is read whatever a feed's paging is, and it is also what holds a generic XML row to a list.** Two halves
  of one fact, and half of it was worse than neither. `pageRows()` reads the field on a paged feed and `rowKeys()` reads
  it under `upsert`; nothing read it on the one-request path, so a sheet with no `page_by` stored the whole envelope
  under a green run row and the setting it had been given did nothing at all -- the shape a generic XML feed and a JSON
  feed answering an envelope both land in. `namedRows()` is that read, on the `else` of the same
  `if (paging && res.ok)`, and an answer holding something other than an array where the sheet said its rows are is this
  poll's failure row rather than a guess at the envelope. The other half is `xmlReaderFor(rowsPath)`, which replaced a
  module-level parser: `isArray` holds `item` and `entry` to a list by name and the sheet's own `rows_path` by `jpath`
  -- the dotted path of the element being read, which is the spelling `rows_path` is already written in -- because a
  generic XML feed names no row element of its own, and without it `rows_path: "data.row"` over a day that answers one
  `<row>` is an object where every other day is an array, with `shapeOf` reporting `{data: "object"}` either way so
  nothing ever grades it. Building a parser per body costs under two microseconds. `namedRows()` carries two refusals of
  its own: an answer that is **already** an array has no envelope for `rows_path` to name -- an RSS body arrives here as
  its items and a CSV as its rows -- and reading the setting as satisfied would hide a sheet that is wrong about its own
  feed; and a row that is not an object is refused by position, the guard `xmlRows` puts on an `<item>` and for the same
  reason, since an array of scalars stored as rows makes `shapeOf` answer null for the whole run, so there is never a
  shape, never a `shape_change`, and `POLL_OK` grades the sheet healthy at zero usable rows for as long as it exists.
- **A zip is a container, and the file inside it is the body.** `zipMembers()` reads the central directory rather than
  walking local headers, because a member written with a data descriptor carries zeroes for its sizes in the local
  header and the directory is the copy that is always right; every offset is checked against the bytes in hand, so a
  truncated or lying archive is a named refusal and never a read past the end. The end-of-central-directory scan walks
  back from the end and takes only a record whose own comment length accounts for every byte after it -- taking the
  first signature found read a one-member archive as an archive of none, because those four bytes occur inside
  compressed data and inside comments. **`zipData()` checks the member against what the directory said it would be**,
  its length and its CRC-32 (`crc32()`, ten lines rather than a dependency): raw deflate carries no checksum and a
  stored member carries nothing at all, so without it a member corrupted on the wire is rows in the sheet under a green
  run row -- while the same payload gzipped is refused, because `DecompressionStream` checks gzip's own trailer. It is
  also what catches a local header whose name and extra lengths disagree with the directory's, which shifted the read
  window and spliced the archive's own filename bytes onto the front of the value. `ZIP_MEMBERS` is the
  extension-to-type table and **names no archive**, which is the table's half of the `BODY_DEPTH_MAX` bound. Exactly one
  member it can name, for the reason `pageRows` refuses a page holding two arrays: which member is the rows is the sheet
  owner's answer to give, and a reader that picked the first would pick a different one next week -- a directory entry
  and a Mac's `__MACOSX/._data.csv` sidecar are not candidates, since a Mac writes one beside every member it zips, and
  the resolved type travels with the member it was read off rather than being computed twice. Refused by name: zip64
  (either the directory's own counts or any member's size, since 65,535 members fits in a small archive where 0xffffffff
  is four gigabytes), an encrypted member (there is nowhere to keep a passphrase), and a compression method that is
  neither stored nor deflate. A deflated member goes through `expand()` with `deflate-raw`, so a zip bomb is bounded
  where a gzip bomb is. **A member name never reaches a headline and always goes through `show()`**: it is up to 64k of
  sender text that may hold newlines, and spliced raw it forged a `Fix:` line of its own in the refusal block and put
  64k of somebody else's text into a failure row and the error log. `NAMES_MAX` bounds how many names one refusal spells
  out before it counts the rest.
- **An HTML body is its one table.** `htmlDelimited()` finds every `<table>` in the document and takes it only when
  there is exactly one, for the reason a zip takes exactly one member it can name: which table holds the rows is the
  sheet owner's answer to give, and a reader that picked the first would pick a different one the week the page gains a
  layout table above it. The refusal names each table it found -- by `id`, else its `<caption>`, else the text it starts
  with -- bounded by `NAMES_MAX`. The parser is `npm:linkedom`: parsing HTML is what "never hand-roll anything that
  parses" is about, it is pure JS so there is no WASM to instantiate on a cold start, and it matched html5ever on every
  malformed table tried -- an implicitly closed `<tr>`, a nested table, a `<table>` written inside a `<script>`. What
  comes back is **written out as a quoted delimited file and read by `parseDelimited`**, the reader a CSV feed and an
  uploaded file both take, so a table's digits are numbers, its blanks are nulls, and a row that does not match its
  header is refused by number in the same words a ragged CSV is -- which is what a `colspan` lands as. Every field is
  quoted, so a cell holding the delimiter needs no thought; `markupCell()` collapses the whitespace inside a cell,
  because in HTML it is layout and not content and because it is what keeps a newline out of the file being built. The
  first row is the header, which is the rule a CSV already has. A `<table>` with no `<tr>` and a `<tr>` with no cells
  are each refused by name: an empty line written into that file is a row the delimited reader passes over, and a row
  passed over is a row the sheet lost. **The rows are ordered by section and then by where they were written**
  (`SECTION_RANK`): HTML 4.01 told authors to put `<tfoot>` before `<tbody>` so a browser could paint the foot before
  the rows arrived, and plenty of pages still do, so read in tree order that footer became the header and the real
  header became a row. Nothing is dropped -- a `<tfoot>` total is a row of the table, and skipping it would be this
  reader deciding what the data means -- it simply lands last. `markupCell()` turns a `<br>` and a block element into a
  space before it takes the text, because `textContent` gives neither any width of its own and `10<br>20` read as the
  single number 1020, which was in neither cell.
- **Run it now, stop it, and see when it runs next.** `paused: true` in `data[0]` takes a net-http or an alert sheet out
  of both tickers: `pollNetSheet` and `pollAlertSheet` -- the one-sheet halves the two 15-second ticks now loop over, so
  the timer and the button are the same code -- return before the fetch and before the row, and put the due entry back
  exactly as they found it, so a paused sheet is looked at on every tick and a started one is due when it always was.
  **A paused sheet is also out of the two graded liveness conditions**, whose sentences say so -- otherwise pausing a
  feed past `POLL_STALE_S`, or an alert past twice its interval, drove the grade below 1.0 and `GET /status` answered
  the 503 it answers when the poller has died. It is read through `pauseSwitch`, and only for the sheets already below
  1.0, worst first, stopping at the first that is not paused: that sheet is the minimum, so a healthy answer opens no
  document at all and the pollers' own 15-second `find` of the same documents is what makes a warm isolate's walk a
  cache hit. `OVERDUE_MAX` is what stops pausing from being a way to pass: it is the reported-only `OVERDUE_CONDITION`'s
  bar, and past it the two conditions stop asking about the switch and grade from the worst sheet. `INTERVAL_MAX_S` is
  the far end of an `interval` field, clamped the way 60 seconds already clamped the near end: past it,
  `now + interval * 1000` is a time `Date` cannot express, and one runaway sheet took the whole account's freshness read
  down with a `RangeError`. `POST /library/:id/run` polls one sheet now through those same two functions -- owner or
  editor through `assertSheetEditor`, one unit of the sheet's budget through `spend(sheet_id, "runs", ...)`, and nothing
  cleared first, because the poll sets the due entry off the sheet's own interval as its first act and the tick a second
  later steps over it. It answers the `net` row the run landed, found by asking for the newest row stamped at or after a
  watermark taken off Postgres's own clock: an append, a 304's move, a repeated body's move and a quiet alert tick all
  land that way, and comparing against the newest row before the poll misread a repeat, which moves whichever row
  carried that body and not always the newest. The watermark is carried as seconds
  (`extract(epoch from
  now()::timestamp)`) and compared with `extract(epoch from created_at)` -- `net.created_at`
  carries no timezone, and a bound timestamp parameter comes back hours off the rows it was taken beside. Refused by
  name: a paused sheet (409), a sheet that is neither net-http nor alert (400), and a run that recorded nothing (409 --
  a feed with no url, an alert with no query, or a host still inside its request gap). `library:freshness` and
  `GET /library/freshness` gained `paused` (read with the same truthiness the pollers read it with, so the two cannot
  disagree about one document; null for a type with no switch and for a document that will not load) and `next_run` (the
  due map's time as an ISO string; null for a sheet the map has not reached, and null rather than a throw for a time
  `Date` cannot express). In the page both documents decode `paused` through `optionalField "paused" D.bool False`, a
  checkbox beside the digest box writes it as one patch, and a "run now" chip -- beside "test the request" on the feed,
  on its own on the alert -- goes out on `runNow` and comes back on `runLoaded`, an answer for another sheet dropped the
  way a pre-flight's is. `runLine` reads the row's own `method` to pick the shape, never the shape that happens to
  decode: a feed's fetched body may itself hold `status` and `delivery` keys. Within an alert's body it reads the
  `status` the same way -- an error run never reached a delivery and says why under `error`.
- **Audit**: one log, the `audit` table, read as `library:audit`. `record()` is the one writer. HTTP reads and writes
  land through one middleware keyed on the route patterns in `AUDITED`, after the route succeeded; the sync socket
  records `open` and a first `edit` per peer per document; MCP records `mcp <tool>`; a query records `query` on every
  sheet it resolves. `via` says which door, `public` being an anonymous reader of a public sheet. An owner or editor
  reads every row about their sheet, everybody reads their own rows, and a row with no account is `who = share link` or
  `anonymous`. A row that cannot be written fails the request it was about.
- **Marketplace**: Stripe Checkout, platform-side. Connect payouts are not wired. A listing carries a `license` from
  `LICENSES` or does not go live (the schema checks it). `POST /shop/:sell_id/report` is one row per account per listing
  on `net-hook:reports`, `POST /shop/:sell_id/review` is the operator's `keep` or `takedown`, and `GET /status` fails
  while a report is open.
- **Codex credentials**: a codex sheet keeps `DSN_KEEP` of them, current and previous; `POST /codex-db/:id` inserts
  beside the row it had and trims the rest. `GET /codex/:id` tries them newest first and falls back only when
  `cannotConnect()` says the credential never got in — postgres.js raises a `PostgresError` only for the far server's
  own answer, and only SQLSTATE 08/28/3D are that server saying the credential is not in; anything the server answered
  about the statement means the credential works and the query is wrong, and running it again under the previous one
  would answer from a connection nobody rotated to. The catch is on `codexTables()` alone, so a fault in our own mapping
  code cannot spend the rollover. `checkCodexDsn()` runs per credential before the connection and its two refusals
  (unparseable, aimed at this server's own database) are never a rollover — a self-aimed DSN quietly held up by an older
  one is a credential nobody remembers writing. `canonicalHost()` reparses the host as an `http:` host because
  `postgres:` is a non-special scheme whose host the URL parser leaves as opaque text, so `127.1`, `2130706433` and
  `[::1]` fold to the one spelling the block list holds. Every attempt lands on one `codexRun()` row whose
  `meta.rolled_over` says which credential answered and whose body is why the newer one could not connect; `POLL_OK`
  grades a rolled-over run failed although it answered, so `library:freshness` and `GET /status` say the newest
  credential is dead while the read still works. A read that spent both is the 502 it always was, its `Source` counting
  the credentials tried and naming none of them, and no refusal quotes the string, because a DSN carries its password.
  The host is checked by `assertPublicHost()`, the same literal-and-resolved check `safeFetch` runs on every hop, on a
  server whose own database is somewhere else: a server whose database is on loopback is a developer's machine, and
  there the only refusal is its own database. A query over `@codex-db:x` whose connection is dead is that refusal, never
  an empty result.
- **Exports**: `GET /export/:id.{csv,json,ndjson,md,ics,xlsx}` is one route over `EXPORTS`, so access, pagination and
  query recursion are `sheet()`'s, and a format is added by adding a row. `xlsx` is the one entry that answers bytes
  rather than text (`npm:xlsx@0.18.5`, SheetJS — a zip of XML parts is well past what we write ourselves; **written with
  and never read with**, and every advisory it carries is in the parsers no route calls). The community edition writes
  values typed off `canonicalType` (`xlsxCell`, so `pct` formats like `percentage`), a number format per column
  (`XLSX_FORMATS`) and a width from the longest value bounded by `XLSX_WIDTH_MAX`; cell styles are Pro-only, so the
  header is a row of text, and no styling dependency is worth one. A date is a number wearing a date format, computed
  off `dateMs()` — the one spelling `icsStamp` also reads, so a date-only cell and a zoneless timestamp are both UTC and
  never the server's timezone, which is what handing SheetJS a `Date` would have used. A value its column cannot hold (a
  word in a `num` column) is written as the text it is rather than coerced or dropped, and one past `XLSX_CELL_MAX`
  (Excel's own 32,767, which `XLSX.write` throws on) is refused by name: where it is and how long, never what it is. The
  workbook's sheet name is the id stripped of the characters Excel refuses and cut to `XLSX_NAME_MAX`, because Excel
  rejects the name rather than repairing it.
- **Outbound webhooks**: `POST /library/:id/webhook` names a url, which must answer a signed `ping` 2xx before it is
  registered, and that ping is where a url inside our network is refused by name. `flushWebhooks()` posts one signed
  `change` per hook per flush for every document `touched` since the last, heard as the storage's `doc-saved` and
  `doc-compacted` metrics rather than the handle's change event, because the save is what the repo debounces per
  document; the first save after a `doc-loaded` is the load itself and is not a change. A row landing on a net sheet
  touches it too. Signed with the sheet's own `hook` key, the one `GET /library/:id/hook` answers, over the receiver's
  path and the body, so one secret serves both directions; a sheet on a provider scheme cannot register one. Every
  delivery spends the sheet's budget as `webhooks`. The outcome lives on the `webhook` row; `WEBHOOK_FAILS_MAX` failures
  in a row take a hook out until its owner sets it again, a dead hook fails `GET /status` until then, the flush is
  bounded by `WEBHOOK_FLUSH_MAX`, and the url list is owner or editor only.
- **Outbound fetches**: `safeFetch` is the one door out and sends `USER_AGENT`; `assertPublicHost()` is its host check,
  by literal address and by every address the name resolves to, and the codex guard asks it the same question. It takes
  the method beside the body, and only a GET is followed through a redirect. A GET carrying a body is refused in
  `netRequest` and nowhere else -- the one place that can name the size without measuring the resolved secret. Its DNS
  answers are read one at a time: only a not-found is a fact about the host, and a resolver that failed some other way
  is a 502 that says so rather than a 400 telling the caller to check a spelling that was right. The per-host gap is the
  poller's alone: `holdHost()` in `pollNetOnce` writes `hostDue` after each poll (a paged feed's pages within one poll
  go out back to back, the way any client reads a paged answer) and on every `Retry-After`, taking the later of the two,
  so two sheets on one host take turns across cycles and the proxy can neither hold a host nor evict a hold.
- **MCP**: hand-rolled JSON-RPC 2.0 at `POST /mcp/:id` -- `initialize`, `ping`, `tools/list`, `tools/call` with
  `read_sheet`, `query_sheet`, `list_sheets`, `write_cells`, and the reading half a client browses rather than calls:
  `resources/list`, `resources/read`, `prompts/list`, `prompts/get`, which `initialize` advertises beside `tools`.
  Reachable with a JWT **or with one sheet's `scrapsheets-key`**, which is what makes an agent something you hand a key
  rather than an account. A key is minted by `POST /library/:id/secret` under one of `API_KEY_NAMES` -- `api`, or
  `api-read` the same way, same rotation, same once-only answer, and the read-only key's repro block drops the POST line
  it would be refused for. `apiKeyScope` reads both names and returns the scope beside the sheet; the middleware records
  `key_sheet`/`key_scope` on the context (unset under a JWT, which is what every guard tests) and its path list is
  `/sheet/:id`, `/openapi/:id`, `/mcp/:id`. `assertKeyWrites` is the one refusal a read-only key meets, asked wherever
  the verb actually is: the path check for `POST /sheet/:id`, and the `write_cells` tool itself, because the MCP
  endpoint is one verb in the path and many in the body. Under either key the borrowed authority stops at the one sheet:
  `mcpSheets` is the caller's library narrowed to `key_sheet` and is what both `list_sheets` and `resources/list`
  answer, `mcpSheetId` refuses a `sheet_id` argument naming another, and `query_sheet` refuses an `@ref` out of it by
  name before the load -- `scanRefs` puts the sheet of a cell ref in `ids` too, so `@other:x.col` is refused like
  `@other:x`. `resources/list` answers one `sheet://<sheet id>` per sheet it lists with mimeType `text/csv` (a sheet
  nobody named is listed under its id); `resources/read` renders that sheet through `sheet()` and the `EXPORTS` csv
  renderer, byte for byte what `GET /export/:id.csv` answers, spending the read budget; an unrecognised uri is a
  JSON-RPC `-32002` carrying the `explain()` block, and an unknown prompt name a `-32602`. `prompts/list` and
  `prompts/get` answer one prompt, `describe_sheet`, whose single user message names every column, its type and how much
  of it is blank, built from `describeRows` -- the same read `describe @ref` answers with, so the prompt and the
  statement cannot describe one sheet two ways -- and asks for a summary. **Both read the whole sheet, not a page**:
  `MCP_WHOLE_SHEET` carries the same limit `GET /export/:id.csv` passes, because `sheet()` pages a net or query sheet
  and one that stopped at the first page would answer a wrong csv and describe a wrong row count rather than a short
  one; the two are the same number on purpose. Audit rows are `mcp <tool>` for a `tools/call` and `mcp <method>` for a
  `resources/read` or a `prompts/get`, recorded against the sheet the uri or the argument named rather than the `:id` in
  the path.
- **`assertNoKeys()` scans two lists in the one pass.** `KEY_SHAPES` is refused as it always was, with no override, and
  `PII_SHAPES` -- an email address, a phone number in E.164 or the North American spelling, a US social security number,
  and a run of digits `luhn()` says is a payment card -- is refused by `POST /library/:id/public` and a priced
  `POST
  /sell/:id` unless the body carries `personal: true`, which `claimsPersonal()` reads and refuses when it is not
  a boolean, so a sheet does not go out on the word "no"; both refusals name the column and the 1-based row and never
  the value, both run under the one `KEY_SCAN_CELLS`/`KEY_SCAN_BYTES` pair, and the card is the one shape carrying a
  checksum rather than a spelling -- matched whole and checked once, which misses a card padded with a junk digit and
  holds the false-positive rate that misses buys. In the page the claim is a second checkbox under the public one,
  `SharePersonal` holding it in `model.share` and the next `SharePublic` sending it through `shareAction` as `personal`,
  which `index.html`'s public branch forwards. **`library:lineage` is what feeds a sheet**: one row per (`sheet_id`,
  `name`, `type`, `depends_on`, `depends_on_name`, `depends_on_type`) for every query, alert and chart sheet the caller
  has a `sheet_usr` row on, the code read from the live automerge document (`data[0].code`, or `chartSql(data[0])` for a
  chart) and never `sheet.row_0`, handed to `scanRefs` and deduped so a self-join is one edge, `depends_on_name` read
  only off the sheets the caller holds a role on, and a document that will not load a row whose `depends_on` is null and
  whose `name` carries the refusal. It answers whole rather than paged the way a dashboard's tiles do, bounded twice --
  `USER_SHEETS_MAX` on the documents it opens and `MAX_QUERY_ROWS` on the edges they name, because nothing caps the refs
  one sheet's code holds -- and it is registered where `library:freshness` is: the early return in `sheet()`,
  `assertNoKeys`'s skip list, and `GET /library/lineage`.

## Query engine (`src/sql.mjs`)

Shared by both engines. `planQuery()` runs the pre-engine passes in the one order that works.

- **Refs**: `@type:doc_id` is a sheet, `@type:doc_id.column` is a cell (one value from a one-row sheet, rewritten to a
  scalar subquery). `scanRefs()` is the one scanner; `checkRefPath` bounds depth and reports a cycle as the path that
  closes it.
- **Windows**: AlaSQL parses `over (…)` and computes it wrong, so `rewriteWindows()` lifts each one out of the top-level
  select list and `applyWindows()` computes it over the returned rows. `qualify` rides the same pass, which is what
  makes an as-of join one statement. A window that is not a select item of its own is refused by name.
- **Unpivot** is ours (AlaSQL drops the columns it is not unpivoting); **pivot** is AlaSQL's, guarded by `checkPivot()`.
- **Extremes**: `rewriteExtremes()` runs first, before either other rewrite, and aims `min(x)`/`max(x)` at
  `min_text`/`max_text` when `x` is a bare column every loaded sheet types as text — `TEXT_TYPES`, which is every type
  whose cells reach the engine as a string, `date` and `timestamp` among them: nothing coerces a date column, so AlaSQL
  is handed ISO text and drops it like any other. It always writes the lowercase name, because `register()` defines
  `min_text` and `MIN_TEXT` and nothing between. Four things it leaves alone: a call followed by `over`, which is a
  window `applyWindows` computes itself and `rewriteWindows` finds by name; an expression; a name typed two ways; and
  **a name the query aliases into being** (`select min(a) from (select n as a …)`), because scope is not something a
  regex can see and that one answered `"10"` for a minimum of 9. All of them land on `checkResultColumns()`. It stops at
  the first unbalanced bracket rather than scanning to the end for every call after it, and `MAX_EXTREMES` bounds the
  calls in one statement — without both, a body of nothing but `min(` was quadratic.
- **`describe @ref`** is intercepted before the engine in both engines, and is the one statement that still answers on a
  sheet whose cells fail the type check. **`explain <query>`** is the other intercepted statement: it runs the query
  with every guard and answers one row per stage (`load @ref`, `plan`, `engine`, `windows`, `total`) with rows in, rows
  out and milliseconds. `timed()` wraps the calls both hosts already make and does nothing on a plain run.
- **A column name this engine generates is quoted, because a name is not always an identifier.** `total`, `store` and
  `class` are AlaSQL keywords that will not parse bare, so `chartIdent` answers `[name]` and every site `chartSql`
  splices one into takes it quoted -- the box branch's `min`, `max` and `percentile` arguments, its
  `where <y> is not null` and its `group by`, the `plot` string and both returns, while the `order by` ordinals stay the
  counted numbers they were. It still refuses a non-string and a name that is not identifier-shaped, which is what keeps
  a `]` out of a bracket and the statement well formed; this quotes what a chart may name and does not widen it.
  `bare()` is the one place a bracket is read back off a name, and it sits above `itemType` because every pass that
  reads a name out of the query's own text asks it: `itemType` and `selectTypes`'s bare-name fallback, which would
  otherwise type a quoted column as nothing and cost every chart its result types; `rewriteUnpivot`; and
  `rewriteExtremes`, which is why `min([total])` is aimed at `min_text` exactly as `min(total)` is. `checkResultColumns`
  needs nothing -- every chart column is aliased. `rewriteUnpivot`'s value column and name column are held to the same
  `/^[A-Za-z_][A-Za-z0-9_]*$/` shape as its in-list, so all three names in one clause obey the one rule and the refusal
  names which of the two is wrong.
- **Types**: `COLUMN_TYPES` is every type a column may declare and what each one is; `NUMERIC_TYPES` is derived from it
  and `knownType()` matches the `enum:` family by prefix. `checkColumnTypes()` is the one place a cell becomes what its
  column says — a blank becomes `null`, a numeric string becomes its number. `selectTypes()` types a result column off
  its select item, not off its name, and `WINDOW_TYPES` says the same thing about a window: `sum` and `avg` follow their
  argument in both, so one name cannot mean two types.
- **Fits**: `fit_exponential()` and `fit_power()` are a log and a line, through the shared `curve()`. `fit_hyperbolic()`
  is the one that is not: Arps decline by Levenberg-Marquardt, bounded by `HYPERBOLIC_STEPS`, over points scaled into a
  unit box so the same curve in barrels and in unix seconds is one search, holding `b` at whichever end of `(0, B_MAX]`
  the fit wants to leave rather than crawling there, and refusing the fit it settled on — never a trial step — when it
  stops at `B_MAX` and still wants a flatter tail. Bounded by `HYPERBOLIC_POINTS` as well: the cost is points times
  steps, and only the steps were counted.
- **Regression over more than one column.** `ols(array(y), array(x1), ...)` fits a plane by the normal equations and
  answers the coefficient array `[b0, b1, ...]`; `ols_predict(coefs, at1, ...)` reads one row back off it, which is what
  puts a residual on every row through the join in `query:yield-response`. Every predictor is scaled to a
  root-mean-square of one and the equations are divided through by their total weight, so Cauchy-Schwarz holds every
  entry in [-1, 1] and `OLS_SINGULAR` reads the same whatever units a sheet holds; a column's scale is taken off its
  largest magnitude before anything is squared, because squaring a raw value overflows and underflows long before the
  column does, and a column that overflowed reached the solve as zeros and was refused as a repeat. `gauss()` is the
  Gaussian elimination with partial pivoting both fits solve through, and it carries back the column it stopped on, so a
  zero pivot names the argument the columns before it already span and only a system with no column to name is refused
  as singular. `design()` is the one validator: arrays of one length, finite numbers, `OLS_POINTS` points, `OLS_TERMS`
  predictors, one point per coefficient. `logit(array(y), array(x1), ...)` is the same equations reweighted by p(1-p)
  and solved again, bounded by `LOGIT_STEPS` with the counter in the refusal, and its `y` refused by value unless every
  one is 0 or 1. Its first step weights every point the same -- the coefficients start at zero, so every fitted
  probability is a half -- which makes that step's matrix the one `ols()` solves: a singular matrix there is a column
  another column reproduces and is named, and a singular one after it is the fit running away. A boundary that separates
  the two outcomes -- every residual under `LOGIT_FIT`, or the weights collapsing until the reweighted matrix goes
  singular -- is one named refusal rather than coefficients that ran away. `logit_predict(coefs, at...)` is that fit as
  a probability, and both predicts refuse a first argument that is not a fit's own array by name, because the array()
  advice every other UDF gives is the wrong fix here. `SELECT_TYPES` types `ols` and `logit` `json` and both predicts
  `num`.
- **Samplers**: `sample_uniform`, `sample_normal` and `sample_triangular` put a distribution on an input, and
  `percentile()` reads the spread back out. Nothing calls `Math.random`: each call builds its own mulberry32 stream,
  seeded by an FNV-1a hash of the whole call with the function's own name in it, so the same call answers the same
  number on the server and in the page, a different seed or a different distribution answers a different one, and one
  `trial` column draws an independent value per distribution. Each refuses a non-number, a low bound above the high one,
  a mode outside the range and a negative spread, and each refuses **the draw itself** when it is not finite: two finite
  bounds far enough apart overflow a double before the fraction is applied, and zero times that Infinity is a NaN, which
  a percentile and every number downstream would carry silently. Only the normal draw reads `Math.log` and `Math.cos`,
  which ECMAScript leaves implementation-defined where it pins `sqrt`, so a normal draw is bit-equal wherever one
  runtime runs both hosts and may differ in the last ulp between a browser and the server. `table:trials` is the column
  of trial numbers they are demonstrated over, in `query:monte-carlo-margin`.
- **Guards**: `checkQueryRows()` caps rows loaded across every `@sheet`; `checkJoinRows()` caps the product of the from
  clause's row counts at `MAX_JOIN_ROWS`, every occurrence counted so a self-join multiplies, because the engine walks
  every pair before a where clause and cannot be stopped once it starts — it is the pairs walked, not the rows kept, so
  a keyed join over big sheets pays it too; `checkResultColumns()` turns AlaSQL's silent undefined column into an error;
  `nearest()` backs every "did you mean".
- **AlaSQL gotchas**: a `group by` expression is evaluated against an empty row, so a UDF named there gets nothing — bin
  in a subquery first. An exception thrown from a function inside a from-clause subquery is discarded;
  `formatQueryError()` replaces the message that destroys. `min()`/`max()` drop text and the compiler never consults
  `alasql.aggr` for those two names, so `rewriteExtremes()` renames the call rather than replacing the function —
  patching `src/alasql.mjs` is not an option, `deno task vendor` rebuilds it. `min_text()`/`max_text()` stay the escape
  hatch for an argument the pass cannot resolve. `total`, `store` and `class` will not parse as identifiers.

## Frontend map (`src/Main.elm`, `src/index.html`)

- **Architecture**: Elm Architecture. `update` is one exhaustive `case` with **no wildcard**, so a new `Msg` fails to
  compile rather than compiling and doing nothing. Its four long branches are `updateDocMsg`, `updateKeyDown`,
  `updatePaste` and `updateShareLoad`.
- **`Doc`**: `Library`, `Shop`, `Tab`, `Query`, `NetHook`, `NetHttp`, `Alert`, `Chart`, `Dashboard`, `NetSocket`. Every
  remaining server type decodes to `Unviewable typ`; give one a real view by replacing its branch in `docDecoder`.
- **Flags**: `{ api, tutorial }`. A missing `api` lands in `model.error` rather than defaulting.
- **Library**: `library()` in `src/page.mjs` merges what this browser stored under everything bundled. System ids skip
  `repo.find`. `viewGallery` reads `model.library`, so a new demo needs no code change. `seen` is stamped by `selectDoc`
  for a sheet the library lists and is the library's `opened` column; it and `trashed` are the two stored fields that
  survive a system entry in the merge, because both are this browser's fact about somebody else's sheet.
  `libraryIdAtRow` reads a row's sheet off the rows as drawn — sorted, filtered, searched — never off the dictionary's
  order.
- **Trash and restore** ride `updateLibrary`, which is the one port that writes this browser's facts about a sheet.
  `Library.set` in `index.html` drops a **null** field out of the patch rather than out of the entry, so restoring
  writes `trashed = False` and never `Nothing`. Trashing asks nothing first — being undoable is the point — and
  `deleteDoc` stays the purge, which is the one that also calls `Views.drop`, so a restored sheet keeps the arrangement
  it had. A trashed sheet is out of the library table, the demo strip and `paletteCommands`; `model.trash` swaps the
  last library column between `Trash` and `Restore` + `Delete`, and hides the footer's new-sheet rows.
- **The star and the trash are one kind of fact: this browser's, about somebody else's sheet.** `starred` joins `seen`
  and `trashed` as the third stored field that survives a system entry in `library()` (`src/page.mjs`), so starring a
  bundled demo outlives the next merge; the three are one list there, overlaid only when truthy, because a stored
  `false` is the absence a bundled entry already carries. `updateLibrary` is still the one port that writes them --
  `Library.set` in `index.html` drops a null field out of the patch, so a fourth field cost it nothing. `Star` is a
  `Type` the way `Trash` is: a `spec` entry, a `seriesEncoder` branch and a `cellDecoder` branch, and nothing in
  `columnTypes`, because no document declares it; its cell carries both halves (`{id, on}`), so the toggle never reads
  its own state off the glyph on screen. `resolveTable`'s Library branch puts starred sheets first **only while
  `sheet.sort` is empty**, and `paletteCommands` sorts its sheet entries the same way: Elm's sorts are stable, so a
  pre-sort left in place under a chosen sort survives as that sort's tie-break -- a header click losing to an order
  nobody asked it to keep. `TrashSelected` is a top-level `Msg` and not a `DocMsg`, because `updateDocMsg` refuses every
  `DocMsg` on the library: it reads the sheet ids off the drawn rows through `libraryIdAtRow` per selected y -- which
  answers `Nothing` for a header row or one past the end, so no y needs filtering out first -- and fans out one
  `updateLibrary` per sheet through `Cmd.batch`. It refuses by name twice: a sheet that is not the library, and a
  selection holding no library row.
- **Cross-sheet queries in the browser**: `sheets(alasql, shelf, find)` in `src/page.mjs`. Only two things come from the
  browser and both are arguments: the library map and `repo.find`.
- **The editor completes a sheet id off the library and a column name off the sheet.** `completionTrigger` is the `@…`
  the cursor sits in, `completionRef` is what a dot after the ref says -- that the question has moved from which sheet
  to which of its columns, and only for the two prefixes a query may reference at all -- and `completionAt` answers
  both. A sheet id needs nothing from the glue; a column name needs everything, because the columns live behind the
  automerge repo in the `sheets()` closure, which is why `columnsFor`/`columnsLoaded` is a port. It is answered by
  running **`describe @<ref>` through the page's own engine**: the statement the typist would have run themselves, so a
  suggestion cannot name a column the query is then refused for. A ref that will not read answers no columns rather than
  an error, and the empty answer is cached too, or the editor asks again on every keystroke -- but it is **logged by
  name** first, because `describe` on a query ref _runs_ that query, so a ref cycle, a join over the row cap and a sheet
  nobody has synced all land in one catch and only the console can say which. `ColumnsLoad` **recomputes the open
  list**, because the columns are asked for by the very keystroke that would have shown them and waiting would put every
  completion one character late; that is what makes a `QueryAutocomplete` holding nothing legal, so the view draws
  nothing for one and `AutocompleteNav` guards its `modBy`, which is a runtime error at zero. The dropdown carries
  `id="complete"` and `src/index.html`'s keydown finds it by that: it used to look for `[style*="z-index: 100"]`, which
  is also the column filter panel, so a panel left open anywhere swallowed the editor's arrow keys.
- **One CSV import, whichever way the file arrives, in two steps.** The footer's file input goes through Elm's
  `CsvImportFile` and the `importCsv` port; a file dropped on the page is read by `setupDragDrop` and handed to the same
  `uploadCsv`. That posts the file to `POST /import/preview`, lays the types this browser settled on for the same header
  last time (`scrapsheets-imports`, through `rememberedTypes` in `src/page.mjs`) over the server's guesses, and hands
  the preview to Elm on `importPreviewed`; `viewImport` shows a select per column and the first rows. `ImportConfirm`
  sends the settled types on `importConfirm`, and the page posts the kept file to `POST /import/csv?types=…`, remembers
  the types by header, and opens the sheet its answer names. The server parses both times through `readImport`, because
  it is the one that registers the sheet, syncs it and coerces the values — a settled type the values do not fit is
  refused on the line that does not. `parseCsv` in `Main.elm` is a different job: the clipboard.
- **Import**: `readImport()` is the one CSV reader; `POST /import/preview` answers its columns, types and first rows
  without a sheet, and `POST /import/csv` makes one. `?types=` is a JSON object keyed by column name, checked against
  `CANONICAL_TYPES` and the file's header before the file is read.
- **Pre-flight**: the "test the request" button on a net-http sheet sends
  `preflight { id, url, headers, method, body }`; the page posts it to `POST /library/:id/preflight`, which runs the
  poller's own request once and writes nothing, and the answer or the refusal comes back on `preflightLoaded` by sheet
  id, held in `sheet.preflight` and drawn by `viewPreflight`. An answer for another sheet is dropped by id.
- **A feed's paging is set where the feed is.** `page_by`, `page_param` and `page_path` live in the net-http document's
  `data[0]` beside `url`, `method` and `body`, decoded through `optionalField` the way `body` is, and `viewNetHttp` is
  where they are chosen: a select over `"" :: pageBy` whose empty option is one request a poll, a page parameter for
  `page`/`offset`/`cursor`, a cursor path for `cursor` alone, and a hint under them saying what that mode sends and
  where it stops. `pageForm` is the one table of what each mode takes, so a fifth mode cannot gain a hint and lose its
  input. `pageBy` is the copy of `PAGE_BY` the language boundary forces, and `pageByDecoder` refuses a mode outside it
  exactly as `methodDecoder` refuses a verb: a select drawn empty over a document holding a mode the page does not know
  is a lie about what the poller asks for. `page_param` and `page_path` are plain strings the decoder does not check,
  because each keystroke writes the document through `InputChange NetPageParam`/`NetPagePath` and a half-typed name
  would otherwise refuse to decode the sheet the typist is looking at; the poller's own regexes are the check, hinted at
  in each input's title. Pre-flight is unchanged and stays one request.
- **What a feed keeps is set where the feed is, too.** `mode`, `key` and `rows_path` live in the net-http document's
  `data[0]` beside `page_by`, decoded through `optionalField` the same way, and `viewNetHttp` draws a select over
  `"" ::
  netModes` whose empty option is append, a `key` input under `upsert` alone, a rows-path input whatever the
  mode, and a hint per mode. `storeForm` is the one table of what each mode takes, the way `pageForm` is; `netModes` is
  the copy of `NET_MODES` the language boundary forces and `netModeDecoder` refuses a mode outside it exactly as
  `pageByDecoder` does; `browser_test.ts` fails when the two lists drift. `key` and `rows_path` are plain strings the
  decoder does not check, for the reason `page_param` and `page_path` are: each keystroke writes the document, and the
  poller's own regexes are the check, hinted at in each input's title. The net-http branch of `docDecoder` is a `D.map2`
  over a `D.map5` and a `D.map6` -- eleven fields are past `D.map8`, and the request is decoded beside what is done with
  its answer.
- **Feed health**: `library:freshness` is read by `index.html` and handed to Elm through `freshnessLoaded`. The
  `freshness` column appears only when the answer is non-empty — a blank column over a logged-out library would read as
  "nothing is wrong".
- **Table UX**: multi-column sort, column hide (keeps its x coordinate; `skipHidden` steps over it), drag-resize,
  drag-reorder (columns, and rows while the table is in document order), pin, row insert/duplicate/fill-down,
  find/replace, undo/redo, command palette (Ctrl/⌘+K), shortcut sheet (Ctrl/⌘+/). `shortcutGroups` carries the `Msg`
  each key runs and `paletteCommands` reads that list, so the two cannot drift. The palette opens with **nothing**
  selected (`selected = -1`, which `PaletteRun` refuses): it opened on the first row, and Enter on a palette nobody had
  pointed at ran whatever that row happened to be — a verb that deletes rows. A query renumbers the matches and selects
  the first again. The export chips are one `List.map` over the formats a table offers — csv and xlsx — the way the
  chart's chip is over svg and png; each is a plain link to `/export/<sheet id>.<format>`, no `Msg` and no port, and the
  server's `EXPORTS` is what decides whether the link answers.
- **Fill-down continues a series.** `fillSeries` is the one rule: the selection's leading run of filled cells is the
  seeds, dates step by the days or the whole months between the last pair, numbers continue their step, text ending in
  digits counts those digits up, and anything else repeats the last seed. Blank is `blankCell`'s answer and not a
  trimmed `cellText`, because a JSON null reads as the word "NULL" and an imported CSV writes one for every gap.
  `seriesEncoder` is what a filled cell is written as, and it is exhaustive on the column type: text, date and timestamp
  stay text, a numeric column gets a JSON number, and a column a series does not belong in — bool, json — repeats the
  raw value the document already held rather than a rendering of it. A date is a series: the calendar is
  `justinmimbs/date` and never arithmetic on the text, which counted January on to a 32nd day. `parseDay` is the one
  thing that says a seed is a date, in `fillSeries` and at the fill-down site both, and it stays hand-rolled off
  `civilDays`: `Date.fromIsoString` reads a bare year as January 1st, which would make a num column of years a date
  series. Every seed a date is a date series; two dates step by the months between them when the later one is the
  earlier plus whole months on the same day of the month (what `Date.add Months n` gives back exactly), and by the days
  between them otherwise; one date steps by a day, which is why the caller's `enough` asks `parseDay` before it asks for
  two seeds — one number still repeats. Every value is `Date.add` off the **last seed** times i, never a walk from the
  value before it: 2026-01-31 by a month is 02-28, 03-31, 04-30, and a cumulative walk clamps to the 28th for good. What
  comes back is an ISO day, then whatever the last seed carried after its first ten characters, so a timestamp column
  keeps its time of day. "Anything else" is also every value a float cannot carry: a counter past fifteen digits, a step
  that overflowed, a precision past `maxDecimals`. Each of those wrote a wrong number into the document rather than
  repeating.
- **The column's panel is where its cells are cleaned; the palette is where the sheet is.** Trim, UPPER, lower and
  drop-blank-rows sit under Hide and Pin, and `SheetRowsDedupe` — which reads every cell of every row, so no column's
  panel can own it — is in `shortcutGroups` and therefore in the palette. Each one is a `DocMsg`, so undo, the viewer
  refusal and the sync path are the ones already written. `cellRewrites()` emits nothing for a cell the change does not
  move and skips one that is not text; `blankCell()` says what blank means and `blankRows()` and `duplicateRows()` both
  ask it; `rowDeletions()` is the splice pair all three share with `SheetRowDelete`. `duplicateRows()` signs a row off
  the cells it holds, never off the columns `data[0]` names, so a cell nothing names still separates two rows. They read
  every row the document holds, not the rows on screen, and reach a table only — the panel asks `movable`,
  `updateDocMsg` answers every other sheet with the computed-cell refusal, and the library, which is a listing and not a
  document, has a refusal of its own.
- **A column is split where it is cleaned.** The panel's "split on" box and its Split button sit under the cleaning
  verbs behind the same `movable` gate. The box writes `sheet.splitOn` through `ColumnSplitInput` on every keystroke and
  never reaches the document at all -- unlike the filter box beside it, which is an arrangement and lands on the column
  when the panel closes, this is an argument to a verb nobody has run yet, and a patch per keystroke would be a sync per
  keystroke for everybody watching. `SheetColumnSplit key delimiter` is the `DocMsg` and `columnSplit()` is the patch
  pair: one `push` on `data[0]` of a column per part position, named `<name> 1` .. `<name> n` off the widest split,
  typed text and keyed past every number the sheet's own keys spell -- a sheet that has had a column deleted carries a
  key at its own length, so the position the undo splices and the key the cells are written under are two different
  numbers -- then one `set` per row per part, so a row with fewer parts leaves its later cells unwritten, a cell that is
  not text has nothing to split, and the column split from stays where it is. `String.split` matches the characters it
  is given, so a `.` is a dot and a `|` is a bar. The undo is the mirror `SheetColumnDelete`'s restore is: a `del` per
  cell written, then one `splice` taking the pushed columns back off. Every way it cannot run is one refusal in
  `model.error` through `updateDocMsg`'s `writeRefusal` with nothing written at all -- an empty delimiter, a key the
  sheet does not carry, a column with no text cell, a delimiter no cell holds, a split past `maxSplitColumns`, and a
  name `nameClash` says is taken, because half a split is exactly the two-columns-of-one-name shape every keyed read
  refuses.
- **A near-duplicate row is found where the column is cleaned, and previewed before it is taken.** Exact dedupe is
  `SheetRowsDedupe` over every cell of every row, so it lives in the palette; this one compares text, so it lives in the
  column's own panel behind the same `movable` gate, with a "near %" box holding `sheet.near` through `ColumnNearInput`
  the way `splitOn` is held. `similarity` and `soundex` in `src/Main.elm` are the language-boundary copies of the two
  UDFs of those names in `src/sql.mjs`; `main_test.ts` and `tests/MainTest.elm` assert the **same pairs**, so a drift
  fails a test rather than quietly putting two rows a different distance apart in the panel than in a query. `soundex`
  buckets and `similarity` scores: comparing every pair is quadratic and no browser can do that to a real sheet, and it
  is also a rule -- two names whose consonants differ are not near each other however many trigrams they share.
  `nearDuplicates` answers the row that goes, the row it matched and how close, compared against the rows that **stay**
  so three spellings of one name collapse to the first rather than to a chain; an exact repeat is left to the other
  verb, and `firstNear` walks a bucket rather than filtering it, because only the first match is used and a filter reads
  the whole bucket to find it. **`maxFuzzyPairs` is the bound that matters, not `maxFuzzyRows`**: `soundex` keeps only
  letters, so a column of invoice numbers or zip codes held as text codes to `""` on every row and the whole sheet is
  one bucket -- which is the commonest column anybody points this at. Five thousand such rows is twelve million
  comparisons and tens of seconds of a frozen tab, and since the preview is drawn from `view` it is tens of seconds
  **per keystroke** in the closeness box. So the comparisons are counted, nothing more is compared once the bound is
  passed, and the refusal names the count and says what makes a column sound alike on every row. It answers a `Result`,
  because the panel draws its refusal as the sentence `updateDocMsg` would answer with -- a closeness outside 1 to 100,
  a sheet past `maxFuzzyRows` with the count in the message, a column with no text, and nothing near enough, which is a
  verb with nothing to do rather than a button that deletes nothing and says nothing. The preview reads `sheet.doc` and
  not `sheet.table`: a table sheet's rows are its document, which is where `updateDocMsg` reads them, so the preview and
  the verb cannot be looking at two different sheets. It also **counts the rows it could not read** -- a cell holding
  something that is not text, which a column retyped to `text` keeps -- the way `viewChartSettings` counts what a fold
  swallowed: a preview saying one row would go while it never looked at half the sheet is a preview lying about the
  sheet. The delete itself is `rowDeletions`, so undo, the viewer refusal and the sync path are the ones already
  written.
- **`formatNumber` is the one place a number becomes text.** The cell, the stats row and the totals row all go through
  it; they used to format independently and a `usd` column's total came out without its `$`. A value `positional` says
  is not written as digits — not finite, or a magnitude JavaScript writes as `1e+21` — skips the currency, the grouping
  and the format, because all of them cut the string by position: an overflowed total read `$In,fin,ity.00`. It takes
  the column's `decimals` and its `format`, and `digitsOf` is where the two meet: absent on both, each type reads the
  way it always did; `fixed` writes exactly the count asked for and `maxDecimals` bounds it; `groupWhole` puts a
  separator every three digits of the whole part, holding the sign and the fraction back from `commas`, which counts
  from the right over whatever it is handed and made `-123` into `-,123` — `usd` asks it too, because money is the
  column that always asked for it; and `scientific` writes a mantissa and a signed exponent, because Elm has no
  `toExponential`: the exponent is `floor (logBase 10 (abs v))`, `descaled` divides it out in two halves because
  `10 ^
  -324` underflows to 0 in one, zero is answered by name, a mantissa with no count is rounded to twelve
  significant digits so the division's own noise is not written, and two digits before the point mean the exponent was
  one too low — `fixed` rounds 9.99 at one place up to "10.0", and `logBase` divides two logs, so 1000 comes back at an
  exponent of 2 and a mantissa of exactly ten. A format lands **on top of** the type and never instead of it: `grouped`
  on a usd column asks for what `usd` already writes, `scientific` on one keeps the symbol with the sign outside it, and
  a percentage keeps its sign whichever way its digits read. `NumberFormat` is the list of ways digits are written —
  `formatSpec` is the one table of name and label and `numberFormat` the only reader of the word a document stores — the
  way `spec` is the list of column types; a word outside the list is a column nobody formatted rather than an error,
  because losing the reading must never cost the numbers. `fixed` scales the magnitude's fraction and not the whole
  value, so a half rounds away from zero on both sides of it and 1e11 at ten places is still digits, and it takes its
  sign off the digits it writes, so -0.4 at zero places is `0` and not `-0`. `fixed` is also what `fillSeries` writes a
  step with.
- **The arrangement is offered where it is kept.** `arrangeControls` is the one predicate `viewHeaderCell` asks: a table
  and a query, because the arrangement is kept; the library and the shop, because their order is how you read a listing
  this app builds and there is no document under it. Everything else is a feed — its rows are a run log, and a sort that
  worked and then forgot read as a bug in saving.
- **The arrangement is stored on the columns, in two homes.** Sort, filter, hidden, pinned, width, decimals and format
  live in `data[0]` as `sort`/`rank`, `filter`, `hidden`, `pinned`, `width`, `decimals`, `format`, so they survive a
  reload and travel with a share. `viewDecoder` reads them on `DocSelect` and `arrange` writes them, diffed against
  `sheet.storedView` so closing an untouched filter panel writes nothing. The panel's two typed fields — the filter box
  and the decimal count — write the model on every keystroke and reach the document when the panel closes: a patch per
  character is a sync per character for everybody watching, and typing "10" meant a 1 nobody chose. The format select
  has no keystrokes to hold back, so one click is one `arrange`. A format spelled in a way nobody wrote is no format,
  the way an out-of-range count is no count and an unusable width is no width — the arrangement is how you were reading
  the rows, and losing it must never cost you the rows. `colViewFields` is at `D.map8` now, which is the ceiling: a
  ninth view field needs `andThen` rather than another `map`. It goes around `updateDocMsg`: a resize is not data and
  does not belong on the undo stack. `tableHome` and `queryHome` are the two addresses — a table's `data[0]` is the
  column list, so the address is the position; a query's is one object, so the fields live under `view`, keyed by column
  name the way its `cols` overrides are. `arrangeable` is the one `case` that picks, and `pruneView` is a table only,
  deliberately.
- **Reorder is a splice, pin is a sum.** A move is one `move` patch — on `data[0]` for a column, on `data` for a row —
  applied by `applyPatches` in `index.html` to the value the document already holds — rows are keyed by `col.key`, so no
  cell moves and the display index stays the document index. It is a `DocMsg` and not an arrangement: everyone looking
  at the sheet sees the new order, so it rides `changeDoc`, `movePatch` the other way is its own undo entry, and a
  viewer's is refused like any edit. `dropOf` is the one place a drop becomes a `DocMsg`. A row's handle is drawn in the
  first data cell only while `inDocumentOrder` holds — nothing sorted, filtered or searched — because a display row then
  _is_ the document row, and `MoveEnd` asks it again so a sort taken mid-drag is refused; the `.grab` glyph is CSS so a
  cell's text stays its value. `pinLeft` sums the widths of the sticky columns before each pinned one and hands the
  answer to `.pin` inline; column 0 is in the sum whether or not anybody pinned it, because `.c0` sticks it regardless.
  Pinning writes `autoColWidth` for any sticky column that sizes itself — the one pinned and column 0 both — because a
  guessed sum either leaves a gap the rows scroll through or slides the pinned column underneath column 0.
- **`arrange` moves `storedView` before the page has done anything, so the page must never drop a batch.** There is
  nothing to roll back to: `storedView` is what _this browser wrote_, not what the document holds, and re-reading the
  document to recover it is exactly what would write a deletion over a collaborator's sort. So `arrangeDoc` in
  `index.html` writes the document first and the browser store last, `Views` keeps what it was given in memory so a full
  store loses nothing, and a missing automerge handle is a named refusal rather than a no-op.
- **A sheet whose document cannot hold the arrangement keeps it in this browser.** `arrangeDoc` is a port of its own so
  that the page knows a batch is only view fields without inspecting a path. `Views` in `index.html` holds those patches
  under `scrapsheets-views` and merges them back in `selectDoc`; `foldView` and `mergeView` in `src/page.mjs` are the
  two halves a test can reach. **Held by `col.key`, never by position** — a table's patch addresses `data[0][x]` and
  `foldView` resolves x against the document's own `data[0]`, because the whole reason a view is held is that somebody
  else owns the document and can reorder it. A held key the document no longer carries is dropped, not created. Two
  sheets need this: one that ships bundled, and one the sync server has refused a write on — which is heard by wrapping
  the ws adapter's `receiveMessage`, since the vendored adapter logs the server's `type: "error"` frame at a debug
  namespace and emits nothing. The same wrapper hears the opposite answer: while an arrangement write is `pending`, a
  `sync` frame whose heads cover the head this browser wrote means the document took it, and `Views.drop` forgets the
  held view. A refusal is remembered per open (`selectDoc` clears it), so a grant taken mid-session is tried on the next
  open. `DocHandle.heads()` is base58 and the wire is hex; `decodeHeads` bridges them, and `main_test.ts` pins that the
  server's reply carries the head at all.
- **Installable**: `src/manifest.webmanifest` and the `src/icon.svg` it names are copied to `dist` like any other `src`
  file and listed in `src/_redirects`; the icon is named by the manifest rather than by `index.html`, so
  `browser_test.ts` checks it by hand.
- **Offline**: `src/sw.js` is the service worker, copied to `dist` like any other `src` file, listed in
  `src/_redirects`, and registered at the end of boot in `index.html` by `navigator.serviceWorker?.register("/sw.js")` —
  a page with no `serviceWorker` (jsdom, plain http) takes the short circuit and a refused registration is logged by
  name rather than taking the boot down, because the app works without one. It answers same-origin GETs network first
  and the cache second: no filename here carries a build hash, so there is nothing for a cache name to key on and
  nothing but the network that knows a copy is still the deployed one; a 200 for a `SHELL` path is written back, so a
  deploy replaces the shell on the next online open and no cache name is ever bumped. `SHELL` is every path `_redirects`
  serves as itself plus `/`, pre-cached by `addAll` on install — which refuses the whole install on one 404,
  deliberately — and it is also the whole of what is cached: `answer()` reads and writes by pathname and never by url,
  so a share link's query string is not a second entry and the cache cannot grow past the list. Offline a cached path
  answers itself, a navigation with nothing cached answers the cached `/` the way the `/*` catch-all does online, and a
  path with neither is a named refusal rather than a silent failure. Nothing cross-origin is answered at all — the API
  and the sync socket are another origin, and the handler returns without calling `respondWith`, which leaves the
  browser doing what it did before there was a worker. `browser_test.ts` fails when `SHELL` and `_redirects` drift in
  either direction and lints `sw.js` through `unresolved()`, the same scope analysis `index.html`'s module script gets;
  `glue_test.ts` runs the worker over a hand-made `self`, `caches` and `fetch`, which is the only way to take the
  network away. Offline means the shell opens: the data still needs the network, and what a document already synced is
  in IndexedDB.
- **A chart may plot more than one thing.** `chartSql` takes `series` beside `x` and `y`: blank or absent is the
  statement it always built (`select <x> as x, <y> as y from <source> order by 1`), and a named one appends
  `, <col> as
  series` and orders `by 3, 1`, so each series arrives whole and in draw order. The name goes through the
  same `chartIdent` the axes do, so a series held as a number -- or as a null somebody hand-wrote into the document --
  is refused by name, while an emptied box is simply no series. `Chart` in `main.ts` carries `series?: string` beside
  `y2?: string` and `annotations?`, so `GET /sheet`, every export and every MCP read answer the split rows through the
  one `sheet()` path. In `src/Main.elm` the `Chart` doc's settings are `Chart_`, named the way `Query_` is because four
  readers spell them; a `ChartSeries` `InputChange` writes the series from the "split by" input in `viewChartSettings`,
  and `chartPoints` -- which takes the row key to read, `"y"` or `"y2"` -- answers
  `List ( String, List ( String, Float )
  )` -- the rows grouped by their `series` cell in first-appearance order, a
  row whose y is not a number dropped from its own series alone, and a sheet with no series column read as one series
  with no name. **Both the label and the series cell go through the lenient `string` decoder, never `D.string`**:
  nothing coerces a chart's columns, so an `int` column arrives as a JSON number (`chart:compa-ratio` plots one on x),
  and read as a blank every row of it shares the one label and the whole chart stacks on a single point. `viewChart`
  plots against the distinct x labels rather than the row positions, so a chart's source must answer **one row per
  (series, x)** -- `examples_test.ts` runs every bundled chart's own statement in both engines and refuses a repeated
  pair, and the fix is a `group by` in the query -- and it draws one polyline, polygon or dot set per series coloured
  from `chartColours` cycled by index, stacks a bar's series on each label so the axis spans the stacked sums, shows the
  first series in a kpi and says which one, and draws a legend inside the same viewBox `downloadChart` clones whenever
  any series is named. `chartColours`'s first entry is the `#468` every chart drew in and an unsplit chart draws no
  legend, so a chart with nothing to split by is the picture it always was, rect for rect. `chart:cohort-curves` is the
  bundled multi-series chart: `@query:cohort-retention`, a line per cohort over the months since its first order.
- **A second column may have a scale of its own.** `y2` rides beside `y` through the same `chartIdent`, selected as one
  more column of the same statement (`order by` counts the series' position rather than typing it, because a series is
  the third column of a one-scale chart and the fourth of a two-scale one). It is refused by name on a `kpi`, which
  draws one number. In the page it is its own list all the way down -- `chartPoints "y2" tbl`, its own
  `top`/`bottom`/`span`/`plotY2`, its own `chartRuns` -- because folding it into the first scale's extent is what a
  second axis exists to avoid, and it is **always drawn as a dashed line whatever the kind**, so a reader never has to
  ask which shape belongs to which axis. Its labels sit at x 796 anchored end, its colours carry on from where the first
  scale's left off so the legend index and the colour on screen are one number, and with two scales every legend entry
  names its column through `legendName` -- the column alone with nothing to split by, and "north · margin" beside "north
  · margin_pct" with a series, since the same series is on both scales and naming it twice identically says nothing
  about which swatch is which. `chart:pair-ratio-z` is the bundled one.
- **A box is the one kind that aggregates in its own query.** Every other chart reads one row per point; a box is five
  numbers about the rows that share an x, and no point can carry five. So `chartSql`'s box branch selects `min`, the
  three `percentile(array(...))` quartiles and `max` grouped by x -- `BOX_QUANTILES` is the one list of them -- and the
  page reads them with `chartBoxes` rather than `chartPoints`, dropping a row missing any of the five whole, because a
  box with no whisker is not a box. **The box query filters its own blanks** (`where <y> is not null`): `percentile`
  refuses a null outright, so one blank cell anywhere in the column refused the whole chart, every group of it, and a
  blank cell is the normal state of a spreadsheet rather than an error in one. A group whose cells are all blank simply
  does not appear, the way a row with no y does on every other kind. A `series` or a `y2` on a box is refused by name:
  the five numbers are already the split. It is placed ordinally, the way bars are and for the same reason, and its
  scale spans the whiskers. `chart:dim-spread` and `chart:scenario-spread` are the bundled ones.
- **The legend wraps, and the plot starts under it.** `legendLayout` flows the entries left to right at an estimated
  width each (SVG cannot be asked how wide a string draws before it draws it, and a measured legend is a second layout
  pass per keystroke), wraps at the width of the plot, and answers how many rows it took. It wraps _before_ the entry
  that would overhang, and never on the first entry of a row, so one name wider than the whole plot overhangs once
  rather than wrapping forever. `plotTop` is `20` plus 14 a row past the first, so **a chart with no legend or a one-row
  legend is drawn in exactly the 240 units every chart always was** -- which is what keeps `chartRuns`' placements and
  the bundled bar chart's rect count unchanged. It is bounded at `legendMax` entries and the rest are counted in a final
  "+N more", and `plotTop` is clamped besides: nothing caps how many distinct values a series column holds, and an
  unbounded legend pushed the top of the plot below its own baseline and drew the chart upside down. `chartColours`
  cycles at six, so past a dozen the swatches have stopped telling the series apart anyway.
- **A day axis can carry marks.** `annotations` in `data[0]` is `{ at, label }` per entry, read through `optionalField`
  so a chart written before there were marks still decodes and one spelled wrong is refused rather than painted as no
  marks. `chartSpan` and `chartAt` are `chartRuns`' own placement lifted out, so a mark and a point are placed by one
  rule; a mark is drawn only where `chartSpan` answered and only for an `at` `parseDay` reads, and **nowhere at all**
  otherwise rather than at the left edge. `chartAt` does not clamp: an annotation may name a day outside the span, and a
  mark dragged to the edge would date it wrong. `viewChartSettings` edits them as one textarea, a day and its label per
  line the way a dashboard's tiles are, through `parseAnnotation` -- which refuses a blank line, since `String.words`
  answers one empty word for a line of spaces and that was marking the day `""`. `InputChange ChartAnnotations` writes a
  list, so it goes through `changeDoc` and not `chartSet`, which writes strings.
- **A chart's x axis is time when every x is a day.** `chartRuns` is the one placement: `Nothing` when any x of any
  series fails `parseDay`, which leaves the ordinal axis -- one place per distinct label, in the order the labels arrive
  -- exactly as it was; otherwise every point sits at its own day between the earliest and the latest day any series
  holds, and a step wider than twice that series' median step ends the run there, so line is one polyline and area one
  polygon per unbroken run and a gap in the data is a gap in the picture. The scatter dots read the same runs. Bars are
  ordinal on a day axis too, deliberately: one bar per day across a sparse year is unreadable. **Neither half trusts the
  order the rows arrive in.** `chartSql` orders by the series and then by the x column, which is a string order: a day
  column with inconsistent zero-padding parses as days and sorts wrong, and a second series' days all arrive after the
  first's whatever days they are. So `chartRuns` sorts each series by day before it measures a step or cuts a run,
  `chartFold` sorts before it buckets, and `viewChart` reads the x labels in day order whenever `chartRuns` answered --
  which is what puts the earliest day under the left end, the latest under the right, and the bars in the order a reader
  expects. `chartFold` is the other half: a series past `chartPointsMax` points is cut into equal buckets in day order
  and each bucket drawn as the mean of its y values at the earliest day it holds, folded on a day axis only, and
  `viewChartSettings` says how many points a fold swallowed beside the row count, so nothing is averaged silently.
  `viewChart` reads `chartFold (chartPoints "y" tbl)`, so the axis, the stacks, the totals and the kpi tile all see the
  same folded points.
- **The palette is where you subscribe to a sheet.** `paletteRows` is what the palette reads now: the
  `subscribe to this
  sheet` command over a table, a query, a net-http or a net-hook, and then `paletteCommands`, whose
  signature stays the library and the query so the shortcut sheet and the palette still cannot drift. The command is a
  plain `DocNew` -- the footer's own new-alert door with `select * from @<sheet id>`, `when: added`, the default
  interval and `model.auth.email` filled in -- and it is offered only while `auth.state` is `LoggedIn`, because what a
  visitor typed into the login form is a string in that same field and not an account. `index.html` is what knows the
  address: it stores the email beside the session at login and hands it back on `authResult`, because logging in reloads
  the page.
- **Known gaps**: `@library:freshness` and `@library:lineage` resolve on the server but not in the page. `describe`
  results carry no type in the page, and `WINDOW_TYPES` is server-only, so a window alias there falls back to the
  sheet's stored `cols`.

## Schema (`schema/db.sql`)

- **usr** — identity, name, email (citext), password, `stripe_customer_id`
- **sheet** — the central polymorphic row. `sheet_id` is generated as `type || ':' || doc_id`. Marketplace fields
  (`sell_id` generated from `md5(doc_id||created_by)`, `sell_type`, `sell_price`, `license`, `buy_id`, `buy_price`),
  document data (`row_0`, `name`, `tags`), and `public boolean` for anonymous read through `syncRole`
- **sheet_usr** — membership, with `role` in owner/editor/viewer
- **db** — external database connections (DSNs for codex sheets, encrypted under `DSN_ENCRYPTION_KEY`). `db_id` identity
  PK and an index on `(sheet_id, created_at desc)`. **No unique key on `sheet_id` on purpose**, the way `secret` has
  none on `(sheet_id, name)`: the newest row is the current credential and the one before it still opens, which is what
  lets an owner rotate. `POST /codex-db/:id` inserts beside the row it had and trims to the newest `DSN_KEEP`
- **secret** — a sheet's own secrets, encrypted. **No unique key on `(sheet_id, name)` on purpose**: the newest row for
  a name is current and the one before it still verifies, which is what lets a sender roll over
- **net** — rows for `net-*` sheets and the run log for `alert` and `codex-*`. `meta` is what the run cost. `net_id`
  identity PK, an index on `(sheet_id, created_at desc)`, and the unique `net_hook_signature_idx`. `trimNet()` keeps the
  newest `NET_KEEP` per sheet behind every write — a sheet that must keep everything writes to a table, which is never
  trimmed
- **webhook** — where a sheet's changes are posted: `url` per sheet, and the last delivery's `delivered_at`, `status`
  and `failures`, because a table sheet has no net log to record it on
- **audit** — who did what to which sheet: `sheet_id` (no foreign key, a deleted sheet's trail stays), nullable
  `usr_id`, `action`, `via`, `detail`. Never trimmed
- **payment** — marketplace transactions
