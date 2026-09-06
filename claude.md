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
  before a feature is added. Time one file with `deno test --allow-all <file>`, and check `top` first: a build job on
  the same machine makes every number here a lie
- `deno task review` — elm-review. Runs clean with zero suppressions; keep it that way
- `deno task status` — print every graded condition from the deployed `GET /status`, exit nonzero if any is below 1.0.
  `.github/workflows/status.yml` runs it on a 15-minute cron; the failure email is the alarm
- `deno task vendor` — re-vendor the browser bundles after bumping the versions at the top of `vendor.ts`
- `deno task db:plan` / `db:apply` — read the generated migration, then run it. **Check `.env` first: `DATABASE_URL` may
  point at production**, and `db:apply` no longer prompts. Its allow list is `INDEX_BUILD,INDEX_DROPPED`, so a
  migration that drops a constraint (a primary key move) is refused for `ACQUIRES_ACCESS_EXCLUSIVE_LOCK`; run the same
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

Five files. Which one a failure belongs in is usually obvious.

- `main_test.ts` — the server. One `Deno.test` of named `t.step`s against in-process PGlite: auth, sync and roles, shop
  and Stripe, `POST /query`, the `src/sql.mjs` UDFs, net-http polling, socket reports, alerts and digests, MCP, export.
  Steps run in order against one database, so a step still depends on what ran before it. What steps buy is a name in
  the failure and every later step still running — **not** isolation, and not `--filter`, which matches test names and
  not step names. A **second** PGlite behind a second gateway on `127.0.0.1:5435` is the codex sheets' external
  database — a second instance and not a second address onto the first, because the gateway hands every connection onto
  one PGlite session and the codex connection sets that session read only, which refused the next insert anywhere in
  the suite; it boots in parallel and is awaited on connect. A DSN that must fail names a loopback port nobody listens
  on, never a hostname: the suite does no DNS.
- `examples_test.ts` — every bundled sheet through **both** engines (`npm:alasql` and the vendored `src/alasql.mjs` the
  page loads), compared row for row.
- `page_test.ts` — the page under jsdom, through two harnesses. `boot` runs the compiled Elm in `dist/index.js` with
  every port answered by hand and the library fed in through `library()`; reach for it for anything about what the page
  renders, and for `rendered()` — the one booted page shared across tests, the library, for the tests that only read
  what it painted — when nothing in the test writes to the model. `glue` runs `src/index.html`'s own
  `<script type="module">` over the same jsdom — its imports rewritten to a destructure, `initializeWasm` and the
  storage stubbed, the websocket adapter genuine, `fetch` and `WebSocket` recorded and answered by the test — so
  `changeDoc`, `arrangeDoc`, `applyPatches`, `Views`, the query re-run guard, the share requests, CSV import, `newDoc`,
  fork and the socket-health report are the real ones; reach for it for anything about what the glue does. Both
  harnesses count a settle off a mutation observer, not by serializing the body per frame, which was most of what a
  settle cost. `docs` hands it a synced document, which is where a write is watched: the handle holds the test's own
  object. `realRepo` swaps the stub repo for automerge itself — slower, and the only way to find out whether a patch
  means the same thing to a real document as it does to a plain object. Both harnesses drive animation frames off the
  event loop rather than jsdom's ~16ms clock: a settle waits for the page to go quiet, not for real time, and that clock
  was most of this file's wall time. Anything that does wait on a real timer — the query debounce, a file being read —
  asks `settle(ms)` for it by name, and `until()` is the bounded poll for the ones where the wait is for something to
  happen; a flat `settle(ms)` is only for proving that something did **not**. Refuses a `dist` older than `src` rather
  than building one: `deno task test` builds once before any file runs, so the files can run in parallel without a
  compiler racing a reader of its output. deno-dom is not enough — it has no `replaceData` on a text node. It also runs
  `src/sw.js` over a hand-made `self`, `caches` and `fetch`, which is the only way to take the network away from a
  service worker.
- `browser_test.ts` — no browser: dist is fresh, `index.html` wires the WASM and the import map, every root-absolute
  asset is in `_redirects`, every imported name is exported, nothing reaches a CDN. `index.html`'s
  `<script type="module">` body is piped to `deno lint` for real scope analysis. `BROWSER_GLOBALS` is the whole
  allowlist of names Deno's global scope lacks. `src/sw.js` is linted the same way, its `SHELL` list is held equal to
  `_redirects` in both directions, and `PAGE_BY`/`pageBy` is one more of the language-boundary copies it reads as
  source text.
- `tests/MainTest.elm` via `elm_test.ts` — pure Elm: selection and navigation, sort and filter, clipboard parsing,
  column stats, `docDecoder`, `chartPoints`.

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
  413s and the alert run's delivery
  line; a 429 is shed unlogged and is not counted, by design.
- **`POLL_OK` / `ALERT_OK` / `RUN_OF` / `RUN_OK` have one definition each.** `GET /status` and `library:freshness` both
  read them from there. Two hand-copied copies had already drifted.
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
- **One spelling per fact.** `API_BASE` in `src/page.mjs` is the only API host, handed to Elm through flags and to
  `index.html` by import. `PORTALS` in `src/portals.mjs` is the only portal list. `Stored` in `index.html` is the only
  `localStorage` key prefix. `spec` in `Main.elm` is the only per-column-type table, and it has no wildcard, so a new
  type fails to compile. `CHART_KINDS` in `src/sql.mjs` is the only list of ways a chart is drawn: `chartSql` refuses
  one that is not on it, `kindSpec` in `Main.elm` is the copy the language boundary forces, and `browser_test.ts` fails
  when the two disagree. `NET_METHODS` in `main.ts` is the only list of verbs a feed is polled with, `netMethods` in
  `Main.elm` is its copy, and the same test fails on the same drift.
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
- **Computed sheets**: `library:freshness`, `library:audit`, `net-hook:errors` and `net-hook:reports` answer through
  `sheet()` without an automerge document, so they page, export and can be selected from a query like any other sheet.
  The operator is `isOperator()`: whoever reads `net-hook:errors`, which `OPERATOR_EMAIL` is granted at seed time.
- **Auth**: JWT middleware; a per-sheet API key (`scrapsheets-key`) is scoped by a path check _before_ routing, so no
  handler has to remember to ask. Email through Resend.
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
  url refused at config read. The run row keeps a url's **host** and never its path: the path is the whole of a webhook's
  authorization, and every viewer of the sheet reads that row.
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
  feed's own way of saying there is no next. Page one carries the number or the
  offset, so the validators, the `{{cursor}}` watermark and the host holdoff still ride the first request alone; a
  cursor and a Link header only arrive with an answer. The walk is bounded by `PAGE_MAX` and by `BODY_CAP` checked as
  the pages sum, a `link` next page must be on the origin the sheet names (scheme and port included), and `page_param`
  naming the sheet's own `cursor` is refused because paging overwrites it on every request. **The sheet keeps nothing
  from a poll that failed part way**: a 429 or a 5xx on any page is the one `later()` retry path for the whole poll,
  every other refusal throws into the catch, and the next scheduled poll starts at page one. The failure row names the
  page that broke, so its repro replays that request. The pre-flight makes one request whatever `page_by` says.
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
  `[::1]` fold to the one spelling the block list holds. Every attempt lands on one `codexRun()` row whose `meta.rolled_over` says which credential answered and whose body is why
  the newer one could not connect; `POLL_OK` grades a rolled-over run failed although it answered, so
  `library:freshness` and `GET /status` say the newest credential is dead while the read still works. A read that
  spent both is the 502 it always was, its `Source` counting the credentials tried and naming none of them, and no
  refusal quotes the string, because a DSN carries its password. The host is checked by `assertPublicHost()`, the same
  literal-and-resolved check `safeFetch` runs on every hop, on a server whose own database is somewhere else: a server
  whose database is on loopback is a developer's machine, and there the only refusal is its own database. A query over
  `@codex-db:x` whose connection is dead is that refusal, never an empty result.
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
  by literal address and by every address the name resolves to, and the codex guard asks it the same question. It takes the method beside the body,
  and only a GET is followed through a redirect. A GET carrying a body is refused in `netRequest` and nowhere else --
  the one place that can name the size without measuring the resolved secret. Its DNS answers are read one at a time:
  only a not-found is a fact about the host, and a resolver that failed some other way is a 502 that says so rather
  than a 400 telling the caller to check a spelling that was right. The per-host gap is the poller's alone:
  `holdHost()` in `pollNetOnce` writes `hostDue` after each poll (a paged feed's pages within one poll go out back to
  back, the way any client reads a paged answer) and on every `Retry-After`, taking the later of the
  two, so two sheets on one host take turns across cycles and the proxy can neither hold a host nor evict a hold.
- **MCP**: hand-rolled JSON-RPC 2.0 at `POST /mcp/:id` — `initialize`, `tools/list`, `tools/call` with `read_sheet`,
  `write_cells`, `query_sheet`, `list_sheets`.

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
- **Cross-sheet queries in the browser**: `sheets(alasql, shelf, find)` in `src/page.mjs`. Only two things come from the
  browser and both are arguments: the library map and `repo.find`.
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
- **Pre-flight**: the "test the request" button on a net-http sheet sends `preflight { id, url, headers, method, body }`;
  the page posts it to `POST /library/:id/preflight`, which runs the poller's own request once and writes nothing, and
  the answer or the refusal comes back on `preflightLoaded` by sheet id, held in `sheet.preflight` and drawn by
  `viewPreflight`. An answer for another sheet is dropped by id.
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
- **`formatNumber` is the one place a number becomes text.** The cell, the stats row and the totals row all go through
  it; they used to format independently and a `usd` column's total came out without its `$`. A value `positional` says
  is not written as digits — not finite, or a magnitude JavaScript writes as `1e+21` — skips the currency, the grouping
  and the format, because all of them cut the string by position: an overflowed total read `$In,fin,ity.00`. It takes
  the column's `decimals` and its `format`, and `digitsOf` is where the two meet: absent on both, each type reads the
  way it always did; `fixed` writes exactly the count asked for and `maxDecimals` bounds it; `groupWhole` puts a
  separator every three digits of the whole part, holding the sign and the fraction back from `commas`, which counts
  from the right over whatever it is handed and made `-123` into `-,123` — `usd` asks it too, because money is the
  column that always asked for it; and `scientific` writes a mantissa and a signed exponent, because Elm has no
  `toExponential`: the exponent is `floor (logBase 10 (abs v))`, `descaled` divides it out in two halves because `10 ^
  -324` underflows to 0 in one, zero is answered by name, a mantissa with no count is rounded to twelve significant
  digits so the division's own noise is not written, and two digits before the point mean the exponent was one too low —
  `fixed` rounds 9.99 at one place up to "10.0", and `logBase` divides two logs, so 1000 comes back at an exponent of 2
  and a mantissa of exactly ten. A format lands **on top of** the type and never instead of it: `grouped` on a usd
  column asks for what `usd` already writes, `scientific` on one keeps the symbol with the sign outside it, and a
  percentage keeps its sign whichever way its digits read. `NumberFormat` is the list of ways digits are written —
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
  `sheet.storedView` so
  closing an untouched filter panel writes nothing. The panel's two typed fields — the filter box and the decimal
  count — write the model on every keystroke and reach the document when the panel closes: a patch per character is a
  sync per character for everybody watching, and typing "10" meant a 1 nobody chose. The format select has no keystrokes
  to hold back, so one click is one `arrange`. A format spelled in a way nobody wrote is no format, the way an
  out-of-range count is no count and an unusable width is no width — the arrangement is how you were reading the rows,
  and losing it must never cost you the rows. `colViewFields` is at `D.map8` now, which is the ceiling: a ninth view
  field needs `andThen` rather than another `map`. It goes around `updateDocMsg`: a resize is not data and does not
  belong on the undo stack. `tableHome` and `queryHome` are the two addresses — a table's `data[0]` is the column list,
  so the address is the position; a query's is one object, so the fields live under `view`, keyed by column name the way
  its `cols` overrides are. `arrangeable` is the one `case` that picks, and `pruneView` is a table only, deliberately.
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
- **Offline**: `src/sw.js` is the service worker, copied to `dist` like any other `src` file, listed in `src/_redirects`,
  and registered at the end of boot in `index.html` by `navigator.serviceWorker?.register("/sw.js")` — a page with no
  `serviceWorker` (jsdom, plain http) takes the short circuit and a refused registration is logged by name rather than
  taking the boot down, because the app works without one. It answers same-origin GETs network first and the cache
  second: no filename here carries a build hash, so there is nothing for a cache name to key on and nothing but the
  network that knows a copy is still the deployed one; a 200 for a `SHELL` path is written back, so a deploy replaces
  the shell on the next online open and no cache name is ever bumped. `SHELL` is every path `_redirects` serves as
  itself plus `/`, pre-cached by `addAll` on install — which refuses the whole install on one 404, deliberately — and it
  is also the whole of what is cached: `answer()` reads and writes by pathname and never by url, so a share link's query
  string is not a second entry and the cache cannot grow past the list. Offline a cached path answers itself, a
  navigation with nothing cached answers the cached `/` the way the `/*` catch-all does online, and a path with neither
  is a named refusal rather than a silent failure. Nothing cross-origin is answered at all — the API and the sync socket
  are another origin, and the handler returns without calling `respondWith`, which leaves the browser doing what it did
  before there was a worker. `browser_test.ts` fails when `SHELL` and `_redirects` drift in either direction and lints
  `sw.js` through `unresolved()`, the same scope analysis `index.html`'s module script gets; `page_test.ts` runs the
  worker over a hand-made `self`, `caches` and `fetch`, which is the only way to take the network away. Offline means
  the shell opens: the data still needs the network, and what a document already synced is in IndexedDB.
- **Known gaps**: `@library:freshness` resolves on the server but not in the page. `describe` results carry no type in
  the page, and `WINDOW_TYPES` is server-only, so a window alias there falls back to the sheet's stored `cols`.

## Schema (`schema/db.sql`)

- **usr** — identity, name, email (citext), password, `stripe_customer_id`
- **sheet** — the central polymorphic row. `sheet_id` is generated as `type || ':' || doc_id`. Marketplace fields
  (`sell_id` generated from `md5(doc_id||created_by)`, `sell_type`, `sell_price`, `license`, `buy_id`, `buy_price`),
  document data (`row_0`, `name`, `tags`), and `public boolean` for anonymous read through `syncRole`
- **sheet_usr** — membership, with `role` in owner/editor/viewer
- **db** — external database connections (DSNs for codex sheets, encrypted under `DSN_ENCRYPTION_KEY`). `db_id` identity
  PK and an index on `(sheet_id, created_at desc)`. **No unique key on `sheet_id` on purpose**, the way `secret` has none
  on `(sheet_id, name)`: the newest row is the current credential and the one before it still opens, which is what lets
  an owner rotate. `POST /codex-db/:id` inserts beside the row it had and trims to the newest `DSN_KEEP`
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
