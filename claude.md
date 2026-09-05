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
| `schema/db.sql`    | Desired schema, no data. `pg-schema-diff` diffs a live DB against it; no migration files    |
| `examples.sql`     | Shop catalogue of query templates, applied by `seed()` on first request                     |
| `vendor.ts`        | Rebuilds the vendored browser bundles in `src/` (automerge, automerge-repo, alasql)         |
| `deno.json`        | Tasks, dependencies, import map                                                             |

## Commands

- `deno task build` — copy `src/*` to `dist`, then `elm make`
- `deno task dev` — build, then serve `dist`
- `deno task test` — the whole suite. **Not** `deno test --allow-all`: the task is the one place `JWT_SECRET`,
  `TOKEN_SECRET` and `DSN_ENCRYPTION_KEY` are set, and `main.ts` refuses to load without all three. It builds `dist`
  once, runs the files in parallel, and fails past ten seconds of wall time, which is the rule that the suite gets
  fixed before a feature is added. Time one file with `deno test --allow-all <file>`, and check `top` first: a build
  job on the same machine makes every number here a lie
- `deno task review` — elm-review. Runs clean with zero suppressions; keep it that way
- `deno task status` — print every graded condition from the deployed `GET /status`, exit nonzero if any is below 1.0.
  `.github/workflows/status.yml` runs it on a 15-minute cron; the failure email is the alarm
- `deno task vendor` — re-vendor the browser bundles after bumping the versions at the top of `vendor.ts`
- `deno task db:plan` / `db:apply` — read the generated migration, then run it. **Check `.env` first: `DATABASE_URL` may
  point at production**, and `db:apply` no longer prompts
- `deno run -A npm:elm-format --yes src/Main.elm` — format Elm
- Watch: `watch src { try { cp -vu src/* dist ; elm make src/Main.elm --debug --output=dist/index.js } }`

Local Postgres default is `postgresql://postgres@127.0.0.1:5434/postgres`. Tests need no real Postgres: they run against
in-process PGlite behind a pg-gateway on that port. Schema changes go in `schema/db.sql`; DML is never generated, splice
it in with `--insert-statement`. **Deploy the code before a check constraint on a column `seed()` writes.** Postgres
checks an `insert ... on conflict do update`'s proposed row before it looks for the conflict, so the running seed must
already propose the column or every request fails on the constraint until the deploy lands. `license` on `sheet` took
production down this way: backfill the rows, deploy, then `db:apply`. The opposite order for a new table the code
writes on every request: `audit` had to exist before the code that inserts into it was deployed.

## Tests

Five files. Which one a failure belongs in is usually obvious.

- `main_test.ts` — the server. One `Deno.test` of named `t.step`s against in-process PGlite: auth, sync and roles, shop
  and Stripe, `POST /query`, the `src/sql.mjs` UDFs, net-http polling, socket reports, alerts and digests, MCP, export.
  Steps run in order against one database, so a step still depends on what ran before it. What steps buy is a name in
  the failure and every later step still running — **not** isolation, and not `--filter`, which matches test names and
  not step names.
- `examples_test.ts` — every bundled sheet through **both** engines (`npm:alasql` and the vendored `src/alasql.mjs` the
  page loads), compared row for row.
- `page_test.ts` — the page under jsdom, through two harnesses. `boot` runs the compiled Elm in `dist/index.js` with
  every port answered by hand and the library fed in through `library()`; reach for it for anything about what the page
  renders. `glue` runs `src/index.html`'s own `<script type="module">` over the same jsdom — its imports rewritten to a
  destructure, `initializeWasm` and the storage stubbed, the websocket adapter genuine, `fetch` and `WebSocket`
  recorded and answered by the test — so `changeDoc`, `arrangeDoc`, `applyPatches`, `Views`, the query re-run guard,
  the share requests, CSV import, `newDoc`, fork and the socket-health report are the real ones; reach for it for
  anything about what the glue does. Both harnesses count a settle off a mutation observer, not by serializing the
  body per frame, which was most of what a settle cost. `docs` hands it a synced document, which is where a write is watched: the handle
  holds the test's own object. `realRepo` swaps the stub repo for automerge itself — slower, and the only way to find
  out whether a patch means the same thing to a real document as it does to a plain object. Both harnesses drive
  animation frames off the event loop rather than jsdom's ~16ms clock: a settle waits for the page to go quiet, not
  for real time, and that clock was most of this file's wall time. Anything that does wait on a real timer — the query
  debounce, a file being read — asks `settle(ms)` for it by name, and `until()` is the bounded poll for the ones where
  the wait is for something to happen; a flat `settle(ms)` is only for proving that something did **not**. Refuses a
  `dist` older than `src` rather than building one: `deno task test` builds once before any file runs, so the files
  can run in parallel without a compiler racing a reader of its output. deno-dom is not enough — it has no
  `replaceData` on a text node.
- `browser_test.ts` — no browser: dist is fresh, `index.html` wires the WASM and the import map, every root-absolute asset
  is in `_redirects`, every imported name is exported, nothing reaches a CDN. `index.html`'s `<script type="module">`
  body is piped to `deno lint` for real scope analysis. `BROWSER_GLOBALS` is the whole allowlist of names Deno's global
  scope lacks.
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
  count, and `docData()` refuses a claimed document with no rows. The few surviving `throw new HTTPException` are passthroughs of an `explain()` block
  `src/sql.mjs` already built. The error log is written after the response and not awaited; `errorLogged()` is how a
  test waits for it instead of sleeping. `Received` goes through `show()`, never `JSON.stringify` — `explain()` drops an undefined
  field, so a stringified `undefined` silently loses the line.
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
- **One budget per sheet, spent through `spend()`.** `hookBucket()` is the bucket and `spend(sheet_id, what, rows,
  bytes, fix)` is the one check-and-charge, synchronous so concurrent callers cannot all pass it. Every door into a
  document sheet spends one unit: a webhook delivery, a socket report, a whole read through `sheet()` (`GET /sheet`,
  an export, an MCP read; the computed sheets are free), and an append whatever it carries, with its bytes as the
  volume. It is taken after the access or signature check, so a refused request spends nothing, and its refusal is a
  429, which `app.onError` does not log.
- **One account is bounded across its sheets.** `rateLimit()` runs once more on the account, in `accountBuckets`
  rather than the address map so address churn cannot evict it; `assertSheetsQuota()` caps the sheets an account owns
  at claim, import and purchase; `assertRoom()` caps a sheet's rows at the engine's own `MAX_QUERY_ROWS` at import
  and append; `sendWithinQuota()` caps an account's alert emails a day, counted off the run log. Fetches need no
  count: a feed polls at most once a minute, so the sheets cap bounds them. A refusal that changed what an account
  keeps or sends says "quota" where `GET /status` reads it, the error log's 413s and the alert run's delivery line; a
  429 is shed unlogged and is not counted, by design.
- **`POLL_OK` / `ALERT_OK` / `RUN_OF` / `RUN_OK` have one definition each.** `GET /status` and `library:freshness` both
  read them from there. Two hand-copied copies had already drifted.
- **The status check grades, never maximizes.** 1.0 is the minimum pass, `grade()` floors, and a condition that cannot
  compute throws by name.
- **Secrets never reach a document.** Sheet secrets are referenced from a net-http header as `{{secret:name}}` and
  resolved at fetch time into a separate object, so the automerge document sync hands every viewer keeps the reference.
- **One spelling per fact.** `API_BASE` in `src/page.mjs` is the only API host, handed to Elm through flags and to
  `index.html` by import. `PORTALS` in `src/portals.mjs` is the only portal list. `Stored` in `index.html` is the only
  `localStorage` key prefix. `spec` in `Main.elm` is the only per-column-type table, and it has no wildcard, so a new
  type fails to compile. `CHART_KINDS` in `src/sql.mjs` is the only list of ways a chart is drawn: `chartSql` refuses
  one that is not on it, `kindSpec` in `Main.elm` is the copy the language boundary forces, and `browser_test.ts`
  fails when the two disagree.
- **A column type is one word everywhere.** `COLUMN_TYPES` in `src/sql.mjs` is the list. Its entries are either a type
  or an `as` alias of one; `CANONICAL_TYPES` is the half anything may write, `NUMERIC_TYPES` and `JSON_TYPES` derive
  through `canonicalType()`, and `main.ts`'s `Type` union plus `columnTypes`/`typeAliases` in `Main.elm` are the copies
  a language boundary forces — `browser_test.ts` reads all three as source text, in both directions, and fails when any
  drifts. An alias is read and never written, so an old `pct` column still loads, still queries and is still checked,
  and no new one is stored. A spelling outside the list is refused by `checkColumnTypes` rather than skipped: skipping
  it is how a percent column stopped being checked at all.
- **A column's declared type is never rewritten.** `col.raw` is the document's own spelling, the way `col.key` is, and
  a header write patches one field (`[0, x, "name"]`) rather than replacing the column object. Replacing it made a
  rename re-encode the type beside the name.
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
  `sheet()` without an automerge document, so they page, export and can be selected from a query like any other sheet. The operator is
  `isOperator()`: whoever reads `net-hook:errors`, which `OPERATOR_EMAIL` is granted at seed time.
- **Auth**: JWT middleware; a per-sheet API key (`scrapsheets-key`) is scoped by a path check _before_ routing, so no
  handler has to remember to ask. Email through Resend.
- **Sync**: official automerge `NodeWSServerAdapter` behind a ws-shim over Hono's `upgradeWebSocket`. Per-document
  access is `syncRole` in the message path, not just `sharePolicy`; a viewer's frames are decoded and rejected if they
  carry changes.
- **Webhook ingest**: `POST /net/:id`, always signed (`scrapsheets-signature: t=…,v2=…`, or a Stripe/GitHub/Shopify
  scheme chosen by the sheet's stored secret name). Replay is refused by the unique index `net_hook_signature_idx` on
  the digest that actually verified.
- **Socket health**: nothing server-side opens a `net-socket` sheet's socket, so a browser with the tab open is the
  only witness and `POST /library/:id/socket` is how it says so. Two states — `connected` and `error`, never a close,
  because `changeId()` closes the socket on every navigation. `library:freshness` admits the sheet only once it has a
  `SOCKET` run, so a socket nobody has watched is absent rather than "never run" forever.
- **Alerts**: an alert's `when` is `rows` (the query returned a row; the default), `added` or `removed` (the answer
  gained or lost a row since the run before). `status` is the verdict and `delivery` what was done, so no reader
  learns a new word; a change condition's first run is a silent baseline and a run past `ALERT_ROWS` under one is an
  `error` row. `ALERT_WHEN` in `main.ts` and `whenSpec` in `Main.elm` are the two copies a language boundary forces.
- **Polling**: `pollNetOnce` and `pollAlertOnce` on a 15-second tick; conditional requests, per-host `Retry-After`,
  bounded retries, and a `net` row per run — including quiet ones, because a healthy quiet alert and a dead timer
  otherwise write the same nothing. A run's row carries `meta.shape`, the columns the body answered with and the JSON
  type of each (`shapeOf`); a run whose shape differs from the run before keeps its rows and carries
  `meta.shape_change` naming what was added, dropped and retyped (`shapeChange`), and `POLL_OK` grades it as failed,
  once, so freshness and the status alarm hear it through the path every other failure takes. A good run's body is
  its idempotency key: its digest rides `meta.sig`, the slot a delivery's signature takes, so `net_hook_signature_idx`
  refuses the same body twice and `netRow` moves the row it matched to now, marked `repeated`. A query over a
  net-http sheet reads only the runs `POLL_OK` grades (`sheet()` adds it when `path_` is non-empty); the sheet view
  and the export keep the whole log, failures among them, so one bad poll cannot empty what is built downstream.
- **Audit**: one log, the `audit` table, read as `library:audit`. `record()` is the one writer. HTTP reads and writes
  land through one middleware keyed on the route patterns in `AUDITED`, after the route succeeded; the sync socket
  records `open` and a first `edit` per peer per document; MCP records `mcp <tool>`; a query records `query` on every
  sheet it resolves. `via` says which door, `public` being an anonymous reader of a public sheet. An owner or editor
  reads every row about their sheet, everybody reads their own rows, and a row with no account is `who = share link`
  or `anonymous`. A row that cannot be written fails the request it was about.
- **Marketplace**: Stripe Checkout, platform-side. Connect payouts are not wired. A listing carries a `license` from
  `LICENSES` or does not go live (the schema checks it). `POST /shop/:sell_id/report` is one row per account per
  listing on `net-hook:reports`, `POST /shop/:sell_id/review` is the operator's `keep` or `takedown`, and `GET /status`
  fails while a report is open.
- **Outbound webhooks**: `POST /library/:id/webhook` names a url, which must answer a signed `ping` 2xx before it is
  registered, and that ping is where a url inside our network is refused by name. `flushWebhooks()` posts one
  signed `change` per hook per flush for every document `touched` since the last, heard as the storage's
  `doc-saved` and `doc-compacted` metrics rather than the handle's change event, because the save is what the repo
  debounces per document; the first save after a `doc-loaded` is the load itself and is not a change. A row landing
  on a net sheet touches it too. Signed with the sheet's own `hook` key, the one `GET /library/:id/hook` answers,
  over the receiver's path and the body, so one secret serves both directions; a sheet on a provider scheme cannot
  register one. Every delivery spends the sheet's budget as `webhooks`. The outcome lives on the `webhook` row;
  `WEBHOOK_FAILS_MAX` failures in a row take a hook out until its owner sets it again, a dead hook fails
  `GET /status` until then, the flush is bounded by `WEBHOOK_FLUSH_MAX`, and the url list is owner or editor only.
- **Outbound fetches**: `safeFetch` is the one door out and sends `USER_AGENT`. Given a body it posts and follows no
  redirect. The per-host gap is the poller's
  alone: `holdHost()` in `pollNetOnce` writes `hostDue` after each request and on every `Retry-After`, taking the
  later of the two, so two sheets on one host take turns across cycles and the proxy can neither hold a host nor
  evict a hold.
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
  whose cells reach the engine as a string, `date` and `timestamp` among them: nothing coerces a date column, so
  AlaSQL is handed ISO text and drops it like any other. It always writes the lowercase name, because `register()`
  defines `min_text` and `MIN_TEXT` and nothing between. Four things it leaves alone: a call followed by `over`, which
  is a window `applyWindows` computes itself and `rewriteWindows` finds by name; an expression; a name typed two ways;
  and **a name the query aliases into being** (`select min(a) from (select n as a …)`), because scope is not something
  a regex can see and that one answered `"10"` for a minimum of 9. All of them land on `checkResultColumns()`. It stops
  at the first unbalanced bracket rather than scanning to the end for every call after it, and `MAX_EXTREMES` bounds
  the calls in one statement — without both, a body of nothing but `min(` was quadratic.
- **`describe @ref`** is intercepted before the engine in both engines, and is the one statement that still answers on a
  sheet whose cells fail the type check. **`explain <query>`** is the other intercepted statement: it runs the query
  with every guard and answers one row per stage (`load @ref`, `plan`, `engine`, `windows`, `total`) with rows in,
  rows out and milliseconds. `timed()` wraps the calls both hosts already make and does nothing on a plain run.
- **Types**: `COLUMN_TYPES` is every type a column may declare and what each one is; `NUMERIC_TYPES` is derived from
  it and `knownType()` matches the `enum:` family by prefix. `checkColumnTypes()` is the one place a cell becomes what
  its column says — a blank becomes `null`, a numeric string becomes its number. `selectTypes()` types a result column
  off its select item, not off its name, and `WINDOW_TYPES` says the same thing about a window: `sum` and `avg` follow
  their argument in both, so one name cannot mean two types.
- **Guards**: `checkQueryRows()` caps rows loaded across every `@sheet`; `checkJoinRows()` caps the product of the
  from clause's row counts at `MAX_JOIN_ROWS`, every occurrence counted so a self-join multiplies, because the engine
  walks every pair before a where clause and cannot be stopped once it starts — it is the pairs walked, not the rows
  kept, so a keyed join over big sheets pays it too; `checkResultColumns()` turns AlaSQL's silent undefined column
  into an error; `nearest()` backs every "did you mean".
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
  `repo.find`. `viewGallery` reads `model.library`, so a new demo needs no code change. `seen` is stamped by
  `selectDoc` for a sheet the library lists and is the library's `opened` column; it and `trashed` are the two stored
  fields that survive a system entry in the merge, because both are this browser's fact about somebody else's sheet.
  `libraryIdAtRow` reads a row's sheet off the rows as drawn — sorted, filtered, searched — never off the dictionary's
  order.
- **Trash and restore** ride `updateLibrary`, which is the one port that writes this browser's facts about a sheet.
  `Library.set` in `index.html` drops a **null** field out of the patch rather than out of the entry, so restoring
  writes `trashed = False` and never `Nothing`. Trashing asks nothing first — being undoable is the point — and
  `deleteDoc` stays the purge, which is the one that also calls `Views.drop`, so a restored sheet keeps the
  arrangement it had. A trashed sheet is out of the library table, the demo strip and `paletteCommands`; `model.trash`
  swaps the last library column between `Trash` and `Restore` + `Delete`, and hides the footer's new-sheet rows.
- **Cross-sheet queries in the browser**: `sheets(alasql, shelf, find)` in `src/page.mjs`. Only two things come from the
  browser and both are arguments: the library map and `repo.find`.
- **One CSV import, whichever way the file arrives, in two steps.** The footer's file input goes through Elm's
  `CsvImportFile` and the `importCsv` port; a file dropped on the page is read by `setupDragDrop` and handed to the
  same `uploadCsv`. That posts the file to `POST /import/preview`, lays the types this browser settled on for the
  same header last time (`scrapsheets-imports`, through `rememberedTypes` in `src/page.mjs`) over the server's
  guesses, and hands the preview to Elm on `importPreviewed`; `viewImport` shows a select per column and the first
  rows. `ImportConfirm` sends the settled types on `importConfirm`, and the page posts the kept file to
  `POST /import/csv?types=…`, remembers the types by header, and opens the sheet its answer names. The server parses
  both times through `readImport`, because it is the one that registers the sheet, syncs it and coerces the values —
  a settled type the values do not fit is refused on the line that does not. `parseCsv` in `Main.elm` is a different
  job: the clipboard.
- **Import**: `readImport()` is the one CSV reader; `POST /import/preview` answers its columns, types and first rows
  without a sheet, and `POST /import/csv` makes one. `?types=` is a JSON object keyed by column name, checked
  against `CANONICAL_TYPES` and the file's header before the file is read.
- **Pre-flight**: the "test the request" button on a net-http sheet sends `preflight { id, url, headers }`; the page
  posts it to `POST /library/:id/preflight`, which runs the poller's own request once and writes nothing, and the
  answer or the refusal comes back on `preflightLoaded` by sheet id, held in `sheet.preflight` and drawn by
  `viewPreflight`. An answer for another sheet is dropped by id.
- **Feed health**: `library:freshness` is read by `index.html` and handed to Elm through `freshnessLoaded`. The
  `freshness` column appears only when the answer is non-empty — a blank column over a logged-out library would read as
  "nothing is wrong".
- **Table UX**: multi-column sort, column hide (keeps its x coordinate; `skipHidden` steps over it), drag-resize,
  drag-reorder (columns, and rows while the table is in document order), pin, row insert/duplicate/fill-down, find/replace, undo/redo, command palette (Ctrl/⌘+K), shortcut
  sheet (Ctrl/⌘+/). `shortcutGroups` carries the `Msg` each key runs and `paletteCommands` reads that list, so the two
  cannot drift.
- **The column's panel is where its cells are cleaned.** Trim, UPPER, lower and drop-blank-rows sit under Hide and Pin,
  each one `DocMsg`, so undo, the viewer refusal and the sync path are the ones already written. `cellRewrites()` emits
  nothing for a cell the change does not move and skips one that is not text; `blankRows()` says what blank means;
  `rowDeletions()` is the splice pair, shared with `SheetRowDelete`. They read every row the document holds, not the
  rows on screen, and are offered only where `movable` holds — a table, never a query's computed columns.
- **`formatNumber` is the one place a number becomes text.** The cell, the stats row and the totals row all go through
  it; they used to format independently and a `usd` column's total came out without its `$`. A value that is not
  finite skips the currency and the digit grouping — `commas` runs over the string form, so an overflowed total read
  `$In,fin,ity.00`.
- **The arrangement is offered where it is kept.** `arrangeControls` is the one predicate `viewHeaderCell` asks: a
  table and a query, because the arrangement is kept; the library and the shop, because their order is how you read a
  listing this app builds and there is no document under it. Everything else is a feed — its rows are a run log, and a
  sort that worked and then forgot read as a bug in saving.
- **The arrangement is stored on the columns, in two homes.** Sort, filter, hidden, pinned and width live in `data[0]`
  as `sort`/`rank`, `filter`, `hidden`, `pinned`, `width`, so they survive a reload and travel with a share.
  `viewDecoder` reads them on `DocSelect` and `arrange` writes them, diffed against `sheet.storedView` so closing an
  untouched filter panel writes nothing. It goes around `updateDocMsg`: a resize is not data and does not belong on the
  undo stack. `tableHome` and `queryHome` are the two addresses — a table's `data[0]` is the column list, so the
  address is the position; a query's is one object, so the fields live under `view`, keyed by column name the way its
  `cols` overrides are. `arrangeable` is the one `case` that picks, and `pruneView` is a table only, deliberately.
- **Reorder is a splice, pin is a sum.** A move is one `move` patch — on `data[0]` for a column, on `data` for a
  row — applied by `applyPatches` in `index.html` to the value the document already holds — rows are keyed by
  `col.key`, so no cell moves and the display index stays the document index. It is a `DocMsg` and not an
  arrangement: everyone looking at the sheet sees the new order, so it rides `changeDoc`, `movePatch` the other way is
  its own undo entry, and a viewer's is refused like any edit. `dropOf` is the one place a drop becomes a `DocMsg`. A
  row's handle is drawn in the first data cell only while `inDocumentOrder` holds — nothing sorted, filtered or
  searched — because a display row then *is* the document row, and `MoveEnd` asks it again so a sort taken mid-drag
  is refused; the `.grab` glyph is CSS so a cell's text stays its value.
  `pinLeft` sums the widths of the sticky columns before each pinned one and hands the answer to `.pin` inline; column
  0 is in the sum whether or not anybody pinned it, because `.c0` sticks it regardless. Pinning writes `autoColWidth`
  for any sticky column that sizes itself — the one pinned and column 0 both — because a guessed sum either leaves a
  gap the rows scroll through or slides the pinned column underneath column 0.
- **`arrange` moves `storedView` before the page has done anything, so the page must never drop a batch.** There is
  nothing to roll back to: `storedView` is what *this browser wrote*, not what the document holds, and re-reading the
  document to recover it is exactly what would write a deletion over a collaborator's sort. So `arrangeDoc` in
  `index.html` writes the document first and the browser store last, `Views` keeps what it was given in memory so a
  full store loses nothing, and a missing automerge handle is a named refusal rather than a no-op.
- **A sheet whose document cannot hold the arrangement keeps it in this browser.** `arrangeDoc` is a port of its own so
  that the page knows a batch is only view fields without inspecting a path. `Views` in `index.html` holds those
  patches under `scrapsheets-views` and merges them back in `selectDoc`; `foldView` and `mergeView` in `src/page.mjs`
  are the two halves a test can reach. **Held by `col.key`, never by position** — a table's patch addresses `data[0][x]`
  and `foldView` resolves x against the document's own `data[0]`, because the whole reason a view is held is that
  somebody else owns the document and can reorder it. A held key the document no longer carries is dropped, not
  created. Two sheets need this: one that ships bundled, and one the sync server has refused a write on — which is
  heard by wrapping the ws adapter's `receiveMessage`, since the vendored adapter logs the server's `type: "error"`
  frame at a debug namespace and emits nothing. The same wrapper hears the opposite answer: while an arrangement
  write is `pending`, a `sync` frame whose heads cover the head this browser wrote means the document took it, and
  `Views.drop` forgets the held view. A refusal is remembered per open (`selectDoc` clears it), so a grant taken
  mid-session is tried on the next open. `DocHandle.heads()` is base58 and the wire is hex; `decodeHeads` bridges
  them, and `main_test.ts` pins that the server's reply carries the head at all.
- **Known gaps**: `@library:freshness` resolves on the server but not in the page. `describe` results carry no type in
  the page, and `WINDOW_TYPES` is server-only, so a window alias there falls back to the sheet's stored `cols`.

## Schema (`schema/db.sql`)

- **usr** — identity, name, email (citext), password, `stripe_customer_id`
- **sheet** — the central polymorphic row. `sheet_id` is generated as `type || ':' || doc_id`. Marketplace fields
  (`sell_id` generated from `md5(doc_id||created_by)`, `sell_type`, `sell_price`, `license`, `buy_id`, `buy_price`),
  document data
  (`row_0`, `name`, `tags`), and `public boolean` for anonymous read through `syncRole`
- **sheet_usr** — membership, with `role` in owner/editor/viewer
- **db** — external database connections (DSNs for codex sheets, encrypted under `DSN_ENCRYPTION_KEY`)
- **secret** — a sheet's own secrets, encrypted. **No unique key on `(sheet_id, name)` on purpose**: the newest row for
  a name is current and the one before it still verifies, which is what lets a sender roll over
- **net** — rows for `net-*` sheets and the run log for `alert` and `codex-*`. `meta` is what the run cost. `net_id`
  identity PK, an index on `(sheet_id, created_at desc)`, and the unique `net_hook_signature_idx`. `trimNet()` keeps the
  newest `NET_KEEP` per sheet behind every write — a sheet that must keep everything writes to a table, which is never
  trimmed
- **webhook** — where a sheet's changes are posted: `url` per sheet, and the last delivery's `delivered_at`,
  `status` and `failures`, because a table sheet has no net log to record it on
- **audit** — who did what to which sheet: `sheet_id` (no foreign key, a deleted sheet's trail stays), nullable
  `usr_id`, `action`, `via`, `detail`. Never trimmed
- **payment** — marketplace transactions
