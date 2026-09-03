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
  `TOKEN_SECRET` and `DSN_ENCRYPTION_KEY` are set, and `main.ts` refuses to load without all three
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
it in with `--insert-statement`.

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
  destructure, `initializeWasm`, `Repo` and its storage stubbed, the websocket adapter genuine, `fetch` recorded and
  answered by the test — so `changeDoc`, `arrangeDoc`, `applyPatches`, `Views`, the query re-run guard, the share
  requests and the sync-refusal hook are the real ones; reach for it for anything about what the glue does. `docs`
  hands it a synced document, which is where a write is watched: the handle holds the test's own object. Always runs `deno task build` first. deno-dom is not enough — it has no
  `replaceData` on a text node.
- `browser_test.ts` — no browser: dist builds, `index.html` wires the WASM and the import map, every root-absolute asset
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
  / received / source / fix. The few surviving `throw new HTTPException` are passthroughs of an `explain()` block
  `src/sql.mjs` already built. `Received` goes through `show()`, never `JSON.stringify` — `explain()` drops an undefined
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
- **`POLL_OK` / `ALERT_OK` / `RUN_OF` / `RUN_OK` have one definition each.** `GET /status` and `library:freshness` both
  read them from there. Two hand-copied copies had already drifted.
- **The status check grades, never maximizes.** 1.0 is the minimum pass, `grade()` floors, and a condition that cannot
  compute throws by name.
- **Secrets never reach a document.** Sheet secrets are referenced from a net-http header as `{{secret:name}}` and
  resolved at fetch time into a separate object, so the automerge document sync hands every viewer keeps the reference.
- **One spelling per fact.** `API_BASE` in `src/page.mjs` is the only API host, handed to Elm through flags and to
  `index.html` by import. `PORTALS` in `src/portals.mjs` is the only portal list. `Stored` in `index.html` is the only
  `localStorage` key prefix. `spec` in `Main.elm` is the only per-column-type table, and it has no wildcard, so a new
  type fails to compile.
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
- **Computed sheets**: `library:freshness` and `net-hook:errors` answer through `sheet()` without an automerge document,
  so they page, export and can be selected from a query like any other sheet.
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
- **Polling**: `pollNetOnce` and `pollAlertOnce` on a 15-second tick; conditional requests, per-host `Retry-After`,
  bounded retries, and a `net` row per run — including quiet ones, because a healthy quiet alert and a dead timer
  otherwise write the same nothing.
- **Marketplace**: Stripe Checkout, platform-side. Connect payouts are not wired.
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
- **`describe @ref`** is intercepted before the engine in both engines, and is the one statement that still answers on a
  sheet whose cells fail the type check.
- **Types**: `COLUMN_TYPES` is every type a column may declare and what each one is; `NUMERIC_TYPES` is derived from
  it and `knownType()` matches the `enum:` family by prefix. `checkColumnTypes()` is the one place a cell becomes what
  its column says — a blank becomes `null`, a numeric string becomes its number. `selectTypes()` types a result column
  off its select item, not off its name, and `WINDOW_TYPES` says the same thing about a window: `sum` and `avg` follow
  their argument in both, so one name cannot mean two types.
- **Guards**: `checkQueryRows()` caps rows loaded across every `@sheet`; `checkResultColumns()` turns AlaSQL's silent
  undefined column into an error; `nearest()` backs every "did you mean".
- **AlaSQL gotchas**: a `group by` expression is evaluated against an empty row, so a UDF named there gets nothing — bin
  in a subquery first. An exception thrown from a function inside a from-clause subquery is discarded;
  `formatQueryError()` replaces the message that destroys. `min()`/`max()` drop text — use `min_text()`/`max_text()`.
  `total`, `store` and `class` will not parse as identifiers.

## Frontend map (`src/Main.elm`, `src/index.html`)

- **Architecture**: Elm Architecture. `update` is one exhaustive `case` with **no wildcard**, so a new `Msg` fails to
  compile rather than compiling and doing nothing. Its four long branches are `updateDocMsg`, `updateKeyDown`,
  `updatePaste` and `updateShareLoad`.
- **`Doc`**: `Library`, `Shop`, `Tab`, `Query`, `NetHook`, `NetHttp`, `Alert`, `Chart`, `Dashboard`, `NetSocket`. Every
  remaining server type decodes to `Unviewable typ`; give one a real view by replacing its branch in `docDecoder`.
- **Flags**: `{ api, tutorial }`. A missing `api` lands in `model.error` rather than defaulting.
- **Library**: `library()` in `src/page.mjs` merges what this browser stored under everything bundled. System ids skip
  `repo.find`. `viewGallery` reads `model.library`, so a new demo needs no code change.
- **Cross-sheet queries in the browser**: `sheets(alasql, shelf, find)` in `src/page.mjs`. Only two things come from the
  browser and both are arguments: the library map and `repo.find`.
- **Feed health**: `library:freshness` is read by `index.html` and handed to Elm through `freshnessLoaded`. The
  `freshness` column appears only when the answer is non-empty — a blank column over a logged-out library would read as
  "nothing is wrong".
- **Table UX**: multi-column sort, column hide (keeps its x coordinate; `skipHidden` steps over it), drag-resize,
  drag-reorder, pin, row insert/duplicate/fill-down, find/replace, undo/redo, command palette (Ctrl/⌘+K), shortcut
  sheet (Ctrl/⌘+/). `shortcutGroups` carries the `Msg` each key runs and `paletteCommands` reads that list, so the two
  cannot drift.
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
- **Reorder is a splice, pin is a sum.** A move is one `move` patch on `data[0]`, applied by `applyPatches` in
  `index.html` to the value the document already holds — rows are keyed by `col.key`, so no cell moves and the display
  index stays the document index. It is a `DocMsg` and not an arrangement: everyone looking at the sheet sees the new
  order, so it rides `changeDoc`, `movePatch to from` is its own undo entry, and a viewer's is refused like any edit.
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
  frame at a debug namespace and emits nothing.
- **Known gaps**: `@library:freshness` resolves on the server but not in the page. `describe` results carry no type in
  the page, and `WINDOW_TYPES` is server-only, so a window alias there falls back to the sheet's stored `cols`.
  `tableBounds` answers `0, 0` for a query sheet, so the keyboard does not move over one.

## Schema (`schema/db.sql`)

- **usr** — identity, name, email (citext), password, `stripe_customer_id`
- **sheet** — the central polymorphic row. `sheet_id` is generated as `type || ':' || doc_id`. Marketplace fields
  (`sell_id` generated from `md5(doc_id||created_by)`, `sell_type`, `sell_price`, `buy_id`, `buy_price`), document data
  (`row_0`, `name`, `tags`), and `public boolean` for anonymous read through `syncRole`
- **sheet_usr** — membership, with `role` in owner/editor/viewer
- **db** — external database connections (DSNs for codex sheets, encrypted under `DSN_ENCRYPTION_KEY`)
- **secret** — a sheet's own secrets, encrypted. **No unique key on `(sheet_id, name)` on purpose**: the newest row for
  a name is current and the one before it still verifies, which is what lets a sender roll over
- **net** — rows for `net-*` sheets and the run log for `alert` and `codex-*`. `meta` is what the run cost. `net_id`
  identity PK, an index on `(sheet_id, created_at desc)`, and the unique `net_hook_signature_idx`. `trimNet()` keeps the
  newest `NET_KEEP` per sheet behind every write — a sheet that must keep everything writes to a table, which is never
  trimmed
- **payment** — marketplace transactions
