# claude.md

The map: files, commands, schema, and the invariants a change must not break. The reasons live in the comment above the
code. Read that comment when you need a reason. Do not copy it here.

## Project

Scrapsheets is a programmable data OS shaped like a spreadsheet. Every table is a queryable database, every query result
is a shareable table, every sheet is an API.

- Backend: Deno + Hono, `main.ts`
- Frontend: Elm, `src/Main.elm`, glued by `src/index.html`
- Database: PostgreSQL, declarative schema in `schema/db.sql`
- Real-time: Automerge CRDT (https://automerge.org/llms-full.txt), documents in `data/automerge/`

## Files

| Path               | What it is                                                                                  |
| ------------------ | ------------------------------------------------------------------------------------------- |
| `main.ts`          | The whole server: routes, sync, polling, alerts, status, MCP                                |
| `src/Main.elm`     | The whole frontend: model, update, view                                                     |
| `src/index.html`   | The glue Elm cannot do: ports, the automerge repo, sockets, `fetch`                         |
| `src/page.mjs`     | The parts of `index.html` that are functions of their input, so tests can reach them        |
| `src/sql.mjs`      | The query engine both sides share: UDFs, ref resolution, the pre-engine passes, `explain()` |
| `src/examples.mjs` | Bundled datasets, reference tables and demo queries: the index of what ships                |
| `src/portals.mjs`  | The live demo feeds, `{ name, ms, init, tick }`, and the one list of portal names           |
| `src/sw.js`        | The service worker: serves the app shell from a cache when the network is gone              |
| `schema/db.sql`    | Desired schema, no data. `pg-schema-diff` diffs a live DB against it. No migration files    |
| `examples.sql`     | Shop catalogue of query templates, applied by `seed()` on first request                     |
| `vendor.ts`        | Rebuilds the vendored browser bundles in `src/` (automerge, automerge-repo, alasql)         |
| `deno.json`        | Tasks, dependencies, import map                                                             |

## Commands

- `deno task build` — copy `src/*` to `dist`, then `elm make`.
- `deno task dev` — build, then serve `dist`.
- `deno task test` — the whole suite. Never `deno test --allow-all`: only the task sets `JWT_SECRET`, `TOKEN_SECRET` and
  `DSN_ENCRYPTION_KEY`, and `main.ts` refuses to load without them. The task builds `dist` once, type checks once beside
  the run, runs the files in parallel, and fails past ten seconds. Time one file with `deno test --allow-all <file>`.
  Check `top` first: a build job on the machine makes every timing wrong.
- `deno task review` — elm-review. Keep it at zero suppressions.
- `deno task status` — grade the deployed `GET /status`; exit nonzero below 1.0. `.github/workflows/status.yml` runs it
  every 15 minutes. The failure email is the alarm.
- `deno task vendor` — rebuild the browser bundles after you bump the versions at the top of `vendor.ts`.
- `deno task db:plan` / `db:apply` — read the generated migration, then apply it. **Check `.env` first: `DATABASE_URL`
  may point at production.** `db:apply` does not prompt. It allows only `INDEX_BUILD,INDEX_DROPPED`. For a migration
  with another hazard, run `pg-schema-diff apply` by hand with that one hazard added. Do not widen the task.
- `deno run -A npm:elm-format --yes src/Main.elm` — format Elm.
- Watch: `watch src { try { cp -vu src/* dist ; elm make src/Main.elm --debug --output=dist/index.js } }`

**Schema changes.** Edit `schema/db.sql`. DML is never generated; splice it in with `--insert-statement`. Local Postgres
is `postgresql://postgres@127.0.0.1:5434/postgres`; tests need none. Deploy first whichever half the other half needs:

- A check constraint on a column `seed()` writes: backfill, deploy the code, then `db:apply`. Postgres checks an
  upsert's proposed row before the conflict, so the reverse order fails every request.
- A table the code writes on every request (like `audit`): `db:apply`, then deploy.

## Tests

`deno test --parallel` runs files side by side, so the slowest file sets the suite's time. Split a file only when it is
that file. Keep the jsdom pairs even in boots: `grep -c "await boot(" page_test.ts library_test.ts`, and the same for
`glue(` in `glue_test.ts sync_test.ts`.

- `main_test.ts` — the server. One `Deno.test` of ordered `t.step`s against in-process PGlite behind pg-gateway on
  `5434`. The steps share one database, so a step depends on the steps before it. `--filter` does not match step names.
  A second PGlite on `5435` is the codex external database, cloned from the first before the schema applies, built on
  first connect. A DSN that must fail names a loopback port nobody listens on, never a hostname. `request()` clears
  `rateLimitBuckets`; the limiter steps call `app.request` directly. **Do not split this file**: it was built and
  measured. The halves share `Deno.env` and `data/automerge`, and the gain was about half a second.
- `examples_test.ts` — every bundled sheet through both engines (`npm:alasql`, `src/alasql.mjs`), row for row. Refuses a
  chart source with a repeated (series, x) pair.
- `page_test.ts` — the table and the query sheet: render, sort, arrange, keyboard.
- `library_test.ts` — the library, the sheets opened from it (feed, alert, chart, dashboard), the palette, and the parts
  of `src/page.mjs` that need no page.
- `glue_test.ts` — what the glue does to a document.
- `sync_test.ts` — what arrives from outside: a CSV chosen or dropped, a socket report, a fork, a real automerge
  document, the row and column verbs, and `src/sw.js` over a hand-made `self`, `caches` and `fetch`.
- `page_harness.ts` — shared by the four jsdom files: compiled Elm, window globals, `boot`, `rendered()`, `until`,
  `settle(ms)`, the page-side engine. Refuses a `dist` older than `src`.
- `glue_harness.ts` — `glue()` runs `index.html`'s module script over jsdom with `fetch` and `WebSocket` recorded.
  `docs` hands it a synced document. `realRepo` swaps in real automerge.
- `browser_test.ts` — no browser. `dist` is fresh, `index.html` wires the WASM and import map, every root-absolute asset
  is in `_redirects`, imports resolve, nothing reaches a CDN. Lints `index.html`'s module script and `src/sw.js` through
  `deno lint` (`BROWSER_GLOBALS` is the allowlist). Holds the language-boundary copies equal (see Invariants).
- `tests/MainTest.elm` via `elm_test.ts` — pure Elm.

Which harness call to use:

- `boot` for what the page renders. `rendered()` when the test writes nothing to the model.
- `glue` for what the glue does. `realRepo` when a patch must mean the same to real automerge.
- `until()` waits for something to happen. `settle(ms)` only proves that something did **not** happen, or waits on a
  named real timer.

Measured dead ends: `elm make --optimize` changes nothing; the flat `settle(ms)` waits cannot shrink; deno-dom lacks
`replaceData`. A boot costs Elm's first paint, so the levers are fewer boots or another file.

## Invariants

A change that breaks one of these is a bug even if the suite is green.

- **One identity.** `usr_id` is a string, set in the one middleware that reads `jwtPayload.sub`. A share-link token has
  no `sub` and gets 403 before any route.
- **One refusal shape.** Every 4xx/5xx is `bad(status, headline, fields)`: expected, received, source, fix. `Received`
  goes through `show()`, never `JSON.stringify`. At the boundary: a NUL in a path or query is refused before routing,
  `bodyLimit` caps bodies at `BODY_CAP`, and `jsonBody()` reads every JSON body. `cselect()` refuses a bad `limit` or
  `offset`. A surviving `throw new HTTPException` only passes through an `explain()` block. Tests wait on
  `errorLogged()`, never on a sleep.
- **No refusal is an oracle.** A signature refusal never prints the secret, the expected digest, or how close a guess
  was.
- **One engine, two hosts.** `src/sql.mjs` runs on the server (`npm:alasql`) and in the page (`/alasql.mjs`). Both call
  `toRecords()`, `loadRefs()` and `planQuery()`; where a sheet comes from is an argument. A query means the same in
  both.
- **Rows are keyed by column name at every boundary**: `GET`/`POST /sheet/:id`, every export, every MCP tool. `col.key`
  never leaves the document. Two columns of one name: refused on read and export; the importer and `nameClash()` refuse
  to make one.
- **Every jsonb write goes through `sql.json(...)`**, never `JSON.stringify`.
- **Every cast out of jsonb is guarded inside a `case`**, never beside it with `and`.
- **Every bounded map goes through `bound(map, max)`.** Every loop, retry and recursion has a bound whose message
  carries the counter.
- **One budget per sheet.** `spend(sheet_id, what, rows, bytes, fix)` on `hookBucket()`, synchronous. Every door into a
  document sheet spends one unit: a webhook delivery, a socket report, a whole read through `sheet()`, an append (its
  bytes as volume). Computed sheets are free. Spend after the access check. The refusal is an unlogged 429.
- **One account is bounded.** `rateLimit()` on `accountBuckets`. `assertSheetsQuota()` at claim, import and purchase.
  `assertRoom()` caps rows at `MAX_QUERY_ROWS` at import and append. `sendWithinQuota()` caps alert deliveries a day,
  email and url alike. A refusal that changed what an account keeps or sends says "quota".
- **One definition each:** `POLL_OK`, `ALERT_OK`, `RUN_OF`, `RUN_OK`, `pauseSwitch`. `GET /status` and
  `library:freshness` read them. A `null` from `pauseSwitch` is "unknown" in freshness and "running" in status.
- **The status check grades, never maximizes.** 1.0 is the minimum pass, `grade()` floors, and a condition that cannot
  compute throws by name.
- **Secrets never reach a document.** A net-http header says `{{secret:name}}`, resolved at fetch time into a separate
  object. `assertNoKeys()` scans on `POST /library/:id/public` and a priced `POST /sell/:id`, after the owner check,
  bounded by `KEY_SCAN_CELLS` and `KEY_SCAN_BYTES`. `KEY_SHAPES` has no override. `PII_SHAPES` (email, phone, US SSN, a
  card `luhn()` accepts) is overridden by `personal: true`, a boolean `claimsPersonal()` checks. Refusals name the
  column and the 1-based row, never the value. A scan that cannot run refuses: the computed ids, `library:lineage` and
  the `codex-` prefix are named skips, and a document that will not load refuses the publish.
- **One spelling per fact.** `API_BASE` in `src/page.mjs` is the only API host. `PORTALS` is the only portal list.
  `Stored` in `index.html` is the only `localStorage` prefix. `spec` in `Main.elm` is the only per-type table, with no
  wildcard.
- **A column type is one word everywhere.** `COLUMN_TYPES` in `src/sql.mjs` is the list; an `as` alias is read and never
  written. `CANONICAL_TYPES` is what may be written. `NUMERIC_TYPES` and `JSON_TYPES` derive through `canonicalType()`.
  `checkColumnTypes` refuses a spelling outside the list.
- **A column's declared type is never rewritten.** `col.raw` is the document's spelling. A header write patches
  `[0, x, "name"]`, never the whole column object.
- **Language-boundary copies.** `browser_test.ts` reads both sides as source text and fails on drift, compared as sets:
  - `CHART_KINDS` (`src/sql.mjs`) ↔ `kindSpec` (`Main.elm`)
  - `NET_METHODS` (`main.ts`) ↔ `netMethods`
  - `NET_MODES` ↔ `netModes`
  - `PAGE_BY` ↔ `pageBy`
  - `ALERT_WHEN` ↔ `whenSpec`
  - `COLUMN_TYPES` ↔ `main.ts`'s `Type` union ↔ `columnTypes` / `typeAliases`
  - `SHELL` (`src/sw.js`) ↔ `_redirects`, both directions

  Held equal elsewhere: `similarity` / `soundex` (`src/sql.mjs` ↔ `Main.elm`) by the same pairs in `main_test.ts` and
  `tests/MainTest.elm`.
- **No runtime CDN.** The page loads only what we serve. `deno task vendor` fails if a bundle still fetches. Automerge
  stays external in the repo bundles so all three share one WASM copy.

## Backend (`main.ts`)

One file on purpose; the header comment says why. The `// ---` sections, in file order:

`refusals` · `secrets & crypto` · `webhook signing` · `types` · `sheet & query core` · `database` · `app & middleware` ·
`seeding` · `automerge sync` · `live portals` · `public routes` · `status` · `delivery signatures` · `delivery budgets`
· `net-http polling` · `alerts` · `authenticated routes` · `freshness` · `sharing` · `secrets` · `import/export` ·
`codex (external databases)` · `mcp`

**Sheets**

- Types (the check constraint in `schema/db.sql` is the list): `template`, `table`, `net-hook`, `net-http`,
  `net-socket`, `query`, `portal`, `alert`, `chart`, `dashboard`, `codex-*`. A sheet id is `type:doc_id`.
- Computed sheets answer through `sheet()` with no automerge document: `library:freshness`, `library:lineage`,
  `library:audit`, `net-hook:errors`, `net-hook:reports`. `isOperator()` is whoever reads `net-hook:errors`;
  `OPERATOR_EMAIL` gets it at seed.
- A new computed sheet registers in three places: the early return in `sheet()`, `assertNoKeys`'s skip list, and its
  `GET /library/...` route.

**Access**

- Auth: JWT middleware. A per-sheet `scrapsheets-key` is scoped by a path check before routing, over `/sheet/:id`,
  `/openapi/:id`, `/mcp/:id`. `POST /library/:id/secret` mints it under `API_KEY_NAMES`: `api` writes, `api-read` reads.
  `apiKeyScope` sets `key_sheet` / `key_scope` (unset under a JWT). `assertKeyWrites` refuses a read-only key where the
  verb is: the path check for `POST /sheet/:id`, and inside `write_cells`. Email through Resend.
- Sync: automerge `NodeWSServerAdapter` behind a ws-shim over Hono's `upgradeWebSocket`. `syncRole` checks access per
  message. A viewer's frames are decoded and refused if they carry changes.
- Audit: the `audit` table, read as `library:audit`. `record()` is the one writer. HTTP routes in `AUDITED` log after
  success; the socket logs `open` and a first `edit` per peer per document; MCP logs `mcp <tool>`; a query logs `query`
  per sheet it resolves. A row that cannot be written fails its request.

**Inbound**

- Webhook ingest: `POST /net/:id`, always signed (`scrapsheets-signature: t=…,v2=…`, or Stripe / GitHub / Shopify by the
  stored secret's name). `net_hook_signature_idx` on the verified digest refuses a replay.
- Socket health: `POST /library/:id/socket` from a browser with the tab open. States: `connected`, `error`, never a
  close. Freshness lists a `net-socket` sheet only after its first `SOCKET` run.
- Import: `readImport()` is the one CSV reader. `POST /import/preview` answers columns, types and first rows.
  `POST /import/csv?types=…` makes the sheet; `types` is keyed by column name and checked against `CANONICAL_TYPES`.

**Polling**

- `pollOnce` every 15 seconds under one re-entrancy flag: `pollNetOnce`, then `pollAlertOnce`. Each half catches its own
  error and spends its own cycle budget (`POLL_CYCLE_MS`, `ALERT_CYCLE_MS`). `pollNetSheet` / `pollAlertSheet` are the
  one-sheet halves the timer and `POST /library/:id/run` share.
- Request: `method` is one of `NET_METHODS`; `netRequest` refuses a GET with a body. Headers and `body` resolve
  `{{secret:name}}` and `{{cursor}}`; a failure row's repro keeps the unresolved text. Conditional requests, per-host
  `Retry-After`, bounded retries. One `net` row per run, quiet runs too. Every row carries its verb and the watermark.
- Shape: `meta.shape` from `shapeOf`. A different shape adds `meta.shape_change` (`shapeChange`), and `POLL_OK` grades
  that run failed.
- Repeats: a good body's digest goes in `meta.sig`. `net_hook_signature_idx` refuses the repeat, and `netRow` moves the
  matched row to now, marked `repeated`.
- A query over a net-http sheet reads only `POLL_OK` rows. The sheet view and the export show the whole log.
- Paging: `pageConfig()` reads `page_by` (one of `PAGE_BY`: `page`, `offset`, `cursor`, `link`), `page_param`,
  `page_path`. `pageRows()` parses a page (a top-level array, or the one array-valued key; two is refused). `nextPage()`
  stops. The pages concatenate into one body, bounded by `PAGE_MAX` and by `BODY_CAP` on the summed parsed size. A
  `link` next page must share the sheet's origin. A failure on any page fails the whole poll and keeps nothing. Page one
  alone carries the validators, `{{cursor}}` and the host hold. Pre-flight is always one request.
- Storage: `storeConfig()` reads `mode` (one of `NET_MODES`: `append`, the default; `replace`; `upsert`), `key`
  (`upsert` only) and `rows_path`. `NET_PATH` checks every dotted path; `atNames()` walks one. `netRow()` inserts and
  supersedes in one transaction: `replace` deletes earlier `POLL_OK` rows; `upsert` writes `meta.keys` through
  `rowKeys()` (refuses a missing key and a number past `Number.MAX_SAFE_INTEGER`) and deletes earlier rows whose keys
  overlap. Only a 2xx supersedes. A 304 keeps the moved row's meta and overwrites only what not-modified changes.
- `namedRows()` applies `rows_path` when there is no paging. It refuses an answer that is already an array, and a row
  that is not an object.
- Pause and run: `paused: true` in `data[0]` skips a net-http or alert sheet and keeps its due entry. A paused sheet
  leaves the two liveness conditions until `OVERDUE_MAX`. `INTERVAL_MAX_S` clamps `interval`. `POST /library/:id/run`
  needs `assertSheetEditor`, spends `runs`, and answers the newest `net` row at or after a watermark from Postgres's
  clock, as epoch seconds (`net.created_at` has no timezone). Refusals: paused 409, wrong type 400, nothing recorded
  409\. Freshness adds `paused` and `next_run`.

**Feed bodies**

- `BODY_PARSERS` maps a declared content type to a reader. `readFeedBody` is the one reader, for the poller and for
  `POST /net/:id`. An unlisted type (JSON too) is stored as text. A non-2xx body is never parsed. A parsed body is
  stored as the JSON text it means.
- `BODY_CAP` bounds three numbers apart: wire bytes, decompressed bytes, and the size the body means.
- A NUL is refused as `readFeedBody`'s last check, on what is stored.
- `jsonMeant()`: a declared `application/json` is trusted. A type this server guessed (a gzip's content, a zip member's
  name) must pass `JSON.parse`. The text that arrived is stored, so `meta.sig` stays stable.
- CSV and TSV go through `parseDelimited`, shared with `readImport()`. NDJSON is one value per line.
- `expand()` is the one bounded decompressor, fed `EXPAND_SLICE` at a time. A gzip's content is JSON if it starts with
  `[` or `{`, else CSV.
- Zip: `zipMembers()` reads the central directory; `zipData()` checks the length and `crc32()`. `ZIP_MEMBERS` maps
  extension to type and names no archive; with `BODY_DEPTH_MAX` that bounds nesting. Exactly one nameable member;
  directories and `__MACOSX/` are not candidates. Refused: zip64, encryption, a method other than stored or deflate. A
  member name never enters a headline and always goes through `show()`; `NAMES_MAX` bounds a list of names.
- XML: `npm:fast-xml-parser`. `xmlDoc()` runs `XMLValidator` before `XMLParser`. `parseTagValue: false`, attributes
  under `@`, `removeNSPrefix`. `xmlReaderFor(rowsPath)` holds `item`, `entry` and `rows_path` to arrays. RSS and Atom:
  `xmlRows()` collects `<item>` / `<entry>` depth first in document order, bounded by `XML_NODES_MAX`, and refuses a row
  that is not an object. `xmlFeedRoot()` refuses a root not in `XML_FEED_ROOTS`. Generic XML stores the document;
  `rows_path` picks the rows.
- HTML: `npm:linkedom`. `htmlDelimited()` takes the page's one `<table>` (more is refused, named by id, caption or
  text), orders rows by `SECTION_RANK`, reads cells through `markupCell()`, and writes a quoted file for
  `parseDelimited`. An empty table or row is refused.
- Parquet: read as bytes. `parquetRows()` checks `PARQUET_MAGIC` at both ends. Compressed codecs are not supported. An
  INT64 is a number inside the safe range, else decimal text. A Date is ISO text. `application/octet-stream` is not
  parsed.
- Encoding: `decodeAs()` is the one decoder: fatal, keeps a second BOM. `readFeedBody` honors the answer's `charset`;
  with none it decodes UTF-8 non-fatally and sniffs nothing. `markupText()` for XML and HTML: BOM or NUL sniff, then the
  answer's `charset`, then `XML_ENCODING` / `HTML_CHARSET` in the first `MARKUP_HEAD_BYTES` (comments and scripts cut),
  then UTF-8.

**Alerts**

- `when` is `rows` (default), `added` or `removed`. `ALERT_WHEN` ↔ `whenSpec`. `status` is the verdict, `delivery` what
  was done. A change condition's first run is a silent baseline. Past `ALERT_ROWS` is an `error` row.
- A `to` matching `^https?://` goes through `sendAlertUrl` and `safeFetch`: Slack `{text}`, Discord `{content}`, Teams
  MessageCard (`webhook.office.com`, `*.webhook.office.com`), anything else `{sheet, name, rows}`. A digest over a url
  is refused. The run row keeps the url's host, never its path. `KEY_SHAPES`' Teams regex matches every host
  `sendAlertUrl` posts to.
- Snooze: `snoozed_until` is an ISO timestamp in `data[0]`; one that will not parse is an error run. A snoozed run is
  decided and recorded with `delivery: snoozed`, after the no-destination check and before the digest branch. `ALERT_OK`
  admits it, `sendWithinQuota` skips it, and it is `stuck`.

**Outbound**

- `safeFetch` is the one door out. It sends `USER_AGENT` and runs `assertPublicHost()` on the literal and every resolved
  address. Only a GET follows a redirect. A DNS not-found is a 400; any other resolver failure is a 502. `holdHost()`
  writes `hostDue`: the later of the gap and `Retry-After`.
- Webhooks: `POST /library/:id/webhook` needs a signed `ping` to answer 2xx. `flushWebhooks()` posts one signed `change`
  per hook per touched document, heard on storage `doc-saved` / `doc-compacted` (the first save after `doc-loaded` is
  not a change); a net row touches too. Signed with the sheet's `hook` key. A provider-scheme sheet cannot register.
  Spends `webhooks`. `WEBHOOK_FAILS_MAX` failures disable a hook, which fails `GET /status`. `WEBHOOK_FLUSH_MAX` bounds
  a flush. Owner or editor only.
- Exports: `GET /export/:id.{csv,json,ndjson,md,ics,xlsx,parquet}` is one route over `EXPORTS`; add a format by adding a
  row. xlsx: `npm:xlsx@0.18.5`, **write only, never read**. `xlsxCell` types by `canonicalType`, `XLSX_FORMATS` per
  column, width bounded by `XLSX_WIDTH_MAX`, dates through `dateMs()` as UTC, a value past `XLSX_CELL_MAX` refused by
  place and length, the sheet name cut to `XLSX_NAME_MAX`. Parquet: `npm:hyparquet-writer`, built off `named()`.
  `PARQUET_TYPES` through `canonicalType`; `parquetColumn()` writes a column whole as STRING when any value does not
  fit.

**Codex (external databases)**

- A sheet keeps `DSN_KEEP` credentials. `POST /codex-db/:id` inserts and trims. `GET /codex/:id` tries the newest first
  and falls back only when `cannotConnect()` says so (SQLSTATE 08, 28, 3D). The catch wraps `codexTables()` alone.
- `checkCodexDsn()` refusals never roll over. `canonicalHost()` normalizes the host; `assertPublicHost()` checks it,
  unless our own database is on loopback.
- Every attempt writes a `codexRun()` row with `meta.rolled_over`; `POLL_OK` grades a rollover failed. No refusal quotes
  a DSN. A dead connection under `@codex-db:x` is a refusal, never an empty result.

**MCP**

- JSON-RPC 2.0 at `POST /mcp/:id`: `initialize`, `ping`, `tools/list`, `tools/call` (`read_sheet`, `query_sheet`,
  `list_sheets`, `write_cells`), `resources/list`, `resources/read`, `prompts/list`, `prompts/get`. JWT or one sheet's
  key.
- Under a key, the authority stops at that sheet: `mcpSheets` narrows the library, `mcpSheetId` refuses another
  `sheet_id`, and `query_sheet` refuses an `@ref` out of it before the load.
- Resources are `sheet://<id>` as `text/csv`, rendered byte for byte like `GET /export/:id.csv`. The one prompt,
  `describe_sheet`, is built from `describeRows`. Both read the whole sheet through `MCP_WHOLE_SHEET`. An unknown uri is
  `-32002`; an unknown prompt `-32602`.

**Lineage (`library:lineage`)**

- One row per edge (`sheet_id`, `name`, `type`, `depends_on`, `depends_on_name`, `depends_on_type`, `columns`) for every
  query, alert and chart sheet the caller holds. The code comes from the live document (`data[0].code`, or `chartSql`),
  never `sheet.row_0`, through `scanRefs`, deduped. Bounded by `USER_SHEETS_MAX` documents and `MAX_QUERY_ROWS` edges.
  An unloadable document is a row with null `depends_on` and the refusal in `name`.
- `columns`: the names the statement mentions (`namesIn()`) that exactly one ref holds (`readColumns()`), sorted and
  comma-joined; `*` when `SELECT_STAR` matches; `?` when a ref's columns cannot be read.

**Money**

- Marketplace: Stripe Checkout, platform side. Connect payouts are not wired. A listing needs a `license` from
  `LICENSES`. `POST /shop/:sell_id/report` adds one row per account per listing to `net-hook:reports`;
  `POST /shop/:sell_id/review` is the operator's `keep` or `takedown`; an open report fails `GET /status`.
- `GET /shop` answers `type` and `tags` and takes `?tags=`, `?name`, `?sell_type`, `?sell_price`.

## Query engine (`src/sql.mjs`)

`planQuery()` runs the pre-engine passes in order.

- Refs: `@type:doc_id` is a sheet; `@type:doc_id.column` is a scalar from a one-row sheet. `scanRefs()` is the one
  scanner. `checkRefPath` bounds depth and names a cycle.
- `rewriteExtremes()` runs first. It aims `min(x)` / `max(x)` at `min_text` / `max_text` when `x` is a bare column every
  loaded sheet types as one of `TEXT_TYPES`. It skips a windowed call, an expression, a name typed two ways, and a name
  the query aliases. `MAX_EXTREMES` bounds the calls.
- Windows: `rewriteWindows()` lifts each `over (…)` from the top-level select list; `applyWindows()` computes it over
  the returned rows. `qualify` rides the same pass. A window that is not its own select item is refused.
- Decomposition: `trend`, `seasonal`, `deseasonalized` (the `DECOMPOSE` list) take `(y, period)`. `decompose()` answers
  a partition once, cached in `decomposed`. A blank y is a null. Refused: period not a whole number ≥ 2, fewer than two
  cycles, an uncovered phase. Not a forecast: nothing adds a row.
- Unpivot is ours; pivot is AlaSQL's, guarded by `checkPivot()`.
- `describe @ref` and `explain <query>` are intercepted before the engine. `timed()` wraps the stages for `explain`.
- Generated names are quoted: `chartIdent(owner, name)` answers `[name]` and refuses a name that is not an identifier.
  `bare()` is the one place a bracket comes off. `rewriteUnpivot` holds its names to the same identifier shape.
- `namesIn()` answers the identifiers a statement mentions, with literals, refs and the statement's own aliases
  (everywhere spelled) blanked first. `x as x` stays. `KEYWORD` is the one list of non-column bare words; a bracketed
  name is exempt. `MAX_NAMES` bounds the count.
- `cohortSql({ source, date, key, value, grain })` writes a cohort table once, at creation, as an ordinary query's
  `code`. `grain` is a key of `COHORT_LABEL`. No input may collide with an output name or with another input.
- Types: `COLUMN_TYPES`, `knownType()` (the `enum:` family by prefix). `checkColumnTypes()` is where a cell becomes its
  column's type. `selectTypes()` and `WINDOW_TYPES` type a result off its select item.
- Fits: `fit_exponential()`, `fit_power()` through `curve()`. `fit_hyperbolic()` is Arps decline by Levenberg-Marquardt,
  bounded by `HYPERBOLIC_STEPS` and `HYPERBOLIC_POINTS`, `b` in `(0, B_MAX]`.
- Regression: `ols(array(y), array(x1), …)` → `[b0, b1, …]`; `ols_predict(coefs, …)`. `logit` / `logit_predict` reweight
  the same equations, bounded by `LOGIT_STEPS`, `y` all 0 or 1, separation refused through `LOGIT_FIT`. `gauss()` is the
  shared solver and names the dependent column. `design()` is the one validator (`OLS_POINTS`, `OLS_TERMS`,
  `OLS_SINGULAR`).
- Samplers: `sample_uniform`, `sample_normal`, `sample_triangular`, read back by `percentile()`. Never `Math.random`:
  each call seeds mulberry32 from an FNV-1a hash of the whole call. A non-finite draw is refused.
- Guards: `checkQueryRows()` caps rows loaded; `checkJoinRows()` caps the from clause's product at `MAX_JOIN_ROWS`;
  `checkResultColumns()` turns a silent undefined column into an error; `nearest()` backs every "did you mean".
- AlaSQL gotchas: a `group by` expression sees an empty row, so bin in a subquery first. A throw inside a from-clause
  subquery is discarded; `formatQueryError()` restores it. `min()` / `max()` drop text. `total`, `store` and `class`
  will not parse bare. Never patch `src/alasql.mjs`; `deno task vendor` rebuilds it.

## Frontend (`src/Main.elm`, `src/index.html`)

**Structure**

- `update` is one exhaustive `case` with no wildcard. Long branches: `updateDocMsg`, `updateKeyDown`, `updatePaste`,
  `updateShareLoad`.
- `Doc`: `Library`, `Shop`, `Tab`, `Query`, `NetHook`, `NetHttp`, `Alert`, `Chart`, `Dashboard`, `NetSocket`. Other
  types decode to `Unviewable typ`; replace that branch in `docDecoder` to give one a view.
- Flags: `{ api, tutorial }`. A missing `api` goes to `model.error`.
- `updateDocMsg` refuses every `DocMsg` on the library, so library verbs (`TrashSelected`, `TagSelected`) are top-level
  `Msg`s. They read ids through `libraryIdAtRow` (drawn order) and fan out one `updateLibrary` each.

**Library**

- `library()` in `src/page.mjs` merges this browser's store under everything bundled. `seen`, `trashed` and `starred`
  overlay a system entry when truthy; `tags` merges, bundled first. `updateLibrary` is the one port that writes them;
  `Library.set` drops a null field from the patch.
- Trash is undoable and asks nothing; `deleteDoc` purges and calls `Views.drop`. A trashed sheet leaves the table, the
  demo strip and the palette. `model.trash` swaps the last column.
- `Star` is a `Type` with no `columnTypes` entry; its cell carries `{id, on}`. Starred sheets sort first only while
  `sheet.sort` is empty.
- A tag goes on many sheets from the strip: `sheet.tag`, `TagInput`, `onTagKeydown`. It is added, trimmed, never
  lowercased. Refused: empty, holds a comma.
- Freshness: `index.html` reads `library:freshness` into `freshnessLoaded`. The column shows only when the answer is
  non-empty.

**Queries and the editor**

- `sheets(alasql, shelf, find)` in `src/page.mjs` runs cross-sheet queries in the page.
- Completion: `completionTrigger`, `completionRef`, `completionAt`. Columns come through the `columnsFor` /
  `columnsLoaded` port, answered by running `describe @<ref>` in the page. A failed ref logs by name and caches an empty
  answer. `ColumnsLoad` recomputes the open list. The dropdown is `id="complete"`.

**Table**

- Multi-sort, hide (`skipHidden`), resize, reorder, pin, row insert / duplicate / fill-down, find/replace, undo/redo,
  palette (Ctrl/⌘+K), shortcuts (Ctrl/⌘+/). `shortcutGroups` carries each key's `Msg`, and `paletteCommands` reads it.
- The palette opens with nothing selected (`selected = -1`). `paletteRows` adds "subscribe to this sheet" (logged in,
  over a table, query, net-http or net-hook) and "build a cohort table" (a date and a key column known) ahead of
  `paletteCommands`.
- Export chips link to `/export/<id>.<format>` for csv and xlsx.
- `arrangeControls` decides where the arrangement is offered: table, query, library, shop.
- The arrangement (sort/`rank`, `filter`, `hidden`, `pinned`, `width`, `decimals`, `format`, `shade`) lives on the
  columns: `tableHome` (position in `data[0]`) or `queryHome` (under `view`, by name), picked by `arrangeable`.
  `viewDecoder` reads it; `arrange` writes the diff against `sheet.storedView`, outside undo. Typed fields reach the
  document when the panel closes; selects write on click. An unreadable field is no arrangement, never an error.
  `pruneView` is table only. `colViewFields` is `D.map8` plus `andThen` for `shade`.
- `arrangeDoc` writes the document first and the browser store last, and never drops a batch. When the document cannot
  hold the arrangement (bundled, or the server refused a write), `Views` keeps it under `scrapsheets-views`, held by
  `col.key`, merged back in `selectDoc` through `foldView` / `mergeView`. A `sync` frame covering the written head calls
  `Views.drop`. `decodeHeads` bridges base58 and hex heads.
- Reorder is one `move` patch through `changeDoc`, undoable, refused for a viewer; `dropOf` builds it. A row handle
  shows only while `inDocumentOrder`. `pinLeft` sums sticky widths, column 0 included.
- Fill-down: `fillSeries` continues dates (`justinmimbs/date`, stepped off the last seed), numbers and trailing digits,
  else repeats. `parseDay` decides a date. `blankCell` decides blank. `seriesEncoder` writes by column type.
- Cleaning, in the column panel behind `movable`: trim, UPPER, lower, drop blank rows, split, near-duplicates.
  `SheetRowsDedupe` is in the palette. Helpers: `cellRewrites()`, `blankRows()`, `duplicateRows()` (signs a row by its
  cells), `rowDeletions()`. They read every document row and reach a table only.
- Split: `sheet.splitOn` via `ColumnSplitInput`, never the document. `SheetColumnSplit key delimiter` → `columnSplit()`:
  new text columns `<name> 1..n` keyed past every existing key; undo is `del`s then one `splice`. Refused with nothing
  written: empty delimiter, unknown key, no text cell, no match, past `maxSplitColumns`, `nameClash`.
- Near-duplicates: `sheet.near` via `ColumnNearInput`. `nearDuplicates` buckets by `soundex`, scores by `similarity`,
  compares against rows that stay. Bounded by `maxFuzzyPairs` and `maxFuzzyRows`. Answers a `Result`; the preview reads
  `sheet.doc` and counts unreadable rows.
- Shade: `Shade` is `Scale` or `Bars`; `shadeSpec` is the table, `shade` the lenient reader. Gated by `numericColumn` in
  the panel and again where `columnExtent` is built. Extent is over the drawn rows. Drawn as `div.shade` inside the
  `td`.
- `formatNumber` is the one place a number becomes text: cell, stats row, totals row. `positional` guards non-digit
  values. `digitsOf`, `fixed`, `groupWhole`, `scientific`, `maxDecimals`. `NumberFormat` / `formatSpec` / `numberFormat`
  are the format list and reader. A format lands on top of the type.
- A `json` cell holding a number list draws a sparkline: `sparkValues`, `sparkMax`, `viewSpark` (shared with
  `viewThumb`).

**Sheets with settings**

- Net-http: `page_by`, `page_param`, `page_path`, `mode`, `key`, `rows_path` sit in `data[0]` through `optionalField`.
  `pageForm` and `storeForm` are the tables of what each mode takes. `pageByDecoder` and `netModeDecoder` refuse unknown
  modes; the free-text fields are not checked. The branch is `D.map2` over `D.map5` and `D.map6`.
- Pre-flight: `preflight { id, url, headers, method, body }` → `POST /library/:id/preflight` → `preflightLoaded`,
  matched by id, drawn by `viewPreflight`.
- Run now and pause: a `paused` checkbox, `runNow` / `runLoaded`. `runLine` picks the shape by the row's `method`.
- Alert snooze: `snoozedUntil`, `isoStamp`; stamps compare as text.
- Import: `CsvImportFile` / `importCsv`, or a drop through `setupDragDrop`, both into `uploadCsv` →
  `POST /import/preview` → `importPreviewed` → `viewImport` → `ImportConfirm` → `POST /import/csv`. `rememberedTypes`
  under `scrapsheets-imports` keeps types by header. `parseCsv` in `Main.elm` is the clipboard, not import.

**Charts**

- `chartSql` builds the statement from `x`, `y`, `series`, `y2`, and `kind` (one of `CHART_KINDS`). `Chart` in `main.ts`
  and `Chart_` in `Main.elm` carry the settings.
- A chart's source must answer one row per (series, x); `examples_test.ts` checks the bundled ones.
- `chartPoints key tbl` groups by `series` in first-seen order. Labels and series cells read through the lenient
  `string`, never `D.string`.
- `y2` has its own scale and is always a dashed line; refused on `kpi`. `legendName` names columns when there are two
  scales.
- Box: `chartSql` aggregates `min`, `BOX_QUANTILES`, `max` per x and filters null y; `chartBoxes` reads it. `series` or
  `y2` on a box is refused.
- Day axis: `chartRuns` places by day when every x `parseDay`s, sorts each series, and breaks a run at twice the median
  step. Bars stay ordinal. `chartFold` folds a series past `chartPointsMax` into bucket means; the settings count what
  folded. `chartSpan` / `chartAt` place points and annotations alike.
- Annotations: `{ at, label }` in `data[0]`, edited through `parseAnnotation`, written by `InputChange ChartAnnotations`
  through `changeDoc`. A mark with no span or no parseable day is not drawn.
- Legend: `legendLayout` wraps by estimated width, bounded by `legendMax` ("+N more"). `plotTop` grows 14 per extra row
  and is clamped. `chartColours` cycles; its first is `#468`. An unsplit chart draws no legend.

**Shell**

- Installable: `src/manifest.webmanifest` names `src/icon.svg`; both are in `src/_redirects`.
- Offline: `src/sw.js`, registered by `navigator.serviceWorker?.register("/sw.js")`; a refusal logs. Same-origin GETs go
  network first, cache second. `SHELL` is every `_redirects` path plus `/`, keyed by pathname. Nothing cross-origin is
  answered. Only the shell works offline.
- Accessibility: `viewModal` takes a label and sets `role="dialog"`, `aria-modal`, `aria-label`. Icon-only buttons and
  placeholder-only inputs carry `aria-label`. The table is `role="grid"`.

**Known gaps**

- `@library:freshness` and `@library:lineage` resolve on the server, not in the page.
- `describe` results carry no type in the page, and `WINDOW_TYPES` is server-only.
- No modal focus trap or restore, no keyboard path for `.grab` / `.grip`, no `gridcell` / `aria-selected`, no
  `aria-activedescendant` in the palette, and two `aria-modal` panels can mount at once.

## Schema (`schema/db.sql`)

- **usr** — identity, name, email (citext), password, `stripe_customer_id`.
- **sheet** — the polymorphic row. `sheet_id` is generated `type || ':' || doc_id`. Marketplace fields (`sell_id` from
  `md5(doc_id||created_by)`, `sell_type`, `sell_price`, `license`, `buy_id`, `buy_price`), `row_0`, `name`, `tags`,
  `public`.
- **sheet_usr** — membership; `role` is owner, editor or viewer.
- **db** — codex DSNs, encrypted under `DSN_ENCRYPTION_KEY`. `db_id` PK, index on `(sheet_id, created_at desc)`. No
  unique key on `sheet_id`: the newest row is current, the one before still opens.
- **secret** — a sheet's secrets, encrypted. No unique key on `(sheet_id, name)`, for the same rollover.
- **net** — rows for `net-*` sheets and the run log for `alert` and `codex-*`. `meta` is what the run cost. `net_id` PK,
  index on `(sheet_id, created_at desc)`, unique `net_hook_signature_idx`. `trimNet()` keeps the newest `NET_KEEP` per
  sheet; a sheet that keeps everything writes to a table.
- **webhook** — `url` per sheet and the last delivery's `delivered_at`, `status`, `failures`.
- **audit** — `sheet_id` (no foreign key), nullable `usr_id`, `action`, `via`, `detail`. Never trimmed.
- **payment** — marketplace transactions.
