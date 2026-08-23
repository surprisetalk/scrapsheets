# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Scrapsheets is a web-based spreadsheet application that combines traditional spreadsheet functionality with modern web
technologies. It uses a hybrid architecture with:

- **Backend**: Deno-based server using Hono framework (main.ts)
- **Frontend**: Elm single-page application (src/Main.elm)
- **Database**: PostgreSQL with schema defined in schema/db.sql
- **Real-time collaboration**: Automerge CRDT for document synchronization (see https://automerge.org/llms-full.txt)
- **Data storage**: File-based automerge documents in data/automerge/

## Important Files

- `main.ts` - Main server application and API routes
- `src/Main.elm` - Frontend application
- `src/examples.mjs` - Bundled example datasets and queries (imported by both the page and the server seeder)
- `src/page.mjs` - The parts of `src/index.html` that are functions of their input rather than of the browser: the
  library merge, the sheet thumbnail, the four things the page does with an HTTP response, and `sheets()` — the page's
  half of the query engine, which resolves `@type:doc_id` refs and recurses through referenced queries. Extracted so
  they can be tested, since nothing boots `index.html` itself
- `schema/db.sql` - Declarative database schema (schema only, no data: `pg-schema-diff` diffs a live DB against it)
- `examples.sql` - Shop catalogue of query templates (applied by `seed()` on first request)
- `deno.json` - Dependencies and import map
- `src/index.html` - Frontend HTML entry point
- `vendor.ts` - Rebuilds the vendored browser bundles in `src/` (automerge, automerge-repo, alasql)
- `data/automerge/` - Document storage directory

## Development Commands

### Building and Running

- **Build frontend**: `deno task build` (copies src/* to dist and runs elm make)
- **Development server**: `deno task dev`
- **Run all tests**: `deno task test`. Not `deno test --allow-all`: the task is the one place `JWT_SECRET`,
  `TOKEN_SECRET` and `DSN_ENCRYPTION_KEY` are set, and `main.ts` refuses to load without all three, so a bare
  `deno test` cannot start. No test drives a browser. The suite is five files, and which one a failure belongs in is
  usually obvious:
  - `main_test.ts` — the server: auth, sync and roles, shop and Stripe, `POST /query`, the `src/sql.mjs` UDFs, net-http
    polling, alerts and digests, MCP, export. One `Deno.test` full of prose-headed blocks against in-process PGlite.
  - `examples_test.ts` — every bundled sheet, run through **both** engines (`npm:alasql` and the vendored
    `src/alasql.mjs` the page loads) and compared row for row. A UDF registered in one and not the other, or a vendored
    bundle built from a different alasql, fails here rather than in somebody's browser.
  - `page_test.ts` — the page itself, booted under jsdom: the compiled Elm in `dist/index.js` initializes, renders, and
    answers clicks, with the library fed in through `library()` from `src/page.mjs` — the same function `index.html`
    calls, not a copy written for the test. Also covers that module on its own. Covers the gallery strip, sorting by
    clicking a header, hiding a column, chart SVG, dashboard tiles, `?embed=1`, the tutorial and the shortcut sheet. It
    always runs `deno task build` first, so it can never pass against a stale bundle. deno-dom is not enough for this —
    it has no `replaceData` on a text node, which is how Elm's virtual-dom patches text in place.
  - `browser_test.ts` — despite the name, no browser: dist builds, index.html wires the WASM and the import map, every
    root-absolute asset is in `_redirects`, every name index.html imports is actually exported, nothing reaches a CDN,
    and the built bundles still export what the page boots from. Nothing boots `index.html`, so its
    `<script type="module">` body is sliced out and piped over stdin to `deno lint` with `no-undef` and
    `no-unused-vars`. That is real scope analysis — module bindings, block scope, hoisting — rather than the regex it
    replaced, which matched call shapes only and so could not see a value use like `PARSERS[ct]`. A free identifier or a
    dead import fails here instead of shipping as a blank page; it is what caught `app` being referenced from the
    fatal-initialization `catch` that its `try` had scoped it out of. `BROWSER_GLOBALS` in that file is the whole
    allowlist — the seven names the page reaches for that Deno's own global scope lacks, `Elm` among them, from the
    classic `<script src="/index.js">`. Everything else it touches, `fetch` and `localStorage` and `WebSocket`, Deno
    already defines, so a new browser API has to be added there on purpose.
  - `tests/MainTest.elm` via `elm_test.ts` — pure Elm: selection and navigation, sort and filter, clipboard parsing,
    column stats, `docDecoder`, `chartPoints`.
- **Re-vendor browser bundles**: `deno task vendor` (after bumping the versions at the top of `vendor.ts`; alasql tracks
  `deno.json`)
- **Status check**: `deno task status` prints every graded condition from the deployed `GET /status` and exits nonzero
  when any of them is below 1.0. `.github/workflows/status.yml` runs it on a 15-minute cron; the failure email is the
  alarm.
- **Elm review**: `deno task review`. Runs clean with zero suppressions; keep it that way.
- **Watch and build**: `watch src { try { cp -vu src/* dist ; elm make src/Main.elm --debug --output=dist/index.js } }`

### Elm Commands (via deno.json imports)

- **Format Elm code**: `deno run -A npm:elm-format`
- **Run Elm tests**: `deno run -A npm:elm-test`
- **Elm review**: `deno task review` (also runs in CI)

### Database Setup

- Initialize database schema: `psql < schema/db.sql`
- Migrate an existing database: edit `schema/db.sql`, then `deno task db:plan` to read the generated migration and
  `deno task db:apply` to run it. Both load `.env` (`DATABASE_URL` required; `TEMP_DATABASE_URL` optional for plan).
  `schema/db.sql` is the desired state; there are no migration files. DML (backfills) is never generated — splice it in
  with `--insert-statement 'index=<n> statement="..."'`. `db:apply` passes `--allow-hazards INDEX_BUILD,INDEX_DROPPED`
  (every plan containing a `CREATE INDEX CONCURRENTLY` trips the first, and concurrent builds cost CPU without locking
  writes; and **changing an index expression is a rename, a concurrent build and a drop**, so without the second the
  gate refuses every index change there is. The cost is real and accepted: a plan that genuinely removes an index no
  longer stops here, only in `db:plan`) and `--skip-confirm-prompt` (the prompt needs a TTY). `db:plan` is therefore the
  only review step — read it before you apply, especially for anything pg-schema-diff does _not_ label a hazard:
  `add column ... generated always as identity` rewrites the whole table under an access exclusive lock and carries no
  warning.
- Default connection: `postgresql://postgres@127.0.0.1:5434/postgres`, but **`.env` may point `DATABASE_URL` somewhere
  else entirely — check it before running `db:apply`**, which now applies without a confirmation prompt
- Tests run against an in-process PGlite over a local pg-gateway on port 5434 (no real Postgres needed)
- External-DB DSNs are encrypted at the application level (AES-GCM via DSN_ENCRYPTION_KEY) before storage

## Architecture Overview

### Backend Architecture (main.ts)

- **Framework**: Hono web framework with JWT middleware
- **Database**: PostgreSQL via postgresjs (pool capped at one connection: the test gateway is a single PGlite session)
- **Real-time sync**: official automerge NodeWSServerAdapter behind a small ws-shim over Hono's upgradeWebSocket;
  per-document access is enforced in the /library/sync message path (`syncRole`), not just sharePolicy. `syncRole`
  returns owner/editor/viewer or null, covering membership, purchase-derived access, `sheet.public`, and share-link
  JWTs; a viewer's frames are inspected with `Automerge.decodeSyncMessage` and rejected if they carry changes
- **Authentication**: JWT-based with email verification via Resend (plain fetch, RESEND_API_KEY)
- **Secrets**: `requireSecret()` **throws at module load** for `JWT_SECRET`, `TOKEN_SECRET` and `DSN_ENCRYPTION_KEY`.
  They used to fall back to `Math.random()` and warn, which meant a restart silently re-rolled them: every session
  dropped, every encrypted DSN unreadable, and — since `TOKEN_SECRET` is the root `hookSecret()` derives every sender's
  signing key from — every webhook delivery refused with a message blaming the sender. A secret-less boot is not a
  recoverable state. `deno task test` sets all three; nothing else does
- **Document types**: table, query, net-hook, net-http, net-socket, portal, alert, chart, codex-*
- **Seeding**: a lazy-once middleware runs `seed()` (examples.sql + src/examples.mjs datasets) on the first request;
  idempotent via `on conflict (doc_id) do update`. It also grants `OPERATOR_EMAIL` a viewer row on `net-hook:errors`,
  creating the `usr` row if it does not exist yet — the sentinel that owns that sheet has no password and cannot be
  logged into, so `/share` cannot reach it. A password-less row is not an account; `POST /signup/:token` upserts on the
  email, so signing up later adopts it along with the grant
- **net-http polling**: `pollNetOnce` scans net-http sheets every 15s, fetches due URLs through the safeFetch SSRF
  guard, and appends bodies to the `net` table. A header value may be `{{secret:name}}` rather than a token:
  `resolveSecrets()` replaces it with the newest secret of that name on the sheet at fetch time, so the automerge
  document — which sync hands to every viewer and every share-link holder — keeps the reference and never the value. It
  resolves into a **separate** object, so the unresolved headers are what a failure row is built from and a resolved
  token cannot reach the log even if `curlFor` one day prints more than the keys. A reference to a secret the sheet does
  not hold is a failure row naming it, **not** a request sent without the header: that would come back as somebody
  else's 401 and read as the API's fault. Rotating the secret needs no edit to the sheet — the next poll reads the
  newest, the same current-secret rule `hookKeys` uses. A failed fetch lands as a row too, shaped by `fetchFailure()`:
  status, the URL actually fetched after redirects, content type, a body snippet, and a `repro` curl line that names the
  header keys the sheet sent but never their values. `/proxy` returns the same shape
- **Webhook ingest**: `POST /net/:id` rejects an unknown sheet (404), a non-net sheet (400), a body carrying a **NUL
  byte** (400, naming the offset — the column is text and Postgres text cannot hold one, so correctly signed bytes used
  to reach the insert and come back as an unexplained 500) and a body over `NET_BODY_CAP` (413), each naming what it
  received. Every other byte, valid UTF-8 or not, is kept as sent. **Every delivery must be signed**, with no per-sheet
  opt-out: without it, anyone who learns a net sheet's id can write rows to it. The header is
  `scrapsheets-signature: t=<unix seconds>,v2=<hex>`, HMAC-SHA256 over the timestamp, the request path and query, and
  **the body's own bytes**, joined by newlines, within `HOOK_SKEW` seconds of the server's clock. `v1` covered the body
  alone, which made two genuinely different deliveries carrying the same body in the same second one delivery — a
  fan-out that discriminates by query string lost every copy after the first to the replay refusal. It is still
  accepted, because refusing it would be the missed delivery the rollover exists to avoid, and `meta.scheme` on the row
  says which one verified. Bytes rather than a decoded string, because `c.req.text()` replaces invalid UTF-8 and a
  sender that signed exactly what it sent would then be refused; and the `t` that is verified is the captured text, not
  a `Number()` round-trip of it. The sheet's secret is **derived until it is stored** — `hookSecret()` is
  `HMAC-SHA256(TOKEN_SECRET, "hook:" + sheet_id)`, the prefix domain-separating it from `createToken()`, which derives
  email-verification tokens from the same root. A sheet may instead hold its own in the `secret` table, and `hookKeys()`
  answers with the newest `SECRET_KEEP` of them, newest first; **the derived key rides along as the implicit previous
  one until a second rotation retires it**, without which storing a sheet's first secret would drop every sender still
  on the derived one at the instant it was written. Which key verified is recorded as `meta.secret_at` (`"derived"`, or
  that row's timestamp), so "is anyone still sending the old one" is a query rather than a guess and a rollover has a
  visible end. A stored secret never enters the automerge document, which is what sync hands a viewer. The four
  rejections (unsigned, malformed, stale, mismatched) are all 401 and each names its own check, but none prints the
  secret or the expected digest, which would make the message a signing oracle. The `v1` digest must be **lowercase**
  hex: `parseInt` is case-insensitive, so an upper-cased signature verifies against the same secret while reading as a
  different string, and the uniqueness that refuses a replay is over the header as sent — every one of the 2^64 case
  variants of one captured signature was otherwise a free replay. A signature this sheet has already stored is refused
  with **409**, not 401 — a replay is not a signing failure, and the status check reads a 401 on `/net/` as a sender
  that cannot sign. The refusal is the unique index `net_hook_signature_idx` on `(sheet_id, (meta->>'sig'))` — **the
  digest that actually verified, canonically spelled**, which `verifyDelivery()` builds from the parsed pieces rather
  than copying out of the header. The header is not the key: Stripe's is a tolerant comma-separated field list, so
  `t=…,v1=…,z=1` verifies against the same secret over the same message and, keyed on the raw header, every junk suffix
  was a fresh replay for the whole skew window; and Shopify's base64 digest has four spellings for one 32-byte value,
  because the last character carries two bits nothing reads — and Shopify signs no timestamp, so those never went stale.
  Only the verifier knows which value it trusted, and only in one spelling. A header name cannot be the key once a sheet
  may be signed by a provider: any fixed order over the four header names picks by order and not by which one was
  checked, so a captured Stripe delivery replayed with a junk `scrapsheets-signature` beside it took a new key every
  time and landed every time. Only the verifier knows which value it trusted. Rows written before the index changed
  carry no `meta.sig`, and nulls do not collide, so a delivery captured before it shipped is unconstrained — by which
  point the skew check has refused it anyway. The **insert** is what asks it: selecting first and inserting after let
  ten parallel copies of one captured delivery all see no prior row and all land, which is the least sophisticated
  version of the attack the check exists for. Under `v2` a delivery's identity is
  `(this sheet, this second, this target, these bytes)`, so what identifies a delivery is what the sender varies; under
  `v1` it was the bytes alone, and the 409 names which of the two it is refusing. The one hole is `trimNet`, which can
  evict the record on a busy sheet and free the signature, by which point the skew check has long refused it.
  `hookSign()` is exported because the tests sign the way a sender does rather than reimplementing it; it takes the
  **secret** rather than a sheet id, because that is what a sender holds and a sheet's key is no longer always the
  derived one. The 404 and the 400 answer _before_ the signature is checked, which is an existence oracle to anyone
  already holding a doc_id — accepted, because a doc_id is 22 unguessable characters and the messages are worth more
  than the disclosure. Owner-only `GET /library/:id/hook` returns `{ url, secret, repro }` under
  `Cache-Control: no-store`, the last a runnable `openssl`/`curl` line with the secret shell-escaped the way `curlFor`
  escapes a url — a provider secret is pasted by hand, and one apostrophe otherwise ends the quoting and the line signs
  the wrong thing; the page reaches it through the existing `shareAction`/`shareLoaded` port pair with the action
  `hook`, behind a button, so a secret nobody asked for is never on screen, and `UrlChange` clears it so it cannot
  follow you to another sheet. A decode failure on that answer is reported rather than swallowed — the alternative is a
  button that does nothing and says nothing
- **Sheet secrets**: `POST /library/:id/secret` takes `{name, value}` and **inserts**, because writing a secret is
  rotating it: the newest row for a name is current, the one before it still verifies, and everything past `SECRET_KEEP`
  is trimmed behind the write. `GET` answers names and timestamps and **never a value** — a value that can be read back
  is a value a share link can eventually be pointed at. Values are sealed with the same AES-GCM `encrypt`/`decrypt` pair
  (and the same `DSN_ENCRYPTION_KEY`) the codex DSNs use, and never reach the automerge document or a cell. A secret
  named `hook` is this sheet's own signing key; `hook:stripe`, `hook:github` and `hook:shopify` each select that
  provider's verifier instead, against that provider's own header. **At most one signing scheme per sheet**, decided
  inside a transaction behind `select ... from sheet ... for update` — the only lock in the codebase, and it is here
  because `insert ... where not exists` is atomic only against another statement on the same connection, while Deno
  Deploy runs many isolates each holding their own. Two concurrent writes of different schemes both saw no clash, both
  landed, and the sheet then refused every delivery with a 500 rather than guess which scheme was meant. With two, the
  header the sender chose would decide which check it faced, and `verifyDelivery()` reads the scheme off the stored
  secret precisely so that it cannot. A name in the `hook`/`hook:*` space that no verifier knows is refused where it is
  typed, since it would otherwise be written happily and then fail every delivery. The store is bounded at
  `SECRET_NAMES_MAX` names per sheet (`SECRET_KEEP` bounds rows within one name); losing the race on that cap costs one
  name over the limit, which is why it stays a plain check. GitHub and Shopify sign no timestamp, so those two have no
  skew window and the unique index is the whole of their replay protection
- **Alerts**: an `alert` sheet is `{ code, to, interval }` and fires when its query returns a row, so the condition is
  the where clause and there is no second expression language. `pollAlertOnce` scans them every 15s and runs each
  through `POST /query` **as its owner** (`createJwt(created_by)`), so an alert can never read a sheet its owner cannot.
  A run whose delivery failed is **not** de-duped away — the same rows are sent again next interval, or one Resend
  outage silences an alert for good — though a run with no destination at all is, since retrying sends nothing.
  **Every** run lands in `net` with `method = 'ALERT'`, not only the ones whose answer changed: a healthy quiet alert
  and a dead `setInterval` write the same empty log otherwise, and nothing outside this log can tell them apart. An
  unchanged run carries `status = 'unchanged'` and the same `fingerprint`, `matched` and `truncated` the next run reads
  back, so the de-dupe and the diff both still work off "the last row"; an alert with no query records `idle`. A
  _repeated_ quiet tick moves that row's `created_at` rather than adding another — liveness reads `max(created_at)` and
  the de-dupe reads the last row, so both still work, and a minute-interval alert stops writing 1440 rows a day and
  pushing the run the daily digest still has to find out past `NET_KEEP` in under 17 hours. An `interval` that is
  present but not a positive number is a **crash**, not a guessed hour: the status check reads that number back and
  reports it as the alert's own interval. The row carries status, row count, `added`/`removed` against the run before,
  the rows it matched up to `ALERT_ROWS`, and what the delivery did — which makes the alert's history a sheet you can
  query, and makes both the de-dupe and the diff survive a restart, because the previous run is read back out of that
  log. Past `ALERT_ROWS` the run records why it could not diff rather than a number it cannot stand behind. `POST /sell`
  refuses an alert: a copy would mail the seller's address on the buyer's timer
- **Status**: `GET /status` is public — an uptime check carries no bearer token — and answers grades and no rows: no
  ids, no names, no addresses, though a grade is a ratio against a limit named in `main.ts`, so a reader can invert one
  back to the count behind it. `status()` returns one entry per likely failure mode, keyed by **the sentence the grade
  is about**, mapping a seconds-ago offset (`STATUS_AGO` = now, an hour ago, a day ago) to the grade as of then. **1.0
  is the minimum passing grade and 0.0 is total failure**, so a number above 1.0 is headroom rather than a score to
  maximize. `grade()` **floors** rather than rounds — 0.999 rounding up to a pass is the Goodhart failure in miniature —
  and throws on a non-finite value, naming the condition, because a check that answers "fine" because its SQL alias was
  renamed is worse than no check. The numbers a sentence quotes (`LATENCY_MS`, `REFUSALS_MAX`, `POLL_STALE_S`,
  `DB_BYTES_CAP`, `HEAP_BYTES_CAP`) are read into the sentence, so a changed limit cannot leave the prose claiming the
  old one. The historical conditions are one query evaluated at three instants — the offsets are a join, not three round
  trips — and the current-state ones carry `"0"` alone. The latency condition throws away its **first** `select 1` and
  times the second: a cold isolate pays TCP, TLS and auth on the first, and a suspended Neon instance pays its own
  wake-up (~1s against ~98ms warm), so timing it graded how cold the caller was rather than how fast the database
  answers — and since nothing keeps a 15-minute cron warm, that failed nearly every scheduled run. Connection setup is a
  different failure mode, and the endpoint timing out is where it already shows. **Every cast out of jsonb is guarded**
  (`substring(x from '^[0-9]{1,9}$')::int`, and `is json` in the where clause, which an aggregate's `filter` is
  evaluated after — **nine digits, not `+`**: shape is not magnitude, and `'99999999999999999999'` is all digits and
  still out of range for an int, which took this endpoint, the alarm itself, to 500): one row this code did not write
  used to take the endpoint to 500, and an uptime checker cannot tell that from a dead server. Two conditions are shaped
  by what an anonymous caller can do — rejections are counted as **deliveries refused on `/net/`** rather than requests
  rejected, because a scanner walking unknown paths would otherwise hold the alarm red with nothing broken; and "Every
  failure is reaching the error log" grades a counter of consecutive log-write failures, because the two conditions
  above it are counted out of that very sheet, so a log that cannot be written would otherwise read as a service with no
  failures. `POLL_OK` and `ALERT_OK` are the two run-success predicates, and `GET /status` and the `library:freshness`
  sheet both read them from there: two hand-copied copies had already drifted, so a run that said `sent` with a delivery
  that failed was a failure to one and a success to the other — and the sheet whose whole job is naming the rotten alert
  read healthy on exactly that one. Every read out of jsonb in them is guarded **inside** a `case` rather than beside it
  with `and`, because Postgres does not promise to evaluate `and` left to right. Alert liveness is graded the way
  net-http freshness is — every alert sheet ran within twice the `interval` its own newest run recorded — which only
  became possible once every run was logged. It is a **left** join onto `net`: an inner join drops an alert that has
  never run at all, and an empty set grades as a pass, so the condition would have reported healthy on exactly the
  failure it is for — a poller that never fired once. A never-run sheet is graded from its own `created_at`. And "either
  delivered or had nothing to deliver" is one condition, not two: a `clear`, `unchanged` or `idle` run sent nothing on
  purpose, and grading that as an undelivered alert held the route at 503 for a day every time an alert went quiet. The
  route answers 503 when a `"0"` grade that **pages** is below 1.0; `REPORTED_ONLY` names the conditions that are graded
  and returned but do not page — today just "Somebody created a sheet in the past 24 hours.", because a product with no
  users is not a service that is down, and a 15-minute email about it would take the twelve technical conditions with
  it. A sentence in `REPORTED_ONLY` that `status()` does not actually return is a 500 by name, since a typo there
  silently starts paging again. `deno task status` prints the same thing, times out at 10s, refuses an answer carrying
  fewer than 13 conditions, lists every grade below 1.0, and **exits on the route's own status code** rather than
  deciding again — a second copy of the paging rule would drift from the first; `.github/workflows/status.yml` runs it
  every 15 minutes beside a plain `curl` of the homepage, which the API's own check cannot see, because the page is
  built by Cloudflare and the API runs on Deno Deploy. A failed scheduled run emails the repo owner, and that email is
  the alarm. There is deliberately no in-process watchdog, because a watchdog dies with what it watches
- **Error sheet**: `net-hook:errors` is seeded by `seed()`, owned by the same sentinel `usr` with the empty email, and
  `app.onError` writes one row to it per failure — `method`, the `explain(...)` block the caller was sent, and
  `meta = {status, path}`. It is a `net-hook` sheet because that already means "a sheet whose rows live in `net`", so it
  reads, pages, exports and trims (`trimNet`, `NET_KEEP`) with no new code, and `select * from @net-hook:errors` works.
  **Names are stored, never values**, for headers _and_ the query string: the sync socket takes `?auth=<jwt>` and a
  share link rides the same parameter, so a failing request would otherwise write a live token into a sheet that can be
  shared and exported. The write is **not awaited** — when the database is what failed, an error response must not wait
  for the logger to discover that too — and its rejection is caught, because a logging failure must never replace the
  failure being logged; that happens, since the rate limiter throws before the seed middleware has run. `logFailure`
  answers **whether a row actually landed**, and only a write that happened may clear `logWriteFailures` — a suppressed
  call resolves like a written one, so clearing on any resolution made a database refusing every insert read as a
  service with no failures. It writes **one row per (status, path) per minute**, and the count a suppressed window hid
  rides the next row that key writes as `meta.folded`, which the two conditions counted out of this sheet add back; that
  count is _subtracted_ rather than zeroed after the insert, because `onError` does not await the call and every failure
  arriving during that round trip has already folded onto the same entry. Two costs, both real: the row keeps **one
  message per key per minute**, so a different failure sharing a status and a path inside that window is counted but its
  own message is not kept; and a burst that _stops_ is flushed by `flushFolds()`, which rides the same 60-second broom
  that evicts `rateLimitBuckets` and writes one `method = 'FOLD'` row per pending key. It claims the pending count
  **before** its await, unlike `logFailure`, which subtracts after — that one sets `seen.at` first, so everything
  arriving mid-write is suppressed onto the same entry, while this one deliberately leaves the window expired and would
  otherwise share the count with a concurrent writer. The row **declares `folded: n - 1`**, because every row in this
  sheet is counted as itself plus its folds (`sum(1 + folded)`) — declaring `n` would count the burst once too often. It
  carries no header names and no `usr_id`: the key is a status and a path, so it may span several callers and there is
  no request left to read anything off. `logSeen` is bounded at `LOG_KEYS_MAX` and evicts oldest-first, because a
  scanner mints a key per path — `rateLimitBuckets` is bounded the same way, since sweeping by idle time alone loses to
  a caller minting keys faster than the broom runs. A **429 is not logged at all**: shedding load is the cheap path by
  definition, and paying two round trips per shed request inverts the point. `logWriteFailures` counts consecutive write
  failures and resets on the next success, which is the one fact about this log that cannot be read out of the log.
  `seed()` grants `OPERATOR_EMAIL` the viewer row, so reading it needs no psql prompt. A caller who is not the operator
  gets **the status and the path but not a 5xx body**: that body is the stack `onError` recorded — file paths, line
  numbers, the failing SQL out of a postgres.js error — and the 500 response deliberately answered "Sorry, something
  went wrong", which this read must not undo. A 4xx body is the `explain()` block about the caller's own request and is
  theirs. `sheet()` answers `net-hook:errors` **before** `assertSheetAccess`, because a caller with no share on it still
  owns the failures their own requests caused: `logFailure` writes `meta.usr_id` from `c.get("usr_id")` (absent on an
  unauthenticated failure and on one the jwt middleware itself raised — the honest answer: nobody owns it), and the one
  read's where clause is the access rule, the operator's viewer grant being the `exists` arm of it. Safe to widen only
  because the row keeps header and query names and never values
- **Charts**: a `chart` sheet is `{ source, kind, x, y }`. `chartSql()` in `src/sql.mjs` turns that into one query both
  engines build identically, so the SVG the page draws and the rows `GET /sheet/chart:abc` and `/export/chart:abc.csv`
  return are the same answer. Only a column name reaches the SQL — anything else is refused by name rather than
  concatenated in — and the page draws it with `elm/svg`, baseline pinned to zero unless the data goes below it,
  dropping a row whose y is not a number rather than reading it as zero. The `svg`/`png` buttons serialize the SVG
  already on screen instead of drawing it again, so a saved chart cannot drift from the shown one
- **Embeds and dashboards**: `?embed=1` on any sheet's own URL renders that sheet with no chrome — same page, same
  access rules, so an embed grants nothing a link would not. A `dashboard` sheet is `{ tiles: ["@chart:a", ...] }` and
  lays those out as a grid of embeds, so nothing in it knows how to draw a chart or a table: the sheet it names already
  does. One page per tile is the cost, and a dashboard cannot be a tile on a dashboard because that nests forever
- **Fork**: the page's `forkDoc` port copies any sheet you can open — a bundled example included — into a new automerge
  document carrying `forked_from`, registers it, and navigates to it. The toolbar reads `forked_from` back out of the
  document, so lineage travels with the sheet rather than living in one browser's localStorage
- **MCP server**: POST /mcp/:id is a hand-rolled JSON-RPC 2.0 endpoint (initialize, tools/list, tools/call) with tools
  read_sheet, write_cells, query_sheet, list_sheets; :id is the default sheet scope

### Key Backend Features

- **Sheet system**: Polymorphic documents identified by `type:doc_id` format
- **Query engine**: SQL execution via AlaSQL for cross-sheet queries using `@sheet_id` syntax. `src/sql.mjs` is shared
  by both engines (server `npm:alasql`, page `/alasql.mjs`): `register()` adds the UDFs AlaSQL lacks — aggregates,
  regression, fuzzy matching, UTC date arithmetic including `fiscal_year`/`fiscal_quarter`/`fiscal_period` — inference
  (`t_test()` is Welch's, `ci_low()`/`ci_high()` the mean's t-based interval, both over one Student-t CDF), robust
  statistics (`mad()`, `robust_z()`, so the outlier cannot widen the ruler it is measured against), curve fitting by log
  transform (`fit_exponential()`, `fit_power()`), `width_bucket()` (which is why a histogram needs no chart kind),
  geometry (`point_in_polygon()` — usable as a join predicate — `polygon_area_km2()` spherical, `bearing_deg()`,
  `geohash()`), `haversine_km()` for great-circle distance (AlaSQL ships no trigonometry at all),
  `min_text()`/`max_text()` because AlaSQL's own `min()`/`max()` are compiled inline over numbers and dates and drop a
  text value, so `min(code)` returns nothing rather than the first code — `checkResultColumns()` names that case and
  points at the replacements. `scanRefs()` is the one `@type:doc_id` scanner — `@type:doc_id.column` is a **cell**, one
  value out of a one-row sheet, rewritten to a scalar subquery so nothing is spliced in as a literal, and `checkCells()`
  refuses a sheet that does not hold exactly one row. `checkResultColumns()` turns AlaSQL's silent undefined column into
  an error. `nearest()` backs every "did you mean": unknown columns and unresolved `@sheet` refs in both engines. The
  server passes sheet rows through `params[0]` so `alasql.from.SHEET` stays request-scoped. **Both engines resolve a
  query through the same three functions in `src/sql.mjs`**: `toRecords()` keys a sheet's rows by column name,
  `loadRefs()` walks the refs — cycle bound, fetch, type check, and the row budget the server spends through its
  `onLoad` — and `planQuery()` runs the four pre-engine passes in the one order that works. Where a sheet comes from is
  the only real difference, and it is an argument: `sheet()` with its access check on the server, a library entry or an
  automerge document in the page. `describing` is why `describe` still answers on a sheet whose cells fail the type
  check, in both places. Two AlaSQL behaviours shape how every query sheet is written: a `group by` expression is
  evaluated against an **empty row**, so a UDF named there receives nothing and the call has to move into a subquery;
  and an exception thrown from a function while a subquery in the from clause is computed is **discarded**, surfacing as
  "Cannot read properties of null (reading 'data')" — `formatQueryError()` replaces that one, because the message it
  destroyed is unrecoverable
- **Window functions**: AlaSQL parses `over (partition by ...)` and computes it wrong (`sum(x) over (...)` came back 0),
  so a window never reaches it. `rewriteWindows()` in `src/sql.mjs` lifts each one out of the **top-level** select list,
  leaves `null as <alias>` where it stood and appends the plain columns it reads as `__w<n>[apo]<n>`; `applyWindows()`
  computes it over the rows the engine returned and drops those columns again. Both engines call the pair, so a window
  means the same thing in the page and in `POST /query`. Ranking, offset and aggregate functions with `rows`/`range`
  frames; the default frame is peer-correct `range unbounded preceding to current row`. A trailing `limit` is stripped
  before the engine runs and re-applied after, because a window is defined over every row the query produced. A window
  that is not a select item of its own — wrapped in an expression, or inside a subquery — is refused by name: returning
  zeros for it is the bug this replaced. `WINDOW_TYPES` gives each alias its result type on the server; the page takes
  it from the query sheet's own `cols`. `ignore nulls` on `lag`/`lead`/`first_value`/`last_value`/`nth_value` steps over
  the rows that have no value, which is what makes `last_value(x) ignore nulls` a forward fill; asking any other
  function to ignore nulls is refused, because it already does. `qualify <condition>` rides the same pass: a window
  named in the condition is lifted into a `__q<n>` column, computed with the rest, filtered on by the engine itself,
  then dropped. It is what makes an as-of join one statement —
  `join ... on p.day <= t.traded_on qualify row_number() over (partition by
t.trade_id order by p.day desc) = 1` —
  instead of two sheets, and an unknown column in the condition is named rather than answered with an empty sheet
- **Pivot and unpivot**: `pivot` is AlaSQL's own and correct, except that a quoted in-list matches nothing and answers
  with zero rows, which `checkPivot()` refuses. `unpivot` is ours: AlaSQL drops every column it is not unpivoting, so
  `rewriteUnpivot()` expands it into the `union all` it means, reading the wide column names off the source sheet's own
  type row. That is why the source has to be a `@sheet` and a subquery is refused
- **Schema introspection**: `describe @table:abc` is intercepted by `describeRef()` before the engine sees it, in both
  engines — inside `executeSql` on the server and inside `runSql` in the page — and answers with
  column/type/rows/nulls/sample. It is the one statement that still works on a sheet whose cells fail the type check,
  because that is the sheet you need to inspect
- **Cost guards**: `checkQueryRows()` caps the rows a single query may load across every `@sheet` (`MAX_QUERY_ROWS`);
  that is the guard that actually stops a runaway, since a single-threaded engine cannot be preempted. `MAX_QUERY_MS`
  only bounds how long the caller waits — the work itself still finishes
- **Type mismatch**: `checkColumnTypes()` runs on each **table** sheet as it loads, in both engines, and rejects a
  non-numeric value in a `num`/`int`/`float`/`usd`/`percentage` column, naming the row, the declared type and the value.
  Without it a `sum()` over a column holding "n/a" was quietly wrong. Query sheets are exempt: their column types are
  the source column's, so `cast(price as string) as price` would trip a check meant for a bad cell
- **Error shape**: `explain(headline, fields)` in `src/sql.mjs` is the one formatter for the aligned
  expected/received/source/fix block
- **Rate limiting**: `callerIp(c)` is the bucket key, and it reads the **rightmost** `x-forwarded-for` entry — the one
  the proxy in front of us appended. The leftmost is whatever the caller typed, so keying on it let a flood rotate its
  own bucket for free, one header value per request. With no header at all nothing proxied us, so `c.env.remoteAddr` is
  the caller. `x-real-ip` is not consulted: nothing in front of us sets it
- **jsonb writes**: every jsonb column is written with `sql.json(...)`, never `JSON.stringify(...)`. postgresjs
  registers `JSON.stringify` as the serializer for the jsonb OID, so a pre-stringified value is encoded **twice** and
  lands as a jsonb _string_ holding JSON rather than an object — which made `meta->>'status'` null on every row
  `logFailure` wrote, and so made the 5xx and refusals conditions in `GET /status` grade a dead check as fine. The `net`
  read casts the three jsonb columns `::text` on the way out, so a cell holds the JSON `json_extract()` reads rather
  than an object postgresjs parsed
- **Share answers name their sheet**: every `shareLoaded` payload carries `{ id, action, ... }`. Without the id, a
  `list` for sheet A that resolved after the user opened sheet B wrote A's members and public flag into B's panel;
  `ShareLoad` now drops a payload whose id is not the open sheet's, silently, because navigating while a request is open
  is ordinary. With the action named, each branch **requires the fields that action promises**, which is what makes a
  renamed field an error rather than an absence — the one failure the sent-vs-decodable split could not see. `UrlChange`
  empties the panel when the sheet changes under it and re-fetches the list, without which navigating with settings open
  left the first sheet's permissions on screen with nothing in flight to correct them
- **Sharing**: `GET/POST/DELETE /library/:id/share` (owner-only, by email + role), `POST /library/:id/public`, and
  `POST /library/:id/link` which mints a viewer-scoped JWT. The link rides the sync socket's existing `?auth=`
  parameter, so there is one read path rather than two. `GET /library/:id/hook` (owner-only, net sheets only) is the
  webhook signing secret and a runnable curl line
- **Marketplace checkout**: `POST /buy/:id` fulfills `$0` listings immediately. A positive `sell_price` creates a Stripe
  Checkout Session (`STRIPE_SECRET_KEY`) and returns `{ checkout_url }`. `POST /stripe` verifies `stripe-signature`
  (`STRIPE_WEBHOOK_SECRET`) and fulfills `checkout.session.completed` with `payment_status=paid`. Checkout is card-only.
  Money lands on the platform account; Connect payouts are not wired.
- **Freshness**: `library:freshness` is a sheet the server **computes rather than stores**, so it pages, exports and can
  be selected from a query sheet (`select * from @library:freshness`) through the paths every other sheet uses. It has
  no automerge document and no `sheet` row, so `sheet()` answers it before the document lookup and before
  `assertSheetAccess`; the join to `sheet_usr` inside it is the access rule. One row per net-http and alert sheet the
  caller can read: last run, last **good** run, and rows since. Both joins onto `net` are `left join lateral`, because a
  sheet that has never run at all is exactly the failure this read is for; and `failures_since_ok` compares
  `(created_at, net_id)` rather than `created_at` alone, the same key the laterals order by — on the timestamp alone, a
  good run and a bad one sharing one left `last_ok` equal to `last_run` and the count at zero, so the sheet this read
  exists to surface reported itself healthy. A good run is `POLL_OK`/`ALERT_OK`, the same two fragments `GET /status`
  grades on, read from one place because two copies of them had already drifted. `GET /library/freshness` is the same
  answer through a plain route. The status check is deliberately **not** extended: a check whose conditions come and go
  with the data cannot be read by an uptime checker. **Known gap**: `@library:freshness` resolves on the server but not
  in the page, where `sheets()` looks a ref up in the library map or `repo.find`
- **Reads and export**: `GET /sheet/:id` is the stable JSON read for every sheet type. `GET /export/:id.<format>` is the
  download path, one route over the `EXPORTS` table: `csv`, `json`, `ndjson`, `md`, `ics`. All go through `sheet()`, so
  they inherit `assertSheetAccess` (membership, purchase, and `public`), pagination, and query-sheet recursion. Export
  asks for 100000 rows so a net or query sheet is not truncated at the default 50. The name-keyed formats (`json`,
  `ndjson`) refuse a sheet with two columns of the same name rather than overwrite one; `ics` needs a
  `date`/`timestamp`/`create` column and says so when there is none
- **CSV import**: `POST /import/csv` rejects a row whose field count does not match the header, naming the line, both
  counts, the raw text and the column it stops at. Type inference requires every non-blank value to parse: at the old
  80% threshold the other fifth became `NaN` silently
- **Deterministic reads**: `/library` had no `order by` at all and `/shop` ordered by a non-unique `name`, so paging
  either repeated and skipped rows. Both now carry a unique tiebreaker, as the `net` read already did
- **Marketplace**: Buy/sell sheets with pricing system
- **Real-time data**: WebSocket portals for live data (time, stock prices)
- **Database codex**: Connect external PostgreSQL databases

### Frontend Architecture (src/Main.elm)

- **Architecture**: Elm Architecture (Model-Update-View)
- **Document types**: Library, Shop, Tab (table), Query, NetHook, NetHttp, NetSocket. Remaining server types (portal,
  template, codex-_) decode to `Unviewable typ`, which the view reports as an error naming the type and a query that can
  read the sheet. Give a type a real view by replacing its `Unviewable` branch in `docDecoder`.
- **Default library**: client-side (localStorage) system entries merge bundled examples from `src/examples.mjs`
  (datasets, reference/crosswalk tables and example queries), 7 live portals, and the tutorial sheet; system ids skip
  `repo.find` in `changeId`.
  `deno eval "const m = await import('./src/examples.mjs'); console.log(m.DATASETS.length,
  Object.keys(m.EXAMPLES).length, JSON.stringify(m.EXAMPLES).length)"`
  prints the counts and the byte size, which has to stay well under the ~5 MB localStorage budget. Reference tables
  carry the `reference` tag and joinable spines `dataset`, the only handle the flat library gives for telling them
  apart. Thirty-odd end-to-end pipelines from the Demo Gallery ship as sheets tagged `demo` plus a domain tag, spanning
  twenty-two domains — finance, healthcare, legal, real estate, manufacturing, government, retail, insurance, logistics,
  hr, education, nonprofit, household, music, sports, transport, science, agriculture and the rest — each a seeded
  source table plus one or two query sheets. **`src/examples.mjs` is the index: the sheets tagged `demo` are the list,
  and `todo.md`'s Demo Gallery names only the ones still unbuilt.** Their data is invented; their shapes are not.
  `store` and `class` are AlaSQL keywords, so no column may be named either: `select store from ...` will not parse.
  `table:assumptions` is the one-row settings sheet those demos read their parameters from, so changing one cell changes
  four sheets. `examples_test.ts` runs **every** bundled query in both engines, resolving `@query:` refs itself, and
  calls `checkResultColumns()` on each result — a typo in a bundled example used to read as a sheet of blanks. Two
  AlaSQL limits shape how the demo SQL is written: a UDF cannot be named in a `group by` (bin or bucket in a subquery,
  then group by the column), and `min()`/`max()` drop a date held as text (`min_text()`/`max_text()`)
- **Library gallery**: `viewGallery` puts a strip above the library table naming every `demo`-tagged query sheet as a
  link and every tag as a filter chip. It reads `model.library`, so a new demo needs no code change
- **Cross-sheet queries in the browser**: `sheets(alasql, shelf, find)` in `src/page.mjs` returns the `runSql` that
  rewrites `@type:doc_id` to `SHEET('id')` and pre-loads each doc before AlaSQL runs — the pre-load has to happen first
  because finding a document is async and an AlaSQL from-function is not. It then applies the cell, unpivot and window
  passes in the same order the server does; `@query:` refs recurse **through `runSql` itself**, so a window inside a
  referenced query is computed rather than handed to AlaSQL. Bounded by `checkRefPath`, which reports a cycle as the
  path that closes it (`a -> b -> a`) and caps depth at `MAX_REF_DEPTH`. Only two things come from the browser and both
  are arguments: `shelf()` is the library map (`Library.get`) and `find(doc_id)` is `repo.find`, which is what lets
  `page_test.ts` drive the whole thing with neither
- **UI chrome**: keyboard shortcut sheet (Ctrl/⌘+/ or "?"), library sparkline thumbnails (computed JS-side into
  localStorage entries), five-step first-run tutorial (localStorage `scrapsheets-tutorial`, -1 = dismissed)
- **Real-time sync**: Ports for Automerge integration
- **UI**: Table-based interface with cell editing, selection, and statistics
- **No runtime CDN**: every asset the page loads is served by us. `deno task vendor` uses esbuild to inline each
  package's dependencies into `src/automerge.mjs`, `src/automerge-repo*.mjs` and `src/alasql.mjs`, and fails the build
  if a bundle would still fetch something. A CDN in the request path also meant root-relative URLs inside third-party
  bundles (`/npm/...`, `/sm/....map`) resolving against our origin and hitting the `/*` catch-all.
- **Automerge loading**: `src/index.html` maps `@automerge/automerge` to the vendored `/automerge.mjs` and calls
  `initializeWasm(fetch("/automerge.wasm"))`. Automerge stays _external_ in the repo bundles so all three share that one
  initialized copy; bundling it into each would give the browser three, only one with WASM.
- **AlaSQL parity**: the page imports `/alasql.mjs`, bundled from the same bare `alasql` specifier the server uses, so
  both engines resolve one version through `deno.lock`. It used to be a CDN `<script>` on a floating `@4` tag.

### Key Frontend Features

- **Live editing**: In-place cell editing with type-aware rendering
- **Table UX**: multi-column sort (click a header to sort, shift-click to add a key; descending flips the comparator per
  key rather than reversing the list, so ties keep their order), column hide via the filter popover with "show all" in
  the filter bar, row insert/duplicate (`Ctrl/⌘+Enter`, `Ctrl/⌘+Shift+Enter`) and fill-down (`Ctrl/⌘+D`). A hidden
  column keeps its x coordinate and only stops rendering — navigation steps over it via `skipHidden`. Filtering the
  column array instead would shift every selection index in the file
- **Statistics**: Real-time column statistics for Number/Usd (numeric), Text (descriptive), Date/Timestamp (first, last,
  span, gaps) and Boolean (true/false/blank); other column types get no stats. A totals row sums numeric columns over
  the rows actually on screen, so it respects the active filter
- **Query interface**: Embedded SQL editor for query sheets
- **Type system**: Rich type system including USD, links, images, forms

### Database Schema (schema/db.sql)

#### Core Tables

- **usr**: User accounts with identity, name, email (citext), password, and `stripe_customer_id`
- **sheet**: Central document table with polymorphic sheet_id format (`type:doc_id`)
  - Types: template, table, net-hook, net-http, net-socket, query, portal, alert, chart, codex-*
  - Marketplace fields: sell_id, sell_type, sell_price, buy_id, buy_price
  - Document data: row_0 (jsonb), name, tags (text[])
  - `public boolean`: anonymous read through `syncRole`
- **sheet_usr**: Many-to-many permissions between sheets and users, with `role` (owner/editor/viewer)
- **db**: External database connections (DSN storage for codex sheets)
- **secret**: A sheet's own secrets, `value_encrypted` under `DSN_ENCRYPTION_KEY`. **No unique key on `(sheet_id, name)`
  on purpose**: the newest row for a name is current and the one before it is previous, which is what lets a sender roll
  over without a missed delivery
- **net**: Webhook data storage for net-\* sheets, and the run log for `alert` sheets, which is why the sheet_id check
  allows both prefixes. The read projects `created_at, body, method, req_headers, query_params` — the last three
  appended, so existing column positions and `select body from @net-hook:x` still hold. The three jsonb columns are cast
  `::text` in that projection, so those cells hold the raw JSON and `json_extract()` reads them; without the cast
  postgresjs parses jsonb into an object, which no cell can hold. `meta` is what the run itself cost:
  `{status, ms, bytes}` for a net-http poll, `{ms, interval}` for an alert run, `{bytes}` for a webhook delivery, and
  `{status, path, folded?}` on `net-hook:errors`. `net_id` identity PK, an index on `(sheet_id, created_at desc)`, and a
  unique index `net_hook_signature_idx` on `(sheet_id, (req_headers->>'scrapsheets-signature'))` — the expression is
  null on every row that is not a signed delivery, and nulls do not collide, so nothing else in the table is
  constrained; the read orders by both, without which paging a log repeats and skips rows. `trimNet()` keeps the newest
  `NET_KEEP` rows per sheet and runs behind every write, so the log is bounded — a sheet that must keep everything
  writes to a table, which is never trimmed
- **payment**: Marketplace transactions (buyer, seller, sell_id, buyer sheet_id, amount, Stripe session)

#### Key Schema Features

- **Generated sheet_id**: Computed as `type || ':' || doc_id` (e.g., "table:abc123")
- **Marketplace system**: sell_id generated from md5(doc_id||created_by), prevents selling and buying same sheet
- **Type constraints**: Enforced sheet types with check constraints
- **citext extension**: Case-insensitive email handling
