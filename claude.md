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
- **One identity rule**: `usr_id` is a string in every handler, and the middleware that reads `jwtPayload.sub` is the
  one place that decides it. A share link is a real token this server mints — it claims a sheet and carries no `sub` —
  so over HTTP it set `usr_id` to `undefined`, which postgres.js refuses to interpolate: the holder of a link we handed
  them got a 500, a row in `net-hook:errors` and a point off the 5xx grade, at six separate query sites. It is now a
  **403** naming the token and not the sheet ("that token opens the sync socket and nothing else"), raised before any
  route runs — which is what let the `?? null` fallbacks in `sheet()` and `freshness()` go: there is no anonymous caller
  past that middleware, so a query defending against one is defending against a value that cannot arrive. The **CSV
  importer** agrees with `checkColumnTypes()` about a blank: only a text column keeps the empty string, and a blank in a
  `num` or `bool` column is stored as **null** rather than as a zero and an invented `false` — `avg()` over an imported
  column and over the same column built by hand now answer the same number. And `bound(map, max)` is the one cap on
  every bounded map, oldest key first out: `rateLimitBuckets`, `hookBuckets`, `logSeen` and the poller's
  `netDue`/`hostDue` all go through it, and the 60-second broom now also drops a due time already in the past, which is
  how a deleted sheet or a dead host stops being remembered. Both poller maps hold a _future_ due time, so evicting a
  live entry re-polls early and loses nothing — that is why the oldest key may go without asking what it was for.
  `RATE_LIMIT_KEYS_MAX` bounds four maps, two of which are not rate limits; the name stayed rather than mint a second
  noun.

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
- **Conditional polling and retry**: a poll no longer refetches what it already has. The newest `net` row is where a
  feed's state lives — the `etag` and `last_modified` the last good body carried, the `cursor` watermark it was fetched
  at, and how many failures have happened in a row since — because `net.meta` is already the log both the alert de-dupe
  and `library:freshness` read back, while the automerge document is what sync hands every viewer and what the user
  edits, so a poller writing to it every tick would fight those edits and mint a change for every open browser. Those
  validators ride the good rows only and go into the sent headers rather than the sheet's, so `curlFor`'s repro line
  still fetches a body by hand instead of reproducing a 304. **A 304 appends nothing and is recorded as `status: 200`
  with `not_modified` beside it**: `POLL_OK` and `library:freshness` both grade `meta->>'status' between 200 and 299`,
  so the status off the wire would read as a feed that has been failing for 23 hours, and it moves that row's
  `created_at` the way a quiet alert tick does rather than appending 23 blank rows a day that would push the feed's own
  data out past `NET_KEEP` — a 304 answering a request that carried no validator is a failure row, since folding it
  would be inventing one. `cursor` on the sheet names the query parameter a feed takes a since-value in, and the
  watermark is when the last good poll **started**, so a row written while that request was in flight is asked for twice
  rather than missed once. A 5xx, a 429 and anything thrown off the wire are the host saying "later" and are retried; an
  `HTTPException` — the SSRF refusal, a non-HTTP scheme, a redirect loop — and every other 4xx are a "no", and retrying
  a "no" is noise on top of the failure row that already answered it. `RETRY_MAX` is 3 and the third failure **throws
  with the counter and the bound in the message**, which lands as the give-up row and hands the sheet back to its own
  interval; the count is read out of jsonb and guarded like every other read out of jsonb, because a count that is not a
  whole number lands as `NaN` in the next due time and a sheet due at `NaN` is due on every tick forever. **Every
  backoff is scheduled and never slept** — `Retry-After` is honoured per **host** (`hostDue`), so both sheets pointed at
  one API wait rather than taking turns stampeding it, and a sheet deferred that way logs nothing because it never ran;
  a malformed `Retry-After` is that sheet's failure row naming what it received, not the poller's problem. That, plus
  `POLL_CYCLE_MS` stopping a cycle from starting further sheets (without advancing their due time, so the next tick
  takes them) and `safeFetch`'s timeout cut from 30s to 10s, is how one cycle stays inside the 15s tick; the tick itself
  refuses to start a second cycle while one runs, which covers a database that hangs. `readBody` reads one byte past
  `NET_BODY_CAP` — the same cap one webhook delivery has, rather than a second number — and a 2xx over it is a failure
  row naming the cap and the size: the old code truncated at 64 KiB silently, which is a parse error one layer down
  blamed on the data. Its chunk loop is bounded too, because a host trickling empty chunks is a loop a byte cap cannot
  end. Failure rows now trim behind themselves like every other write to `net`; a feed that only ever failed used to
  grow until its first success trimmed it
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
- **Per-sheet delivery budgets**: `NET_BODY_CAP` bounds one delivery and `trimNet` bounds one sheet, but nothing bounded
  the _sender_ — inside the global limiter's 100 requests a second, one machine churns all `NET_KEEP` rows in ten
  seconds and every delivery the sheet held before it is gone. `hookBucket()` is a token bucket keyed on the **sheet**,
  not on `callerIp()`: a webhook sender is one machine that will not rotate its address, and what is being protected is
  this sheet's row budget rather than this caller's share of the server. Two bounds in one bucket, because they fail
  differently: `HOOK_ROWS_PER_WINDOW` deliveries and `HOOK_BYTES_PER_WINDOW` bytes per `HOOK_WINDOW_S`, since a count
  alone lets one 1 MB body a second through and a byte budget alone lets a million empty rows through — one key, one
  refill, one broom, one thing to bound, and `RATE_LIMIT_KEYS_MAX` and the same 60-second broom that evicts
  `rateLimitBuckets` bound and sweep it, an idle budget being a full one so dropping it loses nothing. The refusal is
  **429** and each of the two names its own bound, the window and a retry-in computed from the bucket's own deficit; 429
  is the one status `app.onError` does not log, so shedding load stays the cheap path and the refusals condition in
  `GET
  /status` is not inflated by it. The check sits after the 404 and the 400 and before `verifyDelivery`, the
  insert and the trim: keyed before the existence check a caller minting ids would mint map keys, and since those two
  already answer before the signature is checked, a 429 there tells a caller holding a doc_id nothing the 404 did not.
  It deliberately does **not** run before the body is read — the byte bound must spend the bytes actually sent rather
  than the `content-length` a flooder declares, and the body is on the wire either way, so refusing sooner saves
  nothing. The budget is **charged only by a delivery that landed**: a 401 or a 409 spends nothing, because charging
  every attempt would let one attacker replaying one captured delivery exhaust the budget of the sender the sheet
  belongs to, which is the opposite of what the budget protects. The cost is that an unsigned flood is shed by
  `callerIp()`'s global limiter alone and still pays the secret lookup first. The charge re-reads the bucket rather than
  reusing the one it checked, because the awaits between them span an eviction and charging a bucket the map no longer
  holds charges nobody
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
- **Result types**: a query column used to be typed by its name alone — whatever column of that name some loaded sheet
  declared, or `text` — so `cast(price as string) as price` still read usd, `count(*) as n` read text, and every sheet
  downstream inherited the lie. `selectTypes()` in `src/sql.mjs` reads the type off the select item instead, keyed by
  the name the column will carry, and `SELECT_TYPES` is `WINDOW_TYPES` for the rest of the select list: `null` means
  "whatever its argument already was", which is what makes `sum(amount_usd)` usd and `round(avg(price), 2)` usd as well.
  A cast is the one expression whose type is **stated**, and only where AlaSQL performs the cast — `cast(x as text)`,
  `as bool` and `as json` pass the value through untouched and `cast('2026-01-02' as date)` answers the string
  `"26.01.01"`, so those targets are absent from `CAST_TYPES` rather than stating a type that would be the same lie in a
  new place; an average of whole numbers is a `num`, because it is not a whole number. An item it cannot type is **left
  out** rather than guessed at, so the caller falls back to the source column of that name — a type is a hint about an
  answer the engine already computed, and a hint must never be the reason a query that ran stops answering, which is
  also why the pass finds its own closing bracket instead of calling `closeParen()` and never throws before AlaSQL has
  had its say. `executeSql` snapshots `known` — the columns the referenced sheets actually have — **before** merging the
  inferred types in, because a name only this query invented is not evidence the sheets hold it: with the alias merged
  in, `min(code) as lowest` typed itself text and so excused its own empty column, silently retiring the min()-over-text
  check. The page stamps a referenced query sheet's columns the same way in `src/page.mjs`, without which
  `describe @query:x` reported every type as `undefined` there and the real one on the server, off the same query. A
  qualified column is still resolved by its bare name, so two joined sheets with a `region` column of different types
  collide exactly as they always did; `examples_test.ts` skips an ambiguous name and says so.

- **Result types in the page**: `runSql()` in `src/page.mjs` stamps every column it returns with `selectTypes()` over
  the columns of the sheets it loaded, so the sheet on screen and a referenced `@query:` are typed in one place rather
  than two. `src/index.html` used to type the outermost result from the query document's stored `cols` map alone, which
  left the sheet you are actually editing showing the old lie — `cast(price as string) as price` rendered usd,
  `count(*) as n` rendered text — while `POST /query` and every downstream sheet reported the truth off the same
  characters. The stored `cols` are now the **tail**: an item `selectTypes` declines carries no type at all, and the
  sheet's own declaration is what fills it, which is where a hand-declared `percentage` on a ratio still comes from.
  That last resort is the one place the two engines still differ — the server ends at `text`, the page at the
  declaration or nothing — and it cannot be seen in a cell, since `Unknown` and `Text` render a value identically; it
  shows in the footer's type name. A chart's `y` picks up its source column's type by the same route, which is what
  `GET /sheet/chart:abc` already answered. Three known gaps, all on the page's side of that last resort: `describe`
  results carry no type there, the `known` map is table refs only where the server's also carries a referenced query's
  stamped columns, and `WINDOW_TYPES` is server-only, so a window alias falls to the stored cols.

- **Null, empty and zero**: `checkColumnTypes()` is the one place a cell becomes what its column says it is — a blank
  becomes `null` and a numeric string becomes its number — because `Number("")` is 0 and AlaSQL computes with the raw
  JavaScript value: `avg()` over `[1, "", 2, "", 3]` answers 1.2, counting each blank as a reading of zero, and `avg()`
  over `["1","2","3"]` answers 41, because `+` concatenated them into `"123"` first. A null AlaSQL already treats as
  absent, which is why null is what a blank becomes. Only a numeric column is touched, so an empty string in a text
  column is still the empty string and `min_text()` and `array_agg()` still keep it; every other coercion was deleted
  rather than duplicated — `num()`, `nums()` and `haversine_km()` refused a null and read `""` as zero, and now refuse
  both through the one `absent()` predicate `winNum()` already used. `NUMERIC_TYPES` is exported because the tests
  assert what the pass promises and a second copy of the list would drift from the one the check reads. The pass
  **mutates the rows it is given**, which is what carries the same rule into `POST /sheet/:id`. `total` is an AlaSQL
  keyword and will not parse as an alias, the way `store` and `class` will not parse as column names

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
- **Share links expire and can be locked**: `POST /library/:id/link` takes `{ days, password }`, both optional, and an
  **empty body still mints the unlocked 30-day link it always did** — that is the page's own call, and it may not need
  an edit to keep working. `days` is bounded by `LINK_DAYS_MAX` and `password` by `LINK_PASSWORD_MAX`; anything else is
  a 400 naming what arrived and the bound, and the password branch prints the value only when it is not a string, so a
  real password can never be echoed back. The lock is **an HMAC of the password under `TOKEN_SECRET`, not the password
  and not a hash of it**: nothing new is stored, no schema changes, and because the digest is keyed by a server secret,
  holding the link buys nobody an offline guess. `linkMessage()` is `link:<sheet_id>:<password>` — the `link:` prefix
  domain-separates it from `hookSecret()`'s `hook:`, since both derive from the same root and neither may produce the
  other's output, and the sheet id is inside the message so a password that opens one link does not open a link to
  another sheet. The reader sends the password as `?pass=` beside the token, which is the only channel a browser
  WebSocket has. `verifyWsAuth` is **the one place the share claim is read**, so it is the one place the lock is
  enforced — a link gated on the socket and open anywhere else is worse than no gate. It refuses rather than quietly
  downgrading to no access, so the reader is told which check failed instead of watching a sheet never load, and the
  comparison is `hookVerify` (`crypto.subtle.verify`) so no digest equality is written by hand. Neither refusal prints
  the password or the digest: the digest is what an offline guesser would grind against, the same oracle rule the
  delivery refusals in `POST /net/:id` follow. The refusal lands in the handshake, before any frame exists to carry it,
  so it is an ordinary HTTP 401 — which a browser cannot read. **The page therefore asks before it connects, not
  after**: `lock` is a plain claim on the token, so `src/index.html` decodes the `?share=` token's payload and prompts
  for the password when the claim is there, then appends `&pass=` to the sync url it was going to build anyway. The
  claim is read as a **hint and never as a decision** — `verifyWsAuth` is still the only thing that checks a password —
  and an unreadable token is treated as unlocked, since the server refuses it on its own and a password would be asked
  for against a check that will never run. Asking after a failed connect was the other option and is worse: a timer
  cannot tell a locked link from a dead server, and the reader waits out `repo.find`'s two ten-second attempts before
  being asked anything. A wrong password still refuses the handshake, so the sheet never arrives; `changeId`'s
  no-document branch says which of the two it likely was and that reloading asks again, which is the whole of the retry
  — nothing keeps the password, so a reload re-prompts. The prompt fires only when the share token is the credential
  actually in use: a logged-in reader's own JWT wins, and gating their sheet on a link they are not opening with would
  be a password for nothing. `/portal/*/sync` passes `pass` through to the same reader. It discards the role on
  purpose — a portal is a public synthetic stream and honours no share claim — but without the password beside it, the
  lock check fired on the one socket where the link was never going to grant anything. The share panel mints the link:
  a `days` box and a `password` box, both blank by default, and blank sends nothing, which is the empty body the route
  already reads as the unlocked thirty-day link. A `days` box holding something that is not a whole number above zero
  is refused **in the panel** rather than rounded down to blank: `String.toInt` answers `Nothing` for `7.5`, and
  defaulting that to 0 would mint a thirty-day link for somebody who asked for seven and say nothing. The refusals the
  **server** raises reach the user too, which they did not: `send()` in `src/index.html` read the body as JSON and fell
  back to `"POST /link failed (400)."`, but Hono answers an `HTTPException` carrying no `res` as **plain text** — so
  every `explain()` block these routes raise was parsed, thrown, swallowed by the `.catch(() => ({}))` and replaced by
  the status. It reads the body through `httpErrorDetail()` now, the same reader the freshness call uses. The password
  is never put in the url — a lock the link carries is not a lock — so the panel says so under a locked link rather
  than leaving the minter to find out. All four share routes now
  `.catch(() => ({}))` on `c.req.json()`, the shape `POST /library/:id/secret` already used: unguarded, a missing or
  unparseable body was an unexplained 500, a row in `net-hook:errors` and a point off the 5xx grade for a mistake the
  caller could have fixed from the message. `DELETE /library/:id/share` validated nothing at all, so a missing email
  reached the delete and came back as "undefined is not a non-owner member of this sheet" — a sentence about the sheet
  for a mistake in the request
- **Marketplace checkout**: `POST /buy/:id` fulfills `$0` listings immediately. A positive `sell_price` creates a Stripe
  Checkout Session (`STRIPE_SECRET_KEY`) and returns `{ checkout_url }`. `POST /stripe` verifies `stripe-signature`
  (`STRIPE_WEBHOOK_SECRET`) and fulfills `checkout.session.completed` with `payment_status=paid`. Checkout is card-only.
  Money lands on the platform account; Connect payouts are not wired.
- **Freshness**: `library:freshness` is a sheet the server **computes rather than stores**, so it pages, exports and can
  be selected from a query sheet (`select * from @library:freshness`) through the paths every other sheet uses. It has
  no automerge document and no `sheet` row, so `sheet()` answers it before the document lookup and before
  `assertSheetAccess`; the join to `sheet_usr` inside it is the access rule. One row per sheet whose runs land in
  `net` and are written down — a polled feed, a webhook, an alert — the caller can read: last run, last **good** run,
  and rows since. Both joins onto `net` are `left join lateral`, because a
  sheet that has never run at all is exactly the failure this read is for; and `failures_since_ok` compares
  `(created_at, net_id)` rather than `created_at` alone, the same key the laterals order by — on the timestamp alone, a
  good run and a bad one sharing one left `last_ok` equal to `last_run` and the count at zero, so the sheet this read
  exists to surface reported itself healthy. What counts as a run and what counts as a good one are `RUN_OF()` and
  `RUN_OK()`, which sit beside `POLL_OK`/`ALERT_OK` and are spliced into this query **three** times — the count
  subquery and both laterals — so the three cannot drift the way the two already had. A run is a different event per
  type: a net-http sheet's is its poll (`method = 'GET'`, graded `POLL_OK`), an alert's is its tick
  (`method = 'ALERT'`, graded `ALERT_OK`), and a net-hook sheet's is a delivery it was sent — every row, all of them
  good, because a refused delivery never reaches the table. The where clause is
  `s.type in ('net-http', 'net-hook', 'alert')`: deliberately **not** "sheets that have rows in `net`", because a
  webhook nobody has delivered to is the failure this read is for, the same argument the left joins carry; and
  deliberately **not** every type `net`'s check constraint admits, because a `net-socket` sheet is opened by the
  browser against the user's own url and nothing server-side ever writes a run for it, so including it would report
  "never run" forever about a sheet that works — the same false alarm as a blank cell on a rotten feed, pointed the
  other way. Two consequences, both intended: `net-hook:errors` appears in the operator's
  own freshness, and a net-hook's `last_ok` always equals its `last_run`. A `codex-*` sheet cannot appear at any
  price — `net`'s check constraint refuses its id — so codex connection health needs a writer and a schema change
  first. `GET /library/freshness` is the same answer through a plain route. The status check is deliberately **not** extended: a check whose conditions come and go
  with the data cannot be read by an uptime checker. **Known gap**: `@library:freshness` resolves on the server but not
  in the page, where `sheets()` looks a ref up in the library map or `repo.find`
- **Reads and export**: `GET /sheet/:id` is the stable JSON read for every sheet type. `GET /export/:id.<format>` is the
  download path, one route over the `EXPORTS` table: `csv`, `json`, `ndjson`, `md`, `ics`. All go through `sheet()`, so
  they inherit `assertSheetAccess` (membership, purchase, and `public`), pagination, and query-sheet recursion. Export
  asks for 100000 rows so a net or query sheet is not truncated at the default 50. A sheet with two columns of the same
  name is refused rather than overwritten — by every format now, not only `json` and `ndjson`, because `sheet()` itself
  keys a row by name and there are no positions left for `csv` and `md` to carry; `ics` needs a
  `date`/`timestamp`/`create` column and says so when there is none
- **Sheet as an API**: `POST /sheet/:id` is the write half of the stable read, and it takes rows keyed by **column
  name** — the header a CSV carries, and the shape `checkColumnTypes()` reads — storing them keyed by column key, so a
  value this route accepts can never be one a query then refuses. **The read answers in the same spelling.** It did not:
  `sheet()`'s `case "table"` handed back the automerge document verbatim, keyed by `col.key`, while every other branch
  of that switch already answered by name — `cselect` stamps `key: col.name` off the postgres column and `executeSql`
  stamps AlaSQL's `columnid` — so one endpoint pair had two spellings of one row and the two MCP read tools disagreed
  with each other about the same sheet (`read_sheet` → `{"0":"apple"}`, `query_sheet` → `{item:"apple"}`). The table
  branch now projects through `named()` and **restamps `key = name` on the column row**, which is what keeps it a
  one-branch change: every consumer reads a cell as `row[col.key]` — `toRecords()`, all five `EXPORTS` renderers,
  `/stats/:id` — and none of them had to learn a second convention. `POST /sheet/:id` is untouched, because it reads the
  raw document's columns and so keeps storing by the real key; `key` stays the document's own spelling, minted as a
  position and stable across a rename, and never leaves it. The price is paid by a sheet with **two columns of the same
  name**, blanks included: it has no name-keyed row to give and is now refused on the read and on **every** export,
  where `csv`/`md`/`ics` used to carry it positionally. That is the right way round — `toRecords()` has always collapsed
  such a pair, so a query over that sheet was already wrong, silently. Two guards keep that refusal from being a trap
  sprung far from its cause: the CSV importer refuses a duplicate header where the file is (see **CSV import**), and
  `nameClash()` refuses a rename onto another column's name **in the editor** — the rename would otherwise sync
  happily, leave the table on screen looking right, and 400 every export and every `select * from @this` from then on.
  Renaming one of an already-colliding pair is not a clash but the repair, and renaming a column to what it is already
  called is not one either. A document holding **rows under no columns** — every column deleted, the values left
  behind — is refused too, rather than answered with one empty object per row: the write already refuses that shape
  (`no columns to append under`), and a read that reports N rows of nothing as a healthy answer is the silent half of
  the same bug. An empty sheet is still empty rather than an error, since there is nothing to lose.
  `GET /openapi/:id`'s read response now `$ref`s the same `Row` the write takes, behind a `prefixItems` for `data[0]`,
  which is the column row and is not a row. It is **all-or-nothing**: every row is checked before
  the document is touched and the append is one `handle.change`, because automerge cannot roll a change back and a batch
  half-written under a 201 is a silent failure. Only a `table` sheet has rows of its own, so a query, chart or net sheet
  is refused by name rather than with a 500; write access is read through `syncRole` and not `assertSheetAccess`, so a
  viewer holding a share link — or anyone at all on a public sheet — can read it and cannot append. A short or long row
  names both counts, the field it stops at and the row as sent, exactly as `POST /import/csv` does, and a non-scalar
  value is refused because a cell holds a scalar. A script carries a **per-sheet key** rather than a user's JWT:
  `POST
  /library/:id/secret` with `{"name":"api"}` and no value mints `sheet_id.<32 random bytes>`, sealed with the
  same `encrypt` every other sheet secret uses, so rotation, `SECRET_KEEP` trimming, names-without-values and revocation
  all come from the routes that already existed rather than from a second store. The value is **minted, never supplied**
  — a key the owner chose is a key the owner reused elsewhere — and `api:*` is refused where it is typed, the same rule
  as an unknown `hook:*`. The sheet id rides inside the key so verifying reads at most `SECRET_KEEP` rows of one sheet,
  and the comparison is `crypto.subtle.verify`'s, so no key equality is written by hand; the four refusals name what
  arrived without printing a key, a digest, or how close it was. **The scope is a path check in the auth middleware,
  before routing**: a key opens `/sheet/<its id>` and `/openapi/<its id>` and nothing else, which is what stops a key
  minting itself another key, and is why no handler has to remember to ask. The identity it borrows is the sheet's
  creator, because the path is the bound, not the account. With no `scrapsheets-key` header the request takes
  byte-identically the path it always did. `GET /openapi/:id` is that key's target: an OpenAPI 3.1 document derived from
  the sheet's own columns at request time and **never stored**, read through `sheet()` so its access rule is the sheet's
  own, describing this one sheet's read, its write — omitted for a computed sheet, whose POST would be a 400 — and the
  header the key is presented in
- **CSV import**: `POST /import/csv` rejects a row whose field count does not match the header, naming the line, both
  counts, the raw text and the column it stops at. Type inference requires every non-blank value to parse: at the old
  80% threshold the other fifth became `NaN` silently. It also rejects a **header that names a column twice** — blanks
  included, since a blank header is named for its position — because every read is name-keyed and such a file would
  otherwise import happily and then be a sheet nothing can read back, refused by a message about the sheet rather than
  about the file
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
- **Feed health on the library, and a command palette**: `library:freshness` is read by `src/index.html` over
  `GET
  /library/freshness?limit=1000` and handed to Elm through the `freshnessLoaded` port — not by Elm's own `Http`,
  for the reason sharing is not: the JWT lives in that file. A limit above the default page of 50, because a truncated
  answer would report a rotten feed as a sheet with no freshness at all. No token means no request and no answer, which
  is why `libraryCols` grows its `freshness` column **only when `model.freshness` is non-empty**: a blank column over
  every row of a logged-out library reads as "nothing is wrong", which is the one claim this column exists to stop the
  page making. Within an answer the same rule holds per row — `freshnessCell` renders nothing for a sheet the read does
  not cover, `"never run"` for one that never has (the failure both of that query's lateral joins exist to surface), and
  `last run · N failed` when it is failing — because "never polled" and "polled and fine" are different facts and a zero
  would state the second about the first. A decode failure is reported into `model.error` rather than shown as an empty
  column, since a renamed field would otherwise wear the face of a healthy library. A refresh every 60 seconds, because
  the whole point is not waiting for the 15-minute status email. The gallery strip marks a failing sheet where a demo is
  opened from, keyed on the sheet's own freshness rather than its type, so it needs no edit when connection health joins
  that read. **The command palette** (Ctrl/⌘+K, which the shortcut sheet did not already claim) is a second door onto
  what exists, not a new set of commands: `shortcutGroups` gained a third field carrying the `Msg` each key runs, and
  `paletteCommands` reads that list rather than keeping a copy beside it — two hand-written lists drift, and the one
  that drifts is the one nobody opens. `Nothing` there is a key that only means something against a selection, which no
  palette row can supply; the palette lists itself and is not runnable, because it is already open. It matches sheets on
  name **and** id (the id is what you remember of a net sheet), is bounded at twelve rows so a big library is narrowed
  by typing rather than scrolled, and `PaletteRun` takes the index so Enter and a click are one message. `Goto Id` is
  the one new navigation message, and the library's Enter key now uses it instead of inlining `Nav.pushUrl`
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
