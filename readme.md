```nu
deno task build
deno task dev
deno task test
deno task status
```

`deno task status` grades every likely failure mode of the deployed service and exits nonzero when one that pages is
failing. **1.0 is the minimum passing grade**, 0.0 is total failure, and anything above 1.0 is headroom. Usage is graded
and printed but does not page: a product nobody used today is not an outage. `.github/workflows/status.yml` runs it
every 15 minutes; a failed scheduled run emails the repo owner, and that email is the alarm. `GET /status` is the same
answer as JSON, and needs no login.

The server refuses to start without `JWT_SECRET`, `TOKEN_SECRET` and `DSN_ENCRYPTION_KEY`, each a long random string
that must stay the same across restarts. They used to fall back to a random value and only warn, which dropped every
session, made every stored connection string unreadable, and re-rolled every webhook signing key on each restart.

Every failure lands as a row on `net-hook:errors`, so `select * from @net-hook:errors` is the error log. It is owned by
the seeded sentinel account, which has no password and cannot be logged into, so `/share` cannot reach it. Set
`OPERATOR_EMAIL` instead: `seed()` grants that address a viewer row on the next boot, creating the account row if you
have not signed up yet, and signing up later adopts it. Anyone else reading that sheet gets the failures their own
account caused, and nothing else.

A webhook delivery must be signed, and each signature is accepted once. A `v2` signature covers the timestamp, the
request path and query, and the body — so a fan-out that discriminates by query string sends two deliveries rather than
one and a replay. The older `v1`, which covered the body alone, is still accepted; `meta.scheme` on each row says which
one verified it.

Read a net sheet's secret from its panel in the app, or with `GET /library/<sheet_id>/hook`, which also answers with a
runnable line:

```sh
body='{"hello":"world"}'
path="/net/$sheet_id"
t=$(date +%s)
sig=$(printf '%s\n%s\n%s' "$t" "$path" "$body" | openssl dgst -sha256 -hmac "$secret" -r | cut -d' ' -f1)
curl -X POST "https://api.sheets.scrap.land$path" -H 'Content-Type: application/json' \
  -H "scrapsheets-signature: t=$t,v2=$sig" -d "$body"
```

A net-http sheet reads a key from there too: write `X-Api-Key: {{secret:weather}}` in its headers and the value is
resolved at fetch time, so the document holds the reference and never the token. Rotating the secret needs no edit to
the sheet.

A sheet can hold its own secrets instead. `POST /library/<sheet_id>/secret` with `{"name":"hook","value":"..."}` sets
the signing key; writing it again rotates it, and the one before still verifies until a third write retires it. `GET`
answers with the names and timestamps and never a value. Name it `hook:stripe`, `hook:github` or `hook:shopify` instead
and that provider's own signature is what is checked, against its own header — which verifier runs is read off the
stored secret, never off the headers the sender sent.

A sheet is also an API. `POST /sheet/<sheet_id>` appends rows keyed by column name, checked against the column types
before anything is written — a batch is all-or-nothing, because a half-written append under a 201 is a lie.
`GET /sheet/<sheet_id>` answers in the same spelling, so what you read back is what you would send. That costs one
thing: a sheet with two columns of the same name has no name-keyed row to give, and is refused by name until you rename
one. A script carries a key for one sheet rather than your login: `POST /library/<sheet_id>/secret` with
`{"name":"api"}` and no value mints one, answers it once, and it opens that sheet and nothing else.
`GET /openapi/<sheet_id>` describes the read and the write, generated from the sheet's own columns, so it cannot drift
from them.

```sh
curl -X POST "https://api.sheets.scrap.land/sheet/$sheet_id" -H "scrapsheets-key: $key" \
  -H 'Content-Type: application/json' -d '{"rows":[{"city":"Oslo","population":709037}]}'
```

`GET /sheet/library:audit` is who did what to which sheet: every read and write of a sheet over HTTP, every sheet a
query selects from, every open and first edit over the sync socket, and every MCP tool call, with `via` saying which
door. A webhook delivery is not in it, because it is already its own row on that sheet's log. An owner or editor reads every row about
their sheet; everybody reads the rows they made. It is a sheet, so `select * from @library:audit` and
`/export/library:audit.csv` both work, and a refused request is not in it because it did nothing.

`GET /library/freshness` names the feeds that stopped. One row per sheet whose runs are recorded — every polled feed,
every webhook and every alert you can read: when it last ran, when it last succeeded, and how many runs since. A webhook
nobody has delivered to in three days says so beside a poll that has been failing. It is a sheet, so
`select * from @library:freshness` and `/export/library:freshness.csv` both work.

The library table shows the same answer per row — last run, and failures since — and the demo strip marks a sheet whose
feed is failing, so a dead feed is visible where you open it rather than only in the 15-minute alarm email. Ctrl/⌘+K
opens a palette over every sheet and every shortcut; Ctrl/⌘+/ still lists the keys.

The share panel mints a view-only link, with a box for how many days it lives and a box for a password. Both are
optional and blank means the link it always minted: thirty days, openable by anyone holding the url. The password is
never stored and never put in the link — what rides the token is an HMAC of it under `TOKEN_SECRET`, so holding the link
buys nobody an offline guess, and you have to send the password some other way. Opening a locked link asks for it before
the sheet loads: the token says it is locked, and the refusal would otherwise land in a WebSocket handshake, where no
browser can read it. `POST /library/<sheet_id>/link` is the same thing over HTTP, taking `{"days": 7}` and
`{"password": "..."}`.

# polite scraper

Every request this server makes to somebody else's host carries the user agent `Scrapsheets/1.0 (+this page)`. It is
the poller behind net-http sheets and the page's `/proxy`, and nothing else. The poller asks one host at most once per
`HOST_GAP_MS` (`main.ts` names the number) however many sheets point there, it honours `Retry-After` for every sheet on
that host, it follows at most five redirects, and it stops retrying a feed after three failures in a row. To keep it
off a host, block that user agent; to ask about it, open an issue here.

A listing in the shop says what lets it be sold: `POST /sell/<sheet_id>` takes `{"price": 0, "license": "own"}`, where
`license` is one of the values `LICENSES` in `main.ts` lists, and the shop shows it beside the price. Anyone signed in
may report a listing, once, with `POST /shop/<sell_id>/report` and `{"reason": "..."}`; the reports are the sheet
`net-hook:reports`, where a reporter reads their own and the operator reads them all. The operator closes them with
`POST /shop/<sell_id>/review` and `{"action": "keep"}` or `{"action": "takedown"}`, and `deno task status` fails while
a report is waiting.

```nu
# watch mode
watch src { try { cp -vu src/* dist ; elm make src/Main.elm --debug --output=dist/index.js } }
```

The library works anonymously out of the box: bundled datasets (countries, US states, periodic table, CSS colors,
events), example queries that join them with `@sheet_id` refs, seven live portals, and a first-run tutorial.

- [Twinkling lights and nested loops: distributed
  problem solving and spreadsheet development](https://www.lri.fr/~mbl/Stanford/CS477/papers/Nardi-Twinkling-IJMMS.pdf)

<!--
templates/pages/portals <- queries. pages can't reference anything, agents can reference pages but not queries, and queries can reference pages and agents (but not queries).
--->
