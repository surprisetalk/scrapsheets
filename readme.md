```nu
deno task build
deno task dev
deno task test
deno task status
```

`deno task status` grades every likely failure mode of the deployed service and exits nonzero when one that pages is
failing. **1.0 is the minimum passing grade**, 0.0 is total failure, and anything above 1.0 is headroom. The usage goals
(a sheet made, a signup, a signup who made a sheet, a payment, and somebody other than the operator using the app) and
the count of overdue feeds and alerts are graded and printed but do not page: a product nobody used today is not an
outage, and a sheet somebody paused on purpose is a switch working. `.github/workflows/status.yml` runs it every 15
minutes; a failed scheduled run emails the repo owner, and that email is the alarm. `GET /status` is the same answer as
JSON, and needs no login.

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
the sheet. A feed that answers a POST or a PUT is that method and a body on the sheet: the body takes the same
`{{secret:name}}`, and `{{cursor}}` in it is the moment the last good poll started, which is what a cursor query
parameter carries. "Test the request" on the sheet runs the poller's request once, now, with the same fetch, the same
secret resolution and the same body cap, and shows the status, the time, the size and the start of the body, or the
refusal by name, before the sheet has to wait for the poller to find out. `POST /library/<sheet_id>/preflight` with
`{"url": "...", "headers": "...", "method": "POST", "body": "..."}` is the same thing over HTTP, and it writes nothing.

A feed says what a good run does to the runs before it: `mode` on the sheet is `append` (the default, the whole log),
`replace` (a good run deletes the earlier good runs, and a failed one deletes nothing) or `upsert` (a `key` names the
field a row is identified by, and a run supersedes the earlier runs holding the same keys). A feed that answers an
envelope holding two arrays names the one that is the rows with `rows_path`. A body is read by the type the answer
declares: CSV, TSV, NDJSON, gzip, RSS and Atom land as the JSON array they mean, and an HTML page lands as the rows of
its one table, so a query over the feed reads one shape whatever the wire carried, and a body its own type cannot parse
is a failed run naming the line. A zip is the one member inside it this server can read, checked against its own
checksum; an archive holding two it can read, or a page holding two tables, says so rather than guessing. Generic XML
lands as the document it means, decoded the way its own declaration says to decode it, and `rows_path` names the rows in
it. A paused sheet (the checkbox beside the interval) is stepped over by the poller and left out of the status check's
liveness grades, so pausing a feed does not read as an outage; "run now" polls it this second and answers the row it
wrote (`POST /library/<sheet_id>/run` over HTTP), and `library:freshness` says when each sheet runs next. A `cron`
pattern and a `timezone` beside the interval run a feed or an alert on a calendar instead, such as `0 9 * * 1-5` in
`America/Chicago`, and `{"cursor": "2024-01-31"}` on a run asks the feed again from that date.

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
from them. A sheet's API spends the same per-sheet budget a webhook sender spends: a read is one row of it and an append
is the rows it carries, and past the limit the answer is a 429 that names the limit and the window. An account is
bounded too: requests per second across every address it sends from, sheets it may own, rows one sheet may hold, and
alert deliveries a day across every alert it owns, a post to a url costing what an email does. Each refusal names the
count and the limit, and `deno task status` fails while any account has hit the sheets, rows or deliveries cap in the
past day; a request past its rate is shed with a 429 and not counted.

`{"name":"api-read"}` mints the read-only spelling of that key: it opens the same routes and is refused, by name, on
every write. Either key opens `POST /mcp/<sheet_id>` too, so an agent is something you hand a key rather than an
account: it lists, reads, queries and (with the writing key) writes that one sheet and nothing else, and browses it as
an MCP resource (`sheet://<sheet_id>`, the same csv the export answers) and a `describe_sheet` prompt.

```sh
curl -X POST "https://api.sheets.scrap.land/sheet/$sheet_id" -H "scrapsheets-key: $key" \
  -H 'Content-Type: application/json' -d '{"rows":[{"city":"Oslo","population":709037}]}'
```

A sheet tells somebody when it changes. `POST /library/<sheet_id>/webhook` with `{"url": "https://..."}` names where.
The receiver is sent a signed POST carrying `"event": "ping"` first and is registered only if it answers 2xx, so nothing
is ever posted to a url that did not ask. From then on every change to the sheet is posted there a few seconds later,
whoever made it and however: one delivery per flush, a JSON body naming the sheet, and a `scrapsheets-signature` header
signed exactly the way an inbound delivery to that sheet is verified, with the secret `GET /library/<sheet_id>/hook`
answers, over the receiver's path and the body. Each delivery spends the sheet's own budget. `GET` lists each url with
its last outcome, for owners and editors, since a receiver's url is often a credential; a receiver that fails ten times
in a row is left alone until you set it again with the same `POST`, and `DELETE` with the url stops it.

A CSV is imported in two steps. Choosing or dropping a file asks the server how it reads it, and what comes back is
shown before anything is made: each column with the type it was guessed to carry, the first rows under them, and a
select per column to correct a guess. The sheet is made with the types you settled on, and a settled type the values do
not fit is refused on the line that does not fit it. The types are remembered by header in this browser, so the next
file shaped the same opens already corrected. `POST /import/preview` and `POST /import/csv?types={"col":"num"}` are the
two steps over HTTP.

`GET /sheet/library:audit` is who did what to which sheet: every read and write of a sheet over HTTP, every sheet a
query selects from, every open and first edit over the sync socket, and every MCP tool call, with `via` saying which
door. A webhook delivery is not in it, because it is already its own row on that sheet's log. An owner or editor reads
every row about their sheet; everybody reads the rows they made. It is a sheet, so `select * from @library:audit` and
`/export/library:audit.csv` both work, and a refused request is not in it because it did nothing.

A feed that changes shape is a failed run, once: every poll records the columns the body answered with and the type of
each in `meta.shape`, and a run whose columns differ from the run before keeps its rows and carries `meta.shape_change`
naming the columns added, dropped and retyped. It grades as a failure through the same path every other failure takes,
so `library:freshness` counts it and the status check pages, and the run after it is the new normal.

A feed that answers the same body twice is one row: a good run's body is its idempotency key, in the same slot a
delivery's signature takes, so the row it matches moves to now instead of being appended again. And a failed poll is in
the feed's log, where you read it, but not in a query over the feed, so a sheet built downstream keeps what it had while
`library:freshness` says why.

`GET /library/freshness` names the feeds that stopped. One row per sheet whose runs are recorded — every polled feed,
every webhook and every alert you can read: when it last ran, when it last succeeded, and how many runs since. A webhook
nobody has delivered to in three days says so beside a poll that has been failing. It is a sheet, so
`select * from @library:freshness` and `/export/library:freshness.csv` both work.

`GET /library/lineage` is what feeds what: one row per query, alert or chart and the sheet it reads, off the live
document rather than a stale copy, so `select * from @library:lineage where depends_on = '@table:x'` is the list of what
breaks if that sheet goes. Its `columns` column says which columns of that sheet each dependent names — `*` where it
reads them all, `?` where nothing could check — so a rename knows what it breaks. An alert is silenced without being
deleted: "snooze a day" on the alert page records every run and delivers none until the day is up, and "subscribe to
this sheet" in the palette makes an alert that mails you when a sheet you are looking at gains a row. An alert's `to`
may be a Teams Incoming Webhook url as well as a Slack or Discord one, and feeds are polled before the alerts that read
them on one tick, so an alert never reads the cycle before's rows.

A feed answering Parquet lands as rows, and `/export/<sheet_id>.parquet` answers one, typed once per column; a CSV, TSV
or NDJSON feed that declares its `charset` is decoded in it, and one that declares nothing is read as it always was. A
body served under a wrong label is a failure row that names the label.

`trend(y, 12) over (order by month)`, `seasonal(...)` and `deseasonalized(...)` decompose a seasonal series in a query;
`query:visit-decomposition` is the bundled example, and its fit is a second query over the first. "build a cohort table
from this sheet" in the palette writes the cohort SQL for a table or query with a date and a key column and opens it as
a query sheet you then edit.

A numeric column can be shaded by its own values — a colour scale or data bars, from the column's panel, stored with the
arrangement — and a `json` cell holding a list of numbers draws as a sparkline. In the library, one tag goes onto every
selected row from the strip's tag box, and a tag put on a bundled demo survives the next merge. The shop lists type and
tags as columns over the whole catalogue, so the column panel is the filter.

The library table shows the same answer per row — last run, and failures since — and the demo strip marks a sheet whose
feed is failing, so a dead feed is visible where you open it rather than only in the 15-minute alarm email. Ctrl/⌘+K
opens a palette over every sheet and every shortcut; Ctrl/⌘+/ still lists the keys.

A sheet you are done with goes in the trash, and asks nothing first, because the trash is undoable: the 🗑 chip beside
the demo strip counts what is in it, and each row there offers restore, with the sort and the column widths you had
still on it. Delete is still there, inside the trash, and now it means what its warning says.

A star beside each row keeps a sheet at the top of the library and first in the palette, in this browser. A selection
spanning several library rows and "trash selected sheets" (Ctrl/⌘+Shift+Backspace) trashes them all at once.

A column is cleaned from its own panel, beside hide and pin: trim, UPPER, lower, and drop every row this column has
nothing in. Each one is an ordinary edit, so Ctrl/⌘+Z takes it back and everyone else looking at the sheet sees it. The
sheet's own verb is in the palette instead, because it reads every column rather than one: "delete duplicate rows" keeps
the first of every repeat and deletes the ones under it.

A column is split from the same panel: type the delimiter, and "split" pushes one new column per part -- `name 1`,
`name 2`, ... -- beside the column it read, refusing by name when a new name is taken or the delimiter divides nothing.

A column of numbers is written at the number of decimal places you ask it for, in the same panel: the cell, the column
stats and the totals row all read the one count, and an empty box goes back to whatever the number needed. The count
travels with the sheet, the way the sort and the column widths do.

A chart is drawn as a line, bars, an area, a scatter, a box plot, or one big number — `kpi` reads the last point, how
far it moved since the one before, and draws the whole series small beside it. A kind that is not one of those is
refused by name rather than quietly drawn as a line, which is what a typo used to get you.

A chart splits its rows by a `series` column: one line, area or dot set per series, bars stacked, and a legend that
wraps rather than writing its labels over each other, with the picture unchanged when nothing is named. When every x is
a day the axis is time: a point sits at its day, a gap in the data is a gap in the line, and a long series is folded to
a readable number of points with the count shown.

A second column can be drawn against its own axis on the right — a ratio and its z-score, say, which on one scale is a
flat line along the bottom. It is always drawn as a dashed line, whatever the rest of the chart is, so it is never
unclear which shape belongs to which axis.

A box plot is the one chart that summarises rather than plots: the median, the middle half and the two extremes of the
rows at each x, which is how you see that one production line is looser than the others rather than which single
measurement broke a limit.

A chart on a day axis can mark a day — a release, a price change, a storm — with a label on the axis. A day the data
itself does not hold still lands between the days that surround it, and a mark you have not finished typing is drawn
nowhere rather than at the left edge.

A column of near-duplicate rows is found and shown before anything is deleted: type how close counts, and the panel
names the rows that would go and what each one matched, because these are exactly the rows you cannot spot by looking.
"Acme Corp" and "Acme Corp." are one row at 70%; the exact repeats are the palette's verb.

Typing `@table:countries.` in the query editor completes the columns that sheet actually has, read the same way
`describe` reads them, so a suggestion can never name a column the query would then be refused for.

`select min(code) from @table:countries` answers, and so does the earliest date in a column. The engine under the page
compares numbers and real dates, and a cell is neither — it is the text the document holds — so it used to drop the
column out of the answer without a word. The query is now rewritten to the two functions that can compare text before
the engine sees it. Where it cannot tell what a name means — an expression like `min(upper(code))`, or a name a subquery
invented — it says so and names `min_text()` rather than guess.

`ols(array(y), array(x1), array(x2))` fits more than one predictor and answers the coefficients; `ols_predict` reads a
value back off them, which is how a residual lands on every row. `logit` and `logit_predict` are the same over a 0/1
outcome. `sample_uniform`, `sample_normal` and `sample_triangular` put a distribution on an input, seeded so the same
query answers the same numbers on the server and in the page, and `percentile(array(out), 0.9)` reads the spread back;
`table:trials` is the column to run them over.

The share panel mints a view-only link, with a box for how many days it lives and a box for a password. Both are
optional and blank means the link it always minted: thirty days, openable by anyone holding the url. The password is
never stored and never put in the link — what rides the token is an HMAC of it under `TOKEN_SECRET`, so holding the link
buys nobody an offline guess, and you have to send the password some other way. Opening a locked link asks for it before
the sheet loads: the token says it is locked, and the refusal would otherwise land in a WebSocket handshake, where no
browser can read it. `POST /library/<sheet_id>/link` is the same thing over HTTP, taking `{"days": 7}` and
`{"password": "..."}`.

Making a sheet public is refused, naming the column and row, while a cell holds an API key, and while one holds an email
address, a phone number, a social security number or a card number -- unless the share panel's second box says the
personal data belongs there, which the request carries as `personal: true`.

# polite scraper

Every request this server makes to somebody else's host carries the user agent `Scrapsheets/1.0 (+this page)`. It is the
poller behind net-http sheets and the page's `/proxy`, and nothing else. The poller asks one host at most once per
`HOST_GAP_MS` (`main.ts` names the number) however many sheets point there -- a feed that answers in pages is read to
its end within that one poll, page after page, bounded by `PAGE_MAX` -- it honours `Retry-After` for every sheet on that
host, it follows at most five redirects, and it stops retrying a feed after three failures in a row. To keep it off a
host, block that user agent; to ask about it, open an issue here.

A listing in the shop says what lets it be sold: `POST /sell/<sheet_id>` takes `{"price": 0, "license": "own"}`, where
`license` is one of the values `LICENSES` in `main.ts` lists, and the shop shows it beside the price. Anyone signed in
may report a listing, once, with `POST /shop/<sell_id>/report` and `{"reason": "..."}`; the reports are the sheet
`net-hook:reports`, where a reporter reads their own and the operator reads them all. The operator closes them with
`POST /shop/<sell_id>/review` and `{"action": "keep"}` or `{"action": "takedown"}`, and `deno task status` fails while a
report is waiting.

```nu
# watch mode
watch src { try { cp -vu src/* dist ; elm make src/Main.elm --debug --output=dist/index.js } }
```

Where it is going. Scrapsheets is not Google Sheets and not Airtable. Every table is a queryable database, every query
result is a shareable table, every portal is a live data stream, and every sheet is an API. The shop takes payment
through Stripe Checkout. The MCP server lets a model read and write sheets. Pipelines keep sheets up to date.
Scrapscript formulas, which are content-addressed programs, are the part nobody else can copy. `todo.md` is the queue,
and the usage conditions in `deno task status` measure whether anybody outside this repo uses it.

The library works anonymously out of the box: bundled datasets (countries, US states, periodic table, CSS colors,
events), example queries that join them with `@sheet_id` refs, seven live portals, and a first-run tutorial.

- [Twinkling lights and nested loops: distributed
  problem solving and spreadsheet development](https://www.lri.fr/~mbl/Stanford/CS477/papers/Nardi-Twinkling-IJMMS.pdf)

<!--
templates/pages/portals <- queries. pages can't reference anything, agents can reference pages but not queries, and queries can reference pages and agents (but not queries).
--->
