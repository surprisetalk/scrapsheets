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
have not signed up yet, and signing up later adopts it.

A webhook delivery must be signed, and each signature is accepted once. The signature covers the timestamp and the body
and nothing else, so two deliveries carrying the same body in the same second are one delivery — vary the body, not the
query string.

A webhook delivery must be signed. Read a net sheet's secret from its panel in the app, or with
`GET /library/<sheet_id>/hook`, which also answers with a runnable line:

```sh
body='{"hello":"world"}'
t=$(date +%s)
sig=$(printf '%s.%s' "$t" "$body" | openssl dgst -sha256 -hmac "$secret" -r | cut -d' ' -f1)
curl -X POST "https://api.sheets.scrap.land/net/$sheet_id" -H 'Content-Type: application/json' \
  -H "scrapsheets-signature: t=$t,v1=$sig" -d "$body"
```

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
