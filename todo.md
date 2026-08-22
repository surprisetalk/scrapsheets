# Scrapsheets Roadmap

> Programmable data OS: every table is a database, every query is a table, every sheet is an API.

---

## Phase 2 — Growth

Build the features that create network effects. This is where the spreadsheet OS vision comes alive.

- [ ] 100 daily active users
- [ ] **Scrapscript WASM runtime**: compile Scrapscript interpreter to WASM for browser execution
- [ ] **Scrapscript query language**: wire up the existing `Scrapscript` lang type in Main.elm to execute Scrapscript
      programs against sheet data
- [ ] **Scrapscript cell formulas**: `=#` prefix in cells triggers Scrapscript evaluation
- [ ] **Content-addressable formulas**: cross-sheet references via Scrapscript hashes instead of fragile @sheet_id
      strings
- [ ] **Scrapscript marketplace**: sell/share self-contained Scrapscript functions as composable sheet utilities
- [ ] **Formula parser**: `=` prefix in cells triggers formula mode using the existing `Formula` lang type in Main.elm
- [ ] **Basic arithmetic**: =A1 + B1, =SUM(A1:A10), =AVERAGE, =COUNT, =MIN, =MAX
- [ ] **Cross-sheet references**: =@table:abc123.A1
- [ ] **Dependency tracking**: topological sort for evaluation order, cycle detection
- [ ] **Reactive recalculation**: formulas re-evaluate when referenced cells change
- [ ] **Schedule field on sheet**: cron expression or interval for recurring query execution
- [ ] **Server-side query runner**: Deno cron job executing scheduled queries
- [ ] **Pipeline composition**: net-http (extract) -> query (transform) -> table (load)
- [ ] **Execution log**: track pipeline runs, successes, failures per sheet
- [ ] **Google Sheets codex**: OAuth flow for reading Google Sheets as codex data source
- [ ] **Airtable codex**: OAuth flow for reading Airtable bases as codex data source
- [ ] **Notion codex**: OAuth flow for reading Notion databases as codex data source
- [ ] **Bidirectional sync**: write Scrapsheet data back to external sources

---

## Phase 3 — Moonshot

Transform Scrapsheets into a platform. Each feature creates a new axis of composability.

- [ ] **REST writes**: `GET /sheet/:id` returns JSON for every sheet type; POST to write rows does not exist
- [ ] **API key auth**: per-sheet API keys (not just JWT) for programmatic access
- [ ] **OpenAPI auto-generation**: derive OpenAPI spec from sheet column types
- [ ] **Change webhooks**: notify external URLs when sheet data changes
- [ ] **Rate limits per sheet**: configurable throttling for public sheet APIs
- [ ] **User-defined portal sources**: provide a WebSocket URL or HTTP polling config to create custom portals
- [ ] **Portal marketplace**: sell live data feeds as portal sheets
- [ ] **Portal composition**: query across multiple live portals in real-time
- [ ] **Embed code generator**: copy-pasteable iframe snippet in sheet settings
- [ ] **Interactive embeds**: viewers can sort/filter embedded sheets. An embed drops the filter bar with the rest of
      the chrome, so today it is a picture of the sheet, not a copy of it
- [ ] **Live dashboard mode**: combine portal data + embeds for real-time dashboards
- [ ] **Cursor presence**: show other users' cursor positions with colored indicators
- [ ] **Active user list**: display connected collaborators in sheet header
- [ ] **Cell comments**: annotation system with threads on individual cells
- [ ] **Presence indicators in library**: show which sheets have active editors
- [ ] **Timeline view**: slider showing document state over time (Automerge stores full history)
- [ ] **Diff view**: visual diff between any two versions
- [ ] **Named snapshots**: user-created bookmarks in the version timeline
- [ ] **Rollback**: restore sheet to any historical state
- [ ] **PWA manifest**: installable progressive web app with offline support
- [ ] **IndexedDB-first sync**: Automerge already uses IndexedDB; optimize for offline editing
- [ ] **Responsive mobile layout**: touch-friendly cell editing, swipe navigation
- [ ] **Conflict resolution UI**: surface Automerge merge conflicts for user review

---

## Go-to-Market

- [ ] **Seller payouts**: Stripe Connect for marketplace payouts (platform collects on Checkout today)

- [ ] **Public demo post**: skip the pitch deck; post a compelling demo and say you're looking for angels
- [ ] **Dev launch**: file real GitHub issues, publish the demo + a blog post, rally contributors
- [ ] **User testing round**: sit with Clark, Kirk, Jake; convert findings into issues
- [ ] **Investor track**: demo-first deck at /scrapland once the dev launch lands
- [ ] **Podcast circuit**: pitch spreadsheet/data podcasts for interviews
- [ ] **Customer funnel**: define the signup -> first sheet -> paid path and instrument it
- [ ] **Paid acquisition test**: advertise scrapsheets on Reddit and adjacent communities
- [ ] **Community position**: become the face of data analysis, curation, and sanitization
- [ ] **Reach out to Ellen Chisa**: intro via Brandon, lead with the demo
- [ ] **Public-entity data authority**: seed the shop with orgs, people, parcels, colors, songs — every public dataset
      worth owning
- [ ] **Local events database**: build the giant events table as a flagship public dataset

---

## Demo Gallery

End-to-end use-cases, written as sheet pipelines (`net-http` / `net-hook` / `portal` / `codex` -> `query` -> `table`).
Nothing here is finished. What already ships is in `src/examples.mjs` under the `demo` tag, run by `examples_test.ts` in
both engines — that file is the index, not this list. Some entries below name the query half that ships and the ingest
half that does not. The point of this list is coverage: later passes mine it for the missing features and the datasets
worth seeding in the shop. Every one of them dies at the same place, the ingest half, because a seeded table is a feed
nobody has connected yet.

### Flagship (best whole-stack stories)

- [ ] **Restaurant group weekly P&L**: POS `net-hook` + USDA commodity `net-http` + labor `codex` -> `query` plate cost,
      labor %, and store-level margin -> one embedded dashboard the owner reads on Sunday night. `table:pos-weekly` ->
      `query:store-margin` ships the prime-cost read, with a 4-week moving average and a rank per week; the POS hook,
      the commodity feed and the embed are open
- [ ] **Fund 13F drift**: EDGAR `net-http` quarterly filings -> `query` diffing consecutive quarters -> `table` of
      position adds/trims valued live by a price `portal`; publish the diff table to the shop
- [ ] **Contractor WIP schedule**: cost-code `table` + timecard `net-hook` -> `query` percent-complete revenue,
      over/under billing -> the exact schedule the bonding agent and the CPA both ask for. `table:job-costs` ->
      `query:wip-schedule` ships the schedule itself, including the estimate movement that turns a job upside down; the
      timecard hook is open
- [ ] **Municipal budget watchdog**: city checkbook `net-http` -> `query` department burn rate vs adopted budget ->
      public embed a local reporter can cite, republished monthly. `table:city-checkbook` -> `query:budget-ytd` ->
      `query:budget-burn` ships the burn rate and the projected year-end variance; the checkbook pull and the embed are
      open
- [ ] **Solo consultant's whole business**: proposal `net-hook` form -> pipeline `query` -> invoice `table` -> Stripe
      `net-hook` -> cash-collected `query`; the whole back office in six sheets

### Finance & Accounting

- [ ] **Three-statement rolling forecast**: ERP GL `codex` -> `query` actuals by account -> driver-assumption `table` ->
      `query` producing P&L, balance sheet, and cash flow that refresh at each monthly close
- [ ] **Sales commission calculator**: closed-won `net-hook` from CRM -> `query` joining quota, accelerator tiers, and
      clawback rules -> per-rep payout `table`; sell the whole thing as a shop template
- [ ] **Debt covenant monitor**: GL `codex` -> `query` computing DSCR, leverage, and fixed-charge coverage -> webhook
      alert when any ratio lands inside a warning band before the quarter closes
- [ ] **FX exposure and hedge sizing**: rate `portal` + open-invoice `table` by currency -> `query` net exposure per
      currency with suggested forward notional
- [ ] **Overhead allocation**: headcount, square footage, and machine-hour `table`s -> `query` allocating shared cost
      pools to cost centers with a switchable allocation basis
- [ ] **Revenue recognition (ASC 606)**: contract `table` with performance obligations -> `query` producing the monthly
      recognition schedule and deferred revenue rollforward
- [ ] **AR aging and collections queue**: invoice `codex` -> `query` aging buckets + payment-behavior score -> dunning
      `net-hook` that fires the right email at the right bucket. `table:invoices` -> `query:ar-aging` ships the buckets
      and the running exposure; the codex, the behaviour score and the dunning hook are open
- [ ] **Purchase-price allocation and earnout tracking**: deal `table` -> `query` earnout attainment against actuals
      each period

### Trading & Markets

- [ ] **Pairs-trade monitor**: price `portal` for two tickers -> `query` rolling spread z-score -> alert `net-hook` on
      entry/exit bands. `table:pair-prices` -> `query:pair-spread` -> `query:pair-zscore` ships the rolling band and the
      entry signal off a 10-day window frame, with the band width read from `@table:assumptions.entry_z`; the live price
      portal and the alert hook are open
- [ ] **Options chain screener**: chain `net-http` -> `query` filtering IV rank, spread width, and days-to-expiry ->
      candidate `table` refreshed intraday
- [ ] **Crypto treasury view**: exchange balance `net-hook` + on-chain `net-http` -> `query` consolidated position, cost
      basis, and unrealized P&L across wallets and venues
- [ ] **Earnings-day calendar**: filings `net-http` + implied-move `query` -> `table` of positions with an event inside
      the holding window
- [ ] **Alt-data backtest sheet**: any shop dataset -> `query` joining to forward returns -> `table` of information
      coefficient by lag; the pitch to data vendors selling on Scrapsheets

### Healthcare

- [ ] **Hospital price transparency comparison**: machine-readable price file `net-http` across N hospitals -> `query`
      normalizing negotiated rates per CPT -> a shop dataset nobody else has cleaned
- [ ] **Medicare PS&R reconciliation**: scheduled PS&R report pull -> `query` against the internal patient log ->
      variance `table` for the cost report
- [ ] **Drug shortage exposure**: FDA shortage feed `net-http` + formulary/inventory `table` -> `query` at-risk items
      with days-on-hand
- [ ] **Clinical trial competitive map**: ClinicalTrials.gov `net-http` -> `query` overlapping sites, indications, and
      enrollment velocity by sponsor
- [ ] **Infection surveillance**: lab result `net-hook` -> `query` rolling rate per unit per 1000 patient-days ->
      `portal`-fed ward dashboard
- [ ] **Prior-auth turnaround**: payer portal `net-hook` -> `query` median days by payer and service line; the number to
      bring to contract renegotiation
- [ ] **Physician productivity (wRVU)**: encounter `codex` -> `query` wRVU per FTE vs compensation plan thresholds

### Insurance

- [ ] **Catastrophe exposure**: storm track `portal` + geocoded policy `table` -> `query` total insured value inside the
      cone, refreshed as the track updates
- [ ] **Producer commission reconciliation**: carrier statement `net-hook` -> `query` against the book of business ->
      missing/short-paid commission `table`
- [ ] **Claims leakage audit**: closed-claim `codex` -> `query` stratified sample with outlier flags for the audit team
- [ ] **Rate filing comparison**: state DOI filing `net-http` -> `query` competitor rate changes by territory and class

### Legal

- [ ] **Docket watch**: court RSS/PACER `net-http` -> `query` matching client and adverse-party names -> alert `table`
      with new filings each morning
- [ ] **Realization and write-off analysis**: practice-management `codex` -> `query` realization by matter, partner, and
      client -> the table that drives rate discussions
- [ ] **Entity and lien monitoring**: Secretary of State `net-http` -> `query` detecting officer changes, new liens, and
      status lapses across a client portfolio
- [ ] **Patent landscape**: USPTO/EPO `net-http` -> `query` CPC class counts by assignee over time -> a whitespace map
- [ ] **Discovery volume and cost tracking**: vendor invoice `net-hook` -> `query` cost per GB per custodian

### Real Estate

- [ ] **Property tax appeal**: assessor `net-http` -> `query` assessment-to-sale ratio against comparables -> appeal
      packet `table`
- [ ] **Construction draw request**: budget `table` + invoice `net-hook` -> `query` percent-complete draw schedule the
      lender can accept as-is
- [ ] **Short-term rental pricing**: occupancy + local event `portal` -> `query` suggested nightly rate by date
- [ ] **Deal underwriting model**: assumption `table` -> `query` producing IRR, equity multiple, and DSCR across a rent
      growth / exit cap sensitivity grid
- [ ] **CAM reconciliation**: expense `codex` + lease `table` -> `query` per-tenant CAM true-up with the pro-rata math
      shown

### Construction

- [ ] **RFI and submittal log**: email/Procore `net-hook` -> `query` aging by ball-in-court -> escalation `table`
- [ ] **Material escalation clause**: BLS PPI series `net-http` -> `query` computing the contractual price adjustment
      per material line
- [ ] **Weather-at-risk schedule**: NWS forecast `portal` + schedule `table` -> `query` pours and lifts at risk in the
      next 10 days
- [ ] **Equipment utilization**: telematics `net-hook` -> `query` idle vs productive hours per machine -> rent-vs-own
      `table`
- [ ] **Certified payroll compliance**: timecard `net-hook` -> `query` prevailing wage check by classification ->
      WH-347-shaped output
- [ ] **Punch list to closeout**: field `net-hook` form -> `query` open items by trade and floor

### Manufacturing

- [ ] **OEE by line**: PLC/MES count `net-hook` -> `query` availability x performance x quality -> shift `table` with
      the top loss reason
- [ ] **Multi-level BOM cost roll-up**: component `table` -> recursive `query` exploding the BOM with current purchase
      prices
- [ ] **Supplier quality scorecard**: receipt/inspection `codex` -> `query` PPM defective, on-time, and cost of poor
      quality by supplier
- [ ] **Preventive maintenance scheduling**: runtime `portal` -> `query` next service due by machine hours, not calendar
- [ ] **Capacity and takt planning**: demand `table` + cycle-time `table` -> `query` line balance and required shifts

### Logistics & Supply Chain

- [ ] **Landed cost per SKU**: BOM `table` + HTS/tariff `net-http` + FX and freight index `portal` -> `query` true
      landed cost, re-run whenever a duty changes
- [ ] **Carrier scorecard**: EDI 214 status `net-hook` -> `query` on-time %, dwell, and accessorial spend by lane
- [ ] **Inbound container ETA**: AIS vessel `portal` -> `query` ETA slip vs promised date -> exceptions to the planner
- [ ] **Safety stock and reorder points**: WMS `codex` -> `query` deriving reorder point from demand variance and lead
      time variance
- [ ] **Cold chain excursions**: IoT temperature `net-hook` -> `query` excursion events with duration above threshold ->
      quarantine `table`
- [ ] **Tariff and trade rule change impact**: Federal Register `net-http` -> `query` affected HTS codes joined to the
      active SKU list

### Retail & E-commerce

- [ ] **Competitor repricing**: competitor page `net-http` -> `query` undercut and margin floor rules -> repricing
      `table` pushed back out by webhook
- [ ] **Marketplace settlement reconciliation**: Shopify/Amazon payout `net-hook` -> `query` fee-by-fee vs expected ->
      dispute `table`
- [ ] **Multi-channel inventory allocation**: channel demand `table` -> `query` allocating constrained stock by margin
      and velocity
- [ ] **Return abuse detection**: return `net-hook` -> `query` outlier return rate per customer and per SKU
- [ ] **Assortment gap**: search trend `portal` + catalog `table` -> `query` demanded categories with no SKU
- [ ] **Promo lift measurement**: sales `codex` -> `query` comparing promo weeks to a matched baseline

### Restaurants & Hospitality

- [ ] **Recipe costing and menu engineering**: recipe `table` + commodity price `net-http` -> `query` plate cost,
      contribution margin, and the stars/dogs quadrant
- [ ] **Labor vs sales, hourly**: POS `net-hook` -> `query` labor % against forecast by daypart -> overtime alert before
      it happens
- [ ] **Multi-unit P&L rollup**: per-store `codex` -> `query` consolidated P&L with per-store variance flags
- [ ] **Theoretical vs actual usage**: purchase `table` + sales mix `query` -> variance `table` showing where the food
      went
- [ ] **Health inspection watch**: city inspection `net-http` -> `query` own stores and competitors, with score history
- [ ] **Hotel RevPAR and pace**: booking `net-hook` -> `query` pace vs same-time-last-year by segment

### Energy & Utilities

- [ ] **Day-ahead vs real-time spread**: ISO price `portal` -> `query` hourly LMP spread by node -> arbitrage `table`
- [ ] **Solar fleet performance ratio**: inverter `net-hook` + irradiance `portal` -> `query` expected vs actual per
      site -> underperformer `table` ranked by lost revenue
- [ ] **Demand response event**: grid signal `portal` -> webhook shedding loads -> `query` measuring realized
      curtailment against the baseline for settlement
- [ ] **Utility bill audit**: bill `net-hook` -> `query` recomputing the tariff -> billing-error `table` and a
      rate-switch recommendation
- [ ] **Scope 1/2/3 carbon inventory**: activity `table` + emission factor `net-http` -> `query` inventory with factor
      provenance per line

### Agriculture

- [ ] **Irrigation scheduling**: soil moisture `net-hook` + evapotranspiration `portal` -> `query` next irrigation by
      zone
- [ ] **Grain marketing and hedge ratio**: CME futures `portal` + bushels-on-hand `table` -> `query` basis, hedged
      percentage, and breakeven
- [ ] **Crop insurance documentation**: NDVI `net-http` -> `query` stressed acreage timeline for the claim file
- [ ] **Livestock traceability and rations**: animal `table` + feed `table` -> `query` cost of gain per head
- [ ] **Farmers market inventory and CSA shares**: `net-hook` order form -> `query` pick list by route

### Government & Civic

- [ ] **Budget vs actual burn rate**: checkbook `net-http` -> `query` department burn against adopted budget with a
      year-end projection
- [ ] **Permit backlog**: permit portal `net-http` -> `query` median days to issue by type and reviewer
- [ ] **Campaign finance network**: FEC `net-http` -> `query` donor overlap between committees -> shareable graph table
- [ ] **Lobbying vs voting record**: disclosure `net-http` + roll call `net-http` -> `query` correlating spend to votes
- [ ] **Public records request tracker**: intake `net-hook` form -> `query` statutory deadline -> webhook reminder
- [ ] **Grant subrecipient monitoring**: award `table` -> `query` spend-down rate and reporting deadlines by
      subrecipient

### Education

- [ ] **Grant effort reporting**: payroll `codex` -> `query` effort % vs award commitment per investigator
- [ ] **Early-warning student risk**: LMS `net-hook` -> `query` attendance, submission, and grade slope -> advisor
      worklist
- [ ] **Enrollment funnel**: inquiry -> application -> deposit -> matriculation `query` by source and cohort
- [ ] **District spending vs outcomes**: state finance and assessment `net-http` -> `query` per-pupil spend vs growth ->
      public shop dataset
- [ ] **Course demand forecasting**: historical registration `codex` -> `query` sections needed next term

### Nonprofit & Grants

- [ ] **Grant opportunity pipeline**: Grants.gov `net-http` -> `query` scoring fit by eligibility and keyword ->
      go/no-go `table`
- [ ] **Form 990 peer benchmarking**: IRS 990 XML `net-http` -> `query` program-expense ratio and comp benchmarks
      against peers
- [ ] **Program outcome reporting**: intake `net-hook` form -> `query` producing the exact table each funder demands
- [ ] **Restricted fund tracking**: `query` net assets with and without donor restriction, release schedule included
- [ ] **Volunteer scheduling and hours**: signup `net-hook` -> `query` coverage gaps by shift

### Media & Journalism

- [ ] **Story data pipeline**: scrape `net-http` -> cleaning `query` -> `table` -> embedded interactive chart in the
      published article, updating itself after publication
- [ ] **FOIA tracker**: request `table` -> `query` statutory deadlines and appeal windows -> webhook nudges
- [ ] **Newsletter and audience analytics**: ESP event `net-hook` -> `query` open/click cohorts and churn by acquisition
      source
- [ ] **Ad sales pacing**: insertion order `table` -> `query` delivered vs contracted impressions with make-good risk
- [ ] **Beat entity database**: people, orgs, and properties `table`s a newsroom maintains and shares across reporters

### Music & Entertainment

- [ ] **Tour routing and gross potential**: venue `table` + ticket sales `portal` -> `query` routing distance, capacity,
      and settlement projection per night
- [ ] **Catalog and setlist analytics**: Setlist.fm `net-http` -> `query` song performance frequency vs streaming lift
- [ ] **Sync licensing pipeline**: brief `net-hook` -> `query` catalog matches by mood, tempo, and clearance status
- [ ] **Merch inventory per show**: sale `net-hook` -> `query` sell-through by size and city -> restock `table`
- [ ] **Film production budget vs actual**: cost report `codex` -> `query` variance by account and department

### Sports

- [ ] **Player efficiency model**: box score `net-http` -> `query` per-possession metrics and rolling form -> rankings
      `table`
- [ ] **Athlete load management**: wearable `net-hook` -> `query` acute:chronic workload ratio -> flag `table` for the
      trainer
- [ ] **Youth league scheduling**: team, field, and official `table`s -> `query` producing a conflict-free schedule with
      travel balance
- [ ] **Secondary market ticket pricing**: resale `portal` -> `query` dynamic price recommendation per section
- [ ] **Recruiting board**: prospect `table` + results `net-http` -> `query` ranked board with position needs

### Science & Research

- [ ] **Literature watch**: PubMed/arXiv `net-http` -> `query` matching a saved topic with dedupe against what was
      already read
- [ ] **Instrument run QC**: instrument `net-hook` -> `query` control-sample pass/fail with drift detection -> rerun
      queue
- [ ] **Variant annotation**: variant `table` -> `query` joined against a ClinVar `codex` -> annotated report
- [ ] **Field survey collection**: mobile `net-hook` form -> `query` validating observations -> a citable dataset
      published to the shop
- [ ] **Reproducible analysis artifact**: dataset + `query` published together as a content-addressed, re-runnable
      citation

### Marketing & Growth

- [ ] **Blended CAC and payback**: ad platform `net-hook` -> `query` spend joined to cohort revenue -> CAC by channel
      and week
- [ ] **SERP rank tracking**: SERP `net-http` -> `query` rank deltas, cannibalization, and lost-featured-snippet alerts
- [ ] **Multi-touch attribution**: event `net-hook` -> `query` over ordered touchpoint sequences with a switchable model
- [ ] **Influencer and affiliate ROI**: promo code `net-hook` -> `query` incremental revenue per creator
- [ ] **Content calendar to performance**: plan `table` joined to analytics `query` so the calendar grades itself

### Sales & CRM Ops

- [ ] **Pipeline hygiene**: CRM `codex` -> `query` stale, mis-staged, and missing-next-step deals -> nudge webhook per
      rep
- [ ] **Territory and quota planning**: account `table` -> `query` balancing territories by TAM and travel
- [ ] **Lead enrichment and scoring**: signup `net-hook` -> firmographic `net-http` -> `query` fit x intent score ->
      routing `table`
- [ ] **Renewal risk**: product usage `codex` + support ticket `codex` -> `query` churn signal ranked by ARR at risk
- [ ] **Deal desk approvals**: discount request `net-hook` -> `query` approval tier by margin impact
- [ ] **Partner-sourced revenue**: partner `table` -> `query` attributed pipeline and payout

### HR & Recruiting

- [ ] **Applicant funnel**: ATS `net-hook` -> `query` pass-through rate by stage and source, time-to-fill by role
- [ ] **Headcount plan vs actual**: plan `table` -> `query` feeding the finance forecast directly, no re-keying
- [ ] **Immigration and certification dates**: `table` -> `query` upcoming expirations -> webhook to HR and the employee
- [ ] **Onboarding and offboarding checklists**: `net-hook` trigger -> `query` open tasks by owner and day
- [ ] **Engagement survey analysis**: response `net-hook` -> `query` driver analysis by team with small-cell suppression

### Software & DevOps

- [ ] **Incident metrics**: alert `net-hook` -> `query` MTTA/MTTR by service and severity -> reliability review `table`
- [ ] **CVE exposure**: OSV feed `net-http` + SBOM `table` -> `query` affected services ranked by exploitability and
      blast radius
- [ ] **DORA metrics**: deploy and incident `net-hook` -> `query` deploy frequency, lead time, change failure rate
- [ ] **On-call fairness**: rotation `table` + page `net-hook` -> `query` nights and weekends carried per person
- [ ] **Support ticket triage as an MCP sheet**: ticket `codex` -> `query` -> a sheet an AI agent reads and writes back
      through the MCP endpoint
- [ ] **Feature flag cleanup**: flag `net-http` + usage `query` -> stale flags with owner and age

### Transportation & Aviation

- [ ] **Hours-of-service compliance**: ELD `net-hook` -> `query` violations and near-violations before dispatch
- [ ] **Flight ops on-time analysis**: ADS-B `portal` + schedule `table` -> `query` tail utilization, turn time, and
      delay cause
- [ ] **Charter quoting**: aircraft `table` + fuel `portal` -> `query` quote with positioning legs and crew duty limits
- [ ] **Transit reliability**: GTFS-RT `portal` -> `query` headway adherence by route and hour -> public embed

### Climate & Environment

- [ ] **Neighborhood air quality**: sensor `portal` -> `query` exceedance hours by location -> public dashboard
- [ ] **Water sampling compliance**: lab result `net-hook` -> `query` against permit limits -> exceedance report on the
      regulator's schedule
- [ ] **Facility emissions benchmarking**: EPA `net-http` -> `query` intensity per unit output vs sector peers
- [ ] **Wildfire and flood asset risk**: hazard layer `net-http` + asset `table` -> `query` exposure by property
- [ ] **Recycling and waste diversion**: hauler invoice `net-hook` -> `query` diversion rate by stream and site

### Household & Personal

- [ ] **Job search tracker**: application `table` + posting `net-http` -> `query` stale applications and follow-up dates
- [ ] **Renovation bid comparison**: bid `table` -> `query` normalized line-by-line comparison with scope gaps flagged
- [ ] **Collection catalog**: records, books, or plants `table` + price `net-http` -> `query` current value and gaps
- [ ] **Event planning**: RSVP `net-hook` form -> `query` seating, dietary counts, and vendor headcount deadline
- [ ] **Fantasy league and pick pool**: score `portal` -> `query` standings that update themselves during games

---

## Capability Backlog

Everything the Demo Gallery needs that does not exist yet. Untriaged on purpose: this is the raw list, and a later pass
sorts it into phases. Where a line deepens something already in Phase 2/3, it says so.

### Query engine

The single biggest gap. Most demos die here first.

- [ ] **Exact decimal arithmetic**: money must not be a float; typed `Decimal` with scale, no silent precision loss
- [ ] **min()/max() over text**: AlaSQL compiles both inline, restricted to numbers and dates, and drops a text value,
      so `min(code)` returns nothing. `min_text()`/`max_text()` are the workaround and `checkResultColumns()` names the
      case; a real fix is upstream
- [ ] **Timezone-correct timestamps**: store UTC, render local, never guess the zone
- [ ] **Lateral joins and correlated subqueries**: AlaSQL cannot parse `lateral` at all. As-of joins no longer depend on
      it — `qualify` covers that case — but top-N-per-group over an expensive subquery still materializes everything
      first. A scalar subquery in an expression does work, which is what a cell reference is built on (partition by
      t.trade_id order by p.day desc) =
      1`.`query:asof-price`is the worked demo. The candidate pairs are still materialized before the filter, so a
      dedicated`asof
      join` that walks two sorted inputs is open
- [ ] **Strict null semantics**: distinguish null, empty string, and zero everywhere; no silent coercion
- [ ] **Typed query output**: query results carry column types forward, not strings
- [ ] **Materialized query results**: cache with an explicit refresh, so a 40-sheet dependency chain is not re-run per
      keystroke
- [ ] **Incremental query execution**: recompute only what changed since the last run
- [ ] **Chunked / larger-than-memory execution**: a million-row join should not need a million rows in the tab
- [ ] **Predicate pushdown to codex**: filter on the external database, not after transferring the whole table
- [ ] **Explain / profile**: show which step is slow and how many rows each stage produced
- [ ] **@sheet autocomplete**: suggest sheets and columns as the user types, from the real schema
- [ ] **Saved query snippets / UDFs**: reusable named expressions across sheets (leads into Scrapscript)

### Error messages

Per house style, an ambiguous error is the worst bug in the system.

- [ ] **A failed decode says which field failed**: `ShareLoad` reports a bad `hook` answer, but `members` still falls
      back to the last list it decoded, so a stale member list can be shown as current — a permissions UI lying about
      permissions.
  1. Decode `members` strictly and route the `D.Error` through `error`, the way the `hook` branch does.
- [ ] **A failure the user can see**: every failure lands in `net-hook:errors`, which is the operator's sheet — nobody
      else can read it, and a toast is still all a user gets. A per-user error log is the open half.
  1. Decide where a user's failures live: their own sheet is a sheet per account, which is a lifecycle nobody wants.
  2. More likely: `net-hook:errors` gains the `usr_id` that caused it, and the read filters on it.

### Types & validation

- [ ] **Money with currency code**: value + ISO currency, arithmetic refuses to mix currencies without a rate
- [ ] **Percent, ratio, and basis-point types**: display vs stored value handled once, correctly
- [ ] **Date, time, datetime, and timezone-aware timestamp as distinct types**
- [ ] **Duration and interval type**: for hours worked, dwell time, cycle time
- [ ] **Enum / single-select and multi-select**: with a defined option list
- [ ] **Reference type**: a cell that points at a row in another sheet (real foreign keys, not string ids)
- [ ] **Attachment type**: files, images, and PDFs stored per cell
- [ ] **Geo types**: point, polygon, and address
- [ ] **Unit-of-measure type**: quantity + unit with conversion, so lbs and kg cannot silently add
- [ ] **Column constraints**: not-null, unique, range, regex, allowed-values, referential integrity
- [ ] **Validation on write**: reject bad rows loudly at the ingest boundary, quarantine rather than coerce. CSV import
      rejects; `POST /net/:id` and cell edits still do not, and nothing is quarantined
- [ ] **Type inference on import with manual override**: guess, show the guess, let the user correct it
- [ ] **Schema drift detection**: alert when an upstream feed adds, drops, or retypes a column
- [ ] **Computed columns**: a column defined by an expression over its own row
- [ ] **Column-level defaults and generated values**: created-at, row hash, sequence

### Table UX

The unglamorous spreadsheet niceties. Their absence is what makes people leave.

- [ ] **Saved views**: named sort/filter/hidden-column combinations per sheet
- [ ] **Column reorder and pin**: drag-resize and hide already ship; these two do not. Both need a persisted column
      order rather than the display-only hide set
- [ ] **Row drag-reorder**: insert, delete and duplicate ship (`Ctrl/⌘+Enter`, `Ctrl/⌘+Shift+Enter`, `Ctrl/⌘+Delete`);
      reorder needs drag state, not a splice
- [ ] **Drag-fill series**: dates, numbers, patterns. Plain fill-down ships as `Ctrl/⌘+D`
- [ ] **Number formatting**: decimals, thousands separators, currency symbol, percent, scientific, custom masks
- [ ] **Conditional formatting**: color scales, data bars, icon sets, rule-based cell coloring
- [ ] **Group by / outline rows**: collapsible groups with subtotals
- [ ] **Pivot table UI**: drag fields into rows/columns/values without writing SQL
- [ ] **Data cleaning verbs**: trim, dedupe rows, split column, text-to-columns, change case, remove blanks. Binning a
      numeric column is done: `width_bucket(v, lo, hi, n)`, with 0 below the range and n+1 above it so the tails stay
      visible instead of being folded into the end buckets. Filling a gap forward is done:
      `last_value(x) ignore nulls over (order by t rows between unbounded preceding and current
      row)`, with
      `query:meter-daily` as the worked demo. The rest are UI verbs, not SQL
- [ ] **Fuzzy dedupe UI**: cluster near-duplicate rows and merge with a chosen survivor
- [ ] **Dropdown options sourced from another sheet**: checkbox and enum cells already ship
- [ ] **Cell notes**: distinct from threaded comments in Phase 3
- [ ] **Sparklines and mini-charts in cells**
- [ ] **Virtualized rendering**: smooth scrolling on very large sheets

### Charts & dashboards

- [ ] **Core chart set**: line, bar, stacked bar, area, scatter, box plot. Line and bar ship, drawn as SVG from
      `elm/svg`, with the baseline pinned to zero unless the data goes below it — a bar chart that starts anywhere else
      misstates every comparison on it. A row whose y is not a number is dropped rather than read as zero. **Histogram
      needs no chart kind**: `width_bucket(v, lo, hi, n)` bins in SQL and a `bar` chart draws the bins, which is
      `query:no-show-lead` and `chart:no-show-lead`. A `histogram` kind would only mean teaching `viewChart` a fourth
      word for the bars it already draws
- [ ] **Time-series handling**: date axis, gap handling, downsampling for long series
- [ ] **Maps**: point maps and choropleths driven by a geo column
- [ ] **Heatmap and matrix charts**: cohort grids and loss triangles read naturally as heatmaps
- [ ] **KPI tiles**: single number with a delta and a sparkline
- [ ] **Chart annotations**: mark an event on the axis (a release, a price change, a storm)
- [ ] **Dual axis and secondary series**

### Ingest — net-http

- [ ] **Secrets for authenticated requests**: per-sheet headers already ship, but the values sit in the document in
      plain text; they belong in a secret store
- [ ] **OAuth token handling**: authorization code flow plus automatic refresh
- [ ] **Non-GET requests**: POST/PUT with a templated body
- [ ] **Response parsers**: JSON path, CSV/TSV, NDJSON, XML, RSS/Atom, HTML with CSS selectors, XLSX, Parquet
- [ ] **Archive handling**: zip and gzip, including "the one CSV inside this daily zip"
- [ ] **PDF table extraction**: half of government data ships as PDF
- [ ] **Pagination**: page, offset, cursor, and Link-header styles, with a stop condition
- [ ] **Conditional requests**: ETag and If-Modified-Since so a daily file is not re-downloaded hourly
- [ ] **Since-last-run cursor**: fetch only new records, with the watermark stored on the sheet
- [ ] **Append vs replace vs upsert-by-key**: choose the write mode per sheet
- [ ] **Retry with backoff and a failure log**: distinguishing transient from permanent
- [ ] **Rate limiting and politeness**: per-host concurrency and delay, respect for Retry-After
- [ ] **Raw response archive**: keep the original bytes so a parser fix can be replayed without re-fetching
- [ ] **Response size and time caps**: with a clear message rather than a hung sheet
- [ ] **Pre-flight test button**: fetch once, show the parsed preview, then save
- [ ] **Static egress IP**: many enterprise sources require allowlisting

### Ingest — net-hook & forms

- [ ] **You send a Stripe or GitHub webhook straight at a sheet**: ours is one scheme, and every provider signs its own
      way, so today a hook has to be relayed by something that re-signs it.
  1. Read the provider off the sheet's config, not off the request: a spoofed header must not pick the verifier.
  2. Verify Stripe's `stripe-signature`, GitHub's `x-hub-signature-256` and Shopify's base64 HMAC against a stored
     secret. That is the point a secret store stops being optional — a shared secret cannot be derived.
- [ ] **You rotate a hook's secret without a missed delivery**: `hookSecret()` derives from `TOKEN_SECRET`, so today
      rotating one sheet rotates every sheet.
  1. Accept two secrets during a rollover, and record which one a delivery verified against.
- [ ] **A captured delivery cannot be replayed**: the `HOOK_SKEW` window bounds it to five minutes, which is not the
      same as never.
  1. Store the delivery id (or the signature) per sheet and refuse a repeat, bounded the way `trimNet` bounds a log.
- [ ] **Payload mapping**: JSON path -> column mapping, so a webhook lands as typed rows not a blob
- [ ] **Filters**: drop events that do not match a predicate before they hit the table
- [ ] **Dead-letter table**: malformed payloads are kept and inspectable, never dropped
- [ ] **Delivery log**: every request with status, latency, and body, queryable like any sheet
- [ ] **Per-hook rate limits and size caps**
- [ ] **Public form sheets**: generate a real form from column types, with validation and a thank-you page
- [ ] **Form file uploads**: attachments land in the attachment column
- [ ] **Email-in address per sheet**: forward an email and its body/attachments become rows
- [ ] **Inbound SMS**: same idea for a phone number
- [ ] **Response templating**: return a chosen status and body so the sheet can answer a callback synchronously

### Ingest — files & drives

- [ ] **XLSX and JSON upload**: CSV drag-and-drop with type inference already ships
- [ ] **Import mapping UI**: map incoming columns to existing ones, remember the mapping for next time
- [ ] **Multi-sheet XLSX**: pick the tab, handle merged headers and junk header rows
- [ ] **Cloud drive pull**: Drive, Dropbox, Box, OneDrive, S3/R2, SFTP, with a watched folder
- [ ] **Streaming import for large files**: progress bar, resumable, no browser-tab death
- [ ] **OCR and document extraction**: invoices, receipts, and scanned reports into rows
- [ ] **Recurring file import**: the same daily drop, imported on a schedule, with dedupe

### Codex — databases

- [ ] **More engines**: MySQL, SQLite, SQL Server, DuckDB/Parquet, BigQuery, Snowflake, Redshift, ClickHouse, MongoDB,
      Athena
- [ ] **Schema browser and table picker**: choose tables without writing SQL first
- [ ] **Read-only enforcement**: a codex must not be able to write unless explicitly granted
- [ ] **Incremental sync with a watermark column**: pull deltas, not the whole table
- [ ] **CDC / logical replication**: for sources that support it
- [ ] **SSH tunnel and TLS options**: the two things every enterprise connection needs
- [ ] **Connection health checks**: surfaced as freshness on every downstream sheet
- [ ] **Credential rotation**: without breaking live sheets
- [ ] **Row and column masking**: expose a subset of a sensitive table
- [ ] **Sampling for preview**: never `select *` a billion-row table to render a thumbnail

### Codex — SaaS connectors

Ordered roughly by how many demos each unblocks.

- [ ] **Accounting**: QuickBooks, Xero, NetSuite, Sage, SAP, Dynamics
- [ ] **Payments and banking**: Stripe, Square, Plaid, Mercury, Ramp, Brex, PayPal, Adyen. The reference half ships:
      `table:iban-formats` (length per country, the cheapest check on an account number and the one that fails before
      anything reaches a bank), `table:card-networks` (issuer prefixes, lengths, Luhn) and `table:mcc-ranges` (ISO 18245
      merchant categories by range, the only grouping the standard itself defines)
- [ ] **Commerce**: Shopify, Amazon SP-API, WooCommerce, BigCommerce, Etsy, eBay
- [ ] **CRM**: Salesforce, HubSpot, Pipedrive, Close, Attio
- [ ] **Support**: Zendesk, Intercom, Front, Help Scout
- [ ] **Product and project**: Jira, Linear, Asana, Monday, ClickUp, Notion, Airtable, Trello
- [ ] **Dev**: GitHub, GitLab, Sentry, Datadog, PagerDuty, CircleCI, Vercel, Cloudflare
- [ ] **Marketing and ads**: Google Ads, Meta Ads, LinkedIn Ads, TikTok Ads, GA4, Search Console, Klaviyo, Mailchimp,
      Braze
- [ ] **Analytics**: Segment, Mixpanel, Amplitude, PostHog
- [ ] **HR and payroll**: Gusto, Rippling, ADP, Workday, BambooHR, Greenhouse, Lever, Ashby
- [ ] **Comms and calendar**: Slack, Discord, Gmail, Outlook, Google Calendar, Zoom, Calendly, Twilio, DocuSign
- [ ] **Vertical systems**: Procore, ServiceTitan, Toast, Lightspeed, Epic/FHIR, Availity, Clio, MINDBODY, Shipstation
- [ ] **Connector SDK**: a declarative connector definition so users and sellers add their own without a code change

### Scheduling & runs

Phase 2 has the runner. These are what the demos need on top of it.

- [ ] **Cron plus interval plus "on upstream change"**: three trigger kinds, not one
- [ ] **Timezone- and DST-aware schedules**: "9am local on business days" must mean it
- [ ] **Business-day and fiscal-calendar triggers**: third business day after month end
- [ ] **Run now, pause, and disable**: with a visible next-run time
- [ ] **Run history**: start, duration, rows in/out, status, error, per sheet. Every row in `net` now carries a `meta`
      column beside its body — `{status, ms, bytes}` for a net-http poll, `{ms}` for an alert run — so "is this feed
      slow, is it 200-ing an error page" is a query. A scheduled runner has no history because it has no runner
- [ ] **Idempotency keys**: a re-run must not double-append
- [ ] **Backfill**: run a schedule over a historical date range
- [ ] **Concurrency control**: skip or queue if the previous run is still going
- [ ] **Dependency-ordered runs**: a DAG execution order derived from @sheet refs
- [ ] **Partial-failure semantics**: one failed source must not silently empty a downstream table
- [ ] **Run budgets and quotas**: per user and per sheet, with a clear message at the limit
- [ ] **Manual approval step**: pause a pipeline for a human to confirm before the write

### Alerts & notifications

- [ ] **Threshold, change, and anomaly conditions**: value crosses X, value changed by Y%, value outside its usual band.
      Threshold and change are a where clause over a query that already has window functions; an anomaly band needs the
      forecasting work in **Stats & modeling**
- [ ] **Destinations**: email, SMS, Slack, Discord, Teams, webhook, push. Email ships, through the Resend key the signup
      flow already uses; a refusal from Resend is recorded on the alert rather than swallowed
- [ ] **You can tell a quiet alert from a dead one**: `pollAlertOnce` records a run only when the answer changed, so a
      stable alert writes nothing for days and a `setInterval` that died writes nothing either. `GET /status` therefore
      cannot grade alert liveness at all, the way it grades net-http freshness.
  1. Record every run, not only the ones that changed, with `status = 'unchanged'`; the de-dupe already reads the last
     row, so it keeps working.
  2. Then add the status condition: every alert sheet ran within twice its interval.
- [ ] **Snooze, acknowledge, and escalate**
- [ ] **Subscribe to a sheet**: get told when a sheet you follow changes, without owning it

### Actions & write-back

The missing other half: sheets that do something, not just show something.

- [ ] **HTTP action**: POST/PUT to an external API from a sheet, with a templated body
- [ ] **Per-row actions**: run an action for each row matching a predicate, once
- [ ] **Action queue**: retries, backoff, idempotency keys, and a dead-letter table
- [ ] **Dry-run mode**: show exactly what would be sent before anything is sent
- [ ] **Approval gate**: a human confirms before an action batch executes
- [ ] **Secrets vault**: encrypted per-user and per-sheet secrets, never rendered in a cell
- [ ] **Action audit log**: who ran what, against which rows, with which payload
- [ ] **Rate limits and blast-radius caps**: refuse to email 40,000 people by accident
- [ ] **Built-in actions**: send email, send SMS, post to Slack, create a calendar event, write to a codex table

### Lineage, tests & freshness

- [ ] **Dependency graph view**: which sheets feed this one, and what it feeds
- [ ] **Impact analysis**: what breaks if this column is renamed or removed
- [ ] **Freshness indicator**: last-updated and expected-update per sheet, shown in the library
- [ ] **Staleness alerts**: tell me when a feed silently stopped updating
- [ ] **Data assertions**: not-null, unique, accepted values, row-count range, freshness bound, referential integrity
- [ ] **Assertion results in the run log**: with quarantine of failing rows instead of a silent pass
- [ ] **Row-level provenance**: which source and which run produced this row
- [ ] **Downstream change notification**: warn dependents before a schema change lands

### Stats & modeling

The Excel add-in market lives here (see the add-in research item).

- [ ] **Regression**: `regr_slope()`, `regr_intercept()`, `r2()`, `corr()`, `regr_predict()` and `regr_stderr()` ship —
      the fitted value and the spread of the points around the line, in the units of y, because a slope with no standard
      error beside it is a number nobody can argue with. Multiple and logistic regression, and per-row residuals, are
      open
- [ ] **Forecasting**: `regr_predict()` is the straight-line baseline and `fit_exponential()` the log-linear one.
      Seasonal decomposition is open, and it needs a series-to-series function, which neither the aggregate protocol nor
      the window pass can express today
- [ ] **Monte Carlo simulation**: distributions on input cells, sampled outputs, percentile results (the @RISK slot)
- [ ] **Sensitivity and tornado analysis**: which input moves the output most
- [ ] **Scenario manager**: named sets of assumptions, compared side by side
- [ ] **Goal seek and solver**: constrained optimization over a sheet
- [ ] **Clustering and segmentation**
- [ ] **Cohort and retention helpers**: `query:cohort-retention` and `query:cohort-grid` are the worked SQL, which is
      the thing to generalise from — deriving the cohort from the first order, then a pivot. A helper that writes it is
      open
- [ ] **Curve fitting**: `fit_exponential()` and `fit_power()` fit by the transform that straightens the curve — a log
      on y, a log on both — and refuse a value at or below zero by name rather than dropping it and bending the fit.
      That covers exponential decline and learning curves. Hyperbolic decline needs nonlinear least squares and is open;
      loss development factors turned out to be a self-join, not a fit (`query:loss-factors`)

### Geospatial

- [ ] **Geocoding and reverse geocoding**: address <-> point, with a match-confidence score
- [ ] **Address normalization and dedupe**: the hard part of every property and customer dataset
- [ ] **Spatial joins**: `point_in_polygon()` ships and works as a join predicate — `query:buildable-sites` joins a
      parcel to its zoning district on the geometry, since nothing on the parcel row says which district it is in.
      Within-distance is `haversine_km(...) <= n` in a where clause (`query:below-market`). Nearest still materializes
      every pair first, which is the same missing piece as the as-of join
- [ ] **Distance, area, and drive-time**: `haversine_km()`, `bearing_deg()` and `polygon_area_km2()` ship in both
      engines, the last spherical rather than planar, because a county-sized polygon read as flat is wrong by more than
      the decision it is feeding. A ring that crosses the antimeridian is refused rather than answered inside out.
      Drive-time does not exist and nothing reprojects
- [ ] **Boundary datasets as sheets**: counties, tracts, ZCTAs, districts, custom territories
- [ ] **Map rendering**: points, choropleths, and heatmaps in a chart sheet

### AI & MCP

- [ ] **AI column**: classify, extract, summarize, or translate per row, with results cached by row hash
- [ ] **Document to table**: PDF, invoice, or contract into structured rows
- [ ] **Natural language to query**: schema-aware, showing the generated SQL for review before it runs
- [ ] **Entity resolution via embeddings**: match "Acme Corp." to "ACME CORPORATION" across sheets
- [ ] **Semantic search across a library**: find the sheet, not the filename
- [ ] **Cost caps and token budgets**: per sheet and per user, enforced before the spend
- [ ] **MCP write safety**: scoped tokens, per-tool permissions, an audit trail for agent writes
- [ ] **MCP resource exposure**: sheets as resources and prompts, not only tools
- [ ] **Prompt eval sheet**: test cases and scores for an AI column, as a normal sheet

### Reports & export

- [ ] **Export formats**: XLSX with formatting and Parquet. `GET /export/:id.<format>` ships csv, json, ndjson and md
      for every sheet type, and `GET /sheet/:id` is still the API-shaped JSON read
- [ ] **PDF report generation**: a print layout with headers, page breaks, and a title page
- [ ] **Scheduled report delivery**: emailed on a schedule with the file attached
- [ ] **Report templates**: prose plus live sheet embeds, so the narrative regenerates with the numbers

### Permissions & governance

Extends the Phase 2 roles work.

- [ ] **Row-level and column-level permissions**: hide salary from the team that should not see it
- [ ] **Teams, groups, and org accounts**: permissions granted to a group, not one email at a time
- [ ] **SSO / SAML / SCIM**: table stakes for any org-sized customer
- [ ] **Share links with expiry and password**
- [ ] **Ownership transfer and offboarding**: what happens to sheets when someone leaves
- [ ] **Audit log**: reads and writes, exportable, queryable as a sheet
- [ ] **PII tagging and masking**: mark a column sensitive; masked by default in shares and embeds
- [ ] **The server refuses to boot without its secrets**: `JWT_SECRET`, `TOKEN_SECRET` and `DSN_ENCRYPTION_KEY` each
      fall back to `Math.random()` and only warn. `TOKEN_SECRET` is now the root of every webhook signing key, so an
      unset one re-rolls every sender's secret on each restart, and the delivery is then refused with a message that
      points at the sender's secret rather than at the server.
  1. Throw at startup instead of warning; a secret-less boot is not a recoverable state.
  2. The tests import `main.ts` at module load, so they must set the three variables before the import, or the harness
     must set them in `deno.json`.
- [ ] **You read the error log without a psql prompt**: `net-hook:errors` is owned by the seeded sentinel, which has no
      password and cannot be logged into, so `POST /library/net-hook:errors/share` returns 403 for every real account.
  1. Have `seed()` grant the owner row to a configured operator address, or add an owner-transfer path.
- [ ] **Secrets scanning on publish**: refuse to publish a sheet containing an API key
- [ ] **PII scanning on publish**: warn before a dataset with personal data goes public
- [ ] **Retention policies and legal hold**
- [ ] **Backup and restore**: whole-workspace export and reimport
- [ ] **Region pinning**: for customers who must keep data in one jurisdiction

### Search, shop & discovery

- [ ] **Global search**: across sheet names, column names, and cell contents
- [ ] **Shop browse and facets**: by category, source, update cadence, license, and price
- [ ] **Tags and collections**: curated bundles of related sheets
- [ ] **Ratings, reviews, and usage counts**: "used by N sheets" as the trust signal
- [ ] **Free samples**: preview the first rows before buying
- [ ] **Dataset changelogs**: what changed in this dataset since last month
- [ ] **Provenance display**: source URL, license, fetch date, and transformation chain on every published dataset

### Marketplace economics

Extends the Stripe Checkout work, which ships platform-side.

- [ ] **Subscriptions**: recurring price for a dataset that keeps updating (the real model for feeds)
- [ ] **Usage-based pricing and metering**: per query, per row, per API call
- [ ] **Tiered and free plans**: free up to a row cap, paid above
- [ ] **Trials, coupons, and refunds**
- [ ] **License enforcement**: what a buyer may do with a purchased dataset, and what happens on cancellation
- [ ] **Seller dashboard**: revenue, subscribers, churn, and per-sheet analytics
- [ ] **Tax and VAT handling**: plus invoices and receipts
- [ ] **Private and org-only listings**
- [ ] **Bundles**: sell a pipeline of sheets as one product
- [ ] **Referral and affiliate credit**

### Performance & scale

- [ ] **Automerge document size limits**: compaction, history pruning, and a graceful path for large tables
- [ ] **Columnar storage for large sheets**: Automerge for collaborative editing, columnar for bulk data
- [ ] **Server-side pagination and virtual scroll**: for sheets too big to send to the browser
- [ ] **Background computation with progress**: long queries do not block the UI
- [ ] **Cold row archiving**: keep history without keeping it hot
- [ ] **Per-sheet resource metering**: rows, bytes, compute, and fetches, visible to the user before the limit hits

### Developer surface

Extends the Phase 3 API work.

- [ ] **CLI**: push and pull CSVs, run queries, tail run logs
- [ ] **Sheet-as-code**: a text definition of a sheet and its pipeline, checked into git
- [ ] **Staging copies**: clone a sheet, change the query, review the diff, promote
- [ ] **Branch and merge a sheet**: Automerge makes this genuinely possible; nobody else can offer it
- [ ] **Self-host and docker image**: for the customers who cannot send data anywhere
- [ ] **Workspace export/import**: no lock-in, stated loudly as a feature
- [ ] **Client SDKs**: a thin typed wrapper over the REST API
- [ ] **Sandbox / test mode**: fake webhook deliveries and dry-run schedules

### Navigation & workspace UX

- [ ] **Folders, workspaces, and favorites**: the library does not scale to hundreds of sheets flat
- [ ] **Command palette**: jump to any sheet or run any command from the keyboard
- [ ] **Recently viewed and back/forward**
- [ ] **Bulk library operations**: multi-select, move, tag, delete, share
- [ ] **Sheet duplication and templating from an existing sheet**
- [ ] **Accessibility**: keyboard-only operation, screen reader support, contrast, focus order
- [ ] **Locale-aware number and date formatting**: and per-user timezone
- [ ] **Trash and restore**: deleting a sheet must be undoable

### Trust, safety & abuse

- [ ] **Per-user quotas**: fetches, rows, storage, and outbound actions
- [ ] **An anonymous flood cannot outweigh the traffic it replaces**: every 4xx writes a row to `net-hook:errors` and
      runs `trimNet` behind it, so a rejected request costs two round trips — more than serving it would. The rate
      limiter keys on `x-forwarded-for`, which the caller sets, so the bucket is free to rotate. A 429 is already exempt
      from the log; the rest are not.
  1. Suppress repeats: one row per (status, path) per minute rather than one per request.
  2. Key the limiter on the connecting address, and trust `x-forwarded-for` only from the proxy in front of us.
- [ ] **Scraping etiquette controls**: per-host limits and a documented user agent
- [ ] **Shop moderation**: report a listing, review queue, takedown path
- [ ] **Source terms compliance**: record whether a dataset may be redistributed before it can be sold
- [ ] **Abuse detection on outbound actions**: rate anomalies that look like a sheet being used as a spam cannon

---

## Dataset Backlog

The shop inventory implied by the Demo Gallery. Most are public, most are ugly, and cleaning them is the product. Each
should land as a sheet with a stated source, license, update cadence, and provenance.

### Spines (the entity backbones everything joins to)

- [ ] **Organizations**: legal entity, brand, domain, ticker, LEI, EIN, NAICS, address; the join key for half the demos
- [ ] **People**: public figures, officers, physicians, licensees, with disambiguated ids
- [ ] **Places**: countries, states, counties, cities, ZIP/ZCTA, CBSA, census tracts, with geometries. `table:countries`
      and `table:us-states` ship, plus `table:airports` and `table:seaports` as point spines with coordinates. Counties,
      ZCTAs, CBSAs, tracts and every geometry are open
- [ ] **Parcels and buildings**: parcel boundaries, ownership, zoning, footprints
- [ ] **Products**: GTIN/UPC catalog and category taxonomy. `table:gs1-prefixes` ships the prefix-to-issuer ranges,
      which is what reads a barcode's origin; the catalog and taxonomy are open
- [ ] **Events**: the flagship local-events dataset already promised in Go-to-Market
- [ ] **Securities**: ticker <-> CIK <-> LEI <-> ISIN <-> domain crosswalk. `table:exchanges` ships the ISO 10383 MIC
      spine every trade record names the venue by; the identifier crosswalk itself is open
- [ ] **Songs and recordings**: MusicBrainz, ISRC/ISWC, Discogs
- [ ] **Colors**: already promised; the fun one that gets shared

### Reference & crosswalks

- [ ] **Calendars**: business days, public holidays by country and state, fiscal calendars, ISO weeks. `table:holidays`
      ships US federal holidays for 2026-2027 with statutory and observed dates, and `table:fiscal-calendars` the fiscal
      year start months that `fiscal_year()` takes as an argument; `business_days()` and `iso_week()` cover the derived
      parts. Every country and state outside the US is open
- [ ] **Time zones and DST rules**. `table:timezones` ships 96 IANA zones with standard and daylight offsets and an ISO
      3166 country code; the transition dates themselves are open
- [ ] **Currencies and FX rates**: daily and historical, plus a live rate portal. `table:currencies` ships the ISO 4217
      codes with symbol and minor units; the rates themselves are the open half
- [ ] **Geographic crosswalks**: ZIP <-> county <-> CBSA <-> tract <-> congressional district. `table:zip-ranges` ships
      the three-digit ZIP prefix ranges per state — a ZIP is not a shape and does not nest inside a county, but the
      first three digits land in exactly one state, which is the join an address list actually needs. `table:us-states`
      now carries `fips` and its Census `division` beside the region, and `table:fema-regions` and `table:epa-regions`
      ship the two federal groupings that are numbered alike and staffed from different cities. Counties, CBSAs, tracts
      and districts are open
- [ ] **Industry classification**: NAICS, SIC, GICS, with mappings between them. `table:naics-subsectors` ships the
      NAICS 2022 three-digit subsectors keyed to the sector table, which is the level most business data is reported at.
      `table:naics` (20 NAICS 2022 sectors), `table:sic` (11 divisions, which is what EDGAR stamps on a filer) and
      `table:gics` (11 sectors, 25 industry groups) all ship. The 6-digit codes and the crosswalks between the three are
      open, and GICS-to-NAICS never maps cleanly: one classifies revenue, the other establishments
- [ ] **Occupations**: SOC and O*NET codes and descriptions. `table:soc` ships the 23 SOC 2018 major groups; the
      detailed codes and all of O*NET are open

### Economy & government

- [ ] **BLS**: CPI, PPI by commodity, employment, OES wages by area and occupation
- [ ] **BEA**: GDP, personal income by county, industry accounts
- [ ] **FRED / Treasury**: rates, yield curve, spreads, money supply. `table:fed-districts` ships the 12 districts with
      their serial letter, which is what a call report and a Beige Book entry are keyed to
- [ ] **Census**: ACS demographics, County Business Patterns, Building Permits Survey, TIGER geometries
- [ ] **Federal Register and regulations**: rule changes with effective dates. `table:cfr-titles` ships all 50 titles,
      the first grouping any notice feed needs; the notices themselves are open
- [ ] **USAspending and SAM.gov**: contracts, grants, and registered entities. `table:agencies` ships 71 federal
      agencies with the department each bureau sits under, which is the handle three of these feeds disagree on the
      spelling of
- [ ] **Grants.gov**: open funding opportunities with eligibility
- [ ] **FEC**: committees, contributions, expenditures
- [ ] **Congress**: bills, roll-call votes, sponsors, committees
- [ ] **Lobbying disclosures**: LDA filings and registrants
- [ ] **IRS**: Form 990 filings and the exempt-organization master file
- [ ] **State business registries**: entities, officers, status, and UCC liens
- [ ] **Sanctions and PEP lists**: OFAC SDN, EU, UN consolidated

### Finance & markets

- [ ] **SEC EDGAR**: 10-K/10-Q, 8-K, 13F, N-PORT, Form 4, S-1, with XBRL financials extracted. `table:sec-forms` ships
      the form-type spine with who files each and when it is due, which is all a filing index gives you to group by
- [ ] **Equity prices**: end-of-day history plus a live quote portal, with splits and dividends applied
- [ ] **Options chains**: strikes, expiries, implied volatility
- [ ] **Futures and commodities**: CME settlements, LME, energy and ag spot prices
- [ ] **Crypto**: exchange prices, on-chain balances, token metadata
- [ ] **Short interest and insider trades**
- [ ] **Earnings calendar and transcripts**
- [ ] **Bankruptcies and credit events**
- [ ] **Bank and credit union data**: FDIC, NCUA call reports

### Health

- [ ] **CMS**: NPI registry, hospital and physician compare, cost reports, fee schedules, Part D prescribing
- [ ] **Hospital price transparency files**: the normalized cross-hospital rate table nobody has cleaned well
- [ ] **FDA**: drug shortages, recalls, adverse events, NDC directory, device clearances
- [ ] **ClinicalTrials.gov**: trials, sites, sponsors, enrollment
- [ ] **Code sets**: ICD-10, HCPCS, LOINC, RxNorm, SNOMED — note the ones that need a license (CPT). `table:icd10` ships
      the 22 ICD-10-CM chapter ranges and `table:hcpcs-ranges` the Level II letter ranges, which is the whole Level II
      taxonomy: the letter says which benefit category a line falls in before anyone reads the code. `table:pos-codes`
      ships the CMS place-of-service codes, the two digits that decide which of two prices a procedure is paid at, and
      `table:carc` the common claim adjustment reason codes with an `appealable` column. The detailed code sets, LOINC,
      RxNorm and SNOMED are open
- [ ] **CDC**: notifiable disease surveillance, wastewater, mortality, vaccination
- [ ] **Genomics references**: ClinVar, dbSNP, gene and transcript annotations

### Weather, climate & hazard

- [ ] **NWS/NOAA forecasts and observations**: plus a live conditions portal
- [ ] **Historical weather and climate normals**: with growing degree days derived
- [ ] **Storm tracks and severe weather**: hurricanes, hail, wind, tornado reports. `table:hazard-scales` ships
      Saffir-Simpson, Enhanced Fujita, Beaufort, moment magnitude, US AQI and UV index as one bandable table; the
      observations themselves are open
- [ ] **FEMA**: flood maps, disaster declarations, NFIP claims
- [ ] **Wildfire perimeters and risk**: NIFC and related
- [ ] **Air quality**: regulatory monitors plus low-cost sensor networks
- [ ] **Water**: USGS streamflow, quality sampling, drought monitor
- [ ] **Earthquakes and seismic hazard**

### Energy & environment

- [ ] **EIA**: generation, fuel prices, consumption, capacity
- [ ] **ISO/RTO price feeds**: CAISO, ERCOT, PJM, MISO, ISO-NE, NYISO day-ahead and real-time LMP.
      `table:grid-operators` ships the 9 North American operators as the spine; every price feed is open
- [ ] **Solar resource**: irradiance and modeled production
- [ ] **State oil and gas production**: well-level, by state agency
- [ ] **Emissions factors**: eGRID, EPA, DEFRA, for carbon accounting. `table:emission-factors` ships the EPA combustion
      factors per mmBtu and per physical unit plus the eGRID US average; DEFRA and the per-subregion grid factors are
      open
- [ ] **EPA enforcement and emissions**: ECHO, GHG reporting, TRI

### Property & construction

- [ ] **Assessor rolls and deed transfers**: by county
- [ ] **Zoning and land-use codes**: normalized across jurisdictions
- [ ] **Building permits**: by city, plus the national survey
- [ ] **Rent and home price indices**: by ZIP and metro
- [ ] **HUD**: fair market rents, income limits, housing programs
- [ ] **Construction cost indices**: material PPI series and labor rates
- [ ] **Prevailing wage determinations**: Davis-Bacon and state equivalents
- [ ] **OSHA inspections and violations**

### Trade & logistics

- [ ] **HTS tariff schedule**: with duty rates and change history. `table:hs-chapters` ships all 97 HS 2022 chapters
      grouped into their 21 sections, which is the join key; the duty rates and the history are open
- [ ] **Trade flows**: Census trade data and UN Comtrade. `table:edi-transactions` ships the X12 transaction set codes —
      810, 214, 850, 856 and the rest — which is the number that says what an EDI file is and the only thing a logistics
      or claims feed tells you about its own shape
- [ ] **Vessel positions and port calls**: AIS-derived. `table:vessel-types` ships the AIS ship-and-cargo codes, the
      only description a position report carries; the positions are open
- [ ] **Port throughput and congestion**. `table:containers` ships the ISO 6346 types with the TEU each counts as, which
      is what makes a throughput figure comparable; the throughput itself is open
- [ ] **Fuel surcharge basis**: DOE diesel price series
- [ ] **Carrier registry and safety**: FMCSA SAFER, inspections, crashes
- [ ] **Air cargo and flight data**: schedules, ADS-B positions, on-time performance. `table:airports` and
      `table:airlines` ship the IATA/ICAO spines — a schedule carries the IATA code and an ADS-B feed the ICAO one, so
      both are needed to join them. Every movement feed is open
- [ ] **Freight rate indices**: container and truckload spot

### Retail, food & hospitality

- [ ] **Food prices**: USDA commodity and retail price series
- [ ] **Food composition and nutrition**: FoodData Central
- [ ] **Restaurant health inspections**: by city, normalized into one schema
- [ ] **Product catalog and taxonomy**: GTIN, categories, attributes
- [ ] **Search and shopping trend indices**
- [ ] **Alcohol, tobacco, and regulated product registries**

### Agriculture

- [ ] **USDA NASS**: yields, acreage, prices received, by county. `table:hardiness-zones` ships the USDA plant hardiness
      half-zones with their temperature bands — a zone is a temperature, not a place, which is why it joins to a
      planting table rather than to a map
- [ ] **Soil survey**: SSURGO properties by map unit
- [ ] **Satellite vegetation indices**: NDVI time series per field
- [ ] **Crop insurance program data**: RMA actuarial and loss history
- [ ] **Livestock and dairy reports**: prices, inventories, cold storage

### Education, labor & people

- [ ] **IPEDS and NCES**: institutions, enrollment, finance, outcomes
- [ ] **College Scorecard**: earnings and completion by program
- [ ] **School districts and boundaries**: with per-pupil finance
- [ ] **State assessment results**: normalized across states
- [ ] **H-1B and PERM disclosure data**: employer, role, wage
- [ ] **Job postings**: aggregated, with title and skill normalization
- [ ] **Professional licenses**: by state and profession

### Legal & compliance

- [ ] **Court dockets and opinions**: federal and available state. `table:circuits` ships the 13 courts of appeals with
      the states each covers and its authorized judgeships; the dockets are open
- [ ] **Patents and trademarks**: USPTO grants, applications, assignments, CPC codes. `table:nice-classes` ships the 45
      Nice classes, the only subject key a trademark register carries; CPC and the filings are open
- [ ] **UCC filings and liens**
- [ ] **Regulatory enforcement actions**: SEC, FTC, CFPB, state AGs
- [ ] **Statutes and administrative codes**: with citation structure. `table:cfr-titles` is the top of the CFR citation
      tree; parts, sections and the US Code are open

### Media, culture & sport

- [ ] **News and RSS corpus**: with publisher and topic metadata
- [ ] **Wikipedia / Wikidata entity extracts**: as the fallback join key for anything
- [ ] **Sports schedules and box scores**: per league, with a player-id crosswalk
- [ ] **Betting odds**: line history across books
- [ ] **Box office and streaming charts**
- [ ] **Setlists, tour dates, and venue capacities**

### Technology

- [ ] **Vulnerabilities**: NVD, OSV, CISA KEV, with package and version ranges. The CVSS v3.1 severity bands ride
      `table:hazard-scales` as the `cvss-v3` scale, since a score is banded the same way a wind speed is; the feeds are
      open
- [ ] **Package registries**: npm, PyPI, crates, Maven metadata and download counts
- [ ] **Cloud pricing**: AWS, GCP, Azure SKU prices and regions. `table:cloud-regions` ships 73 regions across the three
      clouds with the coordinates of the city each is named for, so a latency or residency question answers without a
      price list; the SKU prices are open
- [ ] **Domain, DNS, and certificate transparency data**. `table:dns-records` ships the IANA record types by number,
      which is the form a wire capture carries; the zone, WHOIS and CT feeds are open
- [ ] **Public status pages and incident history**. `table:http-status` ships the IANA status registry with a `retry`
      column, which is what turns a log of statuses into an answer about whether an integration is broken or busy

### Dataset infrastructure

Not datasets, but the machinery every dataset above needs to be sellable.

- [ ] **Dataset manifest**: source URL, license, attribution requirement, cadence, owner, and refresh status per sheet
- [ ] **Redistribution flag**: whether a source may legally be resold, checked before a listing goes live
- [ ] **Versioned publishing**: buyers pin a version; a changelog explains each release
- [ ] **You see which dataset stopped refreshing**: `GET /status` grades the feeds in aggregate — "every net-http poll
      returned 2xx", "every net-http sheet was polled in the past two hours" — but names none of them, so the alarm says
      something is rotten without saying what.
  1. A `dashboard` sheet over a query of `net` grouped by sheet, which needs no new server code.
  2. Failing that, one condition per feed is the wrong shape: the status check is a fixed list, not a per-row report.
- [ ] **Normalization conventions**: shared column names, date formats, and code sets across all shop datasets
- [ ] **Sample and preview generation**: a free first-N-rows sheet for every paid dataset
- [ ] **Seeding pipeline**: the datasets themselves defined as Scrapsheets pipelines, dogfooding the product

---

## Research

Trains of thought worth chasing; each should end in either a checklist item or a decision to drop it.

- [ ] **TUI spreadsheet prior art**: read xleak, vex-tui, and CacTui for interaction ideas (github.com/bgreenwell/xleak,
      CodeOne45/vex-tui, vkobinski/CacTui)
- [ ] **Competitive teardown**: what Ultorg, Rowboat, Excel add-ons, Wolfram, Airtable, Retool, GSuite, and Linear/Jira
      each do that Scrapsheets should absorb
- [ ] **Template galleries**: study airtable.com/universe and sourcetable.com/excel-templates; decide which categories
      seed the shop
- [ ] **Smartsheet verticals**: catalog smartsheet.com/solutions (PMO, services delivery, marketing, construction,
      education, government, manufacturing, energy) for template niches
- [ ] **Excel add-in market**: which add-ins actually earn (@RISK, Crystal Ball, XLSTAT, JMP, Minitab, Kutools,
      Ablebits, Power Query/Pivot) and which are replaceable by a sheet
- [ ] **Vertical add-in niches**: healthcare, legal, energy, agriculture, retail, government, architecture, linguistics,
      music, sports, real estate, education — find the ones with no good tool
- [ ] **ERP/CRM connector survey**: what a codex connector needs for NetSuite, SAP, Dynamics, Salesforce, HubSpot, Jira,
      Epic
- [ ] **Data vendors as sellers**: small/mid feed providers lacking distribution (crypto, alt-data, news, weather,
      sports, logistics, real estate) who could sell portal sheets
- [ ] **Institutional feed providers**: Refinitiv, Bloomberg, IEX, Tiingo, Twelve Data, Sportradar, FlightAware — what a
      firehose integration actually costs
- [ ] **Finance-ops pain points**: interview controllers and FP&A on accruals, overhead allocation, revenue recognition,
      variance analysis, commission calc — which are just net-http + query pipelines?
- [ ] **Recurring-reconciliation verticals**: healthcare (Medicare PS&R), insurance IBNR, construction percent-complete,
      government contractors (FAR/CAS), university grants — all pull public reports on a schedule
- [ ] **Third-party data finance teams want**: economic indicators, industry benchmarks, commodity pricing, credit risk
      scores, weather seasonality — package as portals
- [ ] **Data-virtualization competition**: Snowflake, Redshift, Denodo, MDM tools, and the human "data steward" workflow
      — where Scrapsheets fits
- [ ] **Beachhead segments**: data vendors selling to hedge funds, mid-size food distributors, RIAs running multi-family
      offices, restaurant-group controllers — pick one to target first
- [ ] **Spreadsheet influencers**: ExcelIsFun, Leila Gharani, Kevin Stratvert, Chandoo, Excel Campus; podcasts
      Spreadsheet Radio, MyExcelOnline, Humans of Data
- [ ] **Read lexega.com/blog/how-lexega-turns-sql-into-signals**: SQL-into-signals framing may map onto query sheets

---

## Strategic Notes

**The flywheel**: marketplace payments (1) attract template creators -> MCP server (1) makes sheets AI-accessible ->
Scrapscript (2) is the moat no one can replicate -> pipelines (2) make sheets self-updating -> sheet-as-API (3) makes
every sheet a microservice.

**What ships at 70%**: the foundation, MCP, and Stripe Checkout (platform-side) are done. Connect payouts are the one
piece of the marketplace still missing. Everything else compounds on top.

**The unique position**: Scrapsheets is not Google Sheets. It is not Airtable. It is a programmable data OS where every
table is a queryable database, every query result is a shareable table, every portal is a live data stream, every sheet
is an API, and every formula is a content-addressable program!
