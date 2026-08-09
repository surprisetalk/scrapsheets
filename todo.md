# Scrapsheets Roadmap

> Programmable data OS: every table is a database, every query is a table, every sheet is an API.

---

## Phase 0 — Patch

Close the gaps that prevent daily use and public demo.

- [ ] **Codebase refactor**: the code has grown stale; consolidate before building on top of it
- [ ] **Automerge WebSocket adapter**: replace custom HonoWebSocketAdapter with official NodeWebSocketAdapter
      (main.ts:465)
- [ ] **Sheet preview thumbnails**: generate mini sparkline/heatmap SVG for library view (Main.elm:335)
- [ ] **Library examples**: ship a handful of real sheets that demo table/query/net/portal in the default library
- [ ] **Keyboard shortcuts**: define and document key bindings; show a shortcut sheet in the library
- [ ] **Tutorial**: guided first-run walkthrough from empty library to a working query sheet

---

## Phase 1 — Foundation

Make Scrapsheets reliable enough to be someone's primary data tool.

- [ ] **Stripe Checkout integration**: create checkout session on /buy/:id with sell_price, redirect to Stripe
- [ ] **Stripe webhooks**: complete purchase on payment confirmation, record transaction
- [ ] **Payment table**: add `payment` table tracking buyer, seller, amount, sheet, timestamp
- [ ] **stripe_customer_id on usr**: link users to Stripe customers for future payouts
- [ ] **Seller payouts**: Stripe Connect for marketplace payouts (can defer, collect platform-side first)
- [ ] **MCP endpoint**: replace the 501 stub at /mcp/:id with a Model Context Protocol server (main.ts:1824)
- [ ] **MCP read_sheet tool**: return sheet data as structured JSON
- [ ] **MCP write_cells tool**: update specific cells
- [ ] **MCP query_sheet tool**: execute SQL/PRQL against sheets
- [ ] **MCP list_sheets tool**: enumerate user's library
- [ ] **net-http config view**: URL input, polling interval selector, header editor
- [ ] **net-socket config view**: WebSocket URL input, connection status indicator
- [ ] **net-hook log view**: table of incoming webhook payloads from `net` table
- [ ] **Net sheet creation**: add net-hook/http/socket options to "new sheet" menu

---

## Phase 2 — Growth

Build the features that create network effects. This is where the spreadsheet OS vision comes alive.

- [ ] **Scrapscript WASM runtime**: compile Scrapscript interpreter to WASM for browser execution
- [ ] **Scrapscript query language**: wire up the existing `Scrapscript` lang type (Main.elm:380) to execute Scrapscript
      programs against sheet data
- [ ] **Scrapscript cell formulas**: `=#` prefix in cells triggers Scrapscript evaluation
- [ ] **Content-addressable formulas**: cross-sheet references via Scrapscript hashes instead of fragile @sheet_id
      strings
- [ ] **Scrapscript marketplace**: sell/share self-contained Scrapscript functions as composable sheet utilities
- [ ] **Formula parser**: `=` prefix in cells triggers formula mode using the existing `Formula` lang type
      (Main.elm:379)
- [ ] **Basic arithmetic**: =A1 + B1, =SUM(A1:A10), =AVERAGE, =COUNT, =MIN, =MAX
- [ ] **Cross-sheet references**: =@table:abc123.A1
- [ ] **Dependency tracking**: topological sort for evaluation order, cycle detection
- [ ] **Reactive recalculation**: formulas re-evaluate when referenced cells change
- [ ] **Role column on sheet_usr**: add owner/editor/viewer roles (currently flat access)
- [ ] **Share dialog UI**: email input + role selector in sheet settings
- [ ] **Public/private toggle**: wire up existing Peers = Private | Public type in Elm
- [ ] **Shareable view-only links**: unauthenticated read access via signed URLs
- [ ] **Role-aware sync policy**: extend sharePolicy (main.ts:471) to respect viewer vs editor
- [ ] **Schedule field on sheet**: cron expression or interval for recurring query execution
- [ ] **Server-side query runner**: Deno cron job executing scheduled queries
- [ ] **Pipeline composition**: net-http (extract) -> query (transform) -> table (load)
- [ ] **Execution log**: track pipeline runs, successes, failures per sheet
- [ ] **Google Sheets codex**: OAuth flow for reading Google Sheets as codex data source (main.ts:1683)
- [ ] **Airtable codex**: OAuth flow for reading Airtable bases as codex data source
- [ ] **Notion codex**: OAuth flow for reading Notion databases as codex data source
- [ ] **Bidirectional sync**: write Scrapsheet data back to external sources

---

## Phase 3 — Moonshot

Transform Scrapsheets into a platform. Each feature creates a new axis of composability.

- [ ] **Stable REST endpoint**: GET /api/v1/sheet/:id returns JSON, POST writes rows
- [ ] **API key auth**: per-sheet API keys (not just JWT) for programmatic access
- [ ] **OpenAPI auto-generation**: derive OpenAPI spec from sheet column types
- [ ] **Change webhooks**: notify external URLs when sheet data changes
- [ ] **Rate limits per sheet**: configurable throttling for public sheet APIs
- [ ] **User-defined portal sources**: provide a WebSocket URL or HTTP polling config to create custom portals
- [ ] **Portal marketplace**: sell live data feeds as portal sheets
- [ ] **Portal composition**: query across multiple live portals in real-time
- [ ] **Embed endpoint**: /embed/:id renders minimal-chrome read-only sheet view
- [ ] **Embed code generator**: copy-pasteable iframe snippet in sheet settings
- [ ] **Interactive embeds**: viewers can sort/filter embedded sheets
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

**What ships at 70%**: Phase 0 + Stripe + MCP is enough to launch publicly. Everything else compounds on top.

**The unique position**: Scrapsheets is not Google Sheets. It is not Airtable. It is a programmable data OS where every
table is a queryable database, every query result is a shareable table, every portal is a live data stream, every sheet
is an API, and every formula is a content-addressable program.
