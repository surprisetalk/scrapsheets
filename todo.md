<!-- deno-fmt-ignore-file -->

# Scrapsheets Roadmap

> Programmable data OS: every table is a database, every query is a table, every sheet is an API.

---

## Misc

- [ ] https://github.com/bgreenwell/xleak
- [ ] https://github.com/CodeOne45/vex-tui
- [ ] https://github.com/vkobinski/CacTui

---

## Phase 0 — Patch

Close the gaps that prevent daily use and public demo.

- [x] **DocDelta efficiency**: send actual Automerge deltas instead of full doc on every change (Main.elm:194, index.html docChanged handler)
- [x] **Boolean cell editing guard**: don't open text editor on boolean/non-editable cells; toggle checkbox instead (Main.elm:1389)
- [x] **DSN encryption**: encrypt external database connection strings with pgcrypto (db.sql:30)
- [ ] **Automerge WebSocket adapter**: replace custom HonoWebSocketAdapter with official NodeWebSocketAdapter (main.ts:465)
- [x] **Net sheet ID normalization**: accept doc_id instead of sheet_id for /net/:id endpoint (main.ts:1547)
- [ ] **Sheet preview thumbnails**: generate mini sparkline/heatmap SVG for library view (Main.elm:335)
- [x] **URL query operators**: support ?q=+any, ?q=-any, ?q==any for bookmarkable filtered views (Main.elm:902)
- [x] **row_0 validation**: add check constraint ensuring row_0 is a JSONB array (db.sql:24)
- [x] **Net table headers**: store req/res metadata and HTTP headers alongside webhook body (db.sql:40)

---

## Phase 1 — Foundation

Make Scrapsheets reliable enough to be someone's primary data tool.

### Marketplace Payments

- [ ] **Stripe Checkout integration**: create checkout session on /buy/:id with sell_price, redirect to Stripe
- [ ] **Stripe webhooks**: complete purchase on payment confirmation, record transaction
- [ ] **Payment table**: add `payment` table tracking buyer, seller, amount, sheet, timestamp
- [ ] **stripe_customer_id on usr**: link users to Stripe customers for future payouts
- [ ] **Seller payouts**: Stripe Connect for marketplace payouts (can defer, collect platform-side first)

### MCP Server

- [ ] **Implement /mcp/:id endpoint**: replace 501 stub with Model Context Protocol server (main.ts:1824)
- [ ] **read_sheet tool**: return sheet data as structured JSON via MCP
- [ ] **write_cells tool**: update specific cells via MCP
- [ ] **query_sheet tool**: execute SQL/PRQL against sheets via MCP
- [ ] **list_sheets tool**: enumerate user's library via MCP

### Net Sheet UI

- [ ] **net-http config view**: URL input, polling interval selector, header editor
- [ ] **net-socket config view**: WebSocket URL input, connection status indicator
- [ ] **net-hook log view**: table of incoming webhook payloads from `net` table
- [ ] **Net sheet creation**: add net-hook/http/socket options to "new sheet" menu

### Security

- [x] **Codex DSN validation**: block connections resolving to application's own database (main.ts:1567)
- [x] **Codex read-only mode**: SET SESSION CHARACTERISTICS AS TRANSACTION READ ONLY for external DBs
- [x] **Codex query timeout**: add connection and statement timeouts for external DB queries
- [x] **Codex rate limiting**: per-user query rate limits for external database access

---

## Phase 2 — Growth

Build the features that create network effects. This is where the spreadsheet OS vision comes alive.

### Scrapscript Integration

- [ ] **Scrapscript WASM runtime**: compile Scrapscript interpreter to WASM for browser execution
- [ ] **Scrapscript query language**: wire up the existing `Scrapscript` lang type (Main.elm:380) to execute Scrapscript programs against sheet data
- [ ] **Scrapscript cell formulas**: `=#` prefix in cells triggers Scrapscript evaluation
- [ ] **Content-addressable formulas**: cross-sheet references via Scrapscript hashes instead of fragile @sheet_id strings
- [ ] **Scrapscript marketplace**: sell/share self-contained Scrapscript functions as composable sheet utilities

### Cell Formulas

- [ ] **Formula parser**: `=` prefix in cells triggers formula mode using the existing `Formula` lang type (Main.elm:379)
- [ ] **Basic arithmetic**: =A1 + B1, =SUM(A1:A10), =AVERAGE, =COUNT, =MIN, =MAX
- [ ] **Cross-sheet references**: =@table:abc123.A1
- [ ] **Dependency tracking**: topological sort for evaluation order, cycle detection
- [ ] **Reactive recalculation**: formulas re-evaluate when referenced cells change

### Sharing & Permissions

- [ ] **Role column on sheet_usr**: add owner/editor/viewer roles (currently flat access)
- [ ] **Share dialog UI**: email input + role selector in sheet settings
- [ ] **Public/private toggle**: wire up existing Peers = Private | Public type in Elm
- [ ] **Shareable view-only links**: unauthenticated read access via signed URLs
- [ ] **Role-aware sync policy**: extend sharePolicy (main.ts:471) to respect viewer vs editor

### Data Pipelines

- [ ] **Schedule field on sheet**: cron expression or interval for recurring query execution
- [ ] **Server-side query runner**: Deno cron job executing scheduled queries
- [ ] **Pipeline composition**: net-http (extract) -> query (transform) -> table (load)
- [ ] **Execution log**: track pipeline runs, successes, failures per sheet

### External Integrations

- [ ] **Google Sheets codex**: OAuth flow for reading Google Sheets as codex data source (main.ts:1683)
- [ ] **Airtable codex**: OAuth flow for reading Airtable bases as codex data source
- [ ] **Notion codex**: OAuth flow for reading Notion databases as codex data source
- [ ] **Bidirectional sync**: write Scrapsheet data back to external sources

---

## Phase 3 — Moonshot

Transform Scrapsheets into a platform. Each feature creates a new axis of composability.

### Sheet-as-API

- [ ] **Stable REST endpoint**: GET /api/v1/sheet/:id returns JSON, POST writes rows
- [ ] **API key auth**: per-sheet API keys (not just JWT) for programmatic access
- [ ] **OpenAPI auto-generation**: derive OpenAPI spec from sheet column types
- [ ] **Change webhooks**: notify external URLs when sheet data changes
- [ ] **Rate limits per sheet**: configurable throttling for public sheet APIs

### Custom Portals

- [ ] **User-defined portal sources**: provide a WebSocket URL or HTTP polling config to create custom portals
- [ ] **Portal marketplace**: sell live data feeds as portal sheets
- [ ] **Portal composition**: query across multiple live portals in real-time

### Embeddable Sheets

- [ ] **Embed endpoint**: /embed/:id renders minimal-chrome read-only sheet view
- [ ] **Embed code generator**: copy-pasteable iframe snippet in sheet settings
- [ ] **Interactive embeds**: viewers can sort/filter embedded sheets
- [ ] **Live dashboard mode**: combine portal data + embeds for real-time dashboards

### Collaboration

- [ ] **Cursor presence**: show other users' cursor positions with colored indicators
- [ ] **Active user list**: display connected collaborators in sheet header
- [ ] **Cell comments**: annotation system with threads on individual cells
- [ ] **Presence indicators in library**: show which sheets have active editors

### Version History

- [ ] **Timeline view**: slider showing document state over time (Automerge stores full history)
- [ ] **Diff view**: visual diff between any two versions
- [ ] **Named snapshots**: user-created bookmarks in the version timeline
- [ ] **Rollback**: restore sheet to any historical state

### Offline & Mobile

- [ ] **PWA manifest**: installable progressive web app with offline support
- [ ] **IndexedDB-first sync**: Automerge already uses IndexedDB; optimize for offline editing
- [ ] **Responsive mobile layout**: touch-friendly cell editing, swipe navigation
- [ ] **Conflict resolution UI**: surface Automerge merge conflicts for user review

---

## Strategic Notes

**The flywheel**: marketplace payments (1) attract template creators -> MCP server (1) makes sheets AI-accessible -> Scrapscript (2) is the moat no one can replicate -> pipelines (2) make sheets self-updating -> sheet-as-API (3) makes every sheet a microservice.

**What ships at 70%**: Phase 0 + Stripe + MCP is enough to launch publicly. Everything else compounds on top.

**The unique position**: Scrapsheets is not Google Sheets. It is not Airtable. It is a programmable data OS where every table is a queryable database, every query result is a shareable table, every portal is a live data stream, every sheet is an API, and every formula is a content-addressable program.
