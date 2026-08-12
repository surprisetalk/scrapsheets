# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Scrapsheets is a web-based spreadsheet application that combines traditional spreadsheet functionality with modern web
technologies. It uses a hybrid architecture with:

- **Backend**: Deno-based server using Hono framework (main.ts)
- **Frontend**: Elm single-page application (src/Main.elm)
- **Database**: PostgreSQL with schema defined in db.sql
- **Real-time collaboration**: Automerge CRDT for document synchronization (see https://automerge.org/llms-full.txt)
- **Data storage**: File-based automerge documents in data/automerge/

## Important Files

- `main.ts` - Main server application and API routes
- `src/Main.elm` - Frontend application
- `src/examples.mjs` - Bundled example datasets and queries (imported by both the page and the server seeder)
- `db.sql` - Database schema and initial data
- `examples.sql` - Shop catalogue of query templates (applied by `seed()` on first request)
- `deno.json` - Dependencies and import map
- `src/index.html` - Frontend HTML entry point
- `vendor.ts` - Regenerates the browser-side automerge assets in `src/`
- `data/automerge/` - Document storage directory

## Development Commands

### Building and Running

- **Build frontend**: `deno task build` (copies src/* to dist and runs elm make)
- **Development server**: `deno task dev`
- **Run all tests**: `deno task test` (or `deno test --allow-all`; browser tests build dist themselves)
- **Re-vendor automerge**: `deno task vendor` (after bumping the versions at the top of `vendor.ts`)
- **Elm review**: `deno task review`. Runs clean with zero suppressions; keep it that way.
- **Watch and build**: `watch src { try { cp -vu src/* dist ; elm make src/Main.elm --debug --output=dist/index.js } }`

### Elm Commands (via deno.json imports)

- **Format Elm code**: `deno run -A npm:elm-format`
- **Run Elm tests**: `deno run -A npm:elm-test`
- **Elm review**: `deno task review` (also runs in CI)

### Database Setup

- Initialize database schema: `psql < db.sql`
- Default connection: `postgresql://postgres@127.0.0.1:5434/postgres`
- Tests run against an in-process PGlite over a local pg-gateway on port 5434 (no real Postgres needed)
- External-DB DSNs are encrypted at the application level (AES-GCM via DSN_ENCRYPTION_KEY) before storage

## Architecture Overview

### Backend Architecture (main.ts)

- **Framework**: Hono web framework with JWT middleware
- **Database**: PostgreSQL via postgresjs (pool capped at one connection: the test gateway is a single PGlite session)
- **Real-time sync**: official automerge NodeWSServerAdapter behind a small ws-shim over Hono's upgradeWebSocket;
  per-document access is enforced in the /library/sync message path (`canSync`), not just sharePolicy
- **Authentication**: JWT-based with email verification via Resend (plain fetch, RESEND_API_KEY)
- **Document types**: table, query, net-hook, net-http, net-socket, portal, codex-*
- **Seeding**: a lazy-once middleware runs `seed()` (examples.sql + src/examples.mjs datasets) on the first request;
  idempotent via `on conflict (doc_id) do update`
- **net-http polling**: `pollNetOnce` scans net-http sheets every 15s, fetches due URLs through the safeFetch SSRF
  guard, and appends bodies to the `net` table
- **MCP server**: POST /mcp/:id is a hand-rolled JSON-RPC 2.0 endpoint (initialize, tools/list, tools/call) with tools
  read_sheet, write_cells, query_sheet, list_sheets; :id is the default sheet scope

### Key Backend Features

- **Sheet system**: Polymorphic documents identified by `type:doc_id` format
- **Query engine**: SQL execution via AlaSQL for cross-sheet queries using `@sheet_id` syntax
- **Marketplace**: Buy/sell sheets with pricing system
- **Real-time data**: WebSocket portals for live data (time, stock prices)
- **Database codex**: Connect external PostgreSQL databases

### Frontend Architecture (src/Main.elm)

- **Architecture**: Elm Architecture (Model-Update-View)
- **Document types**: Library, Shop, Tab (table), Query, NetHook, NetHttp, NetSocket. Remaining server types (portal,
  template, codex-_) decode to `Unviewable typ`, which the view reports as an error naming the type and a query that can
  read the sheet. Give a type a real view by replacing its `Unviewable` branch in `docDecoder`.
- **Default library**: client-side (localStorage) system entries merge bundled examples from `src/examples.mjs` (6
  datasets + example queries), 7 live portals, and the tutorial sheet; system ids skip `repo.find` in `changeId`
- **Cross-sheet queries in the browser**: `resolveSheets` rewrites `@type:doc_id` to `SHEET('id')` and pre-loads each
  doc (library entry or `repo.find`) before AlaSQL runs; `@query:` refs recurse (depth cap 2)
- **UI chrome**: keyboard shortcut sheet (Ctrl/⌘+/ or "?"), library sparkline thumbnails (computed JS-side into
  localStorage entries), five-step first-run tutorial (localStorage `scrapsheets-tutorial`, -1 = dismissed)
- **Real-time sync**: Ports for Automerge integration
- **UI**: Table-based interface with cell editing, selection, and statistics
- **Automerge loading**: `src/index.html` maps `@automerge/automerge` to the esm.sh slim build and calls
  `initializeWasm(fetch("/automerge.wasm"))`. The vendored `src/automerge-repo*.mjs` bundles must import automerge by
  bare specifier, or the browser loads a second copy with no WASM. `deno task vendor` enforces this.

### Key Frontend Features

- **Live editing**: In-place cell editing with type-aware rendering
- **Statistics**: Real-time column statistics for Number/Usd (numeric) and Text (descriptive); other column types get no
  stats
- **Query interface**: Embedded SQL editor for query sheets
- **Type system**: Rich type system including USD, links, images, forms

### Database Schema (db.sql)

#### Core Tables

- **usr**: User accounts with identity, name, email (citext), and password
- **sheet**: Central document table with polymorphic sheet_id format (`type:doc_id`)
  - Types: template, table, net-hook, net-http, net-socket, query, portal, codex-*
  - Marketplace fields: sell_id, sell_type, sell_price, buy_id, buy_price
  - Document data: row_0 (jsonb), name, tags (text[])
- **sheet_usr**: Many-to-many permissions between sheets and users
- **db**: External database connections (DSN storage for codex sheets)
- **net**: Webhook data storage for net-* type sheets (body content)

#### Key Schema Features

- **Generated sheet_id**: Computed as `type || ':' || doc_id` (e.g., "table:abc123")
- **Marketplace system**: sell_id generated from md5(doc_id||created_by), prevents selling and buying same sheet
- **Type constraints**: Enforced sheet types with check constraints
- **citext extension**: Case-insensitive email handling
