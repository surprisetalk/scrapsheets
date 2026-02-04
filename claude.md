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
- `db.sql` - Database schema and initial data
- `deno.json` - Dependencies and import map
- `src/index.html` - Frontend HTML entry point
- `data/automerge/` - Document storage directory

## Development Commands

### Building and Running

- **Development server**: `deno run -A npm:serve dist -s -C -S -n`
- **Build frontend**: `elm make src/Main.elm --debug --output=dist/index.js`
- **Watch and build**: `watch src { try { cp -vu src/* dist ; elm make src/Main.elm --debug --output=dist/index.js } }`
- **Setup dist directory**: `mkdir dist`

### Elm Commands (via deno.json imports)

- **Format Elm code**: `deno run -A npm:elm-format`
- **Run Elm tests**: `deno run -A npm:elm-test`
- **Elm review**: `deno run -A npm:elm-review`

### Database Setup

- Initialize database schema: `psql < db.sql`
- Default connection: `postgresql://postgres@127.0.0.1:5434/postgres`

## Architecture Overview

### Backend Architecture (main.ts)

- **Framework**: Hono web framework with JWT middleware
- **Database**: PostgreSQL via postgresjs
- **Real-time sync**: Custom WebSocket adapter for Automerge
- **Authentication**: JWT-based with email verification via SendGrid
- **Document types**: table, query, net-hook, net-http, net-socket, portal, codex-*

### Key Backend Features

- **Sheet system**: Polymorphic documents identified by `type:doc_id` format
- **Query engine**: SQL execution via AlaSQL for cross-sheet queries using `@sheet_id` syntax
- **Marketplace**: Buy/sell sheets with pricing system
- **Real-time data**: WebSocket portals for live data (time, stock prices)
- **Database codex**: Connect external PostgreSQL databases

### Frontend Architecture (src/Main.elm)

- **Architecture**: Elm Architecture (Model-Update-View)
- **Document types**: Library, Shop, Tab (table), Net, Query, Codex, Portal
- **Real-time sync**: Ports for Automerge integration
- **UI**: Table-based interface with cell editing, selection, and statistics

### Key Frontend Features

- **Live editing**: In-place cell editing with type-aware rendering
- **Statistics**: Real-time column statistics (numeric, text, enumeration)
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
