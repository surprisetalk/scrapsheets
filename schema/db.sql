create extension if not exists citext;

create table usr
( usr_id bigint not null generated always as identity primary key
, created_at timestamp default now()
, name text
, email citext unique not null
, password text
, stripe_customer_id text unique
, digest_at timestamp
);

create table sheet
( sheet_id text not null primary key generated always as (type || ':' || doc_id) stored
, created_at timestamp default now()
, created_by bigint not null references usr(usr_id)
, type text not null check (type in ('template','table','net-hook','net-http','net-socket','query','portal','alert','chart','dashboard') or type like 'codex-%')
, doc_id text not null unique
, name text not null default ''
, tags text[] not null default '{}'::text[]
, sell_id text not null unique generated always as (md5(doc_id||created_by::text)) stored
, sell_type text generated always as (case when type = 'template' then row_0->>'type' when type in ('table','net-hook','net-http','net-socket','query') then 'portal' end) stored
, sell_price numeric check (sell_price >= 0)
, buy_id text references sheet(sell_id)
, buy_price numeric check (buy_price >= 0)
, row_0 jsonb not null default '[]'::jsonb check (jsonb_typeof(row_0) in ('array','object'))
, public boolean not null default false
, check (not (sell_price is not null and buy_price is not null))
);

create table db
( sheet_id text not null primary key references sheet (sheet_id)
, dsn text not null
);

-- A sheet's own secrets, encrypted with the same AES-GCM key the codex DSNs
-- use. Never in the automerge document and never in a cell: the document is
-- what sync hands a viewer, so a net-http auth header living there is a token
-- a share link can be pointed at.
--
-- There is no unique key on (sheet_id, name) on purpose. The newest row for a
-- name is the current secret and the second newest is the previous one, which
-- is what lets a sender roll over without a missed delivery: writing a secret
-- IS rotating it, and both are tried in that order. Older rows are trimmed
-- behind the write, the way net is.
create table secret
( secret_id bigint not null generated always as identity primary key
, sheet_id text not null references sheet(sheet_id)
, name text not null
, value_encrypted text not null
, created_at timestamp default now()
);

create index secret_sheet_id_name_created_at_idx on secret (sheet_id, name, created_at desc);

create table sheet_usr
( sheet_id text not null references sheet(sheet_id)
, usr_id bigint not null references usr(usr_id)
, created_at timestamp default now()
, role text not null default 'editor' check (role in ('owner','editor','viewer'))
, primary key (sheet_id, usr_id)
);

create table net
( net_id bigint not null generated always as identity primary key
, sheet_id text not null references sheet(sheet_id) check (sheet_id ilike 'net-%' or sheet_id ilike 'alert:%')
, created_at timestamp default now()
, method text not null default 'POST'
, req_headers jsonb not null default '{}'::jsonb
, query_params jsonb not null default '{}'::jsonb
, meta jsonb not null default '{}'::jsonb
, body text not null
);

create index net_sheet_id_created_at_idx on net (sheet_id, created_at desc);

-- One delivery per signature per sheet. POST /net/:id used to decide this by
-- selecting first and inserting after, so ten parallel copies of one captured
-- delivery all saw no prior row and all landed -- the control bypassed by the
-- least sophisticated version of the attack it exists for. A unique index is
-- the only thing that decides it under concurrency. The expression is null on
-- every row that is not a signed delivery (a poll, an alert run, an error), and
-- nulls do not collide, so nothing else in this table is constrained.
--
-- The key is the signature that actually verified, which POST /net/:id writes to
-- meta.sig. Keying on a header name instead cannot work once a sheet may be
-- signed by a provider: a coalesce over the four header names picks by a fixed
-- order and not by which one was checked, so a captured Stripe delivery replayed
-- with a junk scrapsheets-signature beside it took a different key every time and
-- landed every time. Only the verifier knows which value it trusted.
--
-- Rows written before this index existed carry no meta.sig, and nulls do not
-- collide, so a delivery captured before it shipped is unconstrained here -- by
-- which point the skew check has refused it for HOOK_SKEW seconds anyway.
create unique index net_hook_signature_idx on net (sheet_id, (meta->>'sig'));

create table payment
( payment_id bigint not null generated always as identity primary key
, created_at timestamp default now()
, buyer_id bigint not null references usr(usr_id)
, seller_id bigint not null references usr(usr_id)
, sell_id text not null references sheet(sell_id)
, sheet_id text references sheet(sheet_id)
, amount numeric not null check (amount >= 0)
, stripe_session_id text unique
, stripe_payment_intent_id text
, check (stripe_session_id is not null or sheet_id is not null)
);
