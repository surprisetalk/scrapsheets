create extension if not exists citext;

create table usr
( usr_id bigint not null generated always as identity primary key
, created_at timestamp default now()
, name text
, email citext unique not null
, password text
);

create table sheet
( sheet_id text not null primary key generated always as (type || ':' || doc_id) stored
, created_at timestamp default now()
, created_by bigint not null references usr(usr_id)
, type text not null check (type in ('template','table','net-hook','net-http','net-socket','query','portal') or type like 'codex-%')
, doc_id text not null unique
, name text not null default ''
, tags text[] not null default '{}'::text[]
, sell_id text not null unique generated always as (md5(doc_id||created_by::text)) stored
, sell_type text generated always as (case when type = 'template' then row_0->>'type' when type in ('table','net-hook','net-http','net-socket','query') then 'portal' end) stored
, sell_price numeric check (sell_price >= 0)
, buy_id text references sheet(sell_id)
, buy_price numeric check (buy_price >= 0)
, row_0 jsonb not null default '[]'::jsonb -- TODO: check is array
, check (not (sell_price is not null and buy_price is not null))
);

create table db
( sheet_id text not null primary key references sheet (sheet_id)
, dsn text not null -- TODO: Encrypt this.
);

create table sheet_usr
( sheet_id text not null references sheet(sheet_id)
, usr_id bigint not null references usr(usr_id)
, created_at timestamp default now()
, primary key (sheet_id, usr_id)
);

-- TODO: Add req/res data, headers, etc.
create table net
( sheet_id text not null references sheet(sheet_id) check (sheet_id ilike 'net-%')
, created_at timestamp default now()
, body text not null
);
