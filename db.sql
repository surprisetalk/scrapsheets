create extension if not exists citext;

create table usr
( usr_id bigint generated always as identity primary key
, created_at timestamp default now()
, name text
, email citext unique not null
, password text
);

create table sheet
( sheet_id text not null primary key
, created_at timestamp default now()
, created_by bigint not null references usr(usr_id)
, supertype text not null generated always as (case when type in ('codex','portal') then 'remote' when type in ('template','page','net','query') then 'local' end)
, type text not null check (type in ('template','page','net','query','portal','codex'))
, sell_id text not null unique
, sell_type text not null check (case when type in ('codex','portal') then sell_type is null when type = 'template' then sell_type in ('template','page','query','net') when type in ('page','query','net') then sell_type = 'portal' end)
, buy_id text references sheet(buy_id) check (not (buy_id is null and type in ('codex','portal')))
, name text not null default ''
, tags text[] not null default '{}'::text[]
, params jsonb[] not null default '{}'::jsonb[]
, args jsonb not null default '{}'::jsonb
, price numeric check (not (price is not null and sell_type is null) and not (price is null and buy_id is not null))
);

create table sheet_usr
( sheet_id text not null references sheet(sheet_id)
, usr_id bigint not null references usr(usr_id)
, created_at timestamp default now()
, primary key (sheet_id, usr_id)
);

create view shop as
select todo
from sheet s
where price >= 0;

-- TODO: Add req/res data, headers, etc.
create table net
( sheet_id text not null references sheet(sheet_id) check (sheet_id ilike 'net:%')
, created_at timestamp default now()
, body text not null
);
