create extension if not exists citext;

create table usr
( usr_id bigint generated always as identity primary key
, created_at timestamp default now()
, name text
, email citext unique not null
, password text
);

create table sheet
( sheet_id text primary key generated always as (type||':'||doc_id) stored
, created_at timestamp default now()
, created_by bigint not null references usr(usr_id)
, type text not null check (type in ('template','page','portal','net','query'))
, doc_id text not null unique
, name text
, tags text[]
, data jsonb check (data <> '{}'::jsonb)
, price numeric
);

create table sheet_usr
( sheet_id text not null references sheet(sheet_id)
, usr_id bigint not null references usr(usr_id)
, created_at timestamp default now()
, primary key (sheet_id, usr_id)
);

create table db
( db_id bigint generated always as identity primary key
, usr_id bigint not null references usr(usr_id)
, created_at timestamp default now()
, name text not null check (name <> '')
, url text not null
, unique (usr_id, name)
);

-- TODO: Add req/res data, headers, etc.
create table net
( sheet_id text not null references sheet(sheet_id) check (sheet_id ilike 'net:%')
, created_at timestamp default now()
, body text not null
);
