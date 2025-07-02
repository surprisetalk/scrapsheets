create extension if not exists citext;

create table usr
( usr_id bigint generated always as identity primary key
, name text
, email citext unique not null
, password text
, created_at timestamp default now()
);

create table sheet
( sheet_id text primary key generated always as (type||':'||doc_id) stored
, created_by bigint not null references usr(usr_id)
, type text not null check (type in ('template','page','portal','agent','query'))
, doc_id text not null unique
, portal_id text not null unique generated always as (md5(doc_id)) stored
, name text
, tags text[]
, created_at timestamp default now()
);

create table sheet_usr
( sheet_id text not null references sheet(sheet_id)
, usr_id bigint not null references usr(usr_id)
, created_at timestamp default now()
, primary key (sheet_id, usr_id)
);
