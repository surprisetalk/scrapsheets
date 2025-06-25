create extension if not exists citext;
create extension if not exists pgcrypto;

create or replace function email_token(ts timestamp, email text) returns text language sql immutable as $$
  select extract(epoch from ts)::text || ':' || encode(digest(extract(epoch from ts)::text || ':' || email || ':' || current_setting('app.secret', true), 'sha256'), 'hex')
$$;

create table usr
( uid bigint generated always as identity primary key
, name text not null
, email citext unique not null
, password text
, created_at timestamp default now()
);

create table sheet
( sheet_id bigint generated always as identity primary key
, created_by bigint not null references usr(uid)
, tags text[]
, created_at timestamp default now()
);

create table sheet_usr
( sheet_id bigint references sheet(sheet_id)
, usr_id bigint references usr(uid)
, is_writer boolean not null
, created_at timestamp default now()
, primary key (sheet_id, usr_id)
);
