
do $$
begin
    if not exists (select 1 from pg_type where typname = 'alpha_numeric') then
      create domain alpha_numeric as varchar(128) check (value ~ '^[A-z0-9\-]+$');
    end if;
end$$;


create table quests (
  id uuid not null primary key,
  game uuid not null,
  name alpha_numeric not null,
  description text not null default '',
  created timestamptz not null default now(),
  updated timestamptz not null default now()
);

create table players (
  id uuid not null primary key,
  name alpha_numeric not null,
  game uuid not null,
  alias alpha_numeric not null,
  created timestamptz not null default now(),
  updated timestamptz not null default now()
);

create table game (
  id uuid not null primary key,
  name alpha_numeric not null,
  game uuid not null,
  created timestamptz not null default now(),
  updated timestamptz not null default now()
);

create table player_quests (
  quest uuid not null primary key,
  player uuid not null primary key
)