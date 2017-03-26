--drop table quest_items; drop table quest_powers; drop table player_quests; drop table player_powers; drop table items; drop table powers; drop table players; drop table quests; drop table games;

create table games (
  id serial primary key,
  name varchar(128) not null,
  created timestamptz not null default now(),
  updated timestamptz not null default now()
);

insert into games(name) values ('Cool game');

create table quests (
  id serial primary key,
  game integer not null references games("id"),
  name varchar(128) not null,
  description text not null default '',
  created timestamptz not null default now(),
  updated timestamptz not null default now()
);

insert into quests(game, name, description) values (1, 'Petition the goddess', 'Petition her!');
insert into quests(game, name, description) values (1, 'Find the lost city of Fatlantis', 'It''s somewhere.');

create table players (
  id serial primary key,
  game integer not null references games("id"),
  name varchar(128) not null,
  alias varchar(128) not null,
  created timestamptz not null default now(),
  updated timestamptz not null default now()
);

insert into players(game, alias, name) values (1, 'Brandon', 'Mr. Buttkix');
insert into players(game, alias, name) values (1, 'Kimberly', 'Ms. Firelips');

create table items (
  id serial primary key,
  game integer not null references games("id"),
  name varchar(128) not null,
  owner integer references players("id"),
  description text not null default '',
  created timestamptz not null default now(),
  updated timestamptz not null default now()
);

insert into items(game, name, description) values (1, 'Lost fragment', 'A fragment without an owner');
insert into items(game, name, owner, description) values (1, 'Found fragment', 1, 'A fragment with an owner');

create table powers (
  id serial primary key,
  game integer not null references games("id"),
  name varchar(128) not null,
  description text not null default '',
  created timestamptz not null default now(),
  updated timestamptz not null default now()
);

insert into powers(game, name, description) values (1, 'Explode trees', 'The ability to explode trees with your mind. This is actually more impressive than it sounds.');
insert into powers(game, name, description) values (1, 'Speak with dirt', 'The ability to talk to dirt. This doesn''t mean it talks back.');

create table player_quests (
  quest integer not null references quests("id"),
  player integer not null references players("id"),
  side boolean not null,
  primary key (quest, player, side)
);

insert into player_quests (quest, player, side) values (1, 1, false);
insert into player_quests (quest, player, side) values (2, 2, false);

insert into player_quests (quest, player, side) values (2, 1, true);
insert into player_quests (quest, player, side) values (1, 2, true);

create table player_powers (
  power integer not null references powers("id"),
  player integer not null references players("id"),
  primary key (power, player)
);

insert into player_powers (power, player) values (1, 1);

create table quest_items (
  quest integer not null references quests("id"),
  item integer not null references items("id"),
  primary key (quest, item)
);

insert into quest_items (quest, item) values (1, 1);

create table quest_powers (
  quest integer not null references quests("id"),
  power integer not null references powers("id"),
  primary key (quest, power)
);

insert into quest_powers (quest, power) values (1, 1);


