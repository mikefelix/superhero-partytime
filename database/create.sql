--drop table quest_items; drop table quest_powers; drop table player_quests; drop table player_powers; drop table items; drop table powers; drop table players; drop table quests; drop table games;

create table games (
  id serial primary key,
  name varchar(128) not null,
  started boolean not null default false,
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
  updated timestamptz not null default now(),
  score integer not null default 0,
  unique (game, alias)
);

insert into players(game, alias, name) values (1, 'Mr. Buttkix', 'Brandon');
insert into players(game, alias, name) values (1, 'Ms. Firelips', 'Kimberly');

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
insert into items(game, name, owner, description) values (1, 'Quantum shovel', 1, 'A shovel');
insert into items(game, name, owner, description) values (1, 'Blue lizard', 2, 'A lizard');

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
  side boolean not null default false,
  completed boolean not null default false,
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
insert into player_powers (power, player) values (2, 2);

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

create table trades (
  id serial primary key,
  game integer not null references games("id"),
  offerer integer not null references players("id"),
  offeree integer not null references players("id"),
  offerer_item integer references items("id"),
  offeree_item integer references items("id"),
  offerer_other varchar(300),
  offeree_other varchar(300),
  stage integer not null default(1)
);

create table chats (
  id        SERIAL PRIMARY KEY,
  game      INTEGER     NOT NULL REFERENCES games ("id"),
  poster    INTEGER REFERENCES players ("id"),
  recipient INTEGER REFERENCES players ("id"),
  chat      TEXT,
  created   TIMESTAMPTZ NOT NULL DEFAULT now()
);