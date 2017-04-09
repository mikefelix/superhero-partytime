--drop table quest_items; drop table quest_powers; drop table player_quests; drop table player_powers; drop table items; drop table powers; drop table players; drop table quests; drop table games;

create table games (
  id      SERIAL PRIMARY KEY,
  name    VARCHAR(128) NOT NULL,
  started BOOLEAN      NOT NULL DEFAULT FALSE,
  created TIMESTAMPTZ  NOT NULL DEFAULT now(),
  updated TIMESTAMPTZ  NOT NULL DEFAULT now()
);

-- insert into games(name) values ('Cool game');

create table quests (
  id          SERIAL PRIMARY KEY,
  game        INTEGER      NOT NULL REFERENCES games ("id"),
  name        VARCHAR(128) NOT NULL,
  description TEXT         NOT NULL DEFAULT '',
  created     TIMESTAMPTZ  NOT NULL DEFAULT now(),
  updated     TIMESTAMPTZ  NOT NULL DEFAULT now()
);

-- insert into quests(game, name, description) values (1, 'Petition the goddess', 'Petition her!');
-- insert into quests(game, name, description) values (1, 'Find the lost city of Fatlantis', 'It''s somewhere.');

create table players (
  id      SERIAL PRIMARY KEY,
  game    INTEGER      NOT NULL REFERENCES games ("id"),
  name    VARCHAR(128) NOT NULL,
  alias   VARCHAR(128) NOT NULL,
  created TIMESTAMPTZ  NOT NULL DEFAULT now(),
  updated TIMESTAMPTZ  NOT NULL DEFAULT now(),
  score   INTEGER      NOT NULL DEFAULT 0,
  UNIQUE (game, alias)
);

-- insert into players(game, alias, name) values (1, 'Mr. Buttkix', 'Brandon');
-- insert into players(game, alias, name) values (1, 'Ms. Firelips', 'Kimberly');

create table items (
  id          SERIAL PRIMARY KEY,
  game        INTEGER      NOT NULL REFERENCES games ("id"),
  name        VARCHAR(128) NOT NULL,
  owner       INTEGER REFERENCES players ("id"),
  description TEXT         NOT NULL DEFAULT '',
  created     TIMESTAMPTZ  NOT NULL DEFAULT now(),
  updated     TIMESTAMPTZ  NOT NULL DEFAULT now()
);

-- insert into items(game, name, description) values (1, 'Lost fragment', 'A fragment without an owner');
-- insert into items(game, name, owner, description) values (1, 'Found fragment', 1, 'A fragment with an owner');
-- insert into items(game, name, owner, description) values (1, 'Quantum shovel', 1, 'A shovel');
-- insert into items(game, name, owner, description) values (1, 'Blue lizard', 2, 'A lizard');

create table powers (
  id          SERIAL PRIMARY KEY,
  game        INTEGER      NOT NULL REFERENCES games ("id"),
  name        VARCHAR(128) NOT NULL,
  description TEXT         NOT NULL DEFAULT '',
  created     TIMESTAMPTZ  NOT NULL DEFAULT now(),
  updated     TIMESTAMPTZ  NOT NULL DEFAULT now()
);

-- insert into powers(game, name, description) values (1, 'Explode trees', 'The ability to explode trees with your mind. This is actually more impressive than it sounds.');
-- insert into powers(game, name, description) values (1, 'Speak with dirt', 'The ability to talk to dirt. This doesn''t mean it talks back.');

create table player_quests (
  quest     INTEGER NOT NULL REFERENCES quests ("id"),
  player    INTEGER NOT NULL REFERENCES players ("id"),
  side      BOOLEAN NOT NULL DEFAULT FALSE,
  completed BOOLEAN NOT NULL DEFAULT FALSE,
  PRIMARY KEY (quest, player, side)
);

-- insert into player_quests (quest, player, side) values (1, 1, false);
-- insert into player_quests (quest, player, side) values (2, 2, false);

-- insert into player_quests (quest, player, side) values (2, 1, true);
-- insert into player_quests (quest, player, side) values (1, 2, true);

create table player_powers (
  power  INTEGER NOT NULL REFERENCES powers ("id"),
  player INTEGER NOT NULL REFERENCES players ("id"),
  PRIMARY KEY (power, player)
);

-- insert into player_powers (power, player) values (1, 1);
-- insert into player_powers (power, player) values (2, 2);

create table quest_items (
  quest INTEGER NOT NULL REFERENCES quests ("id"),
  item  INTEGER NOT NULL REFERENCES items ("id"),
  PRIMARY KEY (quest, item)
);

-- insert into quest_items (quest, item) values (1, 1);

create table quest_powers (
  quest INTEGER NOT NULL REFERENCES quests ("id"),
  power INTEGER NOT NULL REFERENCES powers ("id"),
  PRIMARY KEY (quest, power)
);

-- insert into quest_powers (quest, power) values (1, 1);

create table trades (
  id            SERIAL PRIMARY KEY,
  game          INTEGER NOT NULL REFERENCES games ("id"),
  offerer       INTEGER NOT NULL REFERENCES players ("id"),
  offeree       INTEGER NOT NULL REFERENCES players ("id"),
  offerer_item  INTEGER REFERENCES items ("id"),
  offeree_item  INTEGER REFERENCES items ("id"),
  offerer_other VARCHAR(300),
  offeree_other VARCHAR(300),
  stage         INTEGER NOT NULL DEFAULT (1)
);

create table invites (
  id      SERIAL PRIMARY KEY,
  game    INTEGER NOT NULL REFERENCES games ("id"),
  inviter INTEGER NOT NULL REFERENCES players ("id"),
  invitee INTEGER NOT NULL REFERENCES players ("id"),
  quest   INTEGER,
  stage   INTEGER NOT NULL DEFAULT (1)
);

create table chats (
  id        SERIAL PRIMARY KEY,
  game      INTEGER     NOT NULL REFERENCES games ("id"),
  poster    INTEGER REFERENCES players ("id"),
  recipient INTEGER REFERENCES players ("id"),
  chat      TEXT,
  created   TIMESTAMPTZ NOT NULL DEFAULT now()
);