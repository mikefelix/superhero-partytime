# Trades schema

# --- !Ups

insert into games (
  name
)
values (
  'Test game'
);

# --- !Downs

delete from games;