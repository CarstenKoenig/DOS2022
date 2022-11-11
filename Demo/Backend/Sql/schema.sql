CREATE TABLE IF NOT EXISTS tasks
( id          INTEGER PRIMARY KEY ASC
, description TEXT NOT NULL
, completed   INTEGER NOT NULL
, created_at  INTEGER
);