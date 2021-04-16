-- Taken from github #2958 submitted by @LawrenceJGD
CREATE TABLE IF NOT EXISTS test (
    id     INTEGER NOT NULL PRIMARY KEY,
    mytext TEXT    NOT NULL
);

CREATE TABLE another_test (
    id    INTEGER NOT NULL PRIMARY KEY,
    mynum INTEGER
);
