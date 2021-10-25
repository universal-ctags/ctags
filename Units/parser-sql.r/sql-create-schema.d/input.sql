-- https://www.postgresql.org/docs/current/sql-createschema.html
CREATE SCHEMA myschema;
CREATE SCHEMA AUTHORIZATION joe;
CREATE SCHEMA IF NOT EXISTS test AUTHORIZATION joe;
CREATE SCHEMA hollywood
    CREATE TABLE films (title text, release date, awards text[])
    CREATE VIEW winners AS
        SELECT title, release FROM films WHERE awards IS NOT NULL;

-- TODO: films and winners should be tagged.
