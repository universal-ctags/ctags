-- Taken from https://dev.mysql.com/doc/refman/5.7/en/create-table-select.html

-- bar, m, n
CREATE TABLE bar (m INT) SELECT n FROM foo;

-- bar, n
CREATE TABLE bar (UNIQUE (n)) SELECT n FROM foo;

-- artists_and_works, name, number_of_works,
CREATE TABLE artists_and_works
  SELECT artist.name, COUNT(work.artist_id) AS number_of_works
  FROM artist LEFT JOIN work ON artist.id = work.artist_id
  GROUP BY artist.id;

-- foo, a, a
CREATE TABLE foo (a TINYINT NOT NULL) SELECT b+1 AS a FROM bar;
