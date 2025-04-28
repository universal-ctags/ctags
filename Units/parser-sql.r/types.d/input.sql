-- Taken from https://www.postgresql.jp/document/17/html/sql-createtype.html

CREATE TYPE compfoo AS (f1 int, f2 text);

CREATE FUNCTION getfoo() RETURNS SETOF compfoo AS $$
    SELECT fooid, fooname FROM foo
$$ LANGUAGE SQL;

CREATE TYPE bug_status AS ENUM ('new', 'open', 'closed');

CREATE TABLE bug (
    id serial,
    description text,
    status bug_status
);

CREATE TYPE float8_range AS RANGE (subtype = float8, subtype_diff = float8mi);

CREATE TYPE box;

CREATE FUNCTION my_box_in_function(cstring) RETURNS box AS $$
...				-- fix me
$$ LANGUAGE SQL;

CREATE FUNCTION my_box_out_function(box) RETURNS cstring AS $$
...				-- fix me
$$ LANGUAGE SQL;

CREATE TYPE box (
    INTERNALLENGTH = 16,
    INPUT = my_box_in_function,
    OUTPUT = my_box_out_function
);

CREATE TABLE myboxes (
    id integer,
    description box
);

CREATE TYPE bigobj (
    INPUT = lo_filein, OUTPUT = lo_fileout,
    INTERNALLENGTH = VARIABLE
);
CREATE TABLE big_objs (
    id integer,
    obj bigobj
);
