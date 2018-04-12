--
-- Taken from #1658 submitted by @karlb
--
CREATE TABLE foo(
    col1 text
);

CREATE TABLE bar AS
SELECT 1 AS col2;

--
-- @karlb shows more complicated ones in #1733
--

-- has columns foo, bar
CREATE TABLE table_with_subselect AS
SELECT foo,
    2 AS bar
FROM (
        SELECT 1 AS foo,
               2 AS not_a_column
    ) subsel;

-- has columns foo, bar, b_bar
CREATE TABLE table_with_join AS
SELECT foo,
    a.bar,
    b.bar AS b_bar
FROM table_with_subselect a
    JOIN table_with_subselect b USING (foo);

-- has columns col1, col3, col4
CREATE TABLE table_with_weird_expressions AS
SELECT 'string AS not_a_colname' AS col1,
    'unknown colname; depends on db',
    'string that goes over '
    'two lines' AS col3,
    1 + (SELECT 1 AS inner_colname) AS col4
FROM (SELECT 1 AS foo) AS not_a_column;

-- has columns foo, bar
CREATE TABLE table_with_subselect2 AS
SELECT foo,
    2 AS bar
FROM (
        SELECT 1 AS foo
    ) subsel;

-- has columns nothing
CREATE TABLE tmp_table0 AS
SELECT tmp5 + tmp6 FROM tmp_table1;
