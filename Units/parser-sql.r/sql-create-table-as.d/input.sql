--
-- Taken from #1658 submitted by @karlb
--
CREATE TABLE foo(
    col1 text
);

CREATE TABLE bar AS
SELECT 1 AS col2;
