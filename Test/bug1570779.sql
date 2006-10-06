-- This should find the "address" column when --sql-types=r is used
-- ctags -f - --format=2 --excmd=pattern --fields=nks  --sort=no  --sql-types=cdfFlLPprstTvieURDVnxy bug1570779.sql
-- employees       bug1570779.sql  /^CREATE TABLE employees ($/;"  t       line:2
-- employees.id    bug1570779.sql  /^    id integer NOT NULL,$/;"  F       line:3
-- employees.name  bug1570779.sql  /^    name varchar(20),$/;"     F       line:4
-- employees.address       bug1570779.sql  /^    address varchar(50),$/;"  F   line:5

CREATE TABLE employees (
    id integer NOT NULL,
    name varchar(20),
    address varchar(50),
    primary key (id)
);

