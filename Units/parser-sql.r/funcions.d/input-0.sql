-- Derived from postgresql-13.16/src/test/regress/sql/polymorphism.sql

create function dfunc0(a text DEFAULT '->''Hello', b text DEFAULT 'World''') returns text as $$
  select $1 || ', ' || $2;
$$ language sql;
drop function dfunc0(text, text);

create function dfunc1(r numeric = 20.39, p numeric DEFAULT 99.88) returns numeric as $$
  select $1 + $2;
$$ language sql;
drop function dfunc1(numeric, numeric);

create function dfunc2(anyelement = 'World'::text) returns text as $$
  select 'Hello, ' || $1::text;
$$ language sql;
drop function dfunc2(anyelement);

create function dfunc3(a variadic int[]) returns int as
$$ select array_upper($1, 1) $$ language sql;
