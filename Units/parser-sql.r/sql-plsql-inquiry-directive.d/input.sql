-- Taken from https://github.com/universal-ctags/ctags/issues/3006
-- submitted by bagl
// Fails to create tags after PLSQL inquiry directive is used
// https://docs.oracle.com/en/database/oracle/oracle-database/18/lnpls/plsql-language-fundamentals.html#GUID-E918087C-D5A8-4CEE-841B-5333DE6D4C15
//
// 'parseDollarQuote' causing troubles?
//
create or replace package body demo_pkg is

test_var varchar2(64) := $$PLSQL_UNIT;

function test_func return varchar2
as
begin
    return test_var;
end;

end demo_pkg;

