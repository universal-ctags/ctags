-- Derrived from from https://github.com/universal-ctags/ctags/issues/3006
-- submitted by bagl
create or replace package body demo_pkg1 is

test_var1 varchar2(64) := $$PLSQL_UNIT_OWNER;

function test_func1 return varchar2
as
begin
    return test_var1;
end;

end demo_pkg1;

