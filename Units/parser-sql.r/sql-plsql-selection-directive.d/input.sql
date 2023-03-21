create or replace package body demo_pkg is

function test_func1 return INTEGER
as
begin
    return 1;
end;

$IF $$MY_VERSION_CODE > 3 $THEN

function test_func2 return INTEGER
as
begin
    return 3;
end;

$ELSIF $$MY_VERSION_CODE > 5 $THEN

function test_func3 return INTEGER
as
begin
    return 5;
end;

$ELSE

function test_func4 return INTEGER
as
begin
    return 7;
end;

$END

function test_func5 return INTEGER
as
begin
    return 9;
end;

end demo_pkg;

