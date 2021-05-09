ALTER SESSION SET
PLSQL_CCFlags = 'Some_Flag:1, PLSQL_CCFlags:99'
/
BEGIN
  DBMS_OUTPUT.PUT_LINE($$Some_Flag);
END;
/

CREATE TABLE foo(
    col text
);
