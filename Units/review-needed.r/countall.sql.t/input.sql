rem -----------------------------------------------------------------------
rem URL:        http://www.orafaq.com/scripts/plsql/countall.txt
rem Filename:   countall.sql
rem Purpose:    Count the number of rows for ALL tables in current schema
rem             using PL/SQL
rem Date:       15-Apr-2000
rem Author:     Eberhardt, Roberto (Bolton) (reberhar@husky.ca)
rem -----------------------------------------------------------------------

set serveroutput on size 1000000

DECLARE
  t_c1_tname      user_tables.table_name%TYPE;
  t_command       varchar2(200);
  t_cid           integer;
  t_total_records number(10);
  stat            integer;
  row_count       integer;
  t_limit         integer := 0;    -- Only show tables with more rows
  cursor c1 is select table_name from user_tables order by table_name;
BEGIN
  t_limit := 0;
  open c1;
  loop
        fetch c1 into t_c1_tname;
        exit when c1%NOTFOUND;
        t_command := 'SELECT COUNT(0) FROM '||t_c1_tname;
        t_cid := DBMS_SQL.OPEN_CURSOR;
        DBMS_SQL.PARSE(t_cid,t_command,dbms_sql.native);
        DBMS_SQL.DEFINE_COLUMN(t_cid,1,t_total_records);
        stat := DBMS_SQL.EXECUTE(t_cid);
        row_count := DBMS_SQL.FETCH_ROWS(t_cid);
        DBMS_SQL.COLUMN_VALUE(t_cid,1,t_total_records);
        if t_total_records > t_limit then
                DBMS_OUTPUT.PUT_LINE(rpad(t_c1_tname,55,' ')||
                        to_char(t_total_records,'99999999')||' record(s)');

        end if;
        DBMS_SQL.CLOSE_CURSOR(t_cid);
  end loop;
  close c1;
END;
/
