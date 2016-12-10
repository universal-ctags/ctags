rem -----------------------------------------------------------------------
rem URL:        http://www.orafaq.com/scripts/plsql/readlong.txt
rem Filename:   readlong.sql
rem Purpose:    Fetch Long column values piece-wise from PL/SQL
rem Date:       12-Jan-1999
rem Author:     Frank Naude (frank@ibi.co.za)
rem -----------------------------------------------------------------------

set serveroutput on

-- Create test table
drop table longtable;
create table longtable (longcol long) tablespace TOOLS;
insert into longtable values ( rpad('x', 257, 'QWERTY') );

DECLARE
  cur1       PLS_INTEGER         := DBMS_SQL.OPEN_CURSOR;;
  rc         NUMBER;
  long_piece VARCHAR2(256);
  piece_len  INTEGER             := 0;
  long_tab   DBMS_SQL.VARCHAR2S;
  long_len   INTEGER             := 0;
BEGIN
  DBMS_SQL.PARSE(cur1, 'select longcol from longtable', DBMS_SQL.NATIVE);
  DBMS_SQL.DEFINE_COLUMN_LONG(cur1, 1);
  rc := DBMS_SQL.EXECUTE(cur1);
  rc := DBMS_SQL.FETCH_ROWS(cur1);                       -- Get one row

  -- Loop until all pieces of the long column are processed
  LOOP
     DBMS_SQL.COLUMN_VALUE_LONG(cur1, 1, 256, long_len, long_piece, piece_len);
     EXIT WHEN piece_len = 0;
     DBMS_OUTPUT.PUT_LINE('Long piece len='|| piece_len);

     long_tab( NVL(long_tab.LAST, 0)+1 ) := long_piece;  -- Add piece to table
     long_len := long_len + piece_len;
  END LOOP;
  DBMS_SQL.CLOSE_CURSOR(cur1);
  DBMS_OUTPUT.PUT_LINE('Total long col fetched, len='|| long_len);
END;
/
