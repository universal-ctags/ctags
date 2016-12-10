rem -----------------------------------------------------------------------
rem URL:        http://www.orafaq.com/scripts/plsql/readlob.txt
rem Filename:   readlob.sql
rem Purpose:    Fetch LOB column values piece-wise from PL/SQL
rem Date:       12-Jun-2000
rem Author:     Frank Naude (frank@ibi.co.za)
rem -----------------------------------------------------------------------

set serveroutput on

DROP TABLE lob_table;                  -- Create table to hols LOBs
CREATE TABLE lob_table  (
        id      INTEGER,
        b_lob   BLOB,
        c_lob   CLOB,
        b_file  BFILE );

INSERT INTO lob_table                  -- Create sample record
	VALUES (1, EMPTY_BLOB(), 'abcde', NULL);

DECLARE
   clob_locator CLOB;
   charbuf      VARCHAR2(20);
   read_offset  INTEGER;
   read_amount  INTEGER;
BEGIN
   -- First we need to get the lob locator
   SELECT c_lob INTO clob_locator FROM lob_table WHERE id = 1;

   DBMS_OUTPUT.PUT_LINE('CLOB Size: ' ||
                       DBMS_LOB.GETLENGTH(clob_locator));

   -- Read LOB field contents
   read_offset := 1;
   read_amount := 20;
   dbms_lob.read(clob_locator, read_amount, read_offset, charbuf);
   dbms_output.put_line('CLOB Value: ' || charbuf);
END;
/
