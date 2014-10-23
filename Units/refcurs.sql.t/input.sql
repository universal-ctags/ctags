rem -----------------------------------------------------------------------
rem URL:        http://www.orafaq.com/scripts/plsql/refcurs.txt
rem Filename:   refcurs.sql
rem Purpose:    Pass result sets (REF CURSOR) between procedures and 
rem		functions
rem Date:       15-Jun-2001
rem Author:     Frank Naude (frank@ibi.co.za)
rem -----------------------------------------------------------------------

set serveroutput on

-- Define TYPES package separately to be available to all programming
-- environments...
CREATE OR REPLACE PACKAGE types AS
   TYPE cursortyp is REF CURSOR;   -- use weak form
END;
/

-- Create test package to demonstrate passing result sets...
CREATE OR REPLACE PACKAGE test_ref_cursor AS
   PROCEDURE main;
   FUNCTION  get_cursor_ref(typ NUMBER) RETURN types.cursortyp;
   PROCEDURE process_cursor(cur types.cursortyp);
END;
/
show errors


CREATE OR REPLACE PACKAGE BODY test_ref_cursor AS

  -- Main program entry point
  PROCEDURE main IS
  BEGIN
    process_cursor( get_cursor_ref(1) );
    process_cursor( get_cursor_ref(2) );
  END;

  -- Get and return a CURSOR REF/ Result Set
  FUNCTION get_cursor_ref(typ NUMBER) RETURN types.cursortyp IS
    cur  types.cursortyp;
  BEGIN
    if typ = 1 THEN
      OPEN cur FOR SELECT * FROM emp  WHERE ROWNUM < 5;
    ELSE
      OPEN cur FOR SELECT * FROM dept WHERE ROWNUM < 5;
    END IF;
    RETURN cur;
  END;

  -- Process rows for an EMP or DEPT cursor
  PROCEDURE process_cursor(cur types.cursortyp) IS
    empRec  emp%ROWTYPE;
    deptRec dept%ROWTYPE;
  BEGIN
    LOOP
      FETCH cur INTO empRec;    -- Maybe it was an EMP cursor, try to fetch...
      EXIT WHEN cur%NOTFOUND;
      dbms_output.put_line('EMP ROW: '||empRec.ename);
    END LOOP;
  EXCEPTION
    WHEN ROWTYPE_MISMATCH THEN  -- OK, so it was't EMP, let's try DEPT.
       LOOP
         FETCH cur INTO deptRec;
         EXIT WHEN cur%NOTFOUND;
         dbms_output.put_line('DEPT ROW: '||deptRec.dname);
       END LOOP;
  END;

END;
/
show errors


EXEC test_ref_cursor.main;

