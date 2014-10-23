rem -----------------------------------------------------------------------
rem URL:        http://www.orafaq.com/scripts/plsql/db-trig.txt
rem Filename:   db-trig.sql
rem Purpose:    Demonstrate database triggers (available from Oracle 8i)
rem             Need DBA or CREATE ANY TRIGGER privs
rem Date:       28-Aug-1998
rem Author:     Frank Naude (frank@ibi.co.za)
rem -----------------------------------------------------------------------

create or replace trigger restrict_login
        after logon       on database
declare
        flag number := 0;
begin
        select 1 into flag from sys.v_$session where program like '%sqlplus%';
        if flag = 1 then
                raise_application_error(-20000, 'No access from sqlplus');
        end if;
end;
/
show errors


CREATE OR REPLACE TRIGGER startup_db
after startup on database
begin
    dbms_shared_pool.keep ('SYS.STANDARD','P');
    dbms_shared_pool.keep ('SYS.DBMS_STANDARD','P');
end;
/
show errors
