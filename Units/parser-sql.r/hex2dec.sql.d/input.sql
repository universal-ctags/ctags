rem -----------------------------------------------------------------------
rem URL:        http://www.orafaq.com/scripts/plsql/hex2dec.txt
rem Filename:   hex2dec.sql
rem Purpose:    Functions to convert Hex to Decimal and vice versa
rem Author:     Mark Malakanov, Feb-1999 + Anonymous
rem -----------------------------------------------------------------------

CREATE OR REPLACE FUNCTION hex2dec (hexnum in char) RETURN number IS
  i                 number;
  digits            number;
  result            number := 0;
  current_digit     char(1);
  current_digit_dec number;
BEGIN
  digits := length(hexnum);
  for i in 1..digits loop
     current_digit := SUBSTR(hexnum, i, 1);
     if current_digit in ('A','B','C','D','E','F') then
        current_digit_dec := ascii(current_digit) - ascii('A') + 10;
     else
        current_digit_dec := to_number(current_digit);
     end if;
     result := (result * 16) + current_digit_dec;
  end loop;
  return result;
END hex2dec;
/
show errors

CREATE OR REPLACE FUNCTION num2hex (N in number) RETURN varchar2 IS
  H  varchar2(64) :='';
  N2 integer      := N;
BEGIN
  loop
     select rawtohex(chr(N2))||H
     into   H
     from   dual;

     N2 := trunc(N2 / 256);
     exit when N2=0;
  end loop;
  return H;
END num2hex;
/
show errors

-- Examples:
select hex2dec('FF') from dual;

select num2hex(10) from dual;
