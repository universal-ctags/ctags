/*
------------------------------------------------------------------------------
URL:       http://www.orafaq.com/scripts/plsql/random.txt
Filename:  random.txt
Purpose:   Random number/ string generator package
Author:    Unknown
Original:  http://www.orafaq.org/scripts/sql/random.txt
Edits:
 19990908 Phil Rand <prand@spu.edu> Added functions rand_string(), smaller().
------------------------------------------------------------------------------
*/

create or replace package random
is
   procedure srand(new_seed in number);
   procedure get_rand(r OUT number);
   procedure get_rand_max(r OUT number, n IN number);
   function  rand return number;
   function  rand_max(n IN number) return number;
   function  rand_string(ssiz IN number) return varchar2;
   function  smaller(x IN number, y IN number) return number;
   pragma restrict_references(rand, WNDS);
   pragma restrict_references(rand_max, WNDS);
   pragma restrict_references(random, WNDS, RNPS);
   pragma restrict_references(rand_string, WNDS);
   pragma restrict_references(smaller, WNDS);
end random;
/

create or replace package body random
is
   multiplier   constant number := 22695477;
   increment    constant number := 1;
   "2^32"       constant number := 2 ** 32;
   "2^16"       constant number := 2 ** 16;
   "0x7fff"     constant number := 32767;
   Seed         number          := 1;

   function  smaller(x IN number, y IN number) return number is
   begin
	if x <= y then
	    return x;
	else
	    return y;
	end if;
   end smaller;

   function rand_string(ssiz IN number) return varchar2 is
     i      number;
     m      number;
     c      char;
     result varchar2(2000) := '';
   begin
	m := smaller(ssiz,2000);
	for i in 1..m loop
	    c := substr('abcdefghijklmnopqrstuvwxyz0123456789',rand_max(36),1);
	    result := result || c;
        end loop;
	return result;
   end rand_string;

   procedure srand(new_seed in number) is
   begin
     Seed := new_seed;
   end srand;

   function rand return number is
   begin
     Seed := mod(multiplier * Seed + increment, "2^32");
     return bitand(Seed/"2^16", "0x7fff");
   end rand;

   procedure get_rand(r OUT number) is
   begin
     r := rand;
   end get_rand;

   function rand_max(n IN number) return number is
   begin
     return mod(rand, n) + 1;
   end rand_max;

   procedure get_rand_max(r OUT number, n IN number) is
   begin
     r := rand_max(n);
   end get_rand_max;

begin
   select userenv('SESSIONID')
   into   Seed
   from   dual;
end random;
/

-- Some examples:
select random.rand_max(10) from dual;
select random.rand_max(10) from dual;
select random.rand_string(20) from dual;
select random.rand_string(20) from dual;

