-- Taken from https://en.wikibooks.org/wiki/Ada_Programming/Type_System
--
-- This code has a from of "type foo is new ..."
-- Though "new" is used in the form, "with" doesn't
-- follow "new" like "type foo is new bar with ...".
--
procedure Input  is

   package Pak  is
      type Integer_1  is  range 1 .. 10;
      procedure P (I:  in Integer_1); -- primitive operation, assumes 1 .. 10
      type Integer_2  is  new Integer_1  range 8 .. 10; -- must not break P's assumption
							-- procedure P (I: in Integer_2);  inherited P implicitly defined here
   end Pak;

   package  body Pak  is
      -- omitted
   end Pak;

   use Pak;
   A: Integer_1 := 4;
   B: Integer_2 := 9;

begin

   P (B); -- OK, call the inherited operation

end Derived_Types;
