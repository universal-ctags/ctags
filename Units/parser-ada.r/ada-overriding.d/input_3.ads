--  Taken from https://en.wikibooks.org/wiki/Ada_Programming/Object_Orientation#Overriding_indicators
package Input_3 is
    type Object is tagged null record;

   function  Primitive return access Object; -- new in Ada 2005

   type Derived_Object is new Object with null record;

   not overriding -- new optional keywords in Ada 2005
   procedure Primitive (This : in Derived_Object); -- new primitive operation

   overriding
   function  Primitive return access Derived_Object;
end X;
