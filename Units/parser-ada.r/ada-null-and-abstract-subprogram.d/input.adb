--  Derived from #4505 submitted by @jurgendb
package Shapes is
  type Shape is abstract tagged null record;
  procedure Draw (S : in Shape) is abstract;
  function Area (S : in Shape) return Float is abstract;
  procedure Reset (S : in out Shape) is null;
  procedure Move (S : in out Shape; DX, DY : in Float);
end Shapes;

package body Lists is

  procedure Finalize (Item : in out List_T) is null;

  procedure Assign (To : in out List_T; From : in List_T) is
    Count : Natural := 0;
  begin
    null;
  end Assign;

  function Length (L : in List_T) return Natural is
  begin
    return 0;
  end Length;

end Lists;
