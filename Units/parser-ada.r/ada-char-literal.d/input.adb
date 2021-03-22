--- file: s-valuti.adb
with System.Case_Util; use System.Case_Util;

package body System.Val_Util is

  procedure Bad_Value (S : String) is
  begin
    raise Constraint_Error with "bad input for 'Value: """ & S & '"';
  end Bad_Value;

  procedure Not_Tagged  is
  begin
    null;
  end Not_Tagged;
end System.Val_Util;
