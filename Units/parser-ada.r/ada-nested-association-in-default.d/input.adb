--  Derived from #4505 submitted by @jurgendb
package body Test is

  procedure Full_Iterate (FP_Id    : in Id_T;
                          Criteria : in Criteria_T := (others => True);
                          Logging  : in Boolean := False) is
    Scope : constant String := "FULL_ITERATE";
  begin
    null;
  end Full_Iterate;

  procedure Nested (A : in Matrix_T := (others => (others => 0));
                    B : in Pair_T := (Left => 1, Right => 2);
                    C : in Integer := Compute (Arg => 3)) is
    V : Flags_T := (1 => True, others => False);
  begin
    null;
  end Nested;

  function Last (X : in Flags_T := (others => False)) return Boolean is
  begin
    return X (X'Last);
  end Last;

  procedure After is
  begin
    null;
  end After;

end Test;
