-- Taken from #4205 submitted by @koenmeersman
procedure Truc is
  function Some_Function return Boolean is
    function Inner_Function_Without_End_Label return Boolean is
    begin
      return True;
    end;
    Result : constant Boolean := Inner_Function_Without_End_Label;
  begin
    return Result;
  end Some_Function;
 
  function Smurf return Integer is
  begin
    if Some_Function then
      return 1;
    else
      return 2;
    end if;
  end Smurf;
begin
  if Smurf > 2 then
    raise Program_Error;
  end if;
end Truc;

