package body Input is
  function F (S : Integer) return Integer is
  R : Integer;
  begin
    if S < 1 then
      declare
	T : Integer := 2;
      begin
	if  S > T then
	  T :=  3;
	end if;
	R := S;
      end;
    else
      declare
	P : Integer := 4;
      begin
	return P;
      end;
    end if;
    return R;
  end F;
  function G (A : Integer) return Integer is
  B : Integer;
  begin
    if A < 1 then
      declare
	C : Integer := 2;
      begin
	case A is
	 when 1 => null;
	 when others => null;
	end case;
	B := A;
      end;
    else
      declare
	D : Integer := 4;
      begin
	return D;
      end;
    end if;
    return B;
  end G;
end Input;
