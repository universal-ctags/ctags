function Pow2( N, Exp : integer )  return mylib.myinteger is
  Variable Result   : integer := 1;

begin
  for i in 1 to Exp loop
    Result := Result * N;
  end loop;
  return( Result );
end Pow;
