{ Derrived from https://wiki.freepascal.org/Procedure }
program procedureDemo(input, output, stderr);

var
   x: longint;

procedure foo0;
begin
   inc(x);
end;

// procedure foo1;
// begin
//   inc(x);
// end;

{
procedure foo2;
begin
   inc(x);
end;
}

(*
procedure foo3;
begin
   inc(x);
end;
*)

begin
   x := 42;
   foo0;
   // foo1;
   { foo2; }
   (* foo3; *)
   writeLn(x);
end.
