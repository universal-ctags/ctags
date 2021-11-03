--  Taken from #2933 submitted by @koenmeersman
procedure Go is
begin
  Loop_Label :
  while True loop
    declare
      procedure Not_Tagged is
      begin
        null;
      end Step_T;
    begin
      null;
    end;
  end loop Loop_Label;
end Go;
