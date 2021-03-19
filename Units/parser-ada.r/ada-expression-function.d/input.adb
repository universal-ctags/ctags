with Ada.Text_IO;	

procedure Test is

  My_Type : Boolean := True;

  procedure Tagged_Procedure is
    function Boolean_As_String return String is
      (case My_Type is
         when True  => "True Value",
         when False => "False Value");
	 
    function Another_Boolean_As_String return String is
      (case My_Type is when True  => "; function dummy0 return String is (",
         when False => "; function dummy1 return String is (");	      
    procedure p0 is	
    begin
        Ada.Text_IO.put ("-0");
    end p0;	 	 
    function Yet_Boolean_As_String return String is
      (case My_Type is
         when True  => "1",
         when False => "0");
    procedure p1 is	
    begin
        Ada.Text_IO.put ("-1");
    end p1;	 
  begin
    null;
  end Tagged_Procedure;

  procedure Not_Tagged_Procedure is
  begin
    null;
  end Not_Tagged_Procedure;
begin
  null;
end Test;
