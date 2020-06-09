package body Input_0 is
   function My_Function return Boolean is
   begin
      return True;
   end My_Function;

   procedure My_Procedure is
   begin
      null;
   end My_Procedure;

   task body My_Task is
   begin
      accept GET (X: in My_T) do
	 null;
      end GET;
   end My_Task;

   end Input_0;
