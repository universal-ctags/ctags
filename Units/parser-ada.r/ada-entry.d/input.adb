with Ada.Text_IO;
-- with Mes_Tasches_P;
with Input_1;

-- procedure Client is
procedure Input is

begin

   Ada.Text_IO.Put_Line ("Tasks won't stop, kill it with CTRL-C");
   -- Mes_Tasches_P.Ma_Tasche.Accepter (Continuer => True);
   Input_1.Ma_Tasche.Accepter (Continuer => True);

end Input;
-- end Client;
