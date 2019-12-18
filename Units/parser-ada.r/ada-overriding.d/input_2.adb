--  Taken from #2382 submitted by @JulienPivard
with Ada.Text_IO;

package body input_2
   with Spark_Mode => Off
is

   ---------------------------------------------------------------------------
   task body Tasche_T is
      --  Les déclarations qui vont bien.
   begin
      accept Coucou do
         null;
      end Coucou;

      Ada.Text_IO.Put_Line (Item => "Wesh gros je suis une tâche");

      --  accept Inutile do
      --     null;
      --  end Inutile;
      abort Tasche_T;
   end Tasche_T;
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   overriding
   procedure Inutile
      (This : in out Tasche_T)
   is
   begin
      null;
   end Inutile;
   ---------------------------------------------------------------------------

end input_2;

