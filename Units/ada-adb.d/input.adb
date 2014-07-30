--  Taken from altivec of GNAT examples (http://www.adacore.com/developers/code-samples/gnat-examples/)
--  ====================================================================================================
--  This example shows how to create and manipulate vectors by the mean of high
--  level views.

with GNAT.Altivec;                   use GNAT.Altivec;
with GNAT.Altivec.Conversions;       use GNAT.Altivec.Conversions;
with GNAT.Altivec.Vector_Operations; use GNAT.Altivec.Vector_Operations;
with GNAT.Altivec.Vector_Types;      use GNAT.Altivec.Vector_Types;
with GNAT.Altivec.Vector_Views;      use GNAT.Altivec.Vector_Views;

with GNAT.IO;                        use GNAT.IO;

procedure Altivec is

   View_A   : constant VUI_View := (Values => (1, 2, 3, 4));
   Vector_A : constant vector_unsigned_int := To_Vector (View_A);

   View_B   : constant VUI_View := (Values => (1, 1, 1, 1));
   Vector_B : constant vector_unsigned_int := To_Vector (View_B);

   Vector_C : vector_unsigned_int;
   View_C   : VUI_View;

begin
   Vector_C := vec_add (Vector_A, Vector_B);
   --  C = A + B

   View_C   := To_View (Vector_C);

   for I in View_C.Values'Range loop
      Put_Line (unsigned_int'Image (View_C.Values (I)));
   end loop;

end Altivec;
