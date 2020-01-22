-- package body Mes_Tasches_P is
package body Input_1 is
   ---------------------------------------------------------------------------
   task body Ma_Tasche is
      Var_Continuer : Boolean := False;
   begin
      Boucle_Principale :
      loop
         accept Accepter
            (Continuer : Boolean)
         do
            Var_Continuer := Continuer;
         end Accepter;
      end loop Boucle_Principale;
   end Ma_Tasche;
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   task body Mon_Autre_Tasche is
   begin
      null;
   end Mon_Autre_Tasche;
   ---------------------------------------------------------------------------

   Une_Autre_Tasche_1 : Tasche_Type_1_T;

   ---------------------------------------------------------------------------
   task body Tasche_Type_1_T is
   begin
      null;
   end Tasche_Type_1_T;
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   task body Tasche_Type_2_T is
   begin
      null;
   end Tasche_Type_2_T;
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   task body Tasche_Type_3_T is
   begin
      null;
   end Tasche_Type_3_T;
   ---------------------------------------------------------------------------

   Une_Autre_Tasche_2 : Tasche_Type_2_T;
   Une_Autre_Tasche_3 : Tasche_Type_3_T (Taille => 5);

   ---------------------------------------------------------------------------
   protected body Objet_Protege is
      entry Demarrer
         when Variable
      is
      begin
         null;
      end Demarrer;

      procedure Faire is
      begin
         null;
      end Faire;

      function Tester
         return Boolean
      is
      begin
         return Variable;
      end Tester;
   end Objet_Protege;
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   protected body Type_Protege is
      entry Demarrer
         when Variable
      is
      begin
         null;
      end Demarrer;

      procedure Faire is
      begin
         null;
      end Faire;

      function Tester
         return Boolean
      is
      begin
         return Variable;
      end Tester;
   end Type_Protege;
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   protected body Discriminant_Protege is
      entry Demarrer
         when Variable
      is
      begin
         null;
      end Demarrer;

      procedure Faire is
      begin
         null;
      end Faire;

      function Tester
         return Boolean
      is
      begin
         return Variable;
      end Tester;
   end Discriminant_Protege;
   ---------------------------------------------------------------------------

end Input_1;
-- end Mes_Tasches_P;
