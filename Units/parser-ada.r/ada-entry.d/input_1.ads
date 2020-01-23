-- package Mes_Tasches_P is
package Input_1 is

   task Ma_Tasche is
      entry Accepter
         (Continuer : Boolean);
   end Ma_Tasche;

   task Mon_Autre_Tasche;

   task type Tasche_Type_1_T;

   Une_Tasche : Tasche_Type_1_T;

   task type Tasche_Type_2_T is
      entry Start;
      entry Lire
         (Donnee : out Integer);
   end Tasche_Type_2_T;

   --  Task type with discriminant.
   task type Tasche_Type_3_T
      --  We could have any number of arguments in discriminant
      --  Work exactly like argument in procedure/function/entry/accept
      (Taille : Integer)
   is
      entry Start;
   end Tasche_Type_3_T;

   --  protected objects.

   protected Objet_Protege is
      entry Demarrer;
      procedure Faire;
      function Tester return Boolean;
   private
      Variable : Boolean := True;
   end Objet_Protege;

   protected type Type_Protege is
      entry Demarrer;
      procedure Faire;
      function Tester return Boolean;
   private
      Variable : Boolean := True;
   end Type_Protege;

   protected type Discriminant_Protege
      (Priorite : Natural)
   is
      entry Demarrer;
      procedure Faire;
      function Tester return Boolean;
   private
      Variable : Boolean := True;
   end Discriminant_Protege;

end Input_1;
-- end Mes_Tasches_P;
