--  Taken from #2382 submitted by @JulienPivard
--  @summary
--  Implémentation par une tâche.
--  @description
--  Implémentation par une tache de la classe synchronisé.
--  @group Version tâche
--
--  package Carte_P.Tasche_P
package Input
   with
      Pure           => False,
      Preelaborate   => False,
      Elaborate_Body => True,
      Spark_Mode     => Off
is

   ---------------------------------------------------------------------------
   task type Tasche_T is new Carte_T with
   --  Implémentation par une tâche de l'interface Carte_T.

      -----------------------------------
      overriding
      entry Coucou;
      --  Implémentation par un accept.

      -----------------------------------
      --  overriding
      --  entry Inutile;
      --  Implémentation par un accept.
      --  @param This
      --  La carte.
   end Tasche_T;
   ---------------------------------------------------------------------------

   overriding
   procedure Inutile
      (This : in out Tasche_T);
   --  Implémentation par une procédure.
   --  @param This
   --  La carte.

end Carte_P.Tasche_P;
