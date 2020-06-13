package Input_0 is
   -- Expecting above to create tag 'Input_0/s' as this is 'package spec'-definition with name 'Input_0'.
   -- Emacs tag-search on Input_0/s should navigate to the above.

   function My_Function return Boolean;
   -- Expecting above to create tag 'My_Function/f' as this is 'function'-definition with name My_Function.
   -- Emacs tag-search on My_Function/f should navigate to the above.

   procedure My_Procedure;
   -- Expecting above to create tag 'My_Procedure/p' as this is 'procedure'-definition with name My_Procedure.
   -- Emacs tag-search on My_Procedure/p should navigate to the above.

  -- Expecting above to create tag 'Input_0/s' as this is 'package spec'-definition with name 'Input_0'.
  -- Emacs tag-search on Input_0/s should navigate to the above.

   type My_T is (A, B, C);
  -- Expecting above to create tag 'My_T/t' as this is 'type'-definition with name 'My_T'.
  -- Emacs tag-search on My_T/t should navigate to the above.

  task My_Task is
    -- Expecting above to create tag 'My_Task/k' as this is 'task'-definition with name 'My_Task'.
    -- Emacs tag-search on My_Task/k should navigate to the above.
     entry GET (X : in My_T);
  end My_Task;

end Input_0;
