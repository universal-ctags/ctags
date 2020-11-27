covergroup test;
    immediate_assertion : assert () myTask();
    immediate_cover     : cover () myTask();
    immediate_assume    : assume () myTask();
    deferred_assertion1 : assert #0 () myTask();
    deferred_cover1     : cover #0 () myTask();
    deferred_assume1    : assume #0 () myTask();
    deferred_assertion2 : assert final () myTask();
    deferred_cover2     : cover final () myTask();
    deferred_assume2    : assume final () myTask();
endgroup

covergroup cg @@ ( begin task_end );
endgroup

reg var_to_check_context;
