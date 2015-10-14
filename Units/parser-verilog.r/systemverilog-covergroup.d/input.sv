covergroup test;
    immediate_assertion : assert () task();
    immediate_cover     : cover () task();
    immediate_assume    : assume () task();
    deferred_assertion1 : assert #0 () task();
    deferred_cover1     : cover #0 () task();
    deferred_assume1    : assume #0 () task();
    deferred_assertion2 : assert final () task();
    deferred_cover2     : cover final () task();
    deferred_assume2    : assume final () task();
endgroup

reg var_to_check_context;
