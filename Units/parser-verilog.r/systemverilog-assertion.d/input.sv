initial begin : deferred_immediate_assertions
    immediate_assertion : assert () task();
    immediate_cover     : cover () task();
    immediate_assume    : assume () task();
    deferred_assertion1 : assert #0 () task();
    deferred_cover1     : cover #0 () task();
    deferred_assume1    : assume #0 () task();
    deferred_assertion2 : assert final () task();
    deferred_cover2     : cover final () task();
    deferred_assume2    : assume final () task();
end

property prop1 (
    local input int m,
    logic [1:0] n,
    int o
);

endproperty :prop1

property prop2 (a, b);

endproperty : prop2

concurrent_assertion1   : assert property prop2 (l, m);
