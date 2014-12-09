package MyPackage;
    typedef struct {
        shortreal a;
        real b;
    } MyData;
    function MyData add(MyData x, y);
        add.a = x.a + y.a;
    endfunction
    function MyData mul(MyData x, y);
        mul.b = x.b * y.b;
    endfunction
endpackage : ComplexPkg

reg var_to_check_context;
