// Include module declaration in a comment
// module wrong;
// endmodule
`define DEFINE
`define DEF_WITH_EQ = 1'd100
`define DEF_VALUE   1'd100

class test;
    reg a;
    logic b;
    function mult (a, input b = 0);
        return a * b;
    endfunction : mult

    extern virtual function void extern_func (input bit a, input b);

endclass : test

class supertest extends test;
    logic c;
    function mult (a, input b = 0);
        return a * b * 2;
    endfunction : mult
endclass : test

class paramtest #(type BASE=supertest #(test)) extends BASE;
endclass : paramtest

class paramtest2 #(
  type BASE=supertest #(test)
) extends BASE;
endclass : paramtest2
