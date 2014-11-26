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

module mod#(
    parameter PARAM1 = 10,
    parameter PARAM2 = 2.0
) (
    input wire a,
    b,c,
    d ,
    output wire e ,
    output reg f,
    inout wire g
);

localparam LOCALPARAM = 2**2;

localparam STATE1 = 4'h0,
           STATE2 = 4'h1,
           STATE3 = 4'h2,
           STATE4 = 4'h5    ,
           STATE5 = 4'h6    ,
           STATE6 = 4'h7    ,
           STATE7 = 4'h8;

real k;
integer l;
test t;

task add (
    input x, y,
    output z
);
    z = x + y;
endtask

function mult (
    input x,
    input y);
    reg temp;
    temp = x * y;
    return temp;
endfunction

endmodule // mod
