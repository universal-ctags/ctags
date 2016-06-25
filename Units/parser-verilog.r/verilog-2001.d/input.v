// Include module declaration in a comment
// module wrong;
// endmodule
`define DEFINE
`define DEF_WITH_EQ = 1'd100
`define DEF_VALUE   1'd100

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
reg signed [3:0] scounter;

task add (
    input x, y,
    output z
);
    z = x + y;
endtask

function integer mult (
    input x,
    input y);
    mult = x * y;
endfunction

function [1:0] func_with_range (k, l);
    func_with_range = {k, l};
endfunction

endmodule // mod
