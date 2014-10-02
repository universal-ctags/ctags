// Include module declaration in a comment
// module wrong;
// endmodule
`define DEFINE

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

real k;
integer l;

task add (
    input x, y,
    output z
);
    z = x + y;
endtask

function mult (
    input x,
    input y);
    mult = x * y;
endfunction

endmodule // mod
