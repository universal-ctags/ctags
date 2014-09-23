// Include module declaration in a comment
// module wrong;
// endmodule
`define DEFINE

module mod (
    input wire a,
    b,c,
    d ,
    output wire e ,
    output reg f,
    inout wire g
);

parameter PARAM = 1;

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
