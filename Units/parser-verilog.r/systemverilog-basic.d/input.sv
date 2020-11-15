// Include module declaration in a comment
// module wrong;
// endmodule
`define DEFINE
`define DEF_WITH_EQ = 1'd100
`define DEF_VALUE   1'd100

class test;
    reg a;
    logic b;
    bit [1:0] c[3] = '{0, 0, 0};

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
    `ifndef DEFINE
    output reg f,
    `endif
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
    input x, y
    `ifdef DEFINE
    ,output z
    `endif
);
    z = x + y;
endtask

function mult (
    `ifdef DEFINE
    input x,
    `endif
    input y);
    reg temp;
    temp = x * y;
    return temp;
endfunction

function ref_test (
    ref tref1,
    ref wire tref2
    );
endfunction

wire [PARAM1-1:0] mynet;

genvar gencnt;
generate
    for (gencnt = 0; gencnt < PARAM1; gencnt = gencnt + 1) begin: array
        assign mynet[gencnt] = 1'b0;
    end
endgenerate

endmodule // mod
