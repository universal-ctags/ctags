// Include module declaration in a comment
// module wrong;
// endmodule
`define DEFINE

`define DEF_WITH_EQ = 1'd100
`define DEF_VALUE   1'd100

module foo;
parameter PARAM = 1;

localparam LOCALPARAM = 2**2;

localparam STATE1 = 4'h0,
           STATE2 = 4'h1,
           STATE3 = 4'h2,
           STATE4 = 4'h5    ,
           STATE5 = 4'h6    ,
           STATE6 = 4'h7    ,
           STATE7 = 4'h8;

wire a,b,c,d,e;
reg f;
wire g;
real k;
integer l;

initial begin
    add(a, b, f);
end

task add;
    input x, y;
    output z;
begin
    z = x + y;
end
endtask

function mult;
    input x;
    input y;
begin
    mult = x * y;
end
endfunction

endmodule
