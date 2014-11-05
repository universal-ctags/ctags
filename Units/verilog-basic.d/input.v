// Include module declaration in a comment
// module wrong;
// endmodule
`define DEFINE
`define DEF_WITH_EQ = 1'd100
`define DEF_VALUE   1'd100

module mod (
    a,
    b,c,
    d , e ,
    f,
    g
);

parameter PARAM = 1;

parameter STATE1 = 4'h0,
          STATE2 = 4'h1,
          STATE3 = 4'h2,
          STATE4 = 4'h5    ,
          STATE5 = 4'h6    ,
          STATE6 = 4'h7    ,
          STATE7 = 4'h8;

input a,b, c, d ;
output e;
output f;
inout g;

wire a,b,c,d,e;
reg f;
wire g;
real k;
integer l;

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

endmodule // mod
