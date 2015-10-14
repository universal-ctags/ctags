// Include module declaration in a comment
// module wrong;
// endmodule
`define DEFINE

module mod (
    a,
    b,c,
    d , e ,
    f,
    g
);

parameter PARAM = 1;

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
