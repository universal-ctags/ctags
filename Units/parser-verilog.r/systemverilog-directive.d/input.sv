//
// LRM 22. Compiler Directives
//  `define should have its own kind other than constant (c)

//
// 22.4 `include
//
`include "parts/count.v"
`include "fileB" // including fileB
`include <List.vh>

//
// 22.5 `define, `undef, and `undefineall
//
module directive;
`define D(x,y) initial $display("start", x , y, "end");
`D( "msg1" , "msg2" )
// expands to 'initial $display("start", "msg1" , "msg2", "end");'
`D( " msg1", )
// expands to 'initial $display("start", " msg1" , , "end");'
`D(, "msg2 ")
// expands to 'initial $display("start", , "msg2 ", "end");'
`D(,)
// expands to 'initial $display("start", , , "end");'
`D( , )
// expands to 'initial $display("start", , , "end");'
//`D("msg1")
// illegal, only one argument
//`D()
// illegal, only one empty argument
//`D(,,)
// illegal, more actual than formal arguments

`define MACRO1(a=5,b="B",c) $display(a,,b,,c);
`MACRO1 ( , 2, 3 ) // argument a omitted, replaced by default
// expands to '$display(5,,2,,3);'
`MACRO1 ( 1 , , 3 ) // argument b omitted, replaced by default
// expands to '$display(1,,"B",,3);'
`MACRO1 ( , 2, ) // argument c omitted, replaced by nothing
// expands to '$display(5,,2,,);'
//`MACRO1 ( 1 ) // ILLEGAL: b and c omitted, no default for c

`define MACRO2(a=5, b, c="C") $display(a,,b,,c);
`MACRO2 (1, , 3) // argument b omitted, replaced by nothing
// expands to '$display(1,,,,3);'
`MACRO2 (, 2, ) // a and c omitted, replaced by defaults
// expands to '$display(5,,2,,"C");'
`MACRO2 (, 2) // a and c omitted, replaced by defaults
// expands to '$display(5,,2,,"C");'

`define MACRO3(a=5, b=0, c="C") $display(a,,b,,c);
`MACRO3 ( 1 ) // b and c omitted, replaced by defaults
// expands to '$display(1,,0,,"C");'
`MACRO3 ( ) // all arguments replaced by defaults
// expands to '$display(5,,0,,"C");'
//`MACRO3 // ILLEGAL: parentheses required

`define wordsize 8
logic [1:`wordsize] data;

//define a nand with variable delay
`define var_nand(dly) nand #dly
`var_nand(2) g121 (q21, n10, n11);
`var_nand(5) g122 (q22, n10, n11);

// illegal
//`define first_half "start of string
//$display(`first_half end of string");

`define max(a,b)((a) > (b) ? (a) : (b))
n = `max(p+q, r+s) ;

`define TOP(a,b) a + b
`TOP( `TOP(b,1), `TOP(42,a) )

endmodule

module main;
`define HI Hello
`define LO "`HI, world"
`define H(x) "Hello, x"

    initial begin
        $display("`HI, world");
        $display(`LO);
        $display(`H(world));
    end
endmodule

module directive2;
`define msg(x,y) `"x: `\`"y`\`"`"
`define append(f) f``_master

    initial begin
        $display(`msg(left side,right side));
        `append(clock) = 1'b0;
    end

`define home(filename) `"/home/mydir/filename`"
`include `home(myfile)

endmodule

//
// 22.6 `ifdef, `else, `elsif, `endif, `ifndef
//
module and_op (a, b, c);
    output a;
    input b, c;
    `ifdef behavioral
        wire a = b & c;
    `else
        and a1 (a,b,c);
    `endif
endmodule

module test(out);
    output out;
    `define wow
    `define nest_one
    `define second_nest
    `define nest_two
    `ifdef wow
        initial $display("wow is defined");
        `ifdef nest_one
            initial $display("nest_one is defined");
            `ifdef nest_two
                initial $display("nest_two is defined");
            `else
                initial $display("nest_two is not defined");
            `endif
        `else
            initial $display("nest_one is not defined");
        `endif
    `else
        initial $display("wow is not defined");
        `ifdef second_nest
            initial $display("second_nest is defined");
        `else
            initial $display("second_nest is not defined");
        `endif
    `endif
endmodule

module test;
    `ifdef first_block
        `ifndef second_nest
            initial $display("first_block is defined");
        `else
            initial $display("first_block and second_nest defined");
        `endif
    `elsif second_block
        initial $display("second_block defined, first_block is not");
    `else
        `ifndef last_result
            initial $display("first_block, second_block,",
                " last_result not defined.");
        `elsif real_last
            initial $display("first_block, second_block not defined,",
                " last_result and real_last defined.");
        `else
            initial $display("Only last_result defined!");
        `endif
    `endif
endmodule

// orignal tests
module ifdef_in_port (
    input logic a,
    `ifdef FOO// comment w/o white space
        input logic b1,
    `elsif BAR  // coment w/ white space
        input logic b2,
    `else/* old C comment */ // foo
        input logic b3,
    `endif /* old C comment */ // foo
    input logic c
);

endmodule

typedef logic user_t;
module define_in_port (
    input user_t a,
`define FOO
    input user_t b,
`define BAR
`ifdef FOO
        input user_t c1,
`elsif BAZ
        input user_t c2,
`else
        input user_t c3,c4,
`endif
`ifdef FOO
        output user_t d1 ,
`else
        output user_t d2
`endif
);

endmodule

module define_in_port_messy (
    input user_t a
`define FOO
    ,input user_t b
`define BAR
`ifdef FOO
        ,input user_t c1
`elsif BAZ
        ,input user_t c2
`else
        ,input user_t c3 , c4
`endif
`ifdef FOO
        , output user_t d1
`else
        , output user_t d2
`endif
);

endmodule

`undef  MY_UNDEF
`define MY_DEFINE

`define assert_clk(arg, __clk=clk, __rst_n=rst_n) \
 assert property (@(posedge __clk) disable iff (!__rst_n) arg) 

module forSkipMacro;
`define add_t(f) f``_t
    var `add_t(foo) = '0;

    `macro({e},FOO)
    `macro("string",FOO)
    `macro(bar)
    `macro(int)
    `macro(int,bar)
endmodule
