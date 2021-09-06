//
// LRM: 26. Packages
//
// 26.2 Package declarations
package ComplexPkg;
    typedef struct {
        shortreal i, r;
    } Complex;

    function Complex add(Complex a, b);
        add.r = a.r + b.r;
        add.i = a.i + b.i;
    endfunction

    function Complex mul(Complex a, b);
        mul.r = (a.r * b.r) - (a.i * b.i);
        mul.i = (a.r * b.i) + (a.i * b.r);
    endfunction
endpackage : ComplexPkg

// 26.3 Referencing data in packages
package p;
    typedef enum { FALSE, TRUE } bool_t;
endpackage

package q;
    typedef enum { ORIGINAL, FALSE } teeth_t;
endpackage

module top1 ;
    import p::*;
    import q::teeth_t;

    teeth_t myteeth;

    initial begin
        myteeth = q:: FALSE;    // OK:
        //myteeth = FALSE;        // ERROR: Direct reference to FALSE refers to the
    end // FALSE enumeration literal imported from p
endmodule

module top2 ;
    import p::*;
    import q::teeth_t, q::ORIGINAL, q::FALSE;

    teeth_t myteeth;

    initial begin
        myteeth = FALSE; // OK: Direct reference to FALSE refers to the
    end // FALSE enumeration literal imported from q
endmodule

// 26.4 Using packages in module headers
package A;
    typedef struct {
        bit [ 7:0] opcode;
        bit [23:0] addr;
    } instruction_t;
endpackage: A

package B;
    typedef enum bit {FALSE, TRUE} boolean_t;
endpackage: B

module M import A::instruction_t, B::*;
    #(WIDTH = 32)
     (input [WIDTH-1:0] data,
      input instruction_t a,
      output [WIDTH-1:0] result,
      output boolean_t OK
      );
    // ...
endmodule: M

// original
package MyPackage;
    typedef struct {
        shortreal a;
        real b;
    } MyData;
    function MyData add(MyData x, y);
        add.a = x.a + y.a;
    endfunction
    function MyData mul(MyData x, y);
        mul.b = x.b * y.b;
    endfunction
endpackage : MyPackage

reg var_to_check_context;

// multiple package import declarations, #3150
module mod_a
    import A::*, B::*;
(
    input var logic in_a
);
    logic sig_a;
endmodule

module mod_b
    import A::*;
    import B::*;
(
    input var logic in_b
);
    logic sig_b;
endmodule
