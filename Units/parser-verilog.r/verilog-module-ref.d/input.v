// Taken from #3469 submitted by @my2817
module test (/*AUTOARG*/);
   input i_a, i_b;
   output o_c;

   ref1 int1 ();
   ref1 int2 ();
   ref3 # (.A (aaa),
           .B (bbb))
   int3 ();

endmodule // test
