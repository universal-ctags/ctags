// user defined type port
typedef logic user_t;
module blk_dut1 (input user_t apb, input logic rst); 
endmodule
module blk_dut2 (user_t apb, input logic rst);
endmodule
module blk_dut3 (logic apb, input logic rst);
endmodule
module blk_dut4 #(int BASE_ADDR='h0) (user_t apb,
                                     input logic rst);
endmodule

// no port
module M1;
endmodule

// no port
module M2 ();
endmodule

// non-ANSI
module M3 (a);
  input a;
endmodule

// non-ANSI
module M4 (a, b);
  input a, b;
endmodule

// ANSI
module M5 (input user_t a, b);
endmodule

// ANSI
module M6 (logic a);
endmodule

// ANSI
module M7 (logic a, output b);
endmodule

// ANSI
module M8 (user_t a, b);
endmodule
