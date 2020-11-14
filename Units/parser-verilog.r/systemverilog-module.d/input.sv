// user defined type port
typedef logic apb_if;
module blk_dut1 (input apb_if    apb, input bit rst); 
endmodule
// FIXME: first port has no direction
module blk_dut2 (apb_if    apb, input bit rst);
endmodule
module blk_dut3 (logic    apb, input bit rst);
endmodule
module blk_dut4 #(int BASE_ADDR='h0) (apb_if    apb,
                                     input bit rst);
endmodule
