// Taken from the commet in #2489 submitted by @t0mj0nes.
package my_pack;
    localparam int MY_WIDTH                  = 1;
    localparam int MY_DEPTH                  = 2;
    typedef logic [MY_DEPTH-1:0][MY_WIDTH-1:0]  my_t;
   // Added by @masatake.
    typedef logic   [MY_DEPTH-1:0] 	 [MY_WIDTH-1:0] 	 my_t2;
endpackage // my_pack
