// LRM 6.7.1 Net declarations with built-in net types
module net_decl;
  trireg (large) logic #(0,0,0) cap1; // FIXME: ignored
  typedef logic [31:0] addressT;
  wire addressT w1; // FIXME: ignored
  wire struct packed { logic ecc; logic [7:0] data; } memsig; // FIXME: packed -> memsig

  wire w; // equivalent to "wire logic w;"
  wire [15:0] ww; // equivalent to "wire logic [15:0] ww;"
endmodule

// LRM 9.4.1 Delay control
module delay_control #(d, e);
  int rega, regb, regr;
  initial begin
    #10 rega = regb;
    #d rega = regb; // d is defined as a parameter
    #((d+e)/2) rega = regb; // delay is average of d and e
    #regr regr = regr + 1; // delay is the value in regr
  end
endmodule
