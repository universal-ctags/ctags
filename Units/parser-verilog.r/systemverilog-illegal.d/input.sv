module m;
  struct
  {
    s* ; // pushMembers() caused infinite loop : #2724
  }
  logic foo;
endmodule
