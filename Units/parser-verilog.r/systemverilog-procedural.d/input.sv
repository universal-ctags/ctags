//
// LRM 12. Procedural programming Statements
//    These construct does not generate tags basically.
//    But they must be ignored properly.
//

module procedural;

  int index, rega, regb, result;

  // 12.4 Conditional if–else statement
  always_comb begin
    if (index > 0)
      if (rega > regb) result = rega;
      else  // else applies to preceding if
        result = regb;

    if (index > 0)
      begin
        if (rega > regb)
          result = rega;
      end
    else result = regb;
  end

  string s;
  always_comb begin
    if (index > 0)
      s = "foo bar ) } ";
  end

  // 
  int value, a[3];
  int offset = 10;
  parameter N = 8;
  always_comb begin
    for ( int count = 0; count < 3; count++ )
      value = value +((a[count]) * (count+1));

    for ( int count = 0, done = 0, j = 0; j * count < 125; j++, count++)
      $display("Value j = %d\n", j );

    for (int i = 0, j = i+offset; i < N; i++,j++)
      $display("i = %d, j = %d\n", i, j );
  end

  always_comb begin
    for (int i = 0; i < N; i++) begin:outer
      for (int j = 0; j < N; j++) begin : inner
        // ...
      end
    end
  end

endmodule
