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

  always_comb begin
    logic expression, a, result, flaga, flagb;
    logic [2:1] b;
    logic [1:2] select;
    logic [2:0] encode ;

    // 12.4.1 if–else–if construct
    if (expression) statement;
    else if (expression) statement;
    else if (expression) statement;
    else statement;

    // 12.4.2 unique-if, unique0-if, and priority-if
    unique if ((a==0) || (a==1)) $display("0 or 1");
    else if (a == 2) $display("2");
    else if (a == 4) $display("4"); // values 3,5,6,7 cause a violation report

    priority if (b[2:1]==0) $display("0 or 1");
    else if (b[2] == 0) $display("2 or 3");
    else $display("4 to 7");  // covers all other possible values,
                              // so no violation report

    unique0 if ((a==0) || (a==1)) $display("0 or 1");
    else if (a == 2) $display("2");
    else if (a == 4) $display("4"); // values 3,5,6,7
                                    // cause no violation report
    // 12.5 Case statement
    case (select[1:2])
      2'b00: result = 0;
      2'b01: result = flaga;
      2'b0x,
      2'b0z: result = flaga ? 'x : 0;
      2'b10: result = flagb;
      2'bx0,
      2'bz0: result = flagb ? 'x : 0;
      default result = 'x;
    endcase

    case (1)
      encode[2] : $display("Select Line 2") ;
      encode[1] : $display("Select Line 1") ;
      encode[0] : $display("Select Line 0") ;
      default $display("Error: One of the bits expected ON");
    endcase
  end

  // original
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

  always_comb begin
    enum { FSM_IDLE, FSM_ITER } state;
    case (state)
      FSM_IDLE,
      FSM_ITER : begin
      end
    endcase
  end

endmodule
