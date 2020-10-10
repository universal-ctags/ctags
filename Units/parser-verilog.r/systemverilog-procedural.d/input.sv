//
// LRM 12. Procedural programming statements
//

module procedural;

  string s;
  always_comb begin
    s = "foo bar ( { [ ";
  end

  parameter N = 8;
  always_comb begin
    int a;
    for (int i = 0; i < N; i++) begin : outer1
      int b;
      for (int j = 0; j < N; j++) begin:inner1
        int c;
      end
    end

    for (int i = 0; i < N; i++) begin : outer2
      int d;
      for (int j = 0; j < N; j++) begin:inner2
        int e;
      end:inner2
    end : outer2
  end
endmodule
