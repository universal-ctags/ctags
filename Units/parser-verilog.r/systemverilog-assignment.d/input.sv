module assignment;

  function int bar();
    return 0;
  endfunction

  // src/tlm2/uvm_tlm2_generic_payload.svh:494
  string msg = $sformatf("GP miscompare between '%s' and '%s':\nlhs = %s\nrhs = %s",
                         bar(), 1, 2, 3);

  // LRM 10.9.1 Array assignment patterns
  bit unpackedbits [1:0] = '{1,1};
  int unpackedints [1:0] = '{1'b1, 1'b1};
  int y;
  int unpackedints2 [1:0] = '{2 {y}};
  int n[1:2][1:3] = '{2{'{3{y}}}};

  struct {int a; time b;} abkey[1:0];
  abkey = '{'{a:1, b:2ns}, '{int:5, time:$time}};

  struct {int a; time b;} abkey_fun[1:0] = '{'{a:bar(), b:2ns}, '{int:5, time:$time}};

  // LRM 10.9.2 Structure assignment patterns
  typedef struct {
    int x;
    int y;
  } st;
  int k = 1;
  st s1 = '{1, 2+k}; // by position;
  st s2 = '{x:2, y:3+k}; // by name
  struct { int x; int y; } s3 = '{1, 2+k};
  struct { int x; int y; } s4 = '{x:2, y:3+k};

  st s5 = '{default:2};

  typedef struct { int a; shortreal b; } ab;  // LRM 5.10
  ab abkey[1:0] = '{'{a:1, b:1.0}, '{int:2, shortreal:2.0}};

  struct {
    int A;
    struct {
      int B, C;
    } BC1, BC2;
  } ABC, DEF;
  ABC = '{A:1, BC1:'{B:2, C:3}, BC2:'{B:4,C:5}};
  DEF = '{default:10};

  typedef struct {
    logic [7:0] a;
    bit b;
    bit signed [31:0] c;
    string s;
  } sa;
  sa s2 = '{int:1, default:0, string:""};

  // LRM 10.10 Unpacked array concatenation
  int A3[1:3];
  A3 = {1, 2, 3}; // unpacked array concatenation: A3[1]=1, A3[2]=2, A3[3]=3
  A3 = '{1, 2, 3}; // array assignment pattern: A3[1]=1, A3[2]=2, A3[3]=3

  typedef int AI3[1:3];
  AI3 A3;
  int A9[1:9];
  A3 = '{1, 2, 3};
  A9 = {A3, 4, 5, A3, 6}; // legal, gives A9='{1,2,3,4,5,1,2,3,6}
  A9 = '{9{1}}; // legal, gives A9='{1,1,1,1,1,1,1,1,1}
  // array concatenation
  A9 = {A3, 4, AI3'{5, 6, 7}, 8, 9}; // legal, A9='{1,2,3,4,5,6,7,8,9}

endmodule

