// https://github.com/universal-ctags/ctags/issues/4056

typedef enum {
  `include "test1.txt"
  OTHER_VAL1,
  `include "test2.txt"
  OTHER_VAL2
} my_enum;

my_enum e;

// from Units/parser-verilog.r/systemverilog-struct.d/input.sv
class O;
  // complex struct
  struct packed signed {
  `include "test1.txt"
    logic a,b ;
    logic [15:0] [7:0] s0 , s1;
  `include "test1.txt"
    struct {
  `include "test1.txt"
      logic x, y; // not emitted
  `include "test1.txt"
    } [15:0] [7:0] struct0_s, struct1_s;
  `include "test1.txt"
    enum user_t [1:0] { FOO, BAR, BAZ } [3:0] enum00_e, enum01_e ;
  `include "test1.txt"
    enum logic unsigned{A,B,C}[1:0]enum10_e,enum11_e;
    bit[7:0][1:0]d,e;
  `include "test1.txt"
  } [1:0] complex0_s, complex1_s;
endclass

// https://github.com/universal-ctags/ctags/issues/
typedef enum logic {
    xxx,
    yyy
`ifdef foo
    ,
    zzz = 'x
`endif
} ifdef1_e;

typedef enum logic {
    aaa,
    bbb
`ifdef bar
    ,ccc
`endif
} ifdef2_e;
