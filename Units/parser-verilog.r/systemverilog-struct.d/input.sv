//
// LRM 7. Aggregate data types
//

// 7.2 Structures
class S;
  struct { bit [7:0] opcode; bit [23:0] addr; }IR;  // anonymous structure
                                                    // defines variable IR
  function foo;
    IR.opcode = 1;    // set field in IR.
  endfunction
  typedef struct {
    bit [7:0] opcode;
    bit [23:0] addr;
  } instruction;      // named structure type
  instruction IR1;    // define variable

  // 7.2.1 Packed structures
  struct packed signed {
    int a;
    shortint b;
    byte c;
    bit [7:0] d;
  } pack1; // signed, 2-state

  struct packed unsigned {
    time a;
    integer b;
    logic [31:0] c;
  } pack2; // unsigned, 4-state

  typedef struct packed { // default unsigned
    bit [3:0] GFC;
    bit [7:0] VPI;
    bit [11:0] VCI;
    bit CLP;
    bit [3:0] PT ;
    bit [7:0] HEC;
    bit [47:0] [7:0] Payload;
    bit [2:0] filler;
  } s_atmcell;

  // 7.2.2 Assigning to structures
  typedef struct {
    int addr = 1 + constant;
    int crc;
    byte data [4] = '{4{1}};
  } packet1;

  packet1 pi = '{1,2,'{2,3,4,5}}; //suppresses the typedef initialization
endclass

// 7.3 Unions
class U;
  typedef union { int i; shortreal f; } num;  // named union type
  num n;
  n.f = 0.0;  // set n in floating point format

  typedef struct {
    bit isfloat;
    union { int i; shortreal f; } n;  // anonymous union type
  } tagged_st;                        // named structure

  // 7.3.1 Packed unions
  typedef union packed { // default unsigned
    s_atmcell acell;
    bit [423:0] bit_slice;
    bit [52:0][7:0] byte_slice;
  } u_atmcell;

  u_atmcell u1;
  byte b; bit [3:0] nib;
  b = u1.bit_slice[415:408];    // same as b = u1.byte_slice[51];
  nib = u1.bit_slice [423:420]; // same as nib = u1.acell.GFC;

  // 7.3.2 Tagged unions
  // FIXME: TBD
endclass

// orignal
class O;
  struct { bit [7:0] opcode; }IR;
  struct packed signed { int a; } pack1;
  struct packed unsigned { int a;} pack2;
  union packed unsigned { logic [7:0] a; } union1;
  struct packed signed { int a; } [3:0] pack3, pack4;

  // complex enum
  typedef logic user_t;
  enum user_t [1:0] { FOO, BAR, BAZ } [3:0] enum00_e, enum01_e ;
  enum logic unsigned{A,B,C}[1:0]enum10_e,enum11_e;
  typedef enum user_t [1:0] { FOO, BAR, BAZ } [3:0] enum0_t ;
  typedef enum logic unsigned{A,B,C}[1:0]enum1_t;

  // complex struct
  struct packed signed {
    logic a,b ;
    logic [15:0] [7:0] s0 , s1;
    struct {
      logic x, y; // not emitted
    } [15:0] [7:0] struct0_s, struct1_s;
    enum user_t [1:0] { FOO, BAR, BAZ } [3:0] enum00_e, enum01_e ;
    enum logic unsigned{A,B,C}[1:0]enum10_e,enum11_e;
    bit[7:0][1:0]d,e;
  } [1:0] complex0_s, complex1_s;

  // complex typedef of struct
  typedef struct packed signed {
    logic a,b ;
    logic [15:0] [7:0] s0 , s1;
    struct {
      logic x, y; // not emitted
    } [15:0] [7:0] struct0_s, struct1_s;
    enum user_t [1:0] { FOO, BAR, BAZ } [3:0] enum00_e, enum01_e ;
    enum logic unsigned{A,B,C}[1:0]enum10_e,enum11_e;
    bit[7:0][1:0]d,e;
  } [1:0] complex_t;
endclass
