// LRM 6.3 Data types
module net_decl;
  // 6.3.2.1 Charge strength
  trireg a;
  trireg (large) #(0,0,50) cap1;
  trireg (small) signed [3:0] cap2;

  // 6.5 Nets and variables
  wire w = vara & varb; // net with a continuous assignment
  logic v = consta & constb; // variable with initialization
  logic vw; // no initial assignment
  assign vw = vara & varb; // continuous assignment to a variable
  real circ;
  assign circ = 2.0 * PI * R; // continuous assignment to a variable

  // 6.6 Net types
  // 6.6.7 User-defined nettypes
  // add later !!!
endmodule

// 6.6.8 Generic interconnect
module top();
  interconnect [0:1] iBus;
  lDriver l1(iBus[0]);
  rDriver r1(iBus[1]);
  rlMod m1(iBus);
endmodule : top

module rlMod(input interconnect [0:1] iBus);
  lMod l1(iBus[0]);
  rMod r1(iBus[1]);
endmodule : rlMod

// 6.7 Net declarations
module Net_declarations;
  // 6.7.1 Net declarations with built-in net types
  trireg (large) logic #(0,0,0) cap1;
  typedef logic [31:0] addressT;
  wire addressT w1;
  wire struct packed { logic ecc; logic [7:0] data; } memsig;

  wire w; // equivalent to "wire logic w;"
  wire logic w;
  wire [15:0] ww; // equivalent to "wire logic [15:0] ww;"
  wire logic [15:0] ww;

  interconnect w1;              // legal
  interconnect [3:0] w2;        // legal
  interconnect [3:0] w3 [1:0];  // legal

  // 6.7.2 Net declarations with user-defined nettypes
  nettype T wT;
  nettype T wTsum with Tsum;
  wT w1;
  wT w2[8];
  wTsum w3;
  wTsum w4[8];
  typedef real TR[5];
  nettype TR wTR;
  wTR w5;
  wTR w6[8];
endmodule

// 6.8 Variable declarations
module Variable_declarations;
  shortint s1, s2[0:9];

  var v; // equivalent to "var logic v;"
  var [15:0] vw; // equivalent to "var logic [15:0] vw;"
  var enum bit { clear, error } status;
  input var logic data_in;
  var reg r;

  int i = 0;
endmodule

// 6.9 Vector declarations
module Vector_declarations;
  // 6.9.1 Specifying vectors
  wand w; // a scalar "wand" net
  tri [15:0] busa; // a 16-bit bus
  trireg (small) storeit; // a charge storage node of strength small
  logic a; // a scalar variable
  logic[3:0] v; // a 4-bit vector made up of (from most to
  // least significant)v[3], v[2], v[1], and v[0]
  logic signed [3:0] signed_reg; // a 4-bit vector in range -8 to 7
  logic [-1:4] b; // a 6-bit vector
  wire w1, w2; // declares two nets
  logic [4:0] x, y, z; // declares three 5-bit variables

  // 6.9.2 Vector net accessibility
  tri1 scalared [63:0] bus64; //a bus that will be expanded
  tri vectored [31:0] data; //a bus that may or may not be expanded
endmodule

// 6.16 String data type
module String_data_type;
  parameter string default_name = "John Smith";
  string myName = default_name;

  byte c = "A"; // assigns to c "A"
  bit [10:0] b = "\x41"; // assigns to b 'b000_0100_0001
  bit [1:4][7:0] h = "hello" ; // assigns to h "ello"

  string s0 = "String literal assign";// sets s0 to "String literal assign"
  string s1 = "hello\0world"; // sets s1 to "helloworld"
  bit [11:0] b = 12'ha41;
  string s2 = string'(b); // sets s2 to 16'h0a41

  typedef logic [15:0] r_t;
  r_t r;
  integer i = 1;
  string b = "";
  string a = {"Hi", b};
  r = r_t'(a); // OK
  b = string'(r); // OK
  b = "Hi"; // OK
  b = {5{"Hi"}}; // OK
  a = {i{"Hi"}}; // OK (non-constant replication)
  //r = {i{"Hi"}}; // invalid (non-constant replication)
  a = {i{b}}; // OK
  a = {a,b}; // OK
  a = {"Hi",b}; // OK
  r = {"H",""}; // yields "H\0". "" is converted to 8'b0
  b = {"H",""}; // yields "H". "" is the empty string
  a[0] = "h"; // OK, same as a[0] = "cough"
  //a[0] = b; // invalid, requires a cast
  a[1] = "\0"; // ignored, a is unchanged
endmodule

// 6.17 Event data type
module event_data_type;
  event done; // declare a new event called done
  event done_too = done; // declare done_too as alias to done
  event empty = null; // event variable with no synchronization object
endmodule

// 6.18 User-defined types
module user_define_types_0;
  typedef int intP;
  intP a, b;
endmodule

interface intf_i;
  typedef int data_t;
endinterface

// interface based typedef
module sub(intf_i p);
  typedef p.data_t my_data_t;
  my_data_t data;
endmodule

// interface based typedef with constant bit select
module cbs;
  typedef p[10].data_t cbs_t;
endmodule

module user_define_types_1;
  // forward typedef
  typedef enum type_identifier;
  typedef struct type_identifier;
  typedef union type_identifier;
  typedef class type_identifier;
  typedef interface class type_identifier;
  typedef type_identifier;
endmodule

// 6.19 Enumerations
module enum_test;
  enum {red, yellow, green} light1, light2; // anonymous int type
  // Syntax error: IDLE=2'b00, XX=2'bx <ERROR>, S1=2'b01, S2=2'b10
  //enum bit [1:0] {IDLE, XX='x, S1=2'b01, S2=2'b10} state, next;
  // Correct: IDLE=0, XX='x, S1=1, S2=2
  enum integer {IDLE, XX='x, S1='b01, S2='b10} state, next;
  // Syntax error: IDLE=0, XX='x, S1=??, S2=??
  //enum integer {IDLE, XX='x, S1, S2} state, next;
  enum {bronze=3, silver, gold} medal; // silver=4, gold=5

  // Correct declaration - bronze and gold are unsized
  enum bit [3:0] {bronze='h3, silver, gold='h5} medal2;
  // Correct declaration - bronze and gold sizes are redundant
  enum bit [3:0] {bronze=4'h3, silver, gold=4'h5} medal3;

  // 6.19.1 Defining new data types as enumerated types
  typedef enum {NO, YES} boolean;
  boolean myvar; // named type

  // 6.19.2 Enumerated type ranges
  typedef enum { add=10, sub[5], jmp[6:8] } E1; // FIXME
  enum { register[2] = 1, register[2:4] = 10 } vr; // FIXME

  // original
  enum logic signed [3:0] { foo, bar } [1:0] cmplx_enum1; 
  enum logic unsigned [3:0] { foo, bar } [] cmplx_enum2; 

endmodule

// 9.4.1 Delay control
module delay_control #(d, e);
  int rega, regb, regr;
  initial begin
    #10 rega = regb;
    #d rega = regb; // d is defined as a parameter
    #((d+e)/2) rega = regb; // delay is average of d and e
    #regr regr = regr + 1; // delay is the value in regr
  end
endmodule

// 10.3 Continuous assignments
module delay_control_wire #(d, e);
  wire wirea #10 = wireb;
  wire wireb #d = wireb;
  wire wirec #((d+e)/2) = wireb;
  wire wired #wirer = wirer + 1;
  wire w$ire, wire$;  // '$' included
endmodule

// orignal : LRM 5.8 Time literals
module rst;
  logic trst_n;
  initial begin
    #10.5fs trst_n = 1'b0;
    #10ps   trst_n = 1'b1;
    #10ns   trst_n = 1'b0;
    #10us   trst_n = 1'b1;
    #10ms   trst_n = 1'b0;
    #10s    trst_n = 1'b1;
  end
endmodule
