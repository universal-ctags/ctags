//
// parameter defintion tests
//

// LRM 8.25.1 Class scope resolution operator for parameterized classes
class C #(
  int p = 1
);
  parameter int q = 5;  // local parameter
  static task t;
    int p;
    int x = C::p;  // C::p disambiguates p
    // C::p is not p in the default specialization
  endtask
endclass

class C #(
    int p = 1,
    type T = int
);
  extern static function T f();
endclass

function C::T C::f();
  return p + C::p;
endfunction

// LRM 13.8 Parameterized tasks and functions
virtual class C#(parameter DECODE_W, parameter ENCODE_W = $clog2(DECODE_W));
  static function logic [ENCODE_W-1:0] ENCODER_f
      (input logic [DECODE_W-1:0] DecodeIn);
    ENCODER_f = '0;
    for (int i = 0; i < DECODE_W; i++) begin
      if (DecodeIn[i]) begin
        ENCODER_f = i[ENCODE_W - 1:0];
        break;
      end
    end
  endfunction
  static function logic [DECODE_W-1:0] DECODER_f
      (input logic [ENCODE_W-1:0] EncodeIn);
    DECODER_f = '0;
    DECODER_f[EncodeIn] = 1'b1;
  endfunction
endclass

// LRM 8.26 Interface classes
interface class PutImp #(type PUT_T = logic); // FIXME
  pure virtual function void put(PUT_T a);
endclass

interface class GetImp #(type GET_T = logic); // FIXME
  pure virtual function GET_T get();
endclass

class Fifo #(type T = logic, int DEPTH = 1) implements PutImp#(T), GetImp#(T);
  T myFifo[$:DEPTH-1];
  virtual function void put(T a); // FIXME : to be ignored
    myFifo.push_back(a);
  endfunction
  virtual function T get(); // FIXME : to be ignored
    get = myFifo.pop_front();
  endfunction
endclass

class Stack #(type T = logic, int DEPTH = 1) implements PutImp#(T), GetImp#(T);
  T myFifo[$:DEPTH-1];
  virtual function void put(T a); // FIXME : to be ignored
    myFifo.push_front(a);
  endfunction
  virtual function T get(); // FIXME : to be ignored
    get = myFifo.pop_front();
  endfunction
endclass

// 23.2.3 Parameterized modules
module generic_fifo
  #(parameter MSB=3, LSB=0, DEPTH=4) // these parameters can be redefined
   (input  wire [MSB:LSB] in,
    input  wire clk, read, write, reset,
    output logic [MSB:LSB] out,
    output logic full, empty );
    //...
endmodule

module generic_decoder
  #(num_code_bits = 3, localparam num_out_bits = 1 << num_code_bits)
   (input [num_code_bits-1:0] A, output reg [num_out_bits-1:0] Y);
endmodule

// additional test
typedef int int_t;
module user_defined_type_param
  #(int_t num_code_bits = 3, localparam num_out_bits = 1 << num_code_bits)
   (input [num_code_bits-1:0] A, output reg [num_out_bits-1:0] Y);
endmodule
