//
// Clocking blocks tests
//

// LRM 14.3 Clocking block declaration
interface foo_bus;
  logic clock1;
  logic data, ready, enable, ack, addr;

  clocking bus @(posedge clock1);
    default input #10ns output #2ns;
    input data, ready, enable = top.mem1.enable;
    output negedge ack;
    input #1step addr;
  endclocking
endinterface

// 14.4 Input and output skews
module foo_14_4;
  logic clk, address, data;
  clocking dram @(clk);
    input #1ps address;
    input #5 output #6 data;
  endclocking
endmodule

// 14.8 Multiple clocking blocks example
program test( input phi1, input [15:0] data, output logic write,
              input phi2, inout [8:1] cmd, input enable
);
  reg [8:1] cmd_reg;
  clocking cd1 @(posedge phi1);
    input data;
    output write;
    input state = top.cpu1.state;
  endclocking

  clocking cd2 @(posedge phi2);
    input #2 output #4ps cmd;
    input enable;
  endclocking

  initial begin
    // program begins here
    // ...
    // user can access cd1.data , cd2.cmd , etc…
  end

  assign cmd = enable ? cmd_reg: 'x;
endprogram

// 14.14 Global clocking
module top;
logic clk1, clk2;
  global clocking sys @(clk1 or clk2); endclocking
  // ...
endmodule
