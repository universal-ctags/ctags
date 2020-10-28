//
// block test
//

// LRM 9.3.2 Parallel blocks
module test;
  logic r;
  initial begin
    fork
      #50 r = 'h35;
      #100 r = 'hE2;
      #150 r = 'h00;
      #200 r = 'hF7;
    join
  end

  event enable_a, enable_b;
  logic wa, wb;
  time  ta, tb;
  initial
    fork: fork_1
      @enable_a
      begin
        #ta wa = 0;
        #ta wa = 1;
        #ta wa = 0;
      end
      @enable_b
      begin
        #tb wb = 1;
        #tb wb = 0;
        #tb wb = 1;
      end
    join
endmodule

// LRM 9.3.4 Block names

// orignal tests
module block1 #(N_IF = 8) (
  input logic clk, rst_n
);
  generate for (genvar gi = 0; gi < N_IF; ++gi) begin : b_g1

    always_ff @(posedge clk, negedge rst_n) begin
      logic a;
      if (~rst_n) begin
        ;
      end
    end
    logic var_b_g;

  end endgenerate

  for (genvar gi = 0; gi < N_IF; ++gi) begin : b_g2

    always_comb begin:b1
        logic lb1;
        begin :b2_1
          logic lb2;
        end:b2_1
        begin :b2_2
          logic lb2;
        end:b2_2
    end : b1
    logic var_b_g;

  end : b_g2

endmodule : block1

// fixed by sv-kind-fixes
class nested_block;
  function void func (input logic a, b);
    if (a) begin : outer_block
      if (b) begin : inner_block
        ;
      end : inner_block
    end // no block label
  endfunction

  logic p; // class:nested_block
endclass : nested_block
