//
// Checker tests
//

// LRM 17 Checkers
// 17.2 Checker declaration
// Simple checker containing concurrent assertions
checker my_check1 (logic test_sig, event clock);
    default clocking @clock; endclocking
    property p(logic sig);
        // ...
    endproperty
    a1: assert property (p (test_sig));
    c1: cover property (!test_sig ##1 test_sig);
endchecker : my_check1

// Simple checker containing deferred assertions
checker my_check2 (logic a, b);
    a1: assert #0 ($onehot0({a, b}));
    c1: cover #0 (a == 0 && b == 0);
    c2: cover #0 (a == 1);
    c3: cover #0 (b == 1);
endchecker : my_check2

// Simple checker with output arguments
checker my_check3 (logic a, b, event clock, output bit failure, undef);
    default clocking @clock; endclocking
    a1: assert property ($onehot0({a, b})) failure = 1'b0; else failure = 1'b1;
    a2: assert property ($isunknown({a, b})) undef = 1'b0; else undef = 1'b1;
endchecker : my_check3

// Checker with default input and initialized output arguments
checker my_check4 (input logic in,
                   en = 1'b1, // default value
                   event clock,
                   output int ctr = 0); // initial value
    default clocking @clock; endclocking
    always_ff @clock
        if (en && in) ctr <= ctr + 1;
    a1: assert property (ctr < 5);
endchecker : my_check4

module m;
    default clocking @clk1; endclocking
    default disable iff rst1;
    checker c1;
        // Inherits @clk1 and rst1
        // ...
    endchecker : c1
    checker c2;
        // Explicitly redefines its default values
        default clocking @clk2; endclocking
        default disable iff rst2;
        // ...
    endchecker : c2
    ...
endmodule : m

// 17.3 Checker instantiation
checker mutex (logic [31:0] sig, event clock, output bit failure);
    assert property (@clock $onehot0(sig))
    failure = 1'b0; else failure = 1'b1;
endchecker : mutex

module m(wire [31:0] bus, logic clk);
    logic res, scan;
    // ...
    mutex check_bus(bus, posedge clk, res);
    always @(posedge clk) scan <= res;
endmodule : m


checker c1(event clk, logic[7:0] a, b);
    logic [7:0] sum;
    always_ff @(clk) begin
        sum <= a + 1'b1;
        p0: assert property (sum < `MAX_SUM);
    end
    p1: assert property (@clk sum < `MAX_SUM);
    p2: assert property (@clk a != b);
    p3: assert #0 ($onehot(a));
endchecker

module m(input logic rst, clk, logic en, logic[7:0] in1, in2,
         in_array [20:0]);
    c1 check_outside(posedge clk, in1, in2);
    always @(posedge clk) begin
        automatic logic [7:0] v1=0;
        if (en) begin
            // v1 is automatic, so current procedural value is used
            c1 check_inside(posedge clk, in1, v1);
        end
        for (int i = 0; i < 4; i++) begin
            v1 = v1+5;
            if (i != 2) begin
                // v1 is automatic, so current procedural value is used
                c1 check_loop(posedge clk, in1, in_array[v1]);
            end
        end
    end
endmodule : m

// 17.7 Checker variables
checker counter_model(logic flag);
    bit [2:0] counter = '0;
    always_ff @$global_clock
        counter <= counter + 1'b1;
    assert property (@$global_clock counter == 0 |-> flag);
endchecker : counter_model

checker observer_model(bit valid, reset);
    default clocking @$global_clock; endclocking
    rand bit flag;

    m1: assume property (reset |=> !flag);
    m2: assume property (!reset && flag |=> flag);
    m3: assume property ($rising_gclk(flag) |-> valid);
endchecker : observer_model
