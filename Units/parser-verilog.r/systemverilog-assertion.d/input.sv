initial begin : deferred_immediate_assertions
    immediate_assertion : assert () myTask();
    immediate_cover     : cover () myTask();
    immediate_assume    : assume () myTask();
    deferred_assertion1 : assert #0 () myTask();
    deferred_cover1     : cover #0 () myTask();
    deferred_assume1    : assume #0 () myTask();
    deferred_assertion2 : assert final () myTask();
    deferred_cover2     : cover final () myTask();
    deferred_assume2    : assume final () myTask();
end

property prop1 (
    local input int m,
    logic [1:0] n,
    int o
);

endproperty :prop1

property prop2 (a, b);

endproperty : prop2

concurrent_assertion1   : assert property prop2 (l, m);

module assert_test;
    // 16.3 Immediate assertions
    always_comb begin
        assert_f: assert(f) $info("passed"); else $error("failed");
        assume_inputs: assume (in_a || in_b) $info("assumption holds");
        else $error("assumption does not hold");
        cover_a_and_b: cover (in_a && in_b) $info("in_a && in_b == 1 covered");
    end

    time t;
    always @(posedge clk)
        if (state == REQ)
            assert (req1 || req2)
            else begin
                t = $time;
                #5 $error("assert failed at time %0t",t);
            end

    always @(posedge clk) begin
        assert (myfunc(a,b)) count1 = count + 1; else ->event1;
        assert (y == 0) else flag = 1;
    end

    // 16.4.2 Deferred assertion flush points
    assign not_a = !a;
    always_comb begin : b1
        a1: assert (not_a != a);
        a2: assert #0 (not_a != a); // Should pass once values have settled
    end

    assign a = ...;
    assign b = ...;
    always_comb begin : b1
        c1: cover (b != a);
        c2: cover #0 (b != a);
    end

    function int error_type (int opcode);
        func_assert: assert (opcode < 64) else $display("Opcode error.");
        if (opcode < 32)
            return (0);
        else
            return (1);
        endfunction

        always_comb begin : b1
            a1: assert #0 (my_cond) else
            $error("Error on operation of type %d\n", error_type(opcode));
            a2: assert #0 (my_cond) else
            error_type(opcode);
            ...
        end
endmodule

module dut(input logic clk, input logic a, input logic b);
    logic c;
    always_ff @(posedge clk)
        c <= b;
    a1: assert #0 (!(a & c)) $display("Pass"); else $display("Fail");
    a2: assert final (!(a & c)) $display("Pass"); else $display("Fail");
endmodule

program tb(input logic clk, output logic a, output logic b);
    default clocking m @(posedge clk);
        default input #0;
        default output #0;
        output a;
        output b;
    endclocking

    initial begin
        a = 1;
        b = 0;
        ##10;
        b = 1;
        ##1;
        a = 0;
    end
endprogram

module sva_svtb;
    bit clk;
    logic a, b;
    // ...
    dut dut (.*);
    tb tb (.*);
endmodule

// 16.4.3 Deferred assertions outside procedural code
module m (input a, b);
    a1: assert #0 (a == b);
endmodule

// This is equivalent to the following:
module m (input a, b);
    always_comb begin
        a1: assert #0 (a == b);
    end
endmodule 

// 16.4.4 Disabling deferred assertions
module m (input bad_val, bad_val_ok, a, b, c, clear_b2);
    always @(bad_val or bad_val_ok) begin : b1
        a1: assert #0 (bad_val) else $fatal(1, "Sorry");
        if (bad_val_ok) begin
            disable a1;
        end
    end

    always @(a or b or c) begin : b2
        if (c == 8'hff) begin
            a2: assert #0 (a && b);
        end else begin
         a3: assert #0 (a || b);
        end
    end

    always @(clear_b2) begin : b3
        disable b2;
    end
endmodule

// 16.5 Concurrent assertions overview
// 16.6 Boolean expressions
module m;
    bit a;
    integer b;
    byte q[$];
    property p1;
        $rose(a) |-> q[0];
    endproperty
    property p2;
        integer l_b;
        ($rose(a), l_b = b) |-> ##[3:10] q[l_b];
    endproperty

    bit [2:0] count;
    realtime t;
    initial count = 0;
    always @(posedge clk) begin
        if (count == 0) t = $realtime; //capture t in a procedural context
        count++;
    end

    property p1;
        @(posedge clk)
        count == 7 |-> $realtime – t < 50.5;
    endproperty

    property p2;
        realtime l_t;
        @(posedge clk)
        (count == 0, l_t = $realtime) ##1 (count == 7)[->1] |->
        $realtime – l_t < 50.5;
    endproperty
endmodule

// 16.7 Sequences
module m;
    sequence delay_example(x, y, min, max, delay1);
        x ##delay1 y[*min:max];
    endsequence
    // Legal
    a1: assert property (@(posedge clk) delay_example(x, y, 3, $, 2));

    int z, d;
    // Illegal: z and d are not elaboration-time constants
    // a2_illegal: assert property (@(posedge clk) delay_example(x, y, z, $, d));

    sequence s1;
        @(posedge clk) a ##1 b ##1 c;
    endsequence
    sequence s2;
        @(posedge clk) d ##1 e ##1 f;
    endsequence
    sequence s3;
        @(negedge clk) g ##1 h ##1 i;
    endsequence
    sequence s4;
        @(edge clk) j ##1 k ##1 l;
    endsequence

    sequence s20_1(data,en);
        (!frame && (data==data_bus)) ##1 (c_be[0:3] == en);
    endsequence

    sequence s;
        a ##1 b ##1 c;
    endsequence
    sequence rule;
        @(posedge sysclk)
        trans ##1 start_trans ##1 s ##1 end_trans;
    endsequence

    sequence rule;
        @(posedge sysclk)
        trans ##1 start_trans ##1 (a ##1 b ##1 c) ##1 end_trans ;
    endsequence
    // sequence s1;
    //     @(posedge sysclk) (x ##1 s2);
    // endsequence
    // sequence s2;
    //     @(posedge sysclk) (y ##1 s1);
    // endsequence
endmodule

// 16.8.1 Typed formal arguments in sequence declarations
module m;
    sequence s1(w, x, y);
        w ##1 x ##[2:10] y;
    endsequence
    sequence s2(w, y, bit x);
        w ##1 x ##[2:10] y;
    endsequence

    s1(.w(a), .x(bit'(b)), .y(c));
    s2(.w(a), .x(b), .y(c));

    sequence delay_arg_example (max, shortint delay1, delay2, min);
        x ##delay1 y[*min:max] ##delay2 z;
    endsequence

    parameter my_delay=2;
    cover property (delay_arg_example($, my_delay, my_delay-1, 3));

    cover property (x ##2 y[*3:$] ##1 z);

    sequence event_arg_example (event ev);
        @(ev) x ##1 y;
    endsequence
    cover property (event_arg_example(posedge clk));

    cover property (@(posedge clk) x ##1 y));

    sequence event_arg_example2 (reg sig);
        @(posedge sig) x ##1 y;
    endsequence
    cover property (event_arg_example2(clk));

    cover property (@(posedge clk) x ##1 y));

    sequence s(bit a, bit b);
        bit loc_a;
        (1'b1, loc_a = a) ##0
        (t == loc_a) [*0:$] ##1 b;
    endsequence
endmodule

// 16.10 Local variables
module m;
    sequence s;
        logic u, v = a, w = v || b;
        // ...
    endsequence

    property e;
        int x;
        (valid_in, x = pipe_in) |-> ##5 (pipe_out1 == (x+1));
    endproperty
endmodule

// 16.12 Declaring properties
module m;
    // 16.12.2 Sequence property
    property p3;
        b ##1 c;
    endproperty
    c1: cover property (@(posedge clk) a #-# p3);
    a1: assert property (@(posedge clk) a |-> p3);
endmodule

// 16.13.7 Local variable initialization assignments
module m;
    property p;
        logic v = e;
        (@(posedge clk1) (a == v)[*1:$] |-> b)
        and
        (@(posedge clk2) c[*1:$] |-> d == v)
        ;
    endproperty
    a1: assert property (@(posedge clk) f |=> p);

    property p;
        logic v;
        (@(posedge clk1) (1, v = e) ##0 (a == v)[*1:$] |-> b)
        and
        (@(posedge clk2) (1, v = e) ##0 c[*1:$] |-> d == v)
        ;
    endproperty
endmodule

// 16.14 Concurrent assertions
module m;
    // 16.14.1 Assert statement
    property abc(a, b, c);
        disable iff (a==2) @(posedge clk) not (b ##1 c);
    endproperty
    env_prop: assert property (abc(rst, in1, in2))
    $display("env_prop passed."); else $display("env_prop failed.");

    // 16.14.2 Assume statement
    property abc(a, b, c);
        disable iff (c) @(posedge clk) a |=> b;
    endproperty
    env_prop:
        assume property (abc(req, gnt, rst)) else $error(”Assumption failed.”);

    a1:assume property ( @(posedge clk) req dist {0:=40, 1:=60} ) ;
    property proto ;
        @(posedge clk) req |-> req[*1:$] ##0 ack;
    endproperty

    a1_assertion:assert property ( @(posedge clk) req inside {0, 1} ) ;
    property proto_assertion ;
        @(posedge clk) req |-> req[*1:$] ##0 ack;
    endproperty

    // 16.14.3 Cover statement
    // 16.14.4 Restrict statement
    restrict property (@(posedge clk) ctr == '0);
endmodule

// 16.17 Expect statement
program tst;
    initial begin
        # 200ms;
        expect( @(posedge clk) a ##1 b ##1 c ) else $error( "expect failed" );
        ABC: ; // ...
    end

    integer data;
    //...
    task automatic wait_for( integer value, output bit success );
        expect( @(posedge clk) ##[1:10] data == value ) success = 1;
        else success = 0;
    endtask

    initial begin
        bit ok;
        wait_for( 23, ok ); // wait for the value 23
        // ...
    end
endprogram

// 16.18 Clocking blocks and concurrent assertions
module A;
    logic a, clk;

    clocking cb_with_input @(posedge clk);
        input a;
        property p1;
            a;
        endproperty
    endclocking

    clocking cb_without_input @(posedge clk);
        property p1;
            a;
        endproperty
    endclocking

    property p1;
        @(posedge clk) a;
    endproperty
    
    property p2;
        @(posedge clk) cb_with_input.a;
    endproperty

    a1: assert property (p1);
    a2: assert property (cb_with_input.p1);
    a3: assert property (p2);
    a4: assert property (cb_without_input.p1);
endmodule

// original test
// excerpt from UVM 1.2:examples/integrated/ubus/sv/ubus_slave_monitor.sv
class C;
  protected function void check_transfer_size();
    assert_transfer_size : assert(trans_collected.size == 1) else begin
      `uvm_error(get_type_name(),
        "Invalid transfer size!")
    end
  endfunction : check_transfer_size

  // check_transfer_data_size
  protected function void check_transfer_data_size();
  endfunction : check_transfer_data_size
