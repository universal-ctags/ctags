//
// from LRM-2017
//
module ctags_uni_issues (
  // 6.8 Variable declarations
  input var logic data_in // var:port, data_in:register => data_in:port
);

  const var foo;
  var bar;

  // 6.14 Chandle data type
  chandle variable_name ; // not defined => variable_name:register
endmodule

// 8. Classes
// 8.3
class x;
  rand var ra;
  randc var rb;
endclass

package foo;
  // 16.8 Declaring sequences
  // sequence is not supported
  sequence delay_example(x, y, min, max, delay1);
    x ##delay1 y[*min:max];
  endsequence
endpackage

// 23.2 Module definitions
module mh1 (
  input var int in1, // -> var:port, in1:register => in1:port
  output var int out2 // -> var:port, out2:register => out2:port
);
endmodule
