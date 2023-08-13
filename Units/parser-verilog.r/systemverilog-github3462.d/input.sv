// SystemVerilog: "assertions" parsing error when encountered "::" #3462
// https://github.com/universal-ctags/ctags/issues/3462

module foo;
  import uvm_pkg::*;
  always_comb begin
    // something
    assert (foo) else $error("failed");
    // something
    someclass::method();
    // something
    assert (bar) else $error("failed");
  end
endmodule

package foo;
  import "DPI-C" context function int import_func (string str);
endpackage
