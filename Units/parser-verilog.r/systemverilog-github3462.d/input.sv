// SystemVerilog: "assertions" parsing error when encountered "::" #3462
// https://github.com/universal-ctags/ctags/issues/3462

import uvm_pkg::*;
// something
assert (foo) else do something;
// something
someclass::method();
// something
assert (bar) else do something;

package foo;
  import "DPI-C" context function int import_func (string str);
endpackage
