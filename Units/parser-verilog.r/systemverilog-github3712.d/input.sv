// https://github.com/universal-ctags/ctags/issues/3712

`define XXX_TOP  foo_top
`define XXX          foo
`define xxx_begin
`define xxx_end

module `XXX ();
`define add_t(f) f``_t
endmodule

module `XXX_TOP();
  `XXX U_XXX ();  // cannot be detected
endmodule

class foo;
  // from a design pattern in UVM
  `xxx_begin(bar) // must be ignored
  `xxx_end        // must be ignored

  function new(string name="...");
    super.new(name);
  endfunction : new

  // from systemverilog-directive.d
  var `add_t(foo) = '0;
endclass : foo
