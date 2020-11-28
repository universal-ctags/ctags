// disabled K_PROTOTYPE: don't use "--extras=+q" for coverage
class C;
  extern function ext_func ();
  pure virtual task ext_pure_virt_task (x);
  typedef class   fwd_type_class;
endclass

// from UVM-1.2
package foo;
  import "DPI-C" context function int import_func (string str);
  typedef logic uvm_object;
  function logic bar (uvm_object baz);
    return 1'b1;
  endfunction
endpackage
