
// ---------------------------------------------------------------------------
// TEST CASE for `processFunction()` to parse `function` and `task` body
// ---------------------------------------------------------------------------

package new_test_case; // for better readability

  // 1. Normal function.
  // -----------------------------------------------------------------
  function signed test_func_A();
  endfunction
  task test_task_A; endtask

  // 2. Normal function return class type.
  // -----------------------------------------------------------------
  // 2.1 return Normal class.
  function not_care_class test_func_B();
  endfunction
  // 2.2 return parameterized class without space.
  function not_care_class#(int) test_func_C  ();
  endfunction
  // 2.3 return parameterized class with space.
  function not_care_class #(int) test_func_D (   );
  endfunction
  // 2.4 return parameterized class with parameter.
  function not_care_class #( .T(int) ) test_func_E();
  endfunction

  // 3. The prototype of the class is declared in another file,
  //    and the specific implementation is here.
  // -----------------------------------------------------------------
  // 3.1 Normal function.
  function unsigned [7:0] class_scope::test_func_F();
  endfunction
  // 3.2 return Normal class.
  function not_care_class class_scope::test_func_G();
  endfunction
  // 3.3 return parameterized class without space.
  function not_care_class#(IF) class_scope::test_func_H();
  endfunction
  // 3.4 return parameterized class with space.
  function not_care_class  #(IF)  class_scope::test_func_I;
  endfunction
  // 3.5 return parameterized class with parameter.
  function not_care_class#(.T(real), .size(4)) class_scope::test_func_J;
  endfunction
  // 3.6 task or new function
  function class_scope::new;
  endfunction
  task class_scope::test_task_B ()  ;
  endtask

  // 4. Normal function: return type from a class ( with `::` ).
  // -----------------------------------------------------------------
  // 4.1 from a Normal class
  function not_care_class::TYPE test_func_K ();
  endfunction
  // Subsequent parsing is not affected.
  function automatic bit signed [15:0] test_func_L();
  endfunction
  // 4.2 from a parameterized class
  function not_care_class#(bit [2:0]) :: TYPE test_func_M;
  endfunction
  function not_care_class  #(.T(bit [2:0]))::TYPE test_func_N;
  endfunction
  function not_care_class # (100)  ::TYPE test_func_O  ()  ;
  endfunction
  // Subsequent parsing is not affected.
  int test_var_A = 100;
  function static bit test_func_P; endfunction
  // 4.3 task or new function
  function new; endfunction
  task test_task_C; endtask

  // 5. function with class_scope && return type with class_scope
  // -----------------------------------------------------------------
  // 5.1 from a Normal class
  function not_care_class :: TYPE scope::test_func_Q;
  endfunction
  // Subsequent parsing is not affected
  function void test_func_R ();
  endfunction
  // 5.2 from a parameterized class
  function not_care_class#(shortint)::TYPE scope::test_func_S (  );
  endfunction
  function not_care_class # ( .IF( IF ) )::TYPE scope:: test_func_T;
  endfunction
  // Subsequent parsing is not affected
  longint test_var_B = 1024;
  task test_task_D (  );
  endtask
  function void test_func_U; endfunction
  // 5.3 task or new function
  function scope::new ();
  endfunction
  task scope:: test_task_E;
  endtask

  // 6. Corner TEST
  // -----------------------------------------------------------------
  localparam LOW = 8;
  localparam HIGH = 15;
  function automatic logic [ LOW : HIGH ] test_func_V ();
  endfunction
  function automatic int signed [LOW-1:HIGH-1] test_func_W;
  endfunction
  function automatic A:: B#(IF) ::TYPE test_func_EA ();
  endfunction
  function automatic unsigned [ LOW -1: HIGH -1] scope:: test_func_EB;
  endfunction
  task static A :: B :: C # (.T (int)) :: test_wrong_task ();
  endtask
  function static A::B::TYPE scope # ( .T (IF) ) :: test_wrong_function;
  endfunction
  function :initial void test_specifier_A (); endfunction
  function :extends void test_specifier_B (); endfunction
  function :final   void test_specifier_C (); endfunction
  function : initial : final void test_specifier_D (); endfunction
  function : extends : final void test_specifier_E (); endfunction
  function : final : initial void test_specifier_F (); endfunction
  function : final : extends void test_specifier_G (); endfunction
  virtual function : initial A :: B :: TYPE test_specifier_H ();
  endfunction
  virtual function :extends :final A::B::C A::B::test_specifier_I;
  endfunction
  task :initial :final A :: B :: test_specifier_J ;
  endtask
  task :final : extends A #(IF)::B ::test_specifier_K () ;
  endtask
  function corner_A::corner_B :: new (  );
  endfunction
  function A::B::C::D #(int) corner_A:: test_func_XA();
  endfunction
  function void corner_A # ( .T(real) ) :: test_func_XB;
  endfunction
  function A # ( IF ) :: B :: C corner_A :: test_func_XC ();
  endfunction
  function A :: B # ( 100 )::C::D#(int) test_func_XD;
  endfunction
  task A::B::C::test_task_F ;
  endtask
  function void test_func_Y ( int Y_in_A, int Y_in_B );
  endfunction
  task A #(real):: B#(IF)::C#( .size(64) ) :: D :: test_task_G ();
  endtask
  int test_var_C = 1024;
  function logic [LOW:HIGH] test_func_Z ();
  endfunction
  class test_class;
    static function A::B::C::D test_func_EC () ;
    endfunction
    extern static task test_task_EB ();
    extern static function A :: B :: C :: D test_func_ED ();
  endclass
  static task test_class::test_task_EB ; endtask
  static function A:: B ::C::D  test_class :: test_func_ED;
  endfunction

endpackage : new_test_case

// ---------------------------------------------------------------------------
// REF : https://github.com/universal-ctags/ctags/issues/4109
// ---------------------------------------------------------------------------

virtual class uvm_component extends uvm_report_object;
  typedef bit [1:0] config_mode_t;
  extern virtual function void test_ok();
  // ------------------------------------------------------
  extern virtual function config_mode_t return_scope_res();
  // ------------------------------------------------------
  extern function void m_set_cl_msg_args();
  extern function void m_set_cl_verb;
  extern function void m_set_cl_action;
  extern function void m_set_cl_sev;
endclass

function void uvm_component::test_ok();
  // function body, this function is parsed OK
endfunction

// ---------------------------------------------------------------------
function uvm_component::config_mode_t uvm_component::return_scope_res();
  return 2'b00;
endfunction
// ---------------------------------------------------------------------

int test_scope_variable;

function void uvm_component::m_set_cl_msg_args();
endfunction
function void uvm_component::m_set_cl_verb; return; endfunction
function void uvm_component::m_set_cl_action; return; endfunction
function void uvm_component::m_set_cl_sev; return; endfunction

package test_case_A;
  typedef class extern_class;

  function void test_ok;
  endfunction

  function extern_class::data_type oops();
  endfunction

  function extern_class#(int)::data_type OOPS();
  endfunction

  function void still_ok();
  endfunction
endpackage

package test_case_B;
  function void foo; // OK
    // something
  endfunction

  // return type is a parameterized class, OK
  function uvm_queue#(uvm_callback) test_ok(uvm_object obj);
    return null;
  endfunction

  // OK, even if there is a space between `uvm_queue` and `#(`
  function uvm_queue #(uvm_callback) TEST_OK(uvm_object obj);
    return null;
  endfunction

  // Subsequent parsing is not affected
  function void bar(); // OK
    // something
  endfunction

  // ----------------------------------------------------------------
  function uvm_queue #(uvm_callback)::data_type oops(uvm_object obj);
    int test_scope_var = 1;
    return null;
  endfunction
  // ----------------------------------------------------------------

  function void still_ok();
    // function body
  endfunction
endpackage
