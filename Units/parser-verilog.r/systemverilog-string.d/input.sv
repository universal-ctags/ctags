package test_string;
  int    test_var_a = 1'b1;
  string test_str_a = "Hello World!\n";
  int    test_var_b = 100;
  string test_str_b = "\"";
  int    test_var_c = -1;

  // LRM 5.9.1 Special characters in strings
  // \n \t \\ \" \v \f \a \ddd \xdd ( Others: such as "\b" is treated the same as "b" )
  string test_str_c = "\b \n \t \\ \" \v \f \a \0 \1 \11 \377 \x0 \x01 \xff \e";

  int    test_var_d = 1024;
  string test_str_d = "\"\\\"";
  int    test_var_e = -1;
  string test_str_e = "\\";
  string test_str_f = "\\\\\\\\";
  string test_str_g = "\"\"\"";
  string test_str_h = "\"\"\"\"";
  string test_str_i = "\\\\\\\"";
  string test_str_j = "\"\\\\\\";
  string test_str_k = "\0\188\xf\"\\\"\\\"\\";

  localparam byte test_str_l = "\\" ;
  localparam byte test_str_m = "\"" ;
  parameter string test_str_n = "\t\n\\\"\v\f";

  struct {
    string name = "Hello \" World!";
    bit [23:0] addr;
  } test_struct_str;

  typedef struct {
    string name = "\0\\\"\\";
    bit [23:0] addr;
    string type_id = "\a\t\0\xddVerilog\"";
  } test_typedef_str;

  function void test_func_port (string test_str_port = "\\\"", int test_var_port=1);
  endfunction
  task test_task_port  (int test_int_port, string test_str_port="\"\\", byte test_byte_port = "\"");
  endtask

  // All tags above should NOT be missed
  bit [7:0] test_ending_var;
  string test_ending_str = "'\"'END;'";
  function void test_ending(); endfunction
endpackage
