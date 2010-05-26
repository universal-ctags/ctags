// When it should be demo_pkg.test_func and demo_pkg.test_var
// Tags are created for:
//     packages    
//          demo_pkg
//     variables
//          test_var
//
// But no tags for the function.
//
//
create or replace package demo_pkg is
test_var number;

function test_func return varchar2;
function more.test_func2 return varchar2;
function test_func3 return varchar2;

end demo_pkg;

