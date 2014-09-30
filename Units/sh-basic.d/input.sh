# function foo() {}
  # function bar() {}
  #
  function baz() {}
function baz_2() {
  baz_2_inner()
}

mult1() echo 1; mult2() echo 2;

foo="#"; afterpound1() {}
foo="#\""; afterpound2() {}
foo='#'; afterpound3() {}
foo='#'''; afterpound4() {}

alias alias1=bar; afteralias() {}

function alias(); alias alias2=foo

foo="nofunction()"
