mult1() echo 1; mult2() echo 2; mult3() { echo 1 }; alias alias=mult1

function baz() {}
function baz_2() {
  baz_2_inner() baz
}

function with-dash() {
  with-dash_2() {
    with-dash_inner() baz
  }
}

function noParen {
    :
}

function -starting-with-dash() {
  -starting-with-dash_2() {
    -starting-with-dash_inner() baz
    _starting-with-underscore_inner() baz
  }
}
