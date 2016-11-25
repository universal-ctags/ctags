// from https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Functions/Method_definitions
var obj1 = {
  g1: function*() {
    var index = 0;
    while(true)
      yield index++;
  }
};

// more examples
function *g2() {
  yield 42;
}

var g3 = function*g4(x) {
  while(true) {
    yield x;
    x*=x;
  }
}

var obj2 = function() {
  this.x = 0;
  this.g5 = function*g6() {
    yield this.x;
  }
}
