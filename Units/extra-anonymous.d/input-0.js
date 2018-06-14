var class2 = function() {
  this.c2m1 = function() {
    c2m3(function() {
      return { 'test': {} };
    });
  };
};

var class3 = function() {
  this.c3m1 = function() {
    return function(n) {
      if (n == 42) {
        return 0;
      } else {
        return (n + 1) % 42;
      }
    };
  };
}

function f1(y) {
  return function(x) {
    return x*y;
  }
}

function Mixin(superclass) {
  return class extends superclass {
    hello() {
      return "hello from mixin";
    }
  }
}

class {
	method8(n) { return n * n; }
}

