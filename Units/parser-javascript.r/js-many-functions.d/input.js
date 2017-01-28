function a() {
  function b() {
    function c() {
    }
  }
}

var C1 = {
  m1: function() {
    this._p = 42;
  },
  m2: function(n) {
    function fi1(x) {
      return x*n;
    }
    return fi1;
  }
};

class K1 {
  l1() {
    this._p = 42;
  }
  l2(n) {
    function gi1(x) {
      return x*n;
    }
    return gi1;
  }
}

function f1(y) {
  return function(x) {
    return x*y;
  }
}

function f2() {
  return function f2i1() {
    function f2i1i1() {
      return 0;
    }
    return f2i1i1;
  }
}
