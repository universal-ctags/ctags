
function func1() {
  return { a: 1, b:2 };
}

function func2() {
  return 42;
}

var class1 = function() {
  this.method1 = function() {
    return 42;
  };
  this.method2 = function() {
    return { a:1, b:2 };
  };
  this.method3 = function() {
    return [1, 2, 3];
  };
  this.method4 = function() {
    return "hello";
  };
};

var class2 = function() {
  this.c2m1 = function() {
    c2m3(function() {
      return { 'test': {} };
    });
  };
  this.c2m2 = function(f) {
    return { 'ret': f() };
  };
  this.c2m3 = function(f) {
    return f();
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
  this.c3m2 = function() {
    return 0;
  };
}

var class4 = function() {
  this.method1 = function() {
    return [{a:1, b:2}, {a:3, b:4}, {a:5, b:6}];
  };
  this.method2 = function() {
    return 0;
  };
};
