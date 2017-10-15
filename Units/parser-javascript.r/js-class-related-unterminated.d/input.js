
var Cls = {
  // add a member just so Cls is recognized as a class from the start
  A: {}
}

Cls.B = function(a, b) {
  this.a = a;
  this.b = b;
}

Cls.B.Sub = function() {
  this.a = 0
}

Cls.B.prototype.m1 = function(a) {
  this.a = a;
  if (a > 2) {
    this.a *= 2
  }
}
Cls.B.prototype.m2 = function(b) {
  var a = b
}
Cls.B.prototype.m3 = function(c) {
  this.c = c
}
Cls.B.prototype.m4 = function(d) {
  this.d = d
}
Cls.B.prototype.m5 = function(e) {
  /* this should rather be written `Cls.B.Sub.prototype.dyn1 = this.m6`, but
   * then parser then thinks it's a child of this very scope.  it isn't really
   * possible to fix this as the only reason it's actually not a child of the
   * current scope is because it exists in the root scope but not in this one */
  //var Sub = Cls.B.Sub;
  //Sub.prototype.dyn1 = this.m4
  Cls.B.Sub.prototype.dyn1 = this.m4
}
Cls.B.prototype.m6 = function(f) {
}

Cls.C = function () {
  this.a = 0;
}

Cls.C.prototype = {
  n1: function() {
    Cls.C.prototype = Cls.C.prototype
  },
  n2: function() {
  }
}

function main() {
  var c = new Cls.B(1, 2);
  var d = new Cls.B.Sub();
  print(d.dyn1);
  c.m5();
  print(d.dyn1);
}

main();
