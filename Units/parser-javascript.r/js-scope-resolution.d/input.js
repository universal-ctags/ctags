var Cls = function() {
  this.init = function() {
    print("hello")
  }
}

function extend() {
  /* this extends 'Cls' at the root scope */
  Cls.prototype.extended = function() {
    print("extended")
  }
}

function doesnt_extend() {
  /* this extends the local Cls class */
  var Cls = function() {
    // just so the parser sees a class
    this.dummy1 = function() {}
  }
  Cls.prototype.dummy2 = function() {
    print("dummy")
  }
}

//function extend_dyn(cls) {
//  /* here it's just not possible to figure it out, as it's extending a class
//   * dynamically -- cls is the function's argument.  The only way is to
//   * actually run the code and see on which class(es) this was called.  */
//  cls.prototype.dynamic = function() {
//    print('dynamic')
//  }
//}

/*----------------*/

function main() {
  function assert(expr, message) {
    if (! expr) {
      throw new Error(message || "Assertion failed");
    }
  }

  var ci = new Cls()
  ci.init();

  assert(ci.extended == undefined)
  extend()
  assert(ci.extended != undefined)

  assert(ci.dummy == undefined)
  doesnt_extend()
  assert(ci.dummy == undefined)

  //assert(ci.dynamic == undefined)
  //extend_dyn(Cls)
  //assert(ci.dynamic != undefined)
}
main()
