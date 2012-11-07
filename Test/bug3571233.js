/*
 * "Functions nested inside methods show improper scope with the parent method
 * being reported as "function""
 * 
 * ctags -f - bug3571233.js should output:
 * 
 * classes
 *    MyClass
 * 
 * methods
 *    MyClass.method2
 * 
 * functions
 *    MyClass.method2.nestedFunction1
 *    MyClass.method2.nestedFunction2
 *    function1
 *    function1.nestedFunction3
 *    function2
 *    function2.nestedFunction4
 *    function2.nestedFunction5
 * 
 * 
 * Note that MyClass is shown both as a class and as a function (the parser
 * discovers it actually is a class only later on).  This isn't really easy to
 * fix because a JavaScript function is only a class if it happen to be used as
 * one, for example it has prototypes.
 */

function MyClass() {
}

MyClass.prototype.method2 = function() {
  // these functions have improper scope
  function nestedFunction1() {
    
  }
  
  function nestedFunction2() {
    
  }
};

// following work fine, just here as a reference
function function1() {
  function nestedFunction3() {
  }
};

function2 = function() {
  function nestedFunction4() {
  }
  
  function nestedFunction5() {
  }
};

