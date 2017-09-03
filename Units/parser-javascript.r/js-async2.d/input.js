var Class = {
  method1 : function() {
    return 0;
  },
  method2 : async function() {
    return await resolveAfter2Seconds(42);
  },
  // ES6 shorthand methods
  method3() {
  },
  async method4() {
  },
};

var asyncAnonymousFunc = async function() {
}

class ES6Class {
  async es6AsyncMethod() {}
  es6Method() {}
}
