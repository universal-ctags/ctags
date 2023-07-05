function definedWithFunctionBefore() {};
const functionExpressionBefore = () => {};
let variableBefore = "foo";
function get() {};
function definedWithFunctionAfter() {};
const functionExpressionAfter = () => {};
let variableAfter = "bar";

Foo.prototype.get = function() {};
Foo.prototype.set = () => {};
