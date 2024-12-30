// See #3435.
//  If an object literal is specified as a default value in object restructuring,
// the parser may fail to extract the variable (or constant):
var{ c = {a: 1} } = { c: undefined };
var{ d = {a: 1} } = {d: 3};
var a = 1
var [x = {a: 2}, y] = [, 4];
var [x = [a, 2], z] = [, 4];
var { 'alpha': q = {'x': 9} } = {'alpha': 3};
