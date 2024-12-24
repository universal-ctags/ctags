// https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Destructuring_assignment
//   - Computed object property names and destructuring
let key0 = 'z';
let {[key0]: foo} = {z: 'alpha'};

let key1 = 'x';
let {[key0]: bar = 'X', [key1]: baz} = {x: 'beta'};
