/*
 * Test for properties values.  Everything is valid here and must be
 * correctly parsed.
 * 
 * Output of ctags -f - 3470609.js should be:
 * 
 * Properties:
 *    root.array
 *    root.decimal
 *    root.id
 *    root.neg
 *    root.parentheses
 *    root.string
 *    root.subObject.subProperty
 * 
 * Classes:
 *    root
 *    root.subObject
 * 
 * Methods:
 *    root.method
 *    root.subObject.subFunction
 * 
 * Functions:
 *    f
 */

var root = {
  'string' : 'hello world',
  'method' : function() {
    x = 42;
  },
  'id' : 1,
  'neg' : -1,
  'decimal' : 1.3,
  'subObject' : {
    'subProperty': 42,
    'subFunction': function() {
      y = 43;
    }
  },
  'array' : [1, 2, 3],
  'parentheses' : (2 * (2 + 3))
}

function f() {
  
}

