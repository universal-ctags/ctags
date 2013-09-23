/*
 * ctags should return the following for parsing this file using:
 * ctags -f - simple.js
 * 
 * functions:
 *    func1
 *    func2
 * 
 * variables:
 *    no_re1
 *    no_re2
 *    no_re3
 *    no_re4
 *    no_re5
 *    no_re6
 *    re1
 *    re2
 *    re3
 *    re4
 *    re5
 *    re6
 *    str1
 *    str2
 */

var no_re1 = 1 / 2;
var no_re2 = 1 + (1 + 2) / 3;
var no_re3 = 1 + {0:1}[0] / 2;
var no_re4 = 1 + {0:1} / 8; // gives NaN
var no_re5 = "foo" / 2; // so does this
var no_re6 = no_re1 / 2;

var re1 = /foo/;
var re2 = /\//;
var re3 = /[/]/;
var re4 = /'/;
var re5 = /["'/]/;
var re6 = /\(([a-z]*_)+/;

var str1 = "a/b/c".replace(/\//g, '-');
var str2 = "Hello".replace(/O/ig, 'O');

function func1() {
  return /function bug1(foo){/;
}

function func2() {
  return /\(/;
}
