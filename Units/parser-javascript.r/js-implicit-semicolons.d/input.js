var a = 1
var b = function(){}
function c() {}
var d = {}
function e() {}
var f = []
function g() {}
var h = (1)
function i() {}
do {
} while(0)
function j() {}
var k = new Function('a','b','return a+b')
function l() {}
var m = 0 // a single comment doesn't eat the newline
function n() {}
/* test from https://github.com/universal-ctags/ctags/issues/4192 */
var o = () => {
  return 0
}
var p
function q() {}
