
// All these examples contain various forms of IF statements
// with missing semicolons.  Each of these are valid and must
// be accommodated.
//
// After running ctags: ctags -f tags 1880687.js
// The following tags should be generated:
//     functions
//       a
//       b
//       c
//       d
//       e
//       f
//       g
//       h
//       i
//       j
//       k
//       l
//       m
//       n
//       o
//       p
//       q
//       r
//       s
//       t
//       u
//       v
//       w
//       w.w_sub1
//       w.w_sub2
//
//    classes
//      MyClass
//
//    methods
//      MyClass.MyClass_sub1
//      MyClass.MyClass_sub2

function a(flag){
    if(flag)
        test(1);
    else
        test(2)
}

function b(){
    var b= 33;
}

function c(flag){
    if(flag)
        test(1);
}

function d(){
    var b= 33;
}

function e(flag){
    if(flag)
        test(1)
}

function f(){
    var b= 33;
    if (foo)
            bar();
    else
        test(2);
    function baz() {
    }
}

function g(flag){
    if(flag) {
        test(1)
    }
}

function h(){
    var b= 33;
}

function i(flag){
    if(flag) {
        test(1);
    }
}

function j(){
    var b= 33;
}

function k(flag){
    if(flag) {
        test(1);
    }
    else
        flag = false;
}

function l(){
    var b= 33;
}

function m(flag){
    if(flag) {
        test(1);
    }
    else {
        flag = false;
    }
}

function n(){
    var b= 33;
}

if (1)
    l();
function o(){
    var b= 33;
}

if (1){
    l();
}
function p(){
    var b= 33;
}

if (1){
    l();
} else
    l();
function q(){
    var b= 33;
}

function r(flag){
    if (flag) {
        value = 33
    }
}

function s(){
    var b= 33;
}

function t(flag){
    if (flag) {
        b= new Object()
    }
}

function u(flag){
    if (flag) {
        b= ({})
    }
}

function v(flag){
    if (flag) {
        b= {}
    }
}

function w(){
    function w_sub1(x){
        if (! x)
            x = {foo:bar};
        
        var dummy1, dummy2;
    }
    function w_sub2(){
    }
}

MyClass = {
    MyClass_sub1: function(x){
        if (! x)
            x = { };
        
        var dummy3, dummy4;
    },
    MyClass_sub2: function(x){
        var dummy5 = 42;
    }
};
