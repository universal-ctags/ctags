
// All these examples contain various forms of statements
// with missing semicolons.  Each of these are valid and must
// be accommodated.
//
// After running ctags: ctags -f tags 1880687.js
// The following tags should be generated:
//     functions
//       a
//       aa
//       aa_sub1 [aa]
//       aa_sub2 [aa]
//       b
//       baz [f]
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
//       w_sub1 [w]
//       w_sub2 [w]
//       x
//       x_sub1 [x]
//       x_sub2 [x]
//       y
//       y_sub1 [y]
//       y_sub2 [y]
//       z
//       z_sub1 [z]
//       z_sub2 [z]
//     classes
//       MyClass
//     methods
//       MyClass_sub1 [MyClass]
//       MyClass_sub2 [MyClass]

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

function x(){
    function x_sub1(){
        while (1)
            x_sub2()
    }
    function x_sub2(){
    }
}

function y(){
    function y_sub1(){
        while (1) {
            y_sub2()
        }
    }
    function y_sub2(){
    }
}

function z(){
    function z_sub1(){
        do {
            z_sub2()
        } while (0)
    }
    function z_sub2(){
    }
}

function aa(){
    function aa_sub1(){
        do
            aa_sub2()
        while (0)
    }
    function aa_sub2(){
    }
}
