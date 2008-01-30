
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


