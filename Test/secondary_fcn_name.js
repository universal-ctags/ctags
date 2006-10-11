/*
 * ctags should return the following for parsing this file using:
 * ctags -f - test.js
 *    functions
 *      D1
 *      D2
 *      D2A
 *      theAdd
 *    variables
 *      global
 */
function D1(a, b) 
{                     
    var my_local_var1 = 'local';
    return a+b;
}      
alert(D1(1,2));        // produces 3


// Example D2

var D2=function(a, b) 
{                     
  return a+b;
}                     
alert(D2(1,2));        // produces 3

var my_global_var1 = 'global';

// Example D2A

// Tags should be generated for both:
//    D2A
//    theAdd
var D2A=function theAdd(a, b) 
{                     
  return a+b;
}                     
alert(D2A(1,2));           // produces 3
alert(theAdd(1,2));        // also produces 3
