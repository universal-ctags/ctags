
var A = {}, b = 2;
var c = 3, d = 4;
A.mem1 = function() {return 42;}, A.mem2 = function() {return 43;}
var x = {}, y = {}, z = {}

/* verify handling of unterminated assignation to '{}' */
function func1(){var x = {}} function func2(){}
