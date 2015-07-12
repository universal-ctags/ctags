// this file willfully uses CR-LF line endings to check their handling

var s1 = "I'm invalid because not terminated

var s2 = "I'm valid, I have a line continuation:\
; function bug1(){}";

var s3 = "I'm invalid because I'm not terminated either \
var bug2 = 'this is inside the s3 string'
var s4 = 'this is a separate, valid string'
