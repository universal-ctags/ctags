/*
 * Both functions should be tagged.
 * The embedded quote was fixed in issue:
 * [ 1878155 ] Javascript escaped quotation mark brakes output
 * [ 2023712 ] parseString for javascript broken on embedded quote
 * The fix will be part of the 5.8 release.
 */

function f1() {
var str = 'This function will be listed.';
}

function f2() {
var str = 'This function won\'t.';
}
