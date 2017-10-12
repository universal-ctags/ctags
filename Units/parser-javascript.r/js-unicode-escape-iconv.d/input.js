// literal
function é_1() {}
// UTF-16 escape
function \u00E9_2() {}
function \u00e9_3() {}
// code point escape
function \u{e9}_4() {}
// leading zeros don't matter
function \u{0000000000000000000e9}_5() {}
// mixed with a diacritic
function \u{65}\u0301_6() {}

// similar with a non-BMP character
/* (this tests strings, mostly because I was unable to find a valid identifier
 *  character outside of the BMP) */
var object = {
  '𐌀_1': function(){},
  '\uD800\uDF00_2': function(){},
  '\u{10300}_3': function(){},
};
