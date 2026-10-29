const h = 4;
const half = { y: h / 2 }; // a division, not a regex
const third = (h / 3); // a division in parentheses
function scale(n = h / 2) {} // a division in a default parameter
class Box {
  width = Math.round(h / 2); // a division in a property initializer
  area() { return h / 2; } // a division in a method
}
const braces = { close: /}/ }; // a regex containing a brace
function hasQuote(s: string) { return /'/.test(s); } // a regex after return
const last = 1;
