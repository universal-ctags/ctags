
function f1() {
}
function f2(arg1, arg2) {
}
function f3(
  arg1, // first
  arg2, // second
  arg3  // last
) {
  // ...
}
function f4(a, b, c) {
}

function f5(a="hello", b='hi', c=42){}

function Cls(name) {
  this.name = name;
}
Cls.prototype = {
  get_name: function() {
    return this.name;
  },
  set_name: function(name) {
    this.name = name;
  },
}

Cls.prototype.hello = function(tpl) {
  if (tpl == undefined) tpl = "hello {}";
  return tpl.replace ('{}', this.name);
}

main = function() {
  c = new Cls("John");
  print(c.hello());
}

main();
