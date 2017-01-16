
class Foo {
  foo() { return "foo"; }
}

class Bar extends Foo {
  bar() { return "bar"; }
}

function Mixin(superclass) {
  return class extends superclass {
    hello() {
      return "hello from mixin";
    }
  }
}

class Baz extends Mixin(Bar) {
  hi() {
    return "hi";
  }
}

var i = new Baz();
i.foo();
i.bar();
i.hello();
i.hi();
