// Taken from https://www.sitepoint.com/javascript-decorators-what-they-are/
@log()
@immutable()
class Example {
  @time('demo')
  doSomething() {
    //
  }
}

// Taken from https://babeljs.io/blog/2018/09/17/decorators
class MyClass {
  @decorator
  @dec(arg1, arg2)
  @namespace.decorator
  @(complex ? dec1 : dec2)
  my_method() {}
}

class YourClass {
  @decorator @dec(arg1, arg2) @namespace.decorator @(complex ? dec1 : dec2) your_method() {}
}
