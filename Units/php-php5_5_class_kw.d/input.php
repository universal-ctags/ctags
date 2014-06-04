<?php

class A {
  
}

echo A::class . "\n";

class B {
  public function __construct () {
    echo this::class;
  }
}

new B();

class C {
  
}
