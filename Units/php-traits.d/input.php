Tests for traits

Expected output is

traits:
	tBar
	tFoo

classes:
	A
	B

functions:
	__construct [A]
	__construct [B]
	stuff [tFoo]
	stuff [tBar]

variables:
	prop1 [tBar]
	prop2 [tBar]

<?php

trait tFoo {
	public function stuff() {
		// ...
	}
}

class A {
	use tFoo;
	
	public function __construct() {
		// ...
	}
}

trait tBar {
	protected $prop1 = 0;
	protected $prop2;

	protected function stuff($arg1, $arg2) {
		// ...
	}
}

class B {
	use tBar;
	
	public function __construct($p) {
		// ...
	}
}
