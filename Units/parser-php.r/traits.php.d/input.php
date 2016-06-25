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
