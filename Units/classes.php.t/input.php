Expected output is

classes:
	Bar
	Foo

functions:
	__construct [Foo]
	__construct [Bar]
	method1 [Foo]
	method1 [Bar]
	method2 [Foo]

<?php

class Foo {
	function __construct($a, $b) {
		// ...
	}

	function method1($arg) {
		return 42;
	}

	function method2() {
		// ...
	}
}

class Bar {
	function __construct () {
		/* ... */
	}

	function method1 () {
		// ...
	}
}
