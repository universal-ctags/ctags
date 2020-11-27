Expected output is

classes:
	Abstract1
	Abstracted
	Bar
	Foo
	Inherited
	Interfaced

functions:
	__construct [Foo]
	__construct [Bar]
	abstract_method1 [Abstract1]
	abstract_method1 [Abstracted]
	abstract_method2 [Abstract1]
	abstract_method2 [Abstracted]
	ifaced_method1 [Iface1]
	ifaced_method1 [Interfaced]
	ifaced_method2 [Iface2]
	ifaced_method2 [Interfaced]
	method1 [Foo]
	method1 [Bar]
	method2 [Foo]
	method3 [Abstract1]

interfaces:
	Iface1
	Iface2

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

class Inherited extends Bar {
}

interface Iface1 {
	function ifaced_method1 ();
}

interface Iface2 {
	function ifaced_method2 ();
}

class Interfaced extends Inherited implements Iface1, Iface2 {
	function ifaced_method1 () {
		// ...
	}

	function ifaced_method2 () {
		// ...
	}
}

abstract class Abstract1 {
	abstract function abstract_method1();
	abstract protected function abstract_method2();
	public function method3() {
		return 42;
	}
}

class Abstracted extends Abstract1 {
	function abstract_method1() {
		// ...
	}
	protected function abstract_method2() {
		// ...
	}
}
