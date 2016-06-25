Tests for namespaces (unbraced version)

Expected output is

namespaces:
	Bar\Baz
	Foo

classes:
	B [Bar\Baz]
	C [Foo]

functions:
	__construct [Foo\C]
	__construct [Bar\Baz\B]
	a [Foo]
	a [Bar\Baz]
	b [Foo]
	c [Bar\Baz\B]
	d [Foo\C]

<?php

/* namespace "Foo" */
namespace Foo;

function a() {
	return true;
}

function b() {
	return false;
}

class C {
	function __construct() {
		// ...
	}

	function d() {
		return 42;
	}
}

/* namespace "Bar\Baz" */
namespace Bar\Baz;

function a() {
	return true;
}

class B {
	function __construct() {
		// ...
	}

	function c() {
		return 42;
	}
}
