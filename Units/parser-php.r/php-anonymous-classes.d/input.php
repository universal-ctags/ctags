<?php

$c = new class {
	public function hello() { echo "hello"; }
};

$c->hello();

$d = new class(42) {
	public function __construct($n) { echo $n; }
	public function hello() { echo "hello"; }
};

$d->hello();

class SomeClass {}
interface SomeInterface {}
trait SomeTrait {}

$e = new class(64) extends SomeClass implements SomeInterface {
	use SomeTrait;
	public function __construct($n) { echo $n; }
	public function hello() { echo "hello"; }
};

$e->hello();
