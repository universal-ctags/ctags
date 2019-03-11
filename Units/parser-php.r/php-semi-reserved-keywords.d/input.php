<?php

class C {
    const const = 1;
    const static = 2;
    
    public static function new() { return new self; }
    public function function() {}
    public static function class() { return new self; }
    public function for() {}
    public function while() {}
    public function abstract() {}
    public function public() {}
    protected function protected() {}
    private function private() {}
    public static function namespace() { return new self; }
    public static function trait() { return new self; }

    public function test1() {}
    public function test2() {}
    public function test3() {}
    public function test4() {}
    public function test5() {}
    public function test6() {}
    public function test7() {}
    public function test8() {}
    public function test9() {}
}

function call_c_function($c) {
    return $c->function();
}

$c = C::new();
call_c_function($c);
$c->class();
$c->namespace();
$c->trait();

$c->class()->test1();
function func1() {}
$c->namespace()->test2();
function func2() {}
$c->trait()->test3();
function func3() {}

$c->class()->namespace()->test4();
function func4() {}
$c->namespace()->trait()->test5();
function func5() {}
$c->trait()->class()->test6();
function func6() {}

C::class()->test7();
function func7() {}
C::namespace()->test8();
function func8() {}
C::trait()->test9();
function func9() {}

function something() {}
