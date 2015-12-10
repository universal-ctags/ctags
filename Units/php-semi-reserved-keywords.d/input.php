<?php

class C {
    const const = 1;
    const static = 2;
    
    public static function new() { return new self; }
    public function function() {}
    public function class() {}
    public function for() {}
    public function while() {}
    public function abstract() {}
    public function public() {}
    protected function protected() {}
    private function private() {}
}

function call_c_function($c) {
    return $c->function();
}

$c = C::new();
call_c_function($c);
$c->class();

function something() {}
