<?php

$a = function() {};

$b = function($x, $y) {};

/* return a value */
$c = function() {
  return 42;
};

/* return a function */
$d = function() {
  return function() {
    return "hello";
  };
};

/* return by reference */
$_g = 42;
$e = function&() {
  global $_g;
  return $_g;
};

$f = function&() use (&$_g) {
  
  function f_sub() {}
  
  return $_g;
};
