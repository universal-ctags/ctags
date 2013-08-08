Tests for anonymous functions

Expected output is

functions:
  a
  b
  c
  d
  e

global variables:
  _g

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
