<?php

/* This is sample code just to exercise the parse a little, and cover most of
 * what it handles but other tests don't cover so we exercise most code paths.
 */

function func1($n, $c) {
  $str = '';
  for ($i = 0; $i < $n; $i++) {
    $str .= $c;
  }
  return strlen($str) > 0 ? $str : null;
}

function scisors($width) {
  $left = (int) (($width - 4) / 2);
  return func1($left, '-') . ' >8 ' . func1($width - 4 - $left, '-');
}

abstract class Acls1 {
  public function __construct ($args = null) {
    /* if args are not given, set up defaults */
    if ($args == null)
      $args = array('name' => 'doe', 'give-name' => 'john');

    // make sure arguments are valid
    foreach ($args as $k => $v)
      assert(in_array($k, array('name', 'given-name', 'surname')));

    $this->present($args);
  }

  protected abstract function present($args);
}

class Cls1 extends Acls1 {
  protected function present($args) {
    foreach ($args as $k => $v) {
      if ($k == 'name')
        print "My name is $v\n";
      elseif ($k === 'surname')
        print "Call me $v\n";
    }
  }
}

function func2($a, $b = ((4 + 1) * 2 - 4 / 2)) {
  while ($a > $b)
    $a /= 2;
  return $a;
}

/* this one is *really* silly */
function func3(
  $a = (0 ? 1 : 0) * 2,
  $b = [1, 2, 3],
  $c = "Hello",
  $d = array('foo' => 42, 'bar' => 43)
):string {
  return $c.$d['foo'].$b[1].$a;
}

define('WIDTH', 36 * ((4 + 2) / 3));

# --------------------------------------------------------------------------- #
echo scisors(WIDTH), "\n";
$cls1 = new Cls1();
echo scisors(72), "\n";

echo func2(6, 5), "\n";

$name = <<<NAME
hello
NAME;
$$name = 'world';
echo "hello $hello\n";

echo func3(), "\n";

$obj = new class(array(1, 2, 3), (1 + 2) * 3) {
  function __construct($a, $b) {
    echo $b,"\n";
  }
};
