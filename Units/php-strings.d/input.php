<?php

$a = "hello";

$b = 'hello';

$c = "hello \"buddy\"";

$d = 'hello \'buddy\'';

$e = <<<EOS
hello buddy
EOS;

$f = <<<'EOS'
hello buddy
EOS;

$g = <<<"EOS"
hello buddy
EOS;

# just to check we're correctly out of a string here
$zzz_end = 42;
