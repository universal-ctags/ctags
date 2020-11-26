<?php

$a = <<<EOS
; function bug1() {};
EOS;

$b = <<<'EOS'
; function bug2() {};
EOS;

$c = <<<"EOS"
; function bug3() {};
EOS;

/* PHP 7.3+ relaxed syntax */

$d = <<<EOS
; function bug4() {};
EOS . 'suffix';


$e = <<<EOS
    ; function bug5() {};
  EOS ;

# $f = "EOSNOTYES"
$f = <<<EOS
    EOSNOT
    ; function bug6() {};
    EOS."YES";

# $g = 46
$g = <<<EOS
  42
  EOS+4;

# just to check we're correctly out of a heredoc/nowdoc here
$zzz_end = 42;
