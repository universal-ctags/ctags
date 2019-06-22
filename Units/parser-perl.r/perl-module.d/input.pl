# Taken from https://perldoc.perl.org/functions/use.html
use constant
ONE => 1;
use diagnostics;
use integer;
use sigtrap  qw(SEGV BUS);
use strict   qw(subs vars refs);
use subs     qw(afunc blurfl);
use warnings qw(all);
use sort     qw(stable _quicksort _mergesort);

no integer;
no strict 'refs';
no warnings;
