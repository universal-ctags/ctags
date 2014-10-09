package SL;

use SelfLoader;

$x = &xyz;
print "hi: $x\n";

1;

__DATA__

sub xyz {
    return 1;
}
