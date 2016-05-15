package AL;

use AutoLoader;

$x = &xyz;
print "hi: $x\n";

1;

__END__

sub xyz {
    return 1;
}
