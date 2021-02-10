# Taken from https://metacpan.org/pod/Function::Parameters
use Function::Parameters;
 
# plain function
fun foo($x, $y, $z = 5) {
    return $x + $y + $z;
}
print foo(1, 2), "\n";  # 8
 
# method with implicit $self
method bar($label, $n) {
    return "$label: " . ($n * $self->scale);
}
 
# named arguments: order doesn't matter in the call
fun create_point(:$x, :$y, :$color) {
    print "creating a $color point at ($x, $y)\n";
}
create_point(
    color => "red",
    x     => 10,
    y     => 5,
);
 
package Derived {
    use Function::Parameters qw(
	:std
	:modifiers
	);
    use Moo;
 
    extends 'Base';
 
    has 'go_big' => (
        is => 'ro',
    );
 
    # "around" method with implicit $orig and $self
    around size($x, $y) {
        return $self->$orig() * 2 if $self->go_big;
        return $self->$orig();
    }
}
