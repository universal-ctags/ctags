# Taken from https://metacpan.org/pod/Moose
package Point;
use Moose; # automatically turns on strict and warnings

has 'x' => (is => 'rw', isa => 'Int');
has 'y' => (is => 'rw', isa => 'Int');

has z0 => (is => 'rw');
has 'z1' => (is => 'rw');
has "z2" => (is => 'rw');

has (z0p => (is => 'rw'));
has ('z1p' => (is => 'rw'));
has ("z2p" => (is => 'rw'));

has (	z0ps => (is => 'rw'));
has (	'z1ps' => (is => 'rw'));
has (	"z2ps" => (is => 'rw'));

has [qw#z3	z4 z5#] => ((is => 'rw'),(is => 'rw'),(is => 'rw'));
has ([qw|z6	z7|] => (is => 'rw'), (is => 'rw'));

sub clear {
    my $self = shift;
    $self->x(0);
    $self->y(0);
}

package Point3D;
use Moose;

extends 'Point';

has 'z' => (is => 'rw', isa => 'Int');

before 'clear' => sub {
};

after 'clear' => sub {
};

around 'clear' => sub {
};

package TimeAxis;
use Moose;

package Point4D;
use Moose;

extends 'Point3D',
    'TimeAxis' ;

override "clear" => sub {
};

no Moose;
package Line;
