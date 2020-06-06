# Derrived from https://metacpan.org/pod/Moose::Role

package Eq;
use Moose::Role; # automatically turns on strict and warnings
 
requires 'equal';
 
sub no_equal {
    my ($self, $other) = @_;
    !$self->equal($other);
}

package R0;
use Moose::Role;

package R1;
use Moose::Role;

package R2;
use Moose::Role;

package C0;
use Moose;

package C1;
use Moose;

package C2;
use Moose;

# ... then in your classes
 
package Currency;
use Moose; # automatically turns on strict and warnings

extends 'C1';
with 'Eq';
extends 'C2',
    'C3';
with 'R0';
with 'R1', 'R2';
 
sub equal {
}
