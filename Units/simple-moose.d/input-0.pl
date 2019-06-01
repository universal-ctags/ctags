# Taken from https://metacpan.org/pod/Moo
package Cat::Food;
 
use Moo;
use strictures 2;
use namespace::clean;
 
sub feed_lion {
  my $self = shift;
  my $amount = shift || 1;
 
  $self->pounds( $self->pounds - $amount );
}
 
has taste => (
  is => 'ro',
);
 
has brand => (
  is  => 'ro',
  isa => sub {
    die "Only SWEET-TREATZ supported!" unless $_[0] eq 'SWEET-TREATZ'
  },
);
 
has pounds => (
  is  => 'rw',
  isa => sub { die "$_[0] is too much cat food!" unless $_[0] < 15 },
);
 
1;
