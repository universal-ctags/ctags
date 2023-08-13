# Taken from perl5/subs.pm
package subs;

use strict;
use warnings;

our $VERSION = '1.04';

=head1 NAME

subs - Perl pragma to predeclare subroutine names

=head1 SYNOPSIS

    use subs qw(frob);
    frob 3..10;

=head1 DESCRIPTION

This will predeclare all the subroutines whose names are
in the list, allowing you to use them without parentheses (as list operators)
even before they're declared.

Unlike pragmas that affect the C<$^H> hints variable, the C<use vars> and
C<use subs> declarations are not lexically scoped to the block they appear
in: they affect
the entire package in which they appear.  It is not possible to rescind these
declarations with C<no vars> or C<no subs>.

See L<perlmodlib/Pragmatic Modules> and L<strict/strict subs>.

=cut

sub import {
    my $callpack = caller;
    my $pack = shift;
    my @imports = @_;
    foreach my $sym (@imports) {
        no strict 'refs';
        *{"${callpack}::$sym"} = \&{"${callpack}::$sym"};
    }
};

1;
