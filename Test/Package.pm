# This file is intended to test package keyword support along with
# multi-line subroutine definitions, like this:
#   sub
# 
#   somefunction
#   {...

package Test;

use strict;
use warnings;

sub init {
}

use constant
CONST => 1;

=head2 quo

This is 'quo' subroutine

=cut

sub quo {
    goto END;
    die;
END:
    print "END!\n";
    return;
}

sub
  
dude
{
    1;
}

quo;

1;
