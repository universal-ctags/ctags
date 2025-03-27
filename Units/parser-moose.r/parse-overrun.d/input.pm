# Taken from The test case is taken from Throwable-1.001/lib/Throwable/Error.pm:

# This software is copyright (c) 2022 by Ricardo SIGNES.

# This is free software; you can redistribute it and/or modify it under
# the same terms as the Perl 5 programming language system itself.

# Terms of the Perl programming language system itself

# a) the GNU General Public License as published by the Free
#    Software Foundation; either version 1, or (at your option) any
#    later version, or
# b) the "Artistic License"

package Throwable::Error;
# ABSTRACT: an easy-to-use class for error objects
$Throwable::Error::VERSION = '1.001';
use Moo 1.000001;
with 'Throwable', 'StackTrace::Auto';

#pod =head1 SYNOPSIS
#pod
#pod   package MyApp::Error;
#pod   # NOTE: Moo can also be used here instead of Moose
#pod   use Moose;
#pod   extends 'Throwable::Error';
#pod
#pod   has execution_phase => (
#pod     is  => 'ro',
#pod     isa => 'MyApp::Phase',
#pod     default => 'startup',
#pod   );
#pod
#pod ...and in your app...
#pod
#pod   MyApp::Error->throw("all communications offline");
#pod
#pod   # or...
#pod
#pod   MyApp::Error->throw({
#pod     message         => "all communications offline",
#pod     execution_phase => 'shutdown',
#pod   });
#pod
#pod =head1 DESCRIPTION
#pod
#pod Throwable::Error is a base class for exceptions that will be thrown to signal
#pod errors and abort normal program flow.  Throwable::Error is an alternative to
#pod L<Exception::Class|Exception::Class>, the features of which are largely
#pod provided by the Moo object system atop which Throwable::Error is built.
#pod
#pod Throwable::Error performs the L<Throwable|Throwable> and L<StackTrace::Auto>
#pod roles.  That means you can call C<throw> on it to create and throw an error
#pod object in one call, and that every error object will have a stack trace for its
#pod creation.
#pod
#pod =cut

use overload
  q{""}    => 'as_string',
  fallback => 1;

#pod =attr message
#pod
#pod This attribute must be defined and must contain a string describing the error
#pod condition.  This string will be printed at the top of the stack trace when the
#pod error is stringified.
#pod
#pod =cut

has message => (
  is       => 'ro',
  isa      => Sub::Quote::quote_sub(q{
      die "message must be a string"
          unless defined($_[0]) && !ref($_[0]);
  }),,
  required => 1,
);

#pod =attr stack_trace
#pod
#pod This attribute, provided by L<StackTrace::Auto>, will contain a stack trace
#pod object guaranteed to respond to the C<as_string> method.  For more information
#pod about the stack trace and associated behavior, consult the L<StackTrace::Auto>
#pod docs.
#pod
#pod =method as_string
#pod
#pod This method will provide a string representing the error, containing the
#pod error's message followed by the its stack trace.
#pod
#pod =cut

sub as_string {
  my ($self) = @_;

  my $str = $self->message;
  $str .= "\n\n" . $self->stack_trace->as_string;

  return $str;
}

around BUILDARGS => sub {
  my $orig = shift;
  my $self = shift;

  return {} unless @_;
  return {} if @_ == 1 and ! defined $_[0];

  if (@_ == 1 and (!ref $_[0]) and defined $_[0] and length $_[0]) {
    return { message => $_[0] };
  }

  return $self->$orig(@_);
};

1;

__END__

=pod

=encoding UTF-8

=head1 NAME

Throwable::Error - an easy-to-use class for error objects

=head1 VERSION

version 1.001

=head1 SYNOPSIS

  package MyApp::Error;
  # NOTE: Moo can also be used here instead of Moose
  use Moose;
  extends 'Throwable::Error';

  has execution_phase => (
    is  => 'ro',
    isa => 'MyApp::Phase',
    default => 'startup',
  );

...and in your app...

  MyApp::Error->throw("all communications offline");

  # or...

  MyApp::Error->throw({
    message         => "all communications offline",
    execution_phase => 'shutdown',
  });

=head1 DESCRIPTION

Throwable::Error is a base class for exceptions that will be thrown to signal
errors and abort normal program flow.  Throwable::Error is an alternative to
L<Exception::Class|Exception::Class>, the features of which are largely
provided by the Moo object system atop which Throwable::Error is built.

Throwable::Error performs the L<Throwable|Throwable> and L<StackTrace::Auto>
roles.  That means you can call C<throw> on it to create and throw an error
object in one call, and that every error object will have a stack trace for its
creation.

=head1 PERL VERSION

This library should run on perls released even a long time ago.  It should work
on any version of perl released in the last five years.

Although it may work on older versions of perl, no guarantee is made that the
minimum required version will not be increased.  The version may be increased
for any reason, and there is no promise that patches will be accepted to lower
the minimum required perl.

=head1 ATTRIBUTES

=head2 message

This attribute must be defined and must contain a string describing the error
condition.  This string will be printed at the top of the stack trace when the
error is stringified.

=head2 stack_trace

This attribute, provided by L<StackTrace::Auto>, will contain a stack trace
object guaranteed to respond to the C<as_string> method.  For more information
about the stack trace and associated behavior, consult the L<StackTrace::Auto>
docs.

=head1 METHODS

=head2 as_string

This method will provide a string representing the error, containing the
error's message followed by the its stack trace.

=head1 AUTHORS

=over 4

=item *

Ricardo SIGNES <cpan@semiotic.systems>

=item *

Florian Ragwitz <rafl@debian.org>

=back

=head1 COPYRIGHT AND LICENSE

This software is copyright (c) 2022 by Ricardo SIGNES.

This is free software; you can redistribute it and/or modify it under
the same terms as the Perl 5 programming language system itself.

=cut
