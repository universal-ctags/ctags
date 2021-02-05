package Derived0 {
    use Function::Parameters qw(
	:std
	);
    use Moo;

    extends 'Base';

    has 'go_big0' => (
	is => 'ro',
    );

    # "around" method with implicit $orig and $self
    around size0_dont_capture_me() {
	return $self->$orig() * 2 if $self->go_big;
	return $self->$orig();
    }
}
