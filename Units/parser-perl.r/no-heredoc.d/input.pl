# Derrived from #3588 submitted by @petdance

sub f0tag() {}

my $x = '<<NOT_A_HEREDOC0';

sub f1tag() {}

print "<<NOT_A_HEREDOC0\n";

sub f2tag() {}

print `cat <<<BASH_HERE_STRING`;

sub f3tag() {}

print 'cat <<<heredoct0notag' . <<hereodc0tag;
sub f0notag() {}
hereodc0tag

sub f4tag() {}

print "cat <<<heredoct1notag" . <<hereodc1tag;
sub f1notag() {}
hereodc1tag

sub f5tag() {}

print `cat <<<heredoct1notag` . <<hereodc2tag;
sub f2notag() {}
hereodc2tag

sub f6tag() {}

print "abc" . <<heredoc3tag . 'efg' . << "heredoc4tag" . `ls` . '<<hereodc5notag';
sub f3notag() {}
heredoc3tag
sub f4notag() {}
heredoc4tag
sub f7tag() {}

sub f8tag() {}

my $i = 1;
print "a" . 3 << $i;

sub f9tag() {}
