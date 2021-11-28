sub myfunc0
{
}

   print <<"foo", <<~\bar; # you can stack them
    I said foo.
sub myfunc1
{
}
foo
    I said bar.
    sub myfunc2
    {
    }
    bar

sub myfunc3
{
}

   myfunc0(<< "THIS", 23, <<'THAT');
sub myfunc4
{
}
Here's a line
sub myfunc5
{
}
or two.
sub myfunc6
{
}
THIS
sub myfunc7
{
}
and here's another.
sub myfunc8
{
}
THAT
sub myfunc9
{
}

myfunc0(<< "AB\"CD", << 'EF\GH');
label0:
sub myfunc10
{
label1:
}
label2:
AB"CD
sub myfunc11
{
label3:
}
label4:
EF\GH

sub myfunc12
{
   print "12\n";
}
myfunc12();
