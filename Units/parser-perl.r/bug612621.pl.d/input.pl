=pod
Bugs item #612621, was opened at 2002-09-21 21:23
You can respond by visiting: 
https://sourceforge.net/tracker/?func=detail&atid=106556&aid=612621&group_id=6556

Category: None
Group: None
Status: Open
Resolution: None
Priority: 5
Submitted By: Richard Donkin (rdonkin)
Assigned to: Nobody/Anonymous (nobody)
Summary: Perl POD syntax -> incomplete tags file

Initial Comment:
In the following test file, ctags 5.2.3 (compiled from 
source on Cygwin) only adds the first subroutine (bar) to 
the tags file, when run using:

ctags --totals --
language-force=perl temp

-----------------
=cut
sub bar() 
{

    print "blah\n";

=for
    print "blah2\n";
    # 
Note: next line has trailing space
=cut 


}

sub foo {
    print "hello\n";
}
=pod
The 
tags file looks like 
this:
!_TAG_FILE_FORMAT	2	/extended format; -- format=1 will not append ;" to lines/
!_TAG_FILE_SORTED	1	/0=unsorted, 1=sorted/
!_TAG_PROGRAM_AUTHOR	Darren Hiebert	/dhiebert@users.sourceforge.net/
!_TAG_PROGRAM_NAME	Exuberant Ctags	//
!_TAG_PROGRAM_URL	http://ctags.sourceforge.net	/official site/
!_TAG_PROGRAM_VERSION	5.2.3	//
bar	temp	/^sub bar() {$/;"	s

The workaround is to delete the 
trailing space at the end of the '=cut' line.

=cut
