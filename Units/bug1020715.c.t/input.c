/*
Bugs item #1020715, was opened at 2004-09-01 22:42
Message generated for change (Tracker Item Submitted) made by Item Submitter
You can respond by visiting: 
https://sourceforge.net/tracker/?func=detail&atid=106556&aid=1020715&group_id=6556

Category: None
Group: None
Status: Open
Resolution: None
Priority: 5
Submitted By: Arne Georg Gleditsch (argggh)
Assigned to: Nobody/Anonymous (nobody)
Summary: Strange parsing of C code

Initial Comment:
Running ctags 5.5.4 like this

/usr/local/bin/ctags --fields=+S --excmd=number -f - --c-types=+l /usr/src/source/2.6.6/drivers/scsi/aha152x.c | grep ^done

returns a bogus entry for the function "done" on line
1745 with a wacky signature.  The file aha152x.c is
from Linux 2.6.6, but is appended for convenience.
*/
/*
Date: 2007-08-14 01:00
Sender: elliotth
Hide

i don't really understand why the other lines in the test case were
necessary to exercise the bug, but changing parseParens to call
processAngleBracket instead of just skipToMatch("<>") fixes this bug. i've
committed that since it causes no regressions.


Date: 2007-08-14 00:34
Sender: elliotth
Hide

i've come up with a minimal test case, and committed it. it's pretty
weird:
*/
void f() {
	done(a<<1);
	a->a;
	if (a->a) {
	}
}
/*
change just about anything and the bizarre "done" tag goes away. you can't
even switch from '->' to '.', which seems odd.
*/
