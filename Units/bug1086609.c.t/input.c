/*
Bugs item #1086609, was opened at 2004-12-16 13:07
Message generated for change (Tracker Item Submitted) made by Item Submitter
You can respond by visiting: 
https://sourceforge.net/tracker/?func=detail&atid=106556&aid=1086609&group_id=6556

Category: None
Group: None
Status: Open
Resolution: None
Priority: 5
Submitted By: Mikhail Kruk (meshko)
Assigned to: Nobody/Anonymous (nobody)
Summary: ctags getting confused by #if 0

Initial Comment:
Here is a sample C program which confuses ctags.  I
think every line in it is significant. Dropping any of
the #ifdefs or the #define makes the problem go away:
*/
#if 0
#define __PROC__
int func1(
#if 0
#endif
)
{
}
#endif

int func2(int a)
{
}
/*
Somehow the opening brace from line 3 doesn't get
ignored and the closing brace does get ignored and
ctags drops out on "int func2(int a)" line with "failed
to find match for '(' at line 11" error.
Granted, having #if 0 in the middle of args list is
weird, but perfeclty legal.
*/
