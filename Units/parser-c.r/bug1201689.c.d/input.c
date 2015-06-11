/*
Bugs item #1201689, was opened at 2005-05-13 18:18
Message generated for change (Tracker Item Submitted) made by Item Submitter
You can respond by visiting: 
https://sourceforge.net/tracker/?func=detail&atid=106556&aid=1201689&group_id=6556

Category: None
Group: None
Status: Open
Resolution: None
Priority: 5
Submitted By: Scott Ferguson (shf301)
Assigned to: Nobody/Anonymous (nobody)
Summary: Variable Length Argument Lists in K&R Style not Parsed

Initial Comment:
A function with a K&R style parameter list that has a 
variable length argument list will not be added to the tags file.

For example create a file, say test.c with the following code
*/
void test(a, ...)
char a;
{
    return;
}
/*
Run ctags test.c.  the tags file with only contain the line:
   a	test.c	/^char a;$/;"	v

This occurs with ctags 5.3 and 5.5.4. 

Variable length argument lists work fine if the function is in 
ANSI style, void test(char a, ...) will work fine.
*/
