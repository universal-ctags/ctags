/*
Bugs item #1085585, was opened at 2004-12-14 20:55
Message generated for change (Tracker Item Submitted) made by Item Submitter
You can respond by visiting: 
https://sourceforge.net/tracker/?func=detail&atid=106556&aid=1085585&group_id=6556

Category: None
Group: None
Status: Open
Resolution: None
Priority: 5
Submitted By: boisterous (yadavnav)
Assigned to: Nobody/Anonymous (nobody)
Summary: ctags 5.5.4 doesn't work correctly on some files

Initial Comment:
Hi,

I ran exuberant ctags on the following file

FreeBSD kernel : src/sys/crypto/des/des_setkey.c

If I run the 5.5.4 ctags on version 1.3 of the above
file then it doesn't produce all the tags

The version 1.3 is at:

http://www.freebsd.org/cgi/cvsweb.cgi/src/sys/crypto/des/des_setkey.c?rev=1.3&content-type=text/x-cvsweb-markup

[...]

Why does ctags fail on the 1.3 version of the file ?
*/

/* relevant portion from file */
static int check_parity(des_cblock (*key));
int des_check_key=0;
