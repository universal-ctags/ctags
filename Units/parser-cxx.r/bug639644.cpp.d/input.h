/*
Date: Sun, 17 Nov 2002 04:57:43 -0800
Subject: [ ctags-Bugs-639644 ] anonymous namespaces in headers

Bugs item #639644, was opened at 2002-11-17 13:57
You can respond by visiting: 
https://sourceforge.net/tracker/?func=detail&atid=106556&aid=639644&group_id=6556

Category: None
Group: None
Status: Open
Resolution: None
Priority: 5
Submitted By: Matthias S. Benkmann (mbenkmann)
Assigned to: Nobody/Anonymous (nobody)
Summary: anonymous namespaces in headers

Initial Comment:
--------------------temp3.h----------------
*/
namespace
{
  int foo;
}
/*
---------------------------------------------

> ctags -f - temp3.h

foo     temp3.h /^  int foo;$/;"        m       namespace:

That last field should be "namespace:<anonymous>"
and the kind field should be variable, not member.



----------------------------------------------------------------------

You can respond by visiting: 
https://sourceforge.net/tracker/?func=detail&atid=106556&aid=639644&group_id=6556
*/
