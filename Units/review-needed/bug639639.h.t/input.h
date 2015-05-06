/*
Date: Sun, 17 Nov 2002 04:41:42 -0800
Subject: [ ctags-Bugs-639639 ] incorrect enum field for C++ header

Bugs item #639639, was opened at 2002-11-17 13:41
You can respond by visiting: 
https://sourceforge.net/tracker/?func=detail&atid=106556&aid=639639&group_id=6556

Category: None
Group: None
Status: Open
Resolution: None
Priority: 5
Submitted By: Matthias S. Benkmann (mbenkmann)
Assigned to: Nobody/Anonymous (nobody)
Summary: incorrect enum field for C++ header

Initial Comment:
----------- temp2.h -------------
*/
namespace Namespace1
{
  int function2(char* str);
}

enum {anon2=1000};
----------------------------  
/*
> ctags --excmd=number --fields=+kKmnsSz
--file-scope=no -f - temp2.h

Namespace1      temp2.h 1;"     kind:namespace  line:1
anon2   temp2.h 6;"     kind:enumerator line:6 
enum:Namespace1


As you can see ctags thinks that anon2 belongs to enum
Namespace1 which it obviously doesn't (Namespace1 is
not even an enum). It should be enum:<anonymous>.


----------------------------------------------------------------------

You can respond by visiting: 
https://sourceforge.net/tracker/?func=detail&atid=106556&aid=639639&group_id=6556
*/
