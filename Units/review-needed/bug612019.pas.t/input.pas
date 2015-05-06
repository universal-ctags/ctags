(*
Bugs item #612019, was opened at 2002-09-20 15:29
You can respond by visiting: 
https://sourceforge.net/tracker/?func=detail&atid=106556&aid=612019&group_id=6556

Category: None
Group: None
Status: Open
Resolution: None
Priority: 5
Submitted By: Konstantin Stupnik (xecutor)
Assigned to: Nobody/Anonymous (nobody)
Summary: pascal parser

Initial Comment:
In attached sample there are some lines from tags file
generated for object pascal sources.
There are some problems with this lines.
Generaly speaking they are parsed incorrectly.
*)
unit a;

interface
type
  TTest=class
    procedure Test1;
    procedure  Test2;
    procedure   Test3;
  end;

implementation

end.
