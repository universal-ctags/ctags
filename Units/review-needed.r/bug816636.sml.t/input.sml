(*
Bugs item #816636, was opened at 2003-10-02 11:01
Message generated for change (Tracker Item Submitted) made by Item Submitter
You can respond by visiting: 
https://sourceforge.net/tracker/?func=detail&atid=106556&aid=816636&group_id=6556

Category: None
Group: None
Status: Open
Resolution: None
Priority: 5
Submitted By: Rahul Chaudhry (rahuldotc)
Assigned to: Nobody/Anonymous (nobody)
Summary: sml (standard ML) tag support

Initial Comment:
The program does not handle the "and" keyword of ML.

e.g. on the following code:
*)
fun f x = 1 + g x

and g x = 2 + f 0;
(*
ctags generates a tag for function "f" but not for function "g".
*)
