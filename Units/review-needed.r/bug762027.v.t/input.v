/*
Bugs item #762027, was opened at 2003-06-27 18:32
Message generated for change (Tracker Item Submitted) made by Item Submitter
You can respond by visiting: 
https://sourceforge.net/tracker/?func=detail&atid=106556&aid=762027&group_id=6556

Category: None
Group: None
Status: Open
Resolution: None
Priority: 5
Submitted By: cdic (cdic)
Assigned to: Nobody/Anonymous (nobody)
Summary: multi-line definition w/o back-slash will be missing

Initial Comment:
There is a small bug (language verilog):
*/
     wire N_84, N_83;     // is ok.
  
     wire N_84,
          N_83;     // then N_83 will be missing in tags.
/*
Thanks for fixing it.

cdic
*/
