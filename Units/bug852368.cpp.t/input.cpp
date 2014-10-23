/*
Bugs item #852368, was opened at 2003-12-01 23:20
Message generated for change (Tracker Item Submitted) made by Item Submitter
You can respond by visiting: 
https://sourceforge.net/tracker/?func=detail&atid=106556&aid=852368&group_id=6556

Category: None
Group: None
Status: Open
Resolution: None
Priority: 5
Submitted By: Welti Marco (cider101)
Assigned to: Nobody/Anonymous (nobody)
Summary: c/c++ unabalanced template brackets in signature

Initial Comment:
hi,

ctags 5.5x generates unbalanced tempalte brackets in 
the  method/function signature if a funtion parameter 
is a template.

i.e.
*/
void foo(std::vector<float> &);
/*
generates the following signature
signature:void (std::vector<<float>&)

let me know if you need more details.

regards
cider
*/
