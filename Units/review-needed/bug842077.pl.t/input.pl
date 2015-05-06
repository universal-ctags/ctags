# Bugs item #842077, was opened at 2003-11-14 10:57
# Message generated for change (Tracker Item Submitted) made by Item Submitter
# You can respond by visiting: 
# https://sourceforge.net/tracker/?func=detail&atid=106556&aid=842077&group_id=6556

# Category: None
# Group: None
# Status: Open
# Resolution: None
# Priority: 5
# Submitted By: Christian Reis (kiko_async)
# Assigned to: Nobody/Anonymous (nobody)
# Summary: wrong precedence applied to perl POD and "here document"

# Initial Comment:
# Basically, ctags stops parsing when it reaches code
# with the syntax below:

# ...
$this->print_log(<<EOM);
== Tinderbox Info
...
== End Tinderbox Client Info
EOM
# ...

sub test {
# This subroutine is missed when bug is present.
}

# It sounds to me like ctags thinks it's a POD comment
# when it
# isn't--wrong precedence of POD vs. here document.
