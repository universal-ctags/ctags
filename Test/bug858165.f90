! Bugs item #858165, was opened at 2003-12-11 10:09
! Message generated for change (Tracker Item Submitted) made by Item Submitter
! You can respond by visiting: 
! https://sourceforge.net/tracker/?func=detail&atid=106556&aid=858165&group_id=6556
! 
! Category: None
! Group: None
! Status: Open
! Resolution: None
! Priority: 5
! Submitted By: Blazej Krzeminski (blazk)
! Assigned to: Nobody/Anonymous (nobody)
! Summary: Fortran90: comment line after continuation character &
! 
! Initial Comment:
program test

integer :: a, &   !comment on variable a
           b, &   !comment on variable b
                  !more comment on variable b, CTAGS STOPS HERE
           c, &   !comment on variable c
           d      !comment on variable d
end program test

! ctags will index program test, a,b but not c,d
