! Bugs item #726875, was opened at 2003-04-24 15:30
! Message generated for change (Tracker Item Submitted) made by Item Submitter
! You can respond by visiting: 
! https://sourceforge.net/tracker/?func=detail&atid=106556&aid=726875&group_id=6556
!
! Category: None
! Group: None
! Status: Open
! Resolution: None
! Priority: 5
! Submitted By: sandra schrödter (schubidu)
! Assigned to: Nobody/Anonymous (nobody)
! Summary: ctags 5.5 and f90: common and !
! 
! Initial Comment:
! ctags won't add a common-block definition to the tags
! file when the following occurs:

        common /coma/ a,b,!foobar
     +                c,d

        common /comb/ e,f

        common /comc/ g,h

! The problem seems to be based on the fact that the
! comment "! foobar" is followed by a continuation line
! ("+" in column 6 shows that)
