! Bugs item #877956, was opened at 2004-01-15 17:59
! Message generated for change (Tracker Item Submitted) made by Item Submitter
! You can respond by visiting: 
! https://sourceforge.net/tracker/?func=detail&atid=106556&aid=877956&group_id=6556
!
! Category: None
! Group: None
! Status: Open
! Resolution: None
! Priority: 5
! Submitted By: Randy Hood (randy762)
! Assigned to: Nobody/Anonymous (nobody)
! Summary: Broken Fortran variable listing after =-1
!
! Initial Comment:
! When I run ctags v5.5.2 on Redhat Linux 9 with the command
!
! ctags --Fortran-kinds=v  -x test.f90
!                                      
! where test.f90 is
! ----------------------------------
PROGRAM test

 IMPLICIT NONE
 INTEGER :: cm1 =-1, c2 = 2

END PROGRAM test
! -------------------------------------
!                                      
! I only get this one line of output
!
! cm1              variable      4 test.f90        INTEGER :: cm1 =-1, c2 = 2
!                                                      
! If I change one line of test.f90 so that it is now
! ----------------------------------------
PROGRAM test

 IMPLICIT NONE
 INTEGER :: cm1 = -1, c2 = 2

END PROGRAM test
! -----------------------------------------
! and run the command
!                                      
! ctags --Fortran-kinds=v  -x test.f90
!                                      
! I get this correct output
!                                      
! c2               variable      4 test.f90        INTEGER :: cm1 = -1, c2 = 2
! cm1              variable      4 test.f90        INTEGER :: cm1 = -1, c2 = 2
!
! ----------------------------------------------------------------------
! You can respond by visiting: 
! https://sourceforge.net/tracker/?func=detail&atid=106556&aid=877956&group_id=6556
