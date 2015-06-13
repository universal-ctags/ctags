! Bugs item #726712, was opened at 2003-04-24 08:36
! Message generated for change (Comment added) made by schubidu
! You can respond by visiting: 
! https://sourceforge.net/tracker/?func=detail&atid=106556&aid=726712&group_id=6556
! 
! Category: None
! Group: None
! Status: Open
! Resolution: None
! Priority: 5
! Submitted By: sandra schrödter (schubidu)
! Assigned to: Darren Hiebert (dhiebert)
! Summary: ctags 5.5 and f90: "endsubroutine" not recognized
! 
! Initial Comment:
! Dear Ctags'ers. :)
! 
! First of all: good tool! :) I stumbled across ctags for
! my gvim6, wanted to use the nice plugin taglist.
! 
! My System: HP-Unix and ctags 5.5
! 
! My problem is the following. The command:
! 
! "ctags tagstest.f90"
! 
! builds a tagsfile that lacks 1 of the declared 3
! subroutines. 
! The problem disappears when the declaration of the
! subroutine ends in "end subroutine" and not in
! "endsubroutine", although "endsubroutine" is correct
! Fortran90 syntax, AFAIK.
! 
! Is there a chance to get this corrected?
! 
! 
! The example file tagstest.f90:

      subroutine tagstest_ctrl()
         real :: a
         a = 0.0
         call sub1( a)
         call sub2( a)
      endsubroutine tagstest_ctrl

      subroutine sub1( a)
      real,intent( inout) ::a
      a = 1.1
      endsubroutine sub1
      
      subroutine sub2( a)
      real,intent( inout) ::a
      a = 2.2
      endsubroutine sub2

! ----------------------------------------------------------------------
! 
! Comment By: sandra schrödter (schubidu)
! Date: 2003-05-28 13:17
! 
! Message:
! Logged In: YES 
! user_id=763447
! 
! I realized that there are also problems when a subroutine
! ends only with the keyword
! end
! 
! The following subroutine is listed but cannot be selected
! (that is, the cursor wont go there)
! 
! ----------------------------------------------------------------------
