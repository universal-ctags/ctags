! Bugs item #670443, was opened at 2003-01-18 22:44
! You can respond by visiting: 
! https://sourceforge.net/tracker/?func=detail&atid=106556&aid=670443&group_id=6556
! 
! Category: None
! Group: None
! Status: Open
! Resolution: None
! Priority: 5
! Submitted By: Erik Edelmann (fanerier)
! Assigned to: Nobody/Anonymous (nobody)
! Summary: Fortran: Internal procedures causes trouble
! 
! Initial Comment:
! If a function in a module has an internal procedure
! (after a CONTAINS-statement), everything in the module
! after the parent function will be skipped.  I.e. in the
! attached file, the subroutine 'bar' will not be listed
! in the 'tags' file.  Note that this problem arises only
! when the parent procedure is a function, if it is a
! subroutine everything works fine.

module foobar

contains

    integer function foo (i)

    contains    
        
        real function f(x)
        end function f

    end function foo


    subroutine bar (n)
    end subroutine bar

end module foobar
