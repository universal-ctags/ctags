! Bug reported by Brian Helinski <bjh@absoft.com> on 4 Feb 2003
      module numerical_libraries
      interface
      
      subroutine a2ald (nf, nl, y, nrf, indrf, nef, nfef, indef, conper, iprint&
         , model, aov, ems, vc, ldvc, ymeans, wk, iwk, c13ksp) 
      integer, intent(in) :: nf 
      integer, dimension(*), intent(in) :: nl 
      real(kind(1e0)), dimension(*) :: y 
      integer, intent(in) :: nrf 
      integer, dimension(*), intent(in) :: indrf 
      integer, intent(in) :: nef 
      integer, dimension(*), intent(in) :: nfef 
      integer, dimension(*), intent(in) :: indef 
      real(kind(1e0)), intent(in) :: conper 
      integer, intent(in) :: iprint 
      integer, intent(in) :: model 
      real(kind(1e0)), dimension(*), intent(in) :: aov 
      real(kind(1e0)), dimension(*), intent(inout) :: ems 
      real(kind(1e0)), dimension(ldvc,*), intent(inout) :: vc 
      integer, intent(in) :: ldvc 
      real(kind(1e0)), dimension(*), intent(in) :: ymeans 
      real(kind(1e0)), dimension(*), intent(inout) :: wk 
      integer, dimension(*), intent(in) :: iwk 
      character (len = 13), dimension(*), intent(out) :: c13ksp 
      end subroutine  

      
      subroutine b2lsf (fcn, m, n, xguess, ibtype, xlb, xub, xscale, fscale&
         , iparam, rparam, x, fvec, fjac, ldfjac, wk, iwk) 
      integer, intent(in) :: m 
      integer, intent(in) :: n 
      real(kind(1e0)), dimension(*) :: xguess 
      integer, intent(in) :: ibtype 
      real(kind(1e0)), dimension(*), intent(inout) :: xlb 
      real(kind(1e0)), dimension(*), intent(inout) :: xub 
      real(kind(1e0)), dimension(*) :: xscale 
      real(kind(1e0)), dimension(*) :: fscale 
      integer, dimension(*) :: iparam 
      real(kind(1e0)), dimension(*) :: rparam 
      real(kind(1e0)), dimension(*) :: x 
      real(kind(1e0)), dimension(*) :: fvec 
      real(kind(1e0)), dimension(*) :: fjac 
      integer, intent(in) :: ldfjac 
      real(kind(1e0)), dimension(*) :: wk 
      integer, dimension(*) :: iwk 
      interface
         subroutine fcn(m, n, x, f)
         integer, intent(in) :: m, n
         real(kind(1e0)), intent(in) :: x(*)
         real(kind(1e0)), intent(out) :: f(*)
         end subroutine
      end interface
      end subroutine
       
      END INTERFACE 
      end module
