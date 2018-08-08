#! This test case is derived from #1810, a pull request submitted by @p-vitt
#! and parser-fortran.r/fortran-interface.d/input.f90.
#! Callable needs only string argument
#:def debug_code(code)
  #:if DEBUG > 0
    $:code
  #:endif
#:enddef debug_code

#! Pass code block as first positional argument
#:call debug_code
  if (size(array) > 100) then
    print *, "DEBUG: spuriously large array"
  end if
#:endcall debug_code

program helloworld
print *, "Hello, world."
end program helloworld

module test_interface
#! Callable needs also non-string argument types
#:def repeat_code(code, repeat)
  #:for ind in range(repeat)
    $:code
  #:endfor
#:enddef repeat_code
  type atype
  end type atype
  ! operator overload
  interface operator(+)
    ! subprogram prototype
    type(atype) function add(a, b)
      import atype
      type(atype), intent(in) :: a, b
    end function add
  end interface operator(+)
  ! wrap subprogram prototypes
  interface ! anonymous interface
    subroutine suba()
    end subroutine suba
    subroutine subb()
    end subroutine subb
  end interface
  ! define generic subprograms
#! fypp preprocessor comments  here, and
  interface get
#! there
    ! subprogram name list
    module procedure get_1d
    module procedure get_2d
  end interface get
contains
  ! definition of subprograms
  subroutine get_1d(a)
    real a(:)
  end subroutine get_1d
#! Pass code block as positional argument and 3 as keyword argument "repeat"
#:call repeat_code(repeat=3)
this will be repeated 3 times
#:endcall repeat_code
  subroutine get_2d(a)
    real a(:, :)
  end subroutine get_2d
end module test_interface


