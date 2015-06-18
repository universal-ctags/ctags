module test_interface
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
  interface get
    ! subprogram name list
    module procedure get_1d
    module procedure get_2d
  end interface get
contains
  ! definition of subprograms
  subroutine get_1d(a)
    real a(:)
  end subroutine get_1d
  subroutine get_2d(a)
    real a(:, :)
  end subroutine get_2d
end module test_interface
