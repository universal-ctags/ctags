module test_procedure_keyword

  type :: test_type
    integer :: a = 0
  contains
    procedure, nopass :: suba => suba
    procedure, pass :: subb => subb
  end type test_type

  procedure(real(8)) :: getpid

contains

  subroutine suba()
    ! do nothing
  end subroutine suba

  subroutine subb(self)
    class(test_type), intent(in) :: self
    ! do nothing
  end subroutine subb

end module test_procedure_keyword
