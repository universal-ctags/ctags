module test_fortran_final

  implicit none

  type :: test_type
    integer, allocatable :: a(:)
  contains
    final :: clear
  end type test_type

contains

  subroutine clear(self)
    type(test_type), intent(inout) :: self
    if (allocated(self % a)) deallocate(self % a)
  end subroutine clear

end module test_fortran_final
