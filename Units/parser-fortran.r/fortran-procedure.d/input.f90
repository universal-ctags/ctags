! a module that uses procedure pointer
module proc_pointer

  real :: a
  procedure(sub), pointer :: my_pointer

contains

  subroutine sub(x)
    real, intent(inout) :: x(:)

    integer :: i

    do i=1,size(x)
      x(i) = 0.5**i
    end do
  end subroutine sub

end module proc_pointer
