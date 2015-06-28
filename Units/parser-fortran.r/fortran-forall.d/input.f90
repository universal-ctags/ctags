! a module that uses a forall block

module with_forall

  real :: a

contains

  subroutine sub_with_forall(x)
    real, intent(inout) :: x(:)

    integer :: i

    forall(i=1:size(x))
       x(i) = 0.5**i
    end forall
  end subroutine sub_with_forall


  function two() result(res)
    real :: res

    res = 2.0
  end function two

end module with_forall
