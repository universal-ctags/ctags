module with_associate
  real :: a

contains

  function do_stuff(a) result(c)
    real, intent(in) :: a

    associate (b => a)
      c = b
    end associate
  end function do_stuff

  subroutine do_other_stuff(a)
    real, intent(in out) :: a

    a = 2 * a
  end subroutine do_other_stuff

end module with_associate
