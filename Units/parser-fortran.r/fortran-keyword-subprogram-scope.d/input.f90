! Keywords can name programs, subroutines, and functions, which remain scopes.
program intrinsic
  integer :: x
end program intrinsic

module holder
contains
  subroutine intrinsic()
    integer :: y
  end subroutine intrinsic
end module holder

module other_holder
contains
  integer function intrinsic()
    integer :: z
    intrinsic = 0
  end function intrinsic
end module other_holder
