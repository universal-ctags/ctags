! Keywords can name programs, subroutines, and functions, which remain scopes.
program non_intrinsic
  use program_dep
  integer :: x
end program non_intrinsic

module holder
contains
  subroutine non_intrinsic()
    use subroutine_dep
    integer :: y
  end subroutine non_intrinsic
end module holder

module other_holder
contains
  integer function non_intrinsic()
    use function_dep
    integer :: z
    non_intrinsic = 0
  end function non_intrinsic
end module other_holder
