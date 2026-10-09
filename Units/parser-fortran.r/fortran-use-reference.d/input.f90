program main
  use alpha
  use :: beta
  use, intrinsic :: iso_c_binding
  use, non_intrinsic :: gamma
  use delta, only:
  use epsilon, only: thing, local_name => remote_name
  use zeta, local_name => remote_name
  use operators, only: operator(.foo.), assignment(=)
  use renamed_operators, operator(.local.) => operator(.remote.)
  use type
  use, intrinsic :: &
       iso_fortran_env

  USE, INTRINSIC :: MixedCase
  use split_&
&module
  use semicolon_one; use semicolon_two
100 use labelled_module
  ! use commented_out
  implicit none

  block
    use block_module, only: block_name
    integer :: i
    i = 0
  end block
end program main

module container
  use module_dep
  interface api
    subroutine prototype
      use interface_dep
    end subroutine prototype
  end interface api
contains
  subroutine worker
    use subroutine_dep
  end subroutine worker
  function answer() result(value)
    use function_dep
    integer :: value
    value = 42
  end function answer
end module container

submodule (container) child
  use submodule_dep
end submodule child

block data defaults
  use block_data_dep
end block data defaults

use implicit_program_dep
end
