module mod1
  interface iface1
    subroutine proto1()
    end subroutine proto1
  end interface iface1

  type :: type1
    integer :: x
  end type type1

  structure /struct1/
    integer :: y
  end structure

  enum :: enum1
  end enum

contains

  subroutine sub1()
  end subroutine sub1

  function func1() result(x)
    integer :: x
  end function func1
end module mod1

submodule (mod1) submod1
contains
  module subroutine sub2()
  end subroutine sub2
end submodule submod1

block data block1
  integer :: z
end block data block1

program prog1
end
