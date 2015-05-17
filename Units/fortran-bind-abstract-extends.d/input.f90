module test_bind
  type, extends(type_b) :: type_c
    integer :: i = 0
  end type type_c
  type, bind(c) :: type_d
    integer :: i = 0
  end type type_d
  type, abstract :: type_e
    integer :: i = 0
  end type type_e
end module test_bind
