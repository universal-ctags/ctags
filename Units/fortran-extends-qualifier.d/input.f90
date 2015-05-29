module test_extends
  type shape
    integer :: color
    logical :: filled
    integer :: x
    integer :: y
  end type shape
  type, extends(shape) :: rectangle
    integer :: length
    integer :: width
  end type rectangle
end module test_extends
