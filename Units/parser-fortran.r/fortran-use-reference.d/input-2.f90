module include_container
  include 'empty.inc'
  use iso_fortran_env, only: int32
  include 'empty.inc'
  include 'empty.inc'
  use iso_c_binding, only: c_int
  implicit none
  integer(int32) :: value
end module include_container
