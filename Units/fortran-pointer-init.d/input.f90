type :: my_type
  integer :: a
  integer :: b
end type my_type

type(my_type), pointer :: pa => null()

integer, pointer :: pb => null()
