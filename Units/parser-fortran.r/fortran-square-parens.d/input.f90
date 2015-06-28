module squaretest
  implicit none

  type Symmetry
    integer :: matrix(3,3), iterations
  end type Symmetry


  ! make sure F2003 '[]' arrays don't break the symbol list
  type(Symmetry), parameter :: symmetries(14) = &
     [ Symmetry(reshape([1,0,0,  0,1,0,  0,0,1], [3,3]),1), &
       Symmetry(reshape([1,0,0,  0,0,1,  0,-1,0],[3,3]),3), &
       Symmetry(reshape([0,0,-1, 0,1,0,  1,0,0], [3,3]),3) ]
  integer :: invisible

  ! make sure coarray syntax doesn't break the symbol list
  real, allocatable :: state(:)[:]
  integer :: invisible_two

  ! there are multiple ways to specify `dimension` and `codimension`
  ! a normal dimension and a codimension
  real, intent(in), allocatable, dimension(:), codimension[:] :: state_two
  integer :: invisible_three

  ! a 'normal' scalar (no '()'), but with a codimension '[]'
  real, allocatable :: assignee[:]
  integer :: invisible_four

contains

  subroutine execute(state)
    real, intent(in) :: state(:)[:]
  end subroutine execute

end module squaretest

program main
  use module squaretest
end program main
