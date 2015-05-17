module test_bind

  implicit none

  type, bind(c) :: Earch
    integer :: id = 0
    integer :: type                  ! Type
    integer :: n_cells               ! # of cells within
    integer, allocatable :: cells(:) ! List of cells within
    real(8) :: x0                    ! Translation in x-coordinate
    real(8) :: y0                    ! Translation in y-coordinate
    real(8) :: z0                    ! Translation in z-coordinate
  end type Earch

  type, extends(Earth) :: Universe
    integer :: id = 0
    integer :: type                  ! Type
    integer :: n_cells               ! # of cells within
    integer, allocatable :: cells(:) ! List of cells within
    real(8) :: x0                    ! Translation in x-coordinate
    real(8) :: y0                    ! Translation in y-coordinate
    real(8) :: z0                    ! Translation in z-coordinate
  end type Universe

  type, abstract :: abs_type
    integer :: a
    integer, allocatable :: cells(:) ! List of cells within
  end type abs_type

contains

  pure function add(a, b) bind(C, name="add_c")
    real(8) :: a
    real(8) :: b
  end function add

end module test_bind
