module Members
  implicit none

  TYPE humongous_matrix(k, d)
    !-- Derived-type parameters
    INTEGER, KIND :: k = kind(0.0)
    INTEGER(selected_int_kind(12)), LEN :: d
    REAL(k) :: element(d,d)
  END TYPE

  TYPE general_point(dim)
    INTEGER, KIND :: dim
    REAL :: coordinates(dim)
  END TYPE

contains

  subroutine MySubroutine(arg)
  ! ...
  end subroutine MySubroutine

end module Members
