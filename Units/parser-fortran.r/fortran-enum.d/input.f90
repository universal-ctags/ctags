module Constants
  implicit none

  real, parameter :: pi = 4 * atan(1.0)
  real, parameter :: E_e = 510998.91013

  ! we now have enumerators in F2003/8, for the sake of interop with C
  enum, bind(c) ! unnamed 1
    enumerator :: red =1, blue, black =5
    enumerator yellow
    enumerator gold, silver, bronze
    enumerator :: purple
    enumerator :: pink, lavender
  end enum

  enum ! unnamed 2
    enumerator :: a, b, c
  end enum

  enum :: Named1
    enumerator :: x1, y1, z1
  end enum

  enum Named2
    enumerator :: x2, y2, z2
  end enum

  enum(8) Named3
    enumerator :: x3, y3, z3
  end enum

  enum*8 Named4
    enumerator :: x4, y4, z4
  end enum

  enum(8) :: Named5
    enumerator :: x5, y5, z5
  end enum

  enum*8 :: Named6
    enumerator :: x6, y6, z6
  end enum

  enum, bind(c) :: Named7
    enumerator :: x7, y7, z7
  end enum

  real, parameter :: hc = 12398.4193

  public

end module Constants
