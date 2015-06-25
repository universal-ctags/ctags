module test

  type, abstract :: atype
  contains
    procedure(suba), deferred :: add
  end type
  type, abstract, extends(atype) :: btype
  end type

  abstract interface
    subroutine suba(self, b)
      use, intrinsic :: iso_c_binding
      import :: atype
      import btype
      implicit none
      integer c, d
      class(atype) :: self, b
    end subroutine
  end interface

end module test
