module test_signature

  interface
    subroutine subb (arg)
      type(atype) :: arg
    end subroutine
  end interface
  
contains
  subroutine suba &
      (a, &
      ! comment
    b)
    integer a, b
  end subroutine

  real function fna()
    use, intrinsic :: iso_c_binding
  end function

  subroutine fnb
    integer :: a
  end subroutine

end module test_signature
