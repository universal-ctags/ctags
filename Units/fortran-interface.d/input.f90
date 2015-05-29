program main

  interface suba_interface
    ! We don't need tag for this
    subroutine suba(a, b)
      integer, intent(in) :: a
      integer, intent(inout) :: b
    end subroutine suba
  end interface suba_interface

  integer :: a
  integer :: b

  a = 1
  call suba(a, b)
  write(*, *) b

end program main

! Here is the definition of subroutine suba. We want tag for this.
subroutine suba(a, b)
  integer, intent(in) :: a
  integer, intent(inout) :: b
  b = a
end subroutine suba
