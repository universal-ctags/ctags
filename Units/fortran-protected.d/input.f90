module m_test
  integer, protected :: a
contains
  subroutine init(val)
    integer val
    a = val
  end subroutine init
end module m_test

program test
  use m_test

  call init(5)
  print *, a
  ! if you uncomment these lines, the compiler should flag an error
  !a = 10
  !print *, a
  call init(10)
  print *, a
end program test
