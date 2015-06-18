module Test
  implicit none

  procedure(ProcOne), pointer :: MyProc => null(), &
  MyOtherProc => ProcTwo
  real, parameter :: myparameter

contains

  function ProcOne(arg1, optformat)
    ! not relevant
  end function ProcOne

  function ProcTwo(arg1, optformat)
    ! not relevant
  end function ProcTwo

end module Test

program Main
  implicit none

  ! deliberately break up the line to make sure the tagparser doesn't choke on awkward cases
  procedure(:), pointer :: ProcPointOne &
  => null(), ProcPointTwo => &
  null()
  real :: variable, variable_two
  integer :: variable_three

  ProcPointOne => ProcTwo

end program Main
