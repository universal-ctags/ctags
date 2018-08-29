#:def assertTrue(cond)
if (.not. ${cond}$) then
  print *, "Assert failed"
  error stop
end if
#:enddef assertTrue
