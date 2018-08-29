#! This test case is taken from #1810, a pull request submitted by @p-vitt.
#:def assertTrue(cond)
if (.not. ${cond}$) then
  print *, "Assert failed"
  error stop
end if
#:enddef assertTrue
