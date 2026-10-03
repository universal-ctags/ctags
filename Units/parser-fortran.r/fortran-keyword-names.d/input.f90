! Fortran keywords remain valid names outside their keyword contexts.
module intrinsic
end module intrinsic

submodule (intrinsic) child
contains
  subroutine worker()
  end subroutine worker
end submodule child

submodule (intrinsic:child) grandchild
end submodule grandchild

subroutine outer()
  entry intrinsic()
end subroutine outer
