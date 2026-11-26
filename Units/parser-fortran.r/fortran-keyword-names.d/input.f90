! Fortran keywords remain valid names outside their keyword contexts.
module non_intrinsic
end module non_intrinsic

submodule (non_intrinsic) child
contains
  subroutine worker()
  end subroutine worker
end submodule child

submodule (non_intrinsic:child) grandchild
end submodule grandchild

subroutine outer()
  entry non_intrinsic()
end subroutine outer
