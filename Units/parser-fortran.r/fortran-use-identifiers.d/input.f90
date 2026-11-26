program identifiers
  implicit none
  integer :: use(2)
  integer, target :: p
  ! These assignments start with a keyword used as an identifier.
  use = 1
  use(1) = 2
  use => p
  block
    use valid_in_block
  end block
end program identifiers

! Exercise the specification-part path in an implicit main program, too.
use = 1
use(1) = 2
use => p
end
