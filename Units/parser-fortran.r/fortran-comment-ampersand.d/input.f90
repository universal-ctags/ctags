module m
   !! a comment ending in an ampersand &
   integer :: v1
   integer :: v2  ! trailing comment &
   integer :: v3
   integer :: v4, &  ! continuation before a comment
               v5
   character(len=8) :: s = 'a!b &'
   integer :: v6
contains
   subroutine s1 ()
      !! comment &
      !! second comment
      integer :: v7
   end subroutine s1
end module m
