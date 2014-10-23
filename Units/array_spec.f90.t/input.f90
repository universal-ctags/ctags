! Bug reported by Jim Chen on 31 Dec 2002
module Global_Variables
implicit none

! Example of array-spec in entity-decl
real*8, save ::         &
H (NDIM, NDIM) = 0.D0,  &
H0(NDIM, NDIM),         &
H1(NDIM, NDIM) = 0.D0,  &
H2(NDIM, NDIM) = 0.D0,  &
H3(NDIM, NDIM) = 0.D0,  &
H4(NDIM, NDIM) = 0.D0,  &
H5(NDIM, NDIM) = 0.D0

end module Global_Variables
