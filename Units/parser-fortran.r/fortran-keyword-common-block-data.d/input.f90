! Keywords can name block data, common blocks, namelists, and their members.
block data intrinsic
  integer :: value
  common /intrinsic/ value
end block data intrinsic

program consumer
  integer :: intrinsic(2), other
  common /storage/ intrinsic, other
  namelist /intrinsic/ intrinsic, other
end program consumer
