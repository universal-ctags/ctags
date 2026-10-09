! Keywords can name block data, common blocks, namelists, and their members.
block data non_intrinsic
  integer :: value
  common /non_intrinsic/ value
end block data non_intrinsic

program consumer
  integer :: non_intrinsic(2), other
  common /storage/ non_intrinsic, other
  namelist /non_intrinsic/ non_intrinsic, other
end program consumer
