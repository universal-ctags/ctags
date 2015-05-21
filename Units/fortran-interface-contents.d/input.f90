! -*- coding: utf-8 -*-
!============================================================================
! Description of the module ...
! @author Qingming He, NECP
! @date 2015-05-17 16:59:15
!============================================================================
module test_interface_contents

  implicit none

  interface vstring_new
    module procedure vstring_new_empty
    module procedure vstring_new_from_charstring
    module procedure vstring_new_from_vstring
    module procedure vstring_new_from_chararray
    module procedure vstring_new_from_integer
  end interface vstring_new

  interface suba
    subroutine suba_c ()
      ! do nothing
    end subroutine suba_c
  end interface suba

contains

end module test_interface_contents
