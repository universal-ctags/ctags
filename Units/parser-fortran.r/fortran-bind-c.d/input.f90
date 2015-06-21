! See https://gcc.gnu.org/onlinedocs/gfortran/Interoperability-with-C.html
module test_bind_c
  use iso_c_binding

  ! test derived type
  type, bind(c) :: atype
    real(c_double) :: a
  end type atype
  ! this is
  ! struct {
  !   double a;
  ! } atype;
  ! in C language

  ! test interoperable global variable
  integer(C_INT), bind(C, name="_MyProject_flags") :: global_flag

  ! test Interoperable Subroutines and Functions
  interface
    subroutine print_c(string) bind(C, name="print_C")
      use iso_c_binding, only: c_char
      character(kind=c_char) :: string(*)
    end subroutine print_c
  end interface
end module test_bind_c
