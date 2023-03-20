      module my_mod

      enum, bind(c)
        enumerator :: my_constr = 1, my_second_constr = 4
      end enum

      type my_type
        integer a
        integer b
        integer c
      contains
        procedure :: my_proc => my_func
      end type my_type

      type my_sequence_type
        sequence
        integer a
        integer b
      end type

      type(my_sequence_type) :: my_mod_type

      common /my_mod_common/ my_mod_type

      contains

      function my_func(x)
        class(my_type), intent(in) :: x
        my_func = x%a
        return
        my_func = 2
        return
      end function my_func


      end module

      block data my_block
        integer my_var
        common /my_common/ my_var
        data my_var/123/
      end

      subroutine my_subr
        use my_mod
        integer :: my_enum = my_constr
        print *, my_enum
        return
        entry my_entry
        print *, "an entry!"
      end subroutine my_subr


      program my_main
        use my_mod
        interface my_interface
          subroutine my_subr()
          end subroutine my_subr
        end interface

        integer my_var
        namelist /my_namelist/ my_var
        type(my_type) :: my_conc_type
        common /my_common/ my_var
        call my_subr()
        print *, my_conc_type%my_proc()
      end program my_main
