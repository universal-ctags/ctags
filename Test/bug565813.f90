module ctags_bug

    implicit none
    private
    save

    type :: foo_t
        integer :: bar
    end type foo_t

    integer, parameter :: N = 1000

    public :: foo_t
    public :: foo_setbar
    public :: foo_set_bar
    public :: foo_getbar

contains

    subroutine foo_setbar (f,b)
        type(foo_t), intent(out) :: f
        integer, intent(in) :: b

        f%bar = b
    end subroutine foo_setbar


    pure subroutine foo_set_bar (f,b)
        type(foo_t), intent(out) :: f
        integer, intent(in) :: b

        f%bar = b
    end subroutine foo_set_bar


    integer function foo_getbar (f)
        type(foo_t), intent(in) :: f

        foo_getbar = f%bar
    end function foo_getbar

end module ctags_bug
