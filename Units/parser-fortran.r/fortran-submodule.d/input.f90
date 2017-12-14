!
! Derived from comment #1616 submitted by @cbcoutinho
!
submodule (example_mod) example_smod
contains
    module function g(x) result(y)
        integer :: x,y
        y = x * 2
    end function g
end submodule example_smod

submodule (example_mod:example_smod) example_ssmod
contains
    module function g2(x) result(y)
        integer :: x,y
        y = x * 2
    end function g2
end submodule example_ssmod

submodule (example_mod:example_smod:example_ssmod*broken*) example_sssmod
contains
    module function g3(x) result(y)
        integer :: x,y
        y = x * 2
    end function g3
end submodule example_sssmod

