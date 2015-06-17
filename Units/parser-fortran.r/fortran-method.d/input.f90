module test_method
  type :: atype
  contains
    generic :: add => add_1d, &
      add_2d, &
      add_3d
    generic :: operator(+) => add_1d, add_2d
    generic :: assignment(=) => assigns
    procedure, deferred :: add_1d, add_2d, add_3d
    final clean
  end type atype
end module test_method
