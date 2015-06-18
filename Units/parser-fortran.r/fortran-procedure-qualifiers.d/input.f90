module test
  type atype
  contains
    procedure, pass :: add
    procedure, pass(self) :: append
    procedure, nopass :: print
    procedure, non_overridable :: write
    procedure, deferred :: list
  end type atype
end module test
