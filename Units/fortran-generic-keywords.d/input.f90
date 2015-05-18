module abstract_list_mod

  real(8), parameter ::            &
    PI           = 3.1415926535898_8, & ! pi
    MASS_NEUTRON = 1.008664916,       & ! mass of a neutron in amu
    MASS_PROTON  = 1.007276466812,    & ! mass of a proton in amu
    AMU          = 1.660538921e-27,   & ! 1 amu in kg
    N_AVOGADRO   = 0.602214129,       & ! Avogadro's number in 10^24/mol
    K_BOLTZMANN  = 8.6173324e-11,     & ! Boltzmann constant in MeV/K
    INFINITY     = huge(0.0_8),       & ! positive infinity
    ZERO         = 0.0_8,             &
    ONE          = 1.0_8,             &
    TWO          = 2.0_8

  type, abstract :: list
    private
    integer :: num = 0
    class(link),pointer :: firstLink => null(), &
      secondLink => null()
    class(link),pointer :: lastLink => null()  ! last link in list
    type(link),pointer :: currLink => null()  ! list iterator
  contains
    procedure, non_overridable :: addValue    ! add value to list
    procedure, non_overridable :: firstValue  ! get first value in list
    procedure, non_overridable :: reset       ! reset list iterator
    procedure, non_overridable :: next        !iterate to next value in list
    procedure, non_overridable :: currentValue! get current value in list
    procedure, non_overridable :: moreValues  ! more values to iterate?
    generic, private :: add => addValue, addValues, &
      addValues_2d
    procedure(printValues), deferred :: printList  ! print contents of list
  end type list
  abstract interface
    subroutine printValues(this)
      import list
      class(list) :: this
    end subroutine printValues
  end interface
end module abstract_list_mod
