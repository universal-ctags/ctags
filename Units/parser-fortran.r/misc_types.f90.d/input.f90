! Provided by Brian Helsinki, 7 March 2003
	program testalloc
	integer base_type
	automatic foobar_var 	! automatic breaks parsing
	integer i2
	static s_var		! static breaks parsing
	integer i3
	volatile v_var		! volatile breaks var parsing
	integer i4
	
	DLL_IMPORT foobar2	! break var parsing
	DLL_EXPORT foobar	! breaks var parsing

	integer i5
        common foobar_var, s_var, v_var
	integer i6
	
	structure /my_struct/
	integer(1) :: first_byte
	integer(1) :: %fill
	integer(2) :: align_second_16
	end structure
	
	integer i7
	
	record /m_struct/ the_struct ! break var parsing
	integer i8
	
	real, dimension (:), allocatable :: list ! allocatable
	integer i, status
	
	
	i = 99
	allocate (list(i), stat=status ) ! create array allocate
	list (1) = 1.2
	deallocate (list) 	! deallocate
	
	do i=1,100
		j=i
	end do
	stop
	end

