!	result/recursive broken
	MODULE approx
	interface
		
	recursive function arcsin (angle, terms) ! no result keyword
		double precision arcsin1
		double precision angle
		integer terms
	end function arcsin
		
	recursive function arcsin1 (angle, terms) result (value) ! no explicit type
		double precision angle
		integer terms
		double precision value
	end function arcsin1
	
	double precision recursive function arcsin2 (angle, terms) result (value) ! type + recurs
		double precision angle
		integer terms
	end function arcsin2
			
			
	! only this function seems to be found
	recursive double precision function arcsin3 (angle, terms) result (value ) ! recurs + type
		double precision angle
		integer terms
	end function arcsin3
	
		
	end interface
	double precision :: y
	integer :: parts
	END MODULE


	MODULE approx2
	interface
	
	double precision recursive function as (angle, terms) result (value) ! type + recurs
		double precision angle
		integer terms
	end function as
			
	recursive double precision function as1 (angle, terms) result (value ) ! recurs + type
		double precision angle
		integer terms
	end function as1
	
	! but now I can see this one 
	recursive function as2 (angle, terms) ! no result keyword
		double precision arcsin1
		double precision angle
		integer terms
	end function as2
		
	! .. and this one!
	recursive function as3 (angle, terms) result (value) ! no explicit type
		double precision angle
		integer terms
		double precision value
	end function as3
	
		
	end interface
	double precision :: z
	integer :: parts2
	END MODULE

