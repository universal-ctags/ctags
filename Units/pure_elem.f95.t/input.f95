!	elemental
!
!Pure procedures are procedures declared with the PURE keyword and which 
!satisfy certain constraints that ensure they have no side 
!effects. They can be used in specification expressions and 
!within FORALL constructs and statements. 

!Elemental procedures can be written in Fortran 95 using the ELEMENTAL 
!keyword. Elemental procedures are automatically ``pure''. 

!Example: 

	PURE REAL FUNCTION pure_func(x,y) ! pure okay
		IMPLICIT NONE
		REAL, INTENT(IN) ::  x, y
		pure_func = x*x + y*y + 2*x*y + ASIN(MIN(x/y,y/x))
	END FUNCTION pure_func

	PURE REAL FUNCTION F(x,y) ! pure broken returns 'PURE REAL FU'
		IMPLICIT NONE
		REAL, INTENT(IN) ::  x, y
		F = x*x + y*y + 2*x*y + ASIN(MIN(x/y,y/x))
	END FUNCTION F


	ELEMENTAL REAL FUNCTION elem_maxabs(a,b) ! elemental broke
		IMPLICIT NONE
		REAL,INTENT(IN) :: a,b
		elem_maxabs = MAX(ABS(a),ABS(b))
	END

	PURE REAL FUNCTION pure_maxabs2(a,b) ! pure okay
		IMPLICIT NONE
		REAL,INTENT(IN) :: a,b
		pure_maxabs2 = MAX(ABS(a),ABS(b))
	END

