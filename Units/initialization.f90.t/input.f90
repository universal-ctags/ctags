! Tests for correct parsing of complicated initialization
      MODULE funcon
      IMPLICIT NONE
      REAL(8),DIMENSION(3,3),PARAMETER :: &
         imat= reshape((/1.d0,0.d0,0.d0, &
                         0.d0,1.d0,0.d0, &
                         0.d0,0.d0,1.d0/),(/3,3/))
      REAL(8),DIMENSION(6,6),PARAMETER :: &
         imat6= reshape((/1.d0,0.d0,0.d0,0.d0,0.d0,0.d0, &
                          0.d0,1.d0,0.d0,0.d0,0.d0,0.d0, &
		 	  0.d0,0.d0,1.d0,0.d0,0.d0,0.d0, &
                          0.d0,0.d0,0.d0,1.d0,0.d0,0.d0, &
			  0.d0,0.d0,0.d0,0.d0,1.d0,0.d0, &
                          0.d0,0.d0,0.d0,0.d0,0.d0,1.d0/),(/6,6/))			 

      END MODULE funcon
