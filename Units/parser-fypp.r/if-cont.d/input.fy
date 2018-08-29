STRUCTURE /STRU/
	INTEGER*4 X
#:if cA > 0 &
     DEBUG > 0 &
     TRACE > 0
	INTEGER*4 A0
#:if cB > 0 &
     DEBUG > 0
	INTEGER*4 B0
#:if cC > 0 &
     DEBUG > 0
	INTEGER*4 C
#:elif cCELIF > 0 &
     DEBUG > 0
	INTEGER*4 CELIF
#:else
	INTEGER*4 CELSE
#:endif
	INTEGER*4 B1
#:elif cBELIF > 0 &
     DEBUG > 0
	INTEGER*4 BELIF
#:else
	INTEGER*4 BELSE
#:endif
	INTEGER*4 A1
#:elif cAELIF > 0 &
     DEBUG > 0
	INTEGER*4 AELIF
#:else
	INTEGER*4 AELSE
#:endif
	INTEGER*4 Y
END STRUCTURE
