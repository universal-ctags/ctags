! Provided by Brian Helsinki, 7 March 2003
!	cexternal
!	cglobal
!	pexternal
!	pglobal
!	inline 
!	virtual
!	volatile
!	pascal
	program specs

	VIRTUAL M(10,10), Y(100) 
	VOLATILE V, Z, MAT, /INI/ 

	EXTERNAL ABS ! varations of external and global
	CEXTERNAL ABS1 ! not supported
	CGLOBAL ABS2 ! not supported
	PEXTERNAL ABS3 ! not supported
$IF DEFINED(MAC_DEP)
	PASCAL EXTERNAL ABS3_var2 ! not supported
$ENDIF
	PGLOBAL ABS4
	
	INTEGER INFOOBAR
	INLINE (INFOOBAR=00000) ! not supported

	INTEGER M(5)
	DATA M/5*0/
	
	CALL INFOOBAR(5,4)
	END

