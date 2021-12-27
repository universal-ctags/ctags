	;; Taken from "7.63 '.macro'", the section of the info document for Gas

	.macro comm
	.endm

	.macro plus1 p, p1
	.endm

	.macro plus2 p p1
	.endm

	.macro reserve_str p1=0 p2
	.endm

	.macro m p1:req, p2=0, p3:vararg
	.endm

	;; Taken From linux/arch/m68k/kernel/head.S
	.macro	func_define	name,nr=0
	.macro	\name	arg1,arg2,arg3,arg4
		move_stack	\nr,\arg1,\arg2,\arg3,\arg4
		func_call	\name
	.if	\nr
		lea	%sp@(\nr*4),%sp
	.endif
	.endm
	.endm
