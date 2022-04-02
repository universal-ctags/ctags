#ifndef __ASSEMBLER__

inline void do_nothing_6(void) {}

#elif CONDITION5

	.macro	DO_NOTHING_6a_this_cannot_be_tagged
	 nop
	.endm

#else

	.macro	DO_NOTHING_6b_this_cannot_be_tagged
	 nop
	.endm

#endif
