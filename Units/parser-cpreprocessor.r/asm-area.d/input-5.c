#ifndef __ASSEMBLER__

inline void do_nothing_5(void) {}

#else

	.macro	DO_NOTHING_5
	 nop
	.endm

#endif
