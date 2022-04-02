#if __ASSEMBLER__

	.macro	DO_NOTHING_2
	 nop
	.endm

#else

	inline void do_nothing_2 (void) {}

#endif
