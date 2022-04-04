#if __ASSEMBLER__

	.macro	DO_NOTHING_3
	 nop
	.endm

#elif CONDITION3

	inline void do_nothing_3a (void) {}

#else

	inline void do_nothing_3b (void) {}

#endif
