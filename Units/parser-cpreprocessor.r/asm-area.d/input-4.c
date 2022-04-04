#ifdef __ASSEMBLER__

	.macro	DO_NOTHING_4
	 nop
	.endm

#elif CONDITION4

	inline void do_nothing_4a (void) {}

#else

	inline void do_nothing_4b (void) {}

#endif
