/* Taken from linux/arch/powerpc/include/asm/head-64.h */
#ifdef __ASSEMBLY__
.macro define_ftsec name
	.section ".head.text.\name\()","ax",@progbits
.endm
#endif
