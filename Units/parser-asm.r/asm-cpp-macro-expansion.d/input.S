#define ENTRY(LABEL) .global LABEL	;\
LABEL

#comment
ENTRY(main):
	nop

#define ENTRY2(LABEL,UNUSED) .global LABEL	;\
LABEL
ENTRY2(loop0, "STRING"):
	nop

ENTRY2(loop1, 'c'):
	nop

ENTRY2(loop2, ('c')):
	nop

/* Taken from linux kernel */
#define ASM_NL		 ;
#define SYM_ENTRY(name, linkage, align...)		\
	linkage(name) ASM_NL				\
	align ASM_NL					\
	name:

#define SYM_START(name, linkage, align...)		\
	SYM_ENTRY(name, linkage, align)

#define SYM_CODE_START(name)				\
	SYM_START(name, SYM_L_GLOBAL, SYM_A_ALIGN)

SYM_CODE_START(asm_exc_nmi)
	UNWIND_HINT_IRET_REGS
	ENDBR
