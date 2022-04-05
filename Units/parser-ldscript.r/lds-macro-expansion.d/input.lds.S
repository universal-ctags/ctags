/* Based on linux/include/asm-generic/vmlinux.lds.h and
 * linux/arch/x86/kernel/vmlinux.lds.S */

#define MEM_DISCARD(sec) *(.mem##sec)

#define INIT_TEXT(X,A)				\
	*A					\
	*X					\
	MEM_DISCARD(init.text*)

#define INIT_TEXT_SECTION(inittext_align,Y,B)				\
	. = ALIGN(inittext_align);					\
	.init.text : AT(ADDR(.init.text) - LOAD_OFFSET) {		\
		_sinittext = .;						\
		INIT_TEXT(Y,B)						\
		_einittext = .;						\
	}


SECTIONS
{
	INIT_TEXT_SECTION(PAGE_SIZE,(.text.startup),(.init.text .init.text.*))
#ifdef CONFIG_X86_64
	:init
#endif
}

