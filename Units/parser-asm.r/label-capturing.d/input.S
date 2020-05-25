/* Taken from linux/arch/x86/entry/entry_64.S
   idtentry is a macro. It should not be captured as a label. */

/* SPDX-License-Identifier: GPL-2.0 */
/*
 *  linux/arch/x86_64/entry.S
 *
 *  Copyright (C) 1991, 1992  Linus Torvalds
 *  Copyright (C) 2000, 2001, 2002  Andi Kleen SuSE Labs
 *  Copyright (C) 2000  Pavel Machek <pavel@suse.cz>
 *
 * entry.S contains the system-call and fault low-level handling routines.
 *
 * Some of this is documented in Documentation/x86/entry_64.rst
 *
 * A note on terminology:
 * - iret frame:	Architecture defined interrupt frame from SS to RIP
 *			at the top of the kernel process stack.
 *
 * Some macro usage:
 * - SYM_FUNC_START/END:Define functions in the symbol table.
 * - TRACE_IRQ_*:	Trace hardirq state for lock debugging.
 * - idtentry:		Define exception entry points.
 */
	
.macro idtentry sym do_sym has_error_code:req paranoid=0 shift_ist=-1 ist_offset=0 create_gap=0 read_cr2=0
SYM_CODE_START(\sym)
	UNWIND_HINT_IRET_REGS offset=\has_error_code*8

	/* Sanity check */
	.if \shift_ist != -1 && \paranoid != 1
	.error "using shift_ist requires paranoid=1"
	.endif

	.if \create_gap && \paranoid
	.error "using create_gap requires paranoid=0"
	.endif

	ASM_CLAC

	.if \has_error_code == 0
	pushq	$-1				/* ORIG_RAX: no syscall to restart */
	.endif

	.if \paranoid == 1
	testb	$3, CS-ORIG_RAX(%rsp)		/* If coming from userspace, switch stacks */
	jnz	.Lfrom_usermode_switch_stack_\@
	.endif

	.if \create_gap == 1
	/*
	 * If coming from kernel space, create a 6-word gap to allow the
	 * int3 handler to emulate a call instruction.
	 */
	testb	$3, CS-ORIG_RAX(%rsp)
	jnz	.Lfrom_usermode_no_gap_\@
	.rept	6
	pushq	5*8(%rsp)
	.endr
	UNWIND_HINT_IRET_REGS offset=8
.Lfrom_usermode_no_gap_\@:
	.endif

	idtentry_part \do_sym, \has_error_code, \read_cr2, \paranoid, \shift_ist, \ist_offset

	.if \paranoid == 1
	/*
	 * Entry from userspace.  Switch stacks and treat it
	 * as a normal entry.  This means that paranoid handlers
	 * run in real process context if user_mode(regs).
	 */
.Lfrom_usermode_switch_stack_\@:
	idtentry_part \do_sym, \has_error_code, \read_cr2, paranoid=0
	.endif

_ASM_NOKPROBE(\sym)
SYM_CODE_END(\sym)
.endm

idtentry divide_error			do_divide_error			has_error_code=0
idtentry overflow			do_overflow			has_error_code=0
idtentry bounds				do_bounds			has_error_code=0
idtentry invalid_op			do_invalid_op			has_error_code=0
idtentry device_not_available		do_device_not_available		has_error_code=0
idtentry double_fault			do_double_fault			has_error_code=1 paranoid=2 read_cr2=1
idtentry coprocessor_segment_overrun	do_coprocessor_segment_overrun	has_error_code=0
idtentry invalid_TSS			do_invalid_TSS			has_error_code=1
idtentry segment_not_present		do_segment_not_present		has_error_code=1
idtentry spurious_interrupt_bug		do_spurious_interrupt_bug	has_error_code=0
idtentry coprocessor_error		do_coprocessor_error		has_error_code=0
idtentry alignment_check		do_alignment_check		has_error_code=1
idtentry simd_coprocessor_error		do_simd_coprocessor_error	has_error_code=0

	

	
