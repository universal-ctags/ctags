/* Derived from linux/arch/powerpc/kernel/trace/ftrace_64_pg_entry.S */
/* SPDX-License-Identifier: GPL-2.0-or-later */
/*
 * Split from ftrace_64.S
 */

.pushsection ".tramp.ftrace.text","aw",@progbits;
.globl ftrace_tramp_text
ftrace_tramp_text:
	.space 32
.popsection

.pushsection ".tramp.ftrace.init","aw",@progbits;
.globl ftrace_tramp_init
ftrace_tramp_init:
	.space 32
.popsection

