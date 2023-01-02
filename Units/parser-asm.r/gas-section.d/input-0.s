/* Taken from linux/arch/x86/boot/bioscall.S */

/*
 * "Glove box" for BIOS calls.  Avoids the constant problems with BIOSes
 * touching registers they shouldn't be.
 */

	.code16
	.section ".inittext","ax"
	.globl	intcall
	.type	intcall, @function
intcall:
	/* Self-modify the INT instruction.  Ugly, but works. */
	cmpb	%al, 3f
