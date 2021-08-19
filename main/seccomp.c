/*
*   Copyright (c) 2017, Google, Inc.
*
*   Author: Han-Wen Nienhuys <hanwen@google.com>
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*/

#include "general.h"
#include "debug.h"
#include "interactive_p.h"
#include "routines.h"

#ifdef HAVE_SECCOMP
#include <seccomp.h>


int installSyscallFilter (void)
{
	// Use SCMP_ACT_TRAP to get a core dump.
	scmp_filter_ctx ctx = seccomp_init (SCMP_ACT_KILL);
	if (ctx == NULL)
	{
		return 1;
	}

	// Memory allocation.
	seccomp_rule_add (ctx, SCMP_ACT_ALLOW, SCMP_SYS (mmap), 0);
	seccomp_rule_add (ctx, SCMP_ACT_ALLOW, SCMP_SYS (munmap), 0);
	seccomp_rule_add (ctx, SCMP_ACT_ALLOW, SCMP_SYS (mremap), 0);
	seccomp_rule_add (ctx, SCMP_ACT_ALLOW, SCMP_SYS (brk), 0);

	// I/O
	seccomp_rule_add (ctx, SCMP_ACT_ALLOW, SCMP_SYS (read), 0);
	seccomp_rule_add (ctx, SCMP_ACT_ALLOW, SCMP_SYS (write), 0);

	// Clean exit
	seccomp_rule_add (ctx, SCMP_ACT_ALLOW, SCMP_SYS (exit), 0);
	seccomp_rule_add (ctx, SCMP_ACT_ALLOW, SCMP_SYS (exit_group), 0);

	// The bowels of stdio want to know the size of a file, even for stdout.
	seccomp_rule_add (ctx, SCMP_ACT_ALLOW, SCMP_SYS (fstat), 0);
	seccomp_rule_add (ctx, SCMP_ACT_ALLOW, SCMP_SYS (fstat64), 0);
#ifdef __SNR_newfstatat
	seccomp_rule_add (ctx, SCMP_ACT_ALLOW, SCMP_SYS (newfstatat), 0);
#endif
#ifdef __SNR_statx
	// armhf fallback
	seccomp_rule_add (ctx, SCMP_ACT_ALLOW, SCMP_SYS (statx), 0);
#endif

	// seems unnecessary, but this comes from
	// main/parse.c:2764 : tagFilePosition (&tagfpos);
	seccomp_rule_add (ctx, SCMP_ACT_ALLOW, SCMP_SYS (lseek), 0);
	seccomp_rule_add (ctx, SCMP_ACT_ALLOW, SCMP_SYS (_llseek), 0);

	// libxml2 uses pthread_once, which in turn uses a futex
	seccomp_rule_add (ctx, SCMP_ACT_ALLOW, SCMP_SYS (futex), 0);

	verbose ("Entering sandbox\n");
	int err = seccomp_load (ctx);
	if (err < 0)
	{
		error (WARNING, "Failed to install syscall filter");
		/* Error handling is done in upper layer. */
	}

	seccomp_release (ctx);

	return err;
}

/*
   TODO: on OSX, Seatbelt
   (https://dev.chromium.org/developers/design-documents/sandbox/osx-sandboxing-design)
   should be used for equivalent functionality.
 */

#else
int installSyscallFilter (void)
{
	AssertNotReached ();
	return -1;
}
#endif
