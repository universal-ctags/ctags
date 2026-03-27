/*
*   Copyright (c) 2016, Aman Gupta
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   Defines interface to interactive loop.
*/
#ifndef CTAGS_MAIN_INTERACTIVE_H
#define CTAGS_MAIN_INTERACTIVE_H

#include "general.h"
#include "options_p.h"
#include "routines.h"


struct interactiveModeArgs
{
	bool sandbox;
	const char *fname;			/* --_interactive=oneshot:FNAME */
	size_t limit;				/* Upper limit of input data
								 * received via stdin.
								 * Used in oneshot mode.
								 * 0 means no limit. */
};

#ifdef HAVE_JANSSON
void interactiveLoop (cookedArgs *args, void *user);
bool jsonErrorPrinter (const errorSelection selection, const char *const format, va_list ap,
					  void *data);
#endif

void interactiveOneshot (cookedArgs *args, void *user);

enum syscallSet {
	syscall_coreset    = 1 << 0,
};

int installSyscallFilter (unsigned int set);

#endif  /* CTAGS_MAIN_INTERACTIVE_H */

/* vi:set tabstop=4 shiftwidth=4: */
