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

void interactiveLoop (cookedArgs *args, void *user CTAGS_ATTR_UNUSED);
bool jsonErrorPrinter (const errorSelection selection, const char *const format, va_list ap,
					  void *data);
int installSyscallFilter (void);

#endif  /* CTAGS_MAIN_INTERACTIVE_H */

/* vi:set tabstop=4 shiftwidth=4: */
