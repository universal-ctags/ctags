/*
*   Copyright (c) 2002-2003, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains a lose assortment of shared functions.
*/

#ifndef CTAGS_MAIN_ERORR_H
#define CTAGS_MAIN_ERORR_H

#include "general.h"  /* must always come first */

#include <stdarg.h>
#include "routines.h"

typedef bool (* errorPrintFunc) (const errorSelection selection, const char *const format,
				    va_list ap, void *data);

extern void setErrorPrinter (errorPrintFunc printer, void *data);

extern bool stderrDefaultErrorPrinter (const errorSelection selection, const char *const format, va_list ap,
					  void *data);

#endif
