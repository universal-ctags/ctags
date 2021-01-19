/*
*   Copyright (c) 1998-2003, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   Defines external interface to option processing.
*/
#ifndef CTAGS_MAIN_OPTIONS_H
#define CTAGS_MAIN_OPTIONS_H

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */
#include "gvars.h"

#include <stdarg.h>


/*
*   DATA DECLARATIONS
*/

/*
*   FUNCTION PROTOTYPES
*/
extern void verbose (const char *const format, ...) CTAGS_ATTR_PRINTF (1, 2);

#define BEGIN_VERBOSE(VFP) do { if (ctags_verbose) { \
                                FILE* VFP = stderr
#define END_VERBOSE()      } } while (0)

#define BEGIN_VERBOSE_IF(COND,VFP) do { if (ctags_verbose || (COND)) { \
                                FILE* VFP = stderr


extern bool inSandbox (void);

/* This is for emitting a tag for a common block of Fortran parser*/
extern bool canUseLineNumberAsLocator (void);

#endif  /* CTAGS_MAIN_OPTIONS_H */
