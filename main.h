/*
*   $Id$
*
*   Copyright (c) 1998-2002, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   External interface to main.c
*/
#ifndef _MAIN_H
#define _MAIN_H

/*
*   INCLUDE FILES
*/
#include "general.h"	/* must always come first */

#include <stdio.h>

#include "vstring.h"

/*
*   FUNCTION PROTOTYPES
*/
extern void addTotals (const unsigned int files, const long unsigned int lines, const long unsigned int bytes);
extern boolean isDestinationStdout (void);
extern void processExcludeOption (const char *const option, const char *const parameter);
extern int main (int argc, char **argv);

#endif	/* _MAIN_H */

/* vi:set tabstop=8 shiftwidth=4: */
