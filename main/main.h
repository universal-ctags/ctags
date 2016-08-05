/*
*   Copyright (c) 1998-2002, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   External interface to main.c
*/
#ifndef CTAGS_MAIN_MAIN_H
#define CTAGS_MAIN_MAIN_H

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include <stdio.h>

/*
*   FUNCTION PROTOTYPES
*/
extern void addTotals (const unsigned int files, const long unsigned int lines, const long unsigned int bytes);
extern boolean isDestinationStdout (void);
extern int main (int argc, char **argv);

#endif  /* CTAGS_MAIN_MAIN_H */
