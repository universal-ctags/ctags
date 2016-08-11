/*
*   Copyright (c) 1998-2002, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   External interface to sort.c
*/
#ifndef CTAGS_MAIN_SORT_H
#define CTAGS_MAIN_SORT_H

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include <stdio.h>

#include "mio.h"

/*
*   FUNCTION PROTOTYPES
*/
extern void catFile (MIO *fp);

#ifdef EXTERNAL_SORT
extern void externalSortTags (const boolean toStdout, MIO *tagFile);
#else
extern void internalSortTags (const boolean toStdout,
			      MIO *fp,
			      size_t numTags);
#endif

/* fp is closed in this function. */
extern void failedSort (MIO *const fp, const char* msg);

#endif  /* CTAGS_MAIN_SORT_H */
