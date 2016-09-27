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
extern void catFile (MIO *mio);

#ifdef EXTERNAL_SORT
extern void externalSortTags (const bool toStdout, MIO *tagFile);
#else
extern void internalSortTags (const bool toStdout,
			      MIO *mio,
			      size_t numTags);
#endif

/* mio is closed in this function. */
extern void failedSort (MIO *const mio, const char* msg);

#endif  /* CTAGS_MAIN_SORT_H */
