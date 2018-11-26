/*
*   Copyright (c) 1998-2002, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   main part private interface to keyword.c
*/
#ifndef CTAGS_MAIN_KEYWORD_PRIVATE_H
#define CTAGS_MAIN_KEYWORD_PRIVATE_H

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */
#include <stdio.h>

extern void freeKeywordTable (void);

extern void dumpKeywordTable (FILE *fp);

#ifdef DEBUG
extern void printKeywordTable (void);
#endif

#endif	/* CTAGS_MAIN_KEYWORD_PRIVATE_H */
