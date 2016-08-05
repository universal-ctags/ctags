/*
*   Copyright (c) 1998-2002, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   External interface to keyword.c
*/
#ifndef CTAGS_MAIN_KEYWORD_H
#define CTAGS_MAIN_KEYWORD_H

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include "types.h"
#include "vstring.h"

#define KEYWORD_NONE -1

/*
*   FUNCTION PROTOTYPES
*/
extern void addKeyword (const char *const string, langType language, int value);
extern int lookupKeyword (const char *const string, langType language);
extern void freeKeywordTable (void);
#ifdef DEBUG
extern void printKeywordTable (void);
#endif
extern int analyzeToken (vString *const name, langType language);

#endif  /* CTAGS_MAIN_KEYWORD_H */
