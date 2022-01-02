/*
*   Copyright (c) 1999-2002, Darren Hiebert
*   Copyright 2009-2011 Nick Treleaven <nick(dot)treleaven(at)btinternet(dot)com>
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   Defines external interface to scope nesting levels for tags.
*/
#ifndef CTAGS_MAIN_NESTLEVEL_H
#define CTAGS_MAIN_NESTLEVEL_H

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include "vstring.h"

/*
*   DATA DECLARATIONS
*/
typedef struct NestingLevel NestingLevel;
typedef struct NestingLevels NestingLevels;

struct NestingLevel
{
	int corkIndex;
	char userData [];
};

struct NestingLevels
{
	void *levels;
	int n;					/* number of levels in use */
	int allocated;
	size_t userDataSize;
	/* The second argument is given via nestinglevelsPopFull
	 * or nestinglevelFreeFull */
	void (* deleteUserData) (NestingLevel *, void *);
};

/*
*   FUNCTION PROTOTYPES
*/
extern NestingLevels *nestingLevelsNew(size_t userDataSize);
extern NestingLevels *nestingLevelsNewFull(size_t userDataSize,
										   void (* deleteUserData)(NestingLevel *, void *));
#define nestingLevelsFree(NLS) nestingLevelsFreeFull(NLS, NULL)
extern void nestingLevelsFreeFull(NestingLevels *nls, void *ctxData);
extern NestingLevel *nestingLevelsPush(NestingLevels *nls, int corkIndex);
extern NestingLevel * nestingLevelsTruncate(NestingLevels *nls, int depth, int corkIndex);
#define nestingLevelsPop(NLS) nestingLevelsPopFull(NLS, NULL)
extern void nestingLevelsPopFull(NestingLevels *nls, void *ctxData);
#define nestingLevelsGetCurrent(NLS) nestingLevelsGetNthParent((NLS), 0)
extern NestingLevel *nestingLevelsGetNthFromRoot(const NestingLevels *nls, int n);
extern NestingLevel *nestingLevelsGetNthParent(const NestingLevels *nls, int n);

extern void *nestingLevelGetUserData (const NestingLevel *nl);

#endif  /* CTAGS_MAIN_NESTLEVEL_H */
