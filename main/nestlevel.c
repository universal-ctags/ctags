/*
*   Copyright (c) 1999-2002, Darren Hiebert
*   Copyright 2009-2011 Nick Treleaven <nick(dot)treleaven(at)btinternet(dot)com>
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   Defines external interface to scope nesting levels for tags.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include "debug.h"
#include "entry.h"
#include "routines.h"
#include "nestlevel.h"

#include <string.h>

/* TODO: Alignment */
#define NL_SIZE(nls) (sizeof(NestingLevel) + (nls)->userDataSize)
#define NL_NTH(nls,n) (NestingLevel *)(((char *)((nls)->levels)) + ((n) * NL_SIZE (nls)))

/*
*   FUNCTION DEFINITIONS
*/

extern NestingLevels *nestingLevelsNewFull(size_t userDataSize,
										   void (* deleteUserData)(NestingLevel *, void *))
{
	NestingLevels *nls = xCalloc (1, NestingLevels);
	nls->userDataSize = userDataSize;
	nls->deleteUserData = deleteUserData;
	return nls;
}

extern NestingLevels *nestingLevelsNew(size_t userDataSize)
{
	return nestingLevelsNewFull (userDataSize, NULL);
}

extern void nestingLevelsFreeFull(NestingLevels *nls, void *ctxData)
{
	int i;
	NestingLevel *nl;

	for (i = 0; i < nls->n; i++)
	{
		nl = NL_NTH(nls, i);
		if (nls->deleteUserData)
			nls->deleteUserData (nl, ctxData);
		nl->corkIndex = CORK_NIL;
	}
	if (nls->levels) eFree(nls->levels);
	eFree(nls);
}

extern NestingLevel * nestingLevelsPush(NestingLevels *nls, int corkIndex)
{
	NestingLevel *nl = NULL;

	if (nls->n >= nls->allocated)
	{
		nls->allocated++;
		nls->levels = eRealloc(nls->levels,
				       nls->allocated * NL_SIZE (nls));
	}
	nl = NL_NTH(nls, nls->n);
	nls->n++;

	nl->corkIndex = corkIndex;
	if (nls->userDataSize > 0)
		memset (nl->userData, 0, nls->userDataSize);

	return nl;
}

extern NestingLevel *nestingLevelsTruncate(NestingLevels *nls, int depth, int corkIndex)
{
	NestingLevel *nl;

	nls->n = depth;
	nl = nestingLevelsGetCurrent(nls);
	nl->corkIndex = corkIndex;
	return nl;
}


extern void nestingLevelsPopFull(NestingLevels *nls, void *ctxData)
{
	NestingLevel *nl = nestingLevelsGetCurrent(nls);

	Assert (nl != NULL);
	if (nls->deleteUserData)
		nls->deleteUserData (nl, ctxData);
	nl->corkIndex = CORK_NIL;
	nls->n--;
}

extern NestingLevel *nestingLevelsGetNthFromRoot (const NestingLevels *nls, int n)
{
	Assert (nls != NULL);
	if (nls->n > n && n >= 0)
		return  NL_NTH(nls, n);
	else
		return NULL;
}

extern NestingLevel *nestingLevelsGetNthParent (const NestingLevels *nls, int n)
{
	Assert (nls != NULL);
	return nestingLevelsGetNthFromRoot (nls, nls->n - 1 - n);
}

extern void *nestingLevelGetUserData (const NestingLevel *nl)
{
	return (void *)nl->userData;
}
