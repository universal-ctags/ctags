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

/* struct alignment trick, copied from GObject's gtype.c, which borrows
 * 2*szieof(size_t) from glibc */
#define STRUCT_ALIGNMENT (2 * sizeof (size_t))
#define ALIGN_STRUCT(offset) ((offset + (STRUCT_ALIGNMENT - 1)) & -STRUCT_ALIGNMENT)

/* account for the user data alignment if we have user data, otherwise allocate
 * exactly what's needed not to waste memory for unneeded alignment */
#define NL_SIZE(nls) ((nls)->userDataSize ? (ALIGN_STRUCT (sizeof (NestingLevel)) + ALIGN_STRUCT ((nls)->userDataSize)) : sizeof (NestingLevel))
#define NL_USER_DATA(nl) ((void *)(((char *) nl) + ALIGN_STRUCT (sizeof (NestingLevel))))

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
		memset (NL_USER_DATA (nl), 0, ALIGN_STRUCT (nls->userDataSize));

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
	return NL_USER_DATA (nl);
}
