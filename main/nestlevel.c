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

#include "main.h"
#include "debug.h"
#include "routines.h"
#include "nestlevel.h"

/*
*   FUNCTION DEFINITIONS
*/

extern NestingLevels *nestingLevelsNew(void)
{
	NestingLevels *nls = xCalloc (1, NestingLevels);
	return nls;
}

extern void nestingLevelsFree(NestingLevels *nls)
{
	int i;
	for (i = 0; i < nls->allocated; i++)
		nls->levels[i].corkIndex = CORK_NIL;
	if (nls->levels) eFree(nls->levels);
	eFree(nls);
}

extern NestingLevel * nestingLevelsPush(NestingLevels *nls, int corkIndex)
{
	NestingLevel *nl = NULL;

	if (nls->n >= nls->allocated)
	{
		nls->allocated++;
		nls->levels = xRealloc(nls->levels,
			nls->allocated, NestingLevel);
	}
	nl = &nls->levels[nls->n];
	nls->n++;

	nl->corkIndex = corkIndex;
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


extern void nestingLevelsPop(NestingLevels *nls)
{
	NestingLevel *nl = nestingLevelsGetCurrent(nls);

	Assert (nl != NULL);
	nl->corkIndex = CORK_NIL;
	nls->n--;
}

extern NestingLevel *nestingLevelsGetCurrent(NestingLevels *nls)
{
	Assert (nls != NULL);

	if (nls->n < 1)
		return NULL;

	return &nls->levels[nls->n - 1];
}

/* vi:set tabstop=4 shiftwidth=4: */
