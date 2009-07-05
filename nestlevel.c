/*
*   $Id: nestlevel.c 3788 2009-05-12 15:55:13Z ntrel $
*
*   Copyright (c) 1999-2002, Darren Hiebert
*   Copyright 2009 Nick Treleaven <nick(dot)treleaven(at)btinternet(dot)com>
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   Defines external interface to scope nesting levels for tags.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include "main.h"
#include "nestlevel.h"
#include "debug.h"
#include "routines.h"

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
		vStringDelete(nls->levels[i].name);
	if (nls->levels) eFree(nls->levels);
	eFree(nls);
}

extern void nestingLevelsPush(NestingLevels *nls,
	const vString *name, int type)
{
	NestingLevel *nl = NULL;

	if (nls->n >= nls->allocated)
	{
		nls->allocated++;
		nls->levels = xRealloc(nls->levels,
			nls->allocated, NestingLevel);
		nls->levels[nls->n].name = vStringNew();
	}
	nl = &nls->levels[nls->n];
	nls->n++;

	vStringCopy(nl->name, name);
	nl->type = type;
}

extern void nestingLevelsPop(NestingLevels *nls)
{
	const NestingLevel *nl = nestingLevelsGetCurrent(nls);

	Assert (nl != NULL);
	vStringClear(nl->name);
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
