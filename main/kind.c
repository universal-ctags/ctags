/*
 *
 *  Copyright (c) 2015, Red Hat, Inc.
 *  Copyright (c) 2015, Masatake YAMATO
 *
 *  Author: Masatake YAMATO <yamato@redhat.com>
 *
 *   This source code is released for free distribution under the terms of the
 *   GNU General Public License version 2 or (at your option) any later version.
 *
 */

#include "general.h"

#include <stdio.h>
#include "debug.h"
#include "kind.h"

extern void printRole (const roleDesc* const role)
{
	if (role)
		printf ("%s\t%s\t%s\n", role->name, role->description, role->enabled? "on": "off");
}

extern const char *renderRole (const roleDesc* const role, vString* b)
{
	vStringCatS (b, role->name);
	return vStringValue (b);
}

extern void printKind (const kindOption* const kind, boolean allKindFields, boolean indent)
{
	if (allKindFields)
	{
		printf ("%s%c\t%s\t%s\t%s\treferenceOnly:%s\tnRoles:%d\n", indent ? "\t"           : "",
			kind->letter,
			kind->name        != NULL ? kind->name        : "",
			kind->description != NULL ? kind->description : "",
			kind->enabled             ? "on"              : "off",
			kind->referenceOnly       ? "TRUE"            : "FALSE",
			kind->nRoles);
	}
	else if (!kind->referenceOnly)
	{
		printf ("%s%c  %s%s\n", indent ? "    " : "", kind->letter,
			kind->description != NULL ? kind->description :
			(kind->name != NULL ? kind->name : ""),
			kind->enabled ? "" : " [off]");
	}
}

const char *scopeSeparatorFor (const kindOption *kind, char parentLetter)
{
	scopeSeparator *table;
	Assert (kind);
	table = kind->separators;

	/* If no table is given, use the default generic separator ".".
	   The exception is if a root separator is looked up. In this case,
	   return NULL to notify there is no root separator to the caller. */

	if (table == NULL)
	{
		if (parentLetter == KIND_NULL)
			return NULL;
		else
			return ".";
	}

	while (table - kind->separators < kind->separatorCount)
	{
		/* KIND_WILDCARD cannot be used as a key for finding
		   a root separator.*/
		if ( (table->parentLetter == KIND_WILDCARD
		       && parentLetter != KIND_NULL)
		    || table->parentLetter == parentLetter)
			return table->separator;
		table++;
	}
	if (parentLetter == KIND_NULL)
		return NULL;
	else
		return ".";
}
