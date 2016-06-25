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

#define PR_KIND_WIDTH_LETTER         7
#define PR_KIND_WIDTH_NAME          15
#define PR_KIND_WIDTH_DESCRIPTION   30
#define PR_KIND_WIDTH_ENABLED        8
#define PR_KIND_WIDTH_REFONLY        7
#define PR_KIND_WIDTH_NROLE          6
#define MAKE_KIND_FMT(PREFIX,LETTER_SPEC,NROLL_SPEC)		\
	PREFIX							\
	PR_KIND_FMT (LETTER,LETTER_SPEC)			\
	" "							\
	PR_KIND_FMT (NAME,s)					\
	" "							\
	PR_KIND_FMT (ENABLED,s)					\
	" "							\
	PR_KIND_FMT (REFONLY,s)					\
	" "							\
	PR_KIND_FMT (NROLE,NROLL_SPEC)				\
	" "							\
	PR_KIND_FMT (DESCRIPTION,s)				\
	"\n"

extern void printKindListHeader (boolean indent, boolean tabSeparated)
{
#define KIND_HEADER_COMMON_FMT MAKE_KIND_FMT("%s", s, s)

	const char *fmt = tabSeparated
		? "%s%s%s\t%s\t%s\t%s\t%s\t%s\n"
		: (indent
		   ? PR_KIND_FMT (LANG,s) KIND_HEADER_COMMON_FMT
		   : "%s"                 KIND_HEADER_COMMON_FMT)
		;

	printf (fmt,
		(indent? "#PARSER": ""),
		(indent? (tabSeparated? "\t": " "): ""),
		(indent? "LETTER": "#LETTER"),
		"NAME",
		"ENABLED",
		"REFONLY",
		"NROLES",
		"DESCRIPTION");

#undef KIND_HEADER_COMMON_FMT
}

extern void printKind (const kindOption* const kind, boolean allKindFields, boolean indent,
		       boolean tabSeparated)
{
#define KIND_FMT MAKE_KIND_FMT("", c, d)

	if (allKindFields)
	{
		printf ((tabSeparated
			 ?"%s%c\t%s\t%s\t%s\t%d\t%s\n"
			 :"%s" KIND_FMT),
			(indent? (tabSeparated? "\t": " "): ""),
			kind->letter,
			kind->name        != NULL ? kind->name        : "",
			kind->enabled             ? "on"              : "off",
			kind->referenceOnly       ? "TRUE"            : "FALSE",
			kind->nRoles,
			kind->description != NULL ? kind->description : "");
	}
	else if (!kind->referenceOnly)
	{
		printf ("%s%c  %s%s\n", indent ? "    " : "", kind->letter,
			kind->description != NULL ? kind->description :
			(kind->name != NULL ? kind->name : ""),
			kind->enabled ? "" : " [off]");
	}

#undef KIND_FMT
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
