/*
*   Copyright (c) 2009, Eric Forgeot
*
*   Based on work by Jon Strait
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   This module contains functions for generating tags for Abc files.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"	/* must always come first */

#include <ctype.h>
#include <string.h>

#include "parse.h"
#include "read.h"
#include "vstring.h"
#include "routines.h"
#include "entry.h"

/*
*   DATA DEFINITIONS
*/

static kindDefinition AbcKinds[] = {
	{ true, 'm', "member", "sections" },
	{ true, 's', "struct",  "header1"}
};

/*
*   FUNCTION DEFINITIONS
*/

/* checks if str is all the same character */
/*static bool issame(const char *str)
{
	char first = *str;

	while (*(++str))
	{
		if (*str && *str != first)
			return false;
	}
	return true;
}*/


static void makeAbcTag (const vString* const name, bool name_before)
{
	tagEntryInfo e;
	initTagEntry (&e, vStringValue(name), 0);

	if (name_before)
		e.lineNumber--;	/* we want the line before the underline chars */

	makeTagEntry(&e);
}

/*static void makeAbcTag2 (const vString* const name, bool name_before)
{
	tagEntryInfo e;
	initTagEntry (&e, vStringValue(name));

	if (name_before)
		e.lineNumber--;
	e.kindName = "struct";
	e.kind = 's';

	makeTagEntry(&e);
}*/

static void findAbcTags (void)
{
	vString *name = vStringNew();
	const unsigned char *line;

	while ((line = readLineFromInputFile()) != NULL)
	{
		/*int name_len = vStringLength(name);*/

		/* underlines must be the same length or more */
		/*if (name_len > 0 &&	(line[0] == '=' || line[0] == '-') && issame((const char*) line))
		{
			makeAbcTag(name, true);
		}*/
/*		if (line[1] == '%') {
			vStringClear(name);
			vStringCatS(name, (const char *) line);
			makeAbcTag(name, false);
		}*/
		if (line[0] == 'T') {
			/*vStringClear(name);*/
			vStringCatS(name, " / ");
			vStringCatS(name, (const char *) line);
			makeAbcTag(name, false);
		}
		else {
			vStringClear (name);
			if (! isspace(*line))
				vStringCatS(name, (const char*) line);
		}
	}
	vStringDelete (name);
}

extern parserDefinition* AbcParser (void)
{
	static const char *const patterns [] = { "*.abc", NULL };
	static const char *const extensions [] = { "abc", NULL };
	parserDefinition* const def = parserNew ("Abc");

	def->kindTable = AbcKinds;
	def->kindCount = ARRAY_SIZE (AbcKinds);
	def->patterns = patterns;
	def->extensions = extensions;
	def->parser = findAbcTags;
	return def;
}

