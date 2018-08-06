/*
*   Copyright (c) 2017, Alexey Olenich
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for generating tags for AutoIt functions.
*   Homepage             https://www.autoitscript.com/site/autoit/
*   Online Documentation https://www.autoitscript.com/autoit3/docs/
*
*   Functions: (?i)^func[ \t]{1,}([a-z0-9_]{1,}) >> \1
*   Regions:   (?i)^#region[ \t]*(.*?)           >> \1
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include <string.h>

#include "parse.h"
#include "read.h"
#include "routines.h"
#include "vstring.h"

/*
*   DATA DEFINITIONS
*/
typedef enum {
	K_FUNCTION,
	K_REGION
} AutoItKind;

static kindDefinition AutoItKinds [] = {
	{ true, 'f', "func", "functions" },
	{ true, 'r', "region", "regions" }
};

/*
*   FUNCTION DEFINITIONS
*/
static bool match (const unsigned char *line, const char *word)
{
	size_t len = strlen (word);

	return (strncasecmp ((const char*) line, word, len) == 0 &&
	        /* check that the word is followed by a space or similar */
	        (isspace (line[len]) || line[len] == ';' || line[len] == 0));
}

static int makeSimpleAutoItTag (const NestingLevels *const nls,
                                const vString* const name,
                                const int kindIndex)
{
	int r = CORK_NIL;

	if (isInputLanguageKindEnabled(kindIndex) && name != NULL && vStringLength (name) > 0)
	{
		NestingLevel *nl = nestingLevelsGetCurrent (nls);
		tagEntryInfo e;

		initTagEntry (&e, vStringValue (name), kindIndex);

		if (nl)
			e.extensionFields.scopeIndex = nl->corkIndex;

		r = makeTagEntry (&e);
	}

	return r;
}

static void findAutoItTags (void)
{
	vString *name = vStringNew ();
	const unsigned char *line;
	NestingLevels *nls = nestingLevelsNew (0);

	while ((line = readLineFromInputFile ()) != NULL)
	{
		const unsigned char* p = line;
		if (p [0] == '#')
		{
			p++;
			if (match (p, "region"))
			{
				p += 6; /* strlen("region") = 6 */
				while (isspace ((int) *p))
					++p;
				while (*p != '\0')
				{
					vStringPut (name, (int) *p);
					++p;
				}

				if (vStringLength(name) > 0)
				{
					int k = makeSimpleAutoItTag (nls, name, K_REGION);
					nestingLevelsPush (nls, k);
					vStringClear (name);
				}
			}
			else if (match (p, "endregion"))
				nestingLevelsPop (nls);
		}
		else
		{
			/* skip white space */
			while (isspace ((int) *p))
				++p;
			if (match (p, "func"))
			{
				p += 4;
				while (isspace ((int) *p))
					++p;
				while (isalnum ((int) *p) || *p == '_')
				{
					vStringPut (name, (int) *p);
					++p;
				}
				while (isspace ((int) *p))
					++p;
				if (*p == '(' && (vStringLength(name) > 0))
				{
					int k = makeSimpleAutoItTag (nls, name, K_FUNCTION);
					nestingLevelsPush (nls, k);
					vStringClear (name);
				}
			}
			else if (match (p, "endfunc"))
				nestingLevelsPop (nls);
		}
	}
	vStringDelete (name);
	nestingLevelsFree (nls);
}

parserDefinition *AutoItParser (void)
{
	static char const *extensions[] = { "au3", "AU3", "aU3", "Au3", NULL };
	parserDefinition* def = parserNew ("AutoIt");
	def->kindTable      = AutoItKinds;
	def->kindCount  = ARRAY_SIZE (AutoItKinds);
	def->extensions = extensions;
	def->parser     = findAutoItTags;
	def->useCork    = true;
	return def;
}
