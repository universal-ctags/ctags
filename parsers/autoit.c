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
	K_REGION,
	K_GLOBALVAR,
	K_LOCALVAR,
	K_SCRIPT,
} AutoItKind;

typedef enum {
	R_INCLUDE_SYSTEM,
	R_INCLUDE_LOCAL,
} AutoItIncludeRole;

static roleDefinition AutoItIncludeRoles [] = {
	{ true, "system", "system include" },
	{ true, "local", "local include" },
};

static kindDefinition AutoItKinds [] = {
	{ true, 'f', "func", "functions" },
	{ true, 'r', "region", "regions" },
	{ true, 'g', "global", "global variables" },
	{ true, 'l', "local", "local variables" },
	{ true, 'S', "script", "included scripts",
	  .referenceOnly = true, ATTACH_ROLES (AutoItIncludeRoles) },
};

/*
*   FUNCTION DEFINITIONS
*/

/* it's unclear what *is* an identifier character, so maybe we're too strict */
static bool isIdentChar (int c)
{
	return isalnum (c) || c == '_';
}

static bool match (const unsigned char *line, const char *word)
{
	size_t len = strlen (word);

	return (strncasecmp ((const char*) line, word, len) == 0 &&
	        ! isIdentChar (line[len]));
}

static int makeAutoItTag (const NestingLevels *const nls,
                          const vString* const name,
                          const int kindIndex,
                          const vString *const signature)
{
	int r = CORK_NIL;

	if (isInputLanguageKindEnabled(kindIndex) && name != NULL && vStringLength (name) > 0)
	{
		NestingLevel *nl = nestingLevelsGetCurrent (nls);
		tagEntryInfo e;

		initTagEntry (&e, vStringValue (name), kindIndex);

		if (nl)
			e.extensionFields.scopeIndex = nl->corkIndex;
		if (signature)
			e.extensionFields.signature = vStringValue (signature);

		r = makeTagEntry (&e);
	}

	return r;
}

static int makeSimpleAutoItTag (const NestingLevels *const nls,
                                const vString* const name,
                                const int kindIndex)
{
	return makeAutoItTag (nls, name, kindIndex, NULL);
}

static void findAutoItTags (void)
{
	vString *name = vStringNew ();
	const unsigned char *line;
	NestingLevels *nls = nestingLevelsNew (0);
	unsigned int commentDepth = 0;

	while ((line = readLineFromInputFile ()) != NULL)
	{
		const unsigned char* p = line;
		if (p [0] == '#')
		{
			p++;
			if (match (p, "cs") || match (p, "comments-start"))
				commentDepth++;
			else if (commentDepth > 0)
			{
				if (match (p, "ce") || match (p, "comments-end"))
					commentDepth--;
			}
			else if (match (p, "region"))
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
			else if (match (p, "include"))
			{
				p += 7; /* strlen("include") = 7 */
				while (isspace ((int) *p))
					++p;
				if (*p == '<' || *p == '"')
				{
					const AutoItIncludeRole role = (*p == '<')
						? R_INCLUDE_SYSTEM
						: R_INCLUDE_LOCAL;

					++p;
					while (*p != '\0' && *p != '>' && *p != '"')
					{
						vStringPut (name, (int) *p);
						++p;
					}
					if (vStringLength(name) > 0)
					{
						makeSimpleRefTag (name, K_SCRIPT, role);
						vStringClear (name);
					}
				}
			}
		}
		else if (commentDepth == 0)
		{
			bool isGlobal = false;

			/* skip white space */
			while (isspace ((int) *p))
				++p;
			if (*p == ';')
				/* ignore single-line comments */;
			else if (match (p, "func"))
			{
				p += 4;
				while (isspace ((int) *p))
					++p;
				while (isIdentChar ((int) *p))
				{
					vStringPut (name, (int) *p);
					++p;
				}
				while (isspace ((int) *p))
					++p;
				if (*p == '(' && (vStringLength(name) > 0))
				{
					vString *signature = vStringNew ();
					int k;

					do
						vStringPut (signature, (int) *p);
					while (*p != ')' && *p++);

					k = makeAutoItTag (nls, name, K_FUNCTION, signature);
					nestingLevelsPush (nls, k);
					vStringClear (name);
					vStringDelete (signature);
				}
			}
			else if (match (p, "endfunc"))
				nestingLevelsPop (nls);
			else if ((isGlobal = match (p, "global")) || match (p, "local"))
			{
				p += isGlobal ? 6 : 5;
				while (isspace ((int) *p))
					++p;
				if (match (p, "const"))
				{
					p += 5;
					while (isspace ((int) *p))
						++p;
				}
				if (*p == '$')
				{
					vStringPut (name, (int) *p++);
					while (isIdentChar ((int) *p))
					{
						vStringPut (name, (int) *p);
						++p;
					}
					if (vStringLength(name) > 0)
						makeSimpleAutoItTag (nls, name, isGlobal ? K_GLOBALVAR : K_LOCALVAR);
					vStringClear (name);
				}
			}
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
