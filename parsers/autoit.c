/*
*   Copyright (c) 2017, Alexey Olenich
*   Copyright (c) 2018, Colomban Wendling <colomban@geany.org>
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for generating tags for AutoIt functions.
*   Homepage             https://www.autoitscript.com/site/autoit/
*   Online Documentation https://www.autoitscript.com/autoit3/docs/
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include <string.h>

#include "parse.h"
#include "entry.h"
#include "nestlevel.h"
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

typedef enum {
	F_PROPERTIES,
} AutoItField;

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

static fieldDefinition AutoItFields [] = {
	{
		.name = "properties",
		.description = "properties (static, volatile, ...)",
		.enabled = false,
	},
};

/*
*   FUNCTION DEFINITIONS
*/

/* it's unclear what *is* an identifier character, so maybe we're too strict */
static bool isIdentChar (int c)
{
	return isalnum (c) || c == '_';
}

static bool match (const unsigned char *line, const char *word,
                   const unsigned char **positionAfter)
{
	size_t len = strlen (word);
	bool matched = (strncasecmp ((const char*) line, word, len) == 0 &&
	                ! isIdentChar (line[len]));

	if (matched && positionAfter)
		*positionAfter = line + len;

	return matched;
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

static void setEndLine (const NestingLevels *const nls)
{
	NestingLevel *nl = nestingLevelsGetCurrent (nls);
	tagEntryInfo *entry;

	if (nl && (entry = getEntryInCorkQueue (nl->corkIndex)) != NULL)
		entry->extensionFields.endLine = getInputLineNumber ();
}

static void skipSpaces (const unsigned char **p)
{
	while (isspace ((int) **p))
		++(*p);
}

/* parses after a "func" keyword */
static int parseFunc (const unsigned char *p, NestingLevels *nls)
{
	int k = CORK_NIL;
	vString *name = vStringNew ();

	skipSpaces (&p);
	while (isIdentChar ((int) *p))
	{
		vStringPut (name, (int) *p);
		++p;
	}
	skipSpaces (&p);
	if (*p == '(' && (vStringLength (name) > 0))
	{
		vString *signature = vStringNew ();

		do
			vStringPut (signature, (int) *p);
		while (*p != ')' && *p++);

		k = makeAutoItTag (nls, name, K_FUNCTION, signature);
		nestingLevelsPush (nls, k);
		vStringDelete (signature);
	}

	vStringDelete (name);

	return k;
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
			if (match (p, "cs", NULL) || match (p, "comments-start", NULL))
				commentDepth++;
			else if (commentDepth > 0)
			{
				if (match (p, "ce", NULL) || match (p, "comments-end", NULL))
					commentDepth--;
			}
			else if (match (p, "region", &p))
			{
				skipSpaces (&p);
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
			else if (nls->n > 0 && match (p, "endregion", NULL))
			{
				setEndLine (nls);
				nestingLevelsPop (nls);
			}
			else if (match (p, "include", &p))
			{
				skipSpaces (&p);
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
			bool isStatic = false;

			/* skip white space */
			skipSpaces (&p);
			if (*p == ';')
				/* ignore single-line comments */;
			else if (match (p, "volatile", &p))
			{
				skipSpaces (&p);
				if (match (p, "func", &p))
				{
					int k = parseFunc (p, nls);
					if (k != CORK_NIL)
						attachParserFieldToCorkEntry (k, AutoItFields[F_PROPERTIES].ftype, "volatile");
				}
			}
			else if (match (p, "func", &p))
				parseFunc (p, nls);
			else if (nls->n > 0 && match (p, "endfunc", NULL))
			{
				setEndLine (nls);
				nestingLevelsPop (nls);
			}
			else if ((isStatic = match (p, "static", &p)) ||
			         (isGlobal = match (p, "global", &p)) ||
			         match (p, "local", &p))
			{
				/*
				 * variable-identifier ::= "$[a-zA-Z_0-9]+"
				 * scope-modifier ::= "Local" | "Global"
				 * variable-declaration ::= "Static" [scope-modifier] variable-identifier
				 *                          scope-modifier ["Static" | "Const"] variable-identifier
				 */
				skipSpaces (&p);
				if (isStatic)
				{
					/* skip optional modifiers */
					if ((isGlobal = match (p, "global", &p)) ||
					    match (p, "local", &p))
					{
						skipSpaces (&p);
					}
				}
				else if ((isStatic = match (p, "static", &p)))
					skipSpaces (&p);
				else if (match (p, "const", &p))
					skipSpaces (&p);
				if (*p == '$')
				{
					vStringPut (name, (int) *p++);
					while (isIdentChar ((int) *p))
					{
						vStringPut (name, (int) *p);
						++p;
					}
					if (vStringLength(name) > 0)
					{
						int k = makeSimpleAutoItTag (nls, name, isGlobal ? K_GLOBALVAR : K_LOCALVAR);
						if (k != CORK_NIL && isStatic)
							attachParserFieldToCorkEntry (k, AutoItFields[F_PROPERTIES].ftype, "static");
					}
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
	def->fieldTable = AutoItFields;
	def->fieldCount = ARRAY_SIZE (AutoItFields);
	def->extensions = extensions;
	def->parser     = findAutoItTags;
	def->useCork    = CORK_QUEUE;
	return def;
}
