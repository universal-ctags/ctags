/*
*   Copyright (c) 2002, Venkatesh Prasad Ranganath and Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for generating tags for SML language files.
*/

/*
 *   INCLUDE FILES
 */
#include "general.h"  /* must always come first */

#include <string.h>

#include "debug.h"
#include "entry.h"
#include "parse.h"
#include "read.h"
#include "routines.h"
#include "vstring.h"

/*
 *   DATA DECLARATIONS
 */
typedef enum {
	K_AND = -2,
	K_NONE = -1,
	K_EXCEPTION,
	K_FUNCTION,
	K_FUNCTOR,
	K_SIGNATURE,
	K_STRUCTURE,
	K_TYPE,
	K_VAL,
	K_COUNT			/* must be last */
} smlKind;

/*
 *   DATA DEFINITIONS
 */
static kindDefinition SmlKinds[] = {
	{ true, 'e', "exception", "exception declarations" },
	{ true, 'f', "function",  "function definitions" },
	{ true, 'c', "functor",   "functor definitions" },
	{ true, 's', "signature", "signature declarations" },
	{ true, 'r', "structure", "structure declarations" },
	{ true, 't', "type",      "type definitions" },
	{ true, 'v', "value",     "value bindings" }
};

static struct {
	const char *keyword;
	smlKind kind;
} SmlKeywordTypes [] = {
	{ "abstype",   K_TYPE      },
	{ "and",       K_AND       },
	{ "datatype",  K_TYPE      },
	{ "exception", K_EXCEPTION },
	{ "functor",   K_FUNCTOR   },
	{ "fun",       K_FUNCTION  },
	{ "signature", K_SIGNATURE },
	{ "structure", K_STRUCTURE },
	{ "type",      K_TYPE      },
	{ "val",       K_VAL       }
};

static unsigned int CommentLevel = 0;

/*
 * FUNCTION DEFINITIONS
 */

static void makeSmlTag (smlKind type, vString *name)
{
	tagEntryInfo tag;

	Assert (0 <= type && type < K_COUNT);
	if (! SmlKinds [type].enabled)
		return;

	initTagEntry (&tag, vStringValue (name), type);
	makeTagEntry (&tag);
}

static const unsigned char *skipSpace (const unsigned char *cp)
{
	while (isspace ((int) *cp))
		++cp;
	return cp;
}

static bool isIdentifier (int c)
{
	bool result = false;
	/* Consider '_' as an delimiter to aid user in tracking it's usage. */
	const char *const alternateIdentifiers = "!%&$#+-<>=/?@\\~'^|*_";
	if (isalnum (c))
		result = true;
	else if (c != '\0'  &&  strchr (alternateIdentifiers, c) != NULL)
		result = true;
	return result;
}

static const unsigned char *parseIdentifier (
		const unsigned char *cp, vString *const identifier)
{
	bool stringLit = false;
	vStringClear (identifier);
	while (*cp != '\0'  &&  (!isIdentifier ((int) *cp) || stringLit))
	{
		int oneback = *cp;
		cp++;
		if (oneback == '('  &&  *cp == '*'  &&  stringLit == false)
		{
			CommentLevel++;
			return ++cp;
		}
		if (*cp == '"' && oneback != '\\')
		{
			stringLit = true;
			continue;
		}
		if (stringLit && *cp == '"' && oneback != '\\')
			stringLit = false;
	}
	if (strcmp ((const char *) cp, "") == 0 || cp == NULL)
		return cp;

	while (isIdentifier ((int) *cp))
	{
		vStringPut (identifier, (int) *cp);
		cp++;
	}
	return cp;
}

static smlKind findNextIdentifier (const unsigned char **cp)
{
	smlKind result = K_NONE;
	vString *const identifier = vStringNew ();
	unsigned int count = ARRAY_SIZE (SmlKeywordTypes);
	unsigned int i;
	*cp = parseIdentifier (*cp, identifier);
	for (i = 0  ;  i < count  &&  result == K_NONE ;  ++i)
	{
		const char *id = vStringValue (identifier);
		if (strcmp (id, SmlKeywordTypes [i].keyword) == 0)
			result = SmlKeywordTypes [i].kind;
	}
	vStringDelete (identifier);
	return result;
}

static void findSmlTags (void)
{
	vString *const identifier = vStringNew ();
	const unsigned char *line;
	smlKind lastTag = K_NONE;

	while ((line = readLineFromInputFile ()) != NULL)
	{
		const unsigned char *cp = skipSpace (line);
		do
		{
			smlKind foundTag;
			if (CommentLevel != 0)
			{
				cp = (const unsigned char *) strstr ((const char *) cp, "*)");
				if (cp == NULL)
					continue;
				else
				{
					--CommentLevel;
					cp += 2;
				}
			}
			foundTag = findNextIdentifier (&cp);
			if (foundTag != K_NONE)
			{
				cp = skipSpace (cp);
				cp = parseIdentifier (cp, identifier);
				if (foundTag == K_AND)
				{
					if (lastTag != K_NONE)
						makeSmlTag (lastTag, identifier);
				}
				else
				{
					makeSmlTag (foundTag, identifier);
					lastTag = foundTag;
				}
			}
			if (strstr ((const char *) cp, "(*") != NULL)
			{
				cp += 2;
				cp = (const unsigned char *) strstr ((const char *) cp, "*)");
				if (cp == NULL)
					++CommentLevel;
			}
		} while (cp != NULL  &&  strcmp ((const char *) cp, "") != 0);
	}
	vStringDelete (identifier);
}

extern parserDefinition *SmlParser (void)
{
	static const char *const extensions[] = { "sml", "sig", NULL };
	parserDefinition *def = parserNew ("SML");
	def->kindTable = SmlKinds;
	def->kindCount = ARRAY_SIZE (SmlKinds);
	def->extensions = extensions;
	def->parser = findSmlTags;
	return def;
}
