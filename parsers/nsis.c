/*
*   Copyright (c) 2000-2002, Darren Hiebert
*   Copyright (c) 2009-2011, Enrico Tröger
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   This module contains functions for generating tags for NSIS scripts
*   (https://en.wikipedia.org/wiki/Nullsoft_Scriptable_Install_System).
*
*   Based on sh.c.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"	/* must always come first */

#include <string.h>

#include "entry.h"
#include "parse.h"
#include "read.h"
#include "vstring.h"
#include "routines.h"

/*
*   DATA DEFINITIONS
*/
typedef enum {
	K_SECTION,
	K_FUNCTION,
	K_VARIABLE,
	K_DEFINITION,
	K_MACRO,
	K_SECTION_GROUP,
} NsisKind;

static kindDefinition NsisKinds [] = {
	{ true, 's', "section", "sections"},
	{ true, 'f', "function", "functions"},
	{ true, 'v', "variable", "variables"},
	{ true, 'd', "definition", "definitions"},
	{ true, 'm', "macro", "macros"},
	{ true, 'S', "sectionGroup", "section groups"},
};

/*
*   FUNCTION DEFINITIONS
*/

static const unsigned char* skipWhitespace (const unsigned char* cp)
{
	while (isspace ((int) *cp))
		++cp;
	return cp;
}

static const unsigned char* skipFlags (const unsigned char* cp)
{
	while (*cp == '/')
	{
		++cp;
		while (! isspace ((int) *cp))
			++cp;
		while (isspace ((int) *cp))
			++cp;
	}
	return cp;
}

static int makeSimpleTagWithScope(vString *name, int kindIndex, int parentCorkIndex)
{
	tagEntryInfo e;
	initTagEntry (&e, vStringValue (name), kindIndex);
	e.extensionFields.scopeIndex = parentCorkIndex;
	return makeTagEntry (&e);
}

#define lineStartingWith(CP,EXPECTED,EOL)								\
	(strncasecmp ((const char*) CP, EXPECTED, strlen(EXPECTED)) == 0	\
		&& (EOL ? (isspace ((int) CP [strlen(EXPECTED)]) || CP [strlen(EXPECTED)] == '\0') \
			: isspace ((int) CP [strlen(EXPECTED)])))

#define fillName(NAME,CP,CONDITION)				\
	while (CONDITION)							\
	{											\
		vStringPut ((NAME), (int) *(CP));		\
		++(CP);									\
	}											\
	do {} while (0)

static const unsigned char* parseSection (const unsigned char* cp, vString *name,
										  int kindIndex, int scopeIndex, int *corkIndex)
{
	bool in_quotes = false;
	cp = skipWhitespace (cp);
	cp = skipFlags (cp);
	while (isalnum ((int) *cp) || isspace ((int) *cp) ||
		   *cp == '_' || *cp == '-' || *cp == '.' || *cp == '!' || *cp == '"'
		   || (in_quotes && (*cp == '$' || *cp == '{' || *cp == '}' )))
	{
		if (*cp == '"')
		{
			if (in_quotes)
			{
				++cp;
				break;
			}
			else
			{
				in_quotes = true;
				++cp;
				continue;
			}
		}
		vStringPut (name, (int) *cp);
		++cp;
	}
	int r = makeSimpleTagWithScope (name, kindIndex, scopeIndex);
	if (corkIndex)
		*corkIndex = r;
	if (vStringLength (name) > 0)
	{
		/*
		 * Try to capture section_index_output.
		 */
		vStringClear (name);
		cp = skipWhitespace (cp);

		fillName (name, cp, (isalnum ((int) *cp) || *cp == '_'));

		if (vStringLength (name) > 0)
		{
			makeSimpleTag (name, K_DEFINITION);
			vStringClear (name);
		}
	}
	return cp;
}

static void findNsisTags (void)
{
	int sectionGroupIndex = CORK_NIL;
	vString *name = vStringNew ();
	const unsigned char *line;

	while ((line = readLineFromInputFile ()) != NULL)
	{
		const unsigned char* cp = line;

		while (isspace (*cp))
			cp++;

		if (*cp == '#' || *cp == ';')
			continue;

		/* functions */
		if (lineStartingWith (cp, "function", false))
		{
			cp += 8;
			cp = skipWhitespace (cp);

			fillName (name, cp,
					  (isalnum ((int) *cp) || *cp == '_' || *cp == '-' || *cp == '.' || *cp == '!'));

			makeSimpleTag (name, K_FUNCTION);
			vStringClear (name);
		}
		/* variables */
		else if (lineStartingWith (cp, "var", false))
		{
			cp += 3;
			cp = skipWhitespace (cp);
			cp = skipFlags (cp);

			fillName (name, cp, (isalnum ((int) *cp) || *cp == '_'));

			makeSimpleTag (name, K_VARIABLE);
			vStringClear (name);
		}
		/* section groups */
		else if (lineStartingWith (cp, "sectiongroup", false))
		{
			cp += 12;
			cp = parseSection (cp, name, K_SECTION_GROUP, CORK_NIL, &sectionGroupIndex);
		}
		else if (lineStartingWith (cp, "sectiongroupend", true))
		{
			cp += 15;
			sectionGroupIndex = CORK_NIL;
		}
		/* sections */
		else if (lineStartingWith (cp, "section", false))
		{
			cp += 7;
			cp = parseSection (cp, name, K_SECTION, sectionGroupIndex, NULL);
		}
		/* definitions */
		else if (lineStartingWith (cp, "!define", false))
		{
			cp += 7;
			cp = skipWhitespace (cp);
			cp = skipFlags (cp);

			fillName (name, cp, (isalnum ((int) *cp) || *cp == '_'));

			makeSimpleTag (name, K_DEFINITION);
			vStringClear (name);
		}
		/* macro */
		else if (lineStartingWith(cp, "!macro", false))
		{
			cp += 6;
			cp = skipWhitespace (cp);
			cp = skipFlags (cp);

			fillName (name, cp, (isalnum ((int) *cp) || *cp == '_'));

			makeSimpleTag (name, K_MACRO);
			/* TODO: tag parameters */
			vStringClear (name);
		}
	}
	vStringDelete (name);
}

extern parserDefinition* NsisParser (void)
{
	static const char *const extensions [] = {
		"nsi", "nsh", NULL
	};
	parserDefinition* def = parserNew ("NSIS");
	def->kindTable  = NsisKinds;
	def->kindCount  = ARRAY_SIZE (NsisKinds);
	def->extensions = extensions;
	def->parser     = findNsisTags;
	def->useCork    = true;
	return def;
}
