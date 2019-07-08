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
} NsisKind;

static kindDefinition NsisKinds [] = {
	{ true, 's', "section", "sections"},
	{ true, 'f', "function", "functions"},
	{ true, 'v', "variable", "variables"},
	{ true, 'd', "definition", "definitions"},
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

static void findNsisTags (void)
{
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
		if (strncasecmp ((const char*) cp, "function", (size_t) 8) == 0 &&
			isspace ((int) cp [8]))
		{
			cp += 8;
			cp = skipWhitespace (cp);
			while (isalnum ((int) *cp) || *cp == '_' || *cp == '-' || *cp == '.' || *cp == '!')
			{
				vStringPut (name, (int) *cp);
				++cp;
			}
			makeSimpleTag (name, K_FUNCTION);
			vStringClear (name);
		}
		/* variables */
		else if (strncasecmp ((const char*) cp, "var", (size_t) 3) == 0 &&
			isspace ((int) cp [3]))
		{
			cp += 3;
			cp = skipWhitespace (cp);
			cp = skipFlags (cp);

			while (isalnum ((int) *cp) || *cp == '_')
			{
				vStringPut (name, (int) *cp);
				++cp;
			}
			makeSimpleTag (name, K_VARIABLE);
			vStringClear (name);
		}
		/* sections */
		else if (strncasecmp ((const char*) cp, "section", (size_t) 7) == 0  &&
				 isspace ((int) cp [7]))
		{
			bool in_quotes = false;
			cp += 7;
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
			makeSimpleTag (name, K_SECTION);
			if (vStringLength (name) > 0)
			{
				/*
				 * Try to capture section_index_output.
				 */
				vStringClear (name);
				cp = skipWhitespace (cp);
				while (isalnum ((int) *cp) || *cp == '_')
				{
					vStringPut (name, (int) *cp);
					++cp;
				}
				makeSimpleTag (name, K_DEFINITION);
			}
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
	return def;
}
