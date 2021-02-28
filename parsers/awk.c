/*
 *   Copyright (c) 2000-2002, Darren Hiebert
 *
 *   This source code is released for free distribution under the terms of the
 *   GNU General Public License version 2 or (at your option) any later version.
 *
 *   This module contains functions for generating tags for AWK functions.
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
typedef enum eAwkKinds {
	K_FUNCTION
} awkKind;

static kindDefinition AwkKinds [] = {
	{ true, 'f', "function", "functions" }
};

/*
 *   FUNCTION DEFINITIONS
 */

static void findAwkTags (void)
{
	vString *name = vStringNew ();
	const unsigned char *line;

	while ((line = readLineFromInputFile ()) != NULL)
	{
		while (isspace ((int) *line))
			++line;

		if (strncmp ((const char *) line, "function", (size_t) 8) == 0  &&
			isspace ((int) line [8]))
		{
			const unsigned char *cp = line + 8;

			while (isspace ((int) *cp))
				++cp;
			while (isalnum ((int) *cp)  ||  *cp == '_')
			{
				vStringPut (name, (int) *cp);
				++cp;
			}
			while (isspace ((int) *cp))
				++cp;
			if (*cp == '(')
				makeSimpleTag (name, K_FUNCTION);
			vStringClear (name);
			if (*cp != '\0')
				++cp;
		}
	}
	vStringDelete (name);
}

extern parserDefinition *AwkParser (void)
{
	static const char *const extensions [] = { "awk", "gawk", "mawk", NULL };
	static const char *const aliases [] = { "gawk", "mawk", NULL };
	parserDefinition *def = parserNew ("Awk");
	def->kindTable  = AwkKinds;
	def->kindCount  = ARRAY_SIZE (AwkKinds);
	def->extensions = extensions;
	def->aliases    = aliases;
	def->parser     = findAwkTags;
	return def;
}
