/*
*   $Id$
*
*   Copyright (c) 2000-2002, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   This module contains functions for generating tags for scripts for the
*   Bourne shell (and its derivatives, the Korn and Z shells).
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
	K_ALIAS,
	K_FUNCTION
} shKind;

static kindOption ShKinds [] = {
	{ TRUE, 'a', "alias", "aliases"},
	{ TRUE, 'f', "function", "functions"}
};

/*
*   FUNCTION DEFINITIONS
*/

/*  Reject any tag "main" from a file named "configure". These appear in
 *  here-documents in GNU autoconf scripts and will add a haystack to the
 *  needle.
 */
static boolean hackReject (const vString* const tagName)
{
	const char *const scriptName = baseFilename (vStringValue (File.name));
	boolean result = (boolean) (
			strcmp (scriptName, "configure") == 0  &&
			strcmp (vStringValue (tagName), "main") == 0);
	return result;
}

static void findShTags (void)
{
	vString *name = vStringNew ();
	const unsigned char *line;

	while ((line = fileReadLine ()) != NULL)
	{
		const unsigned char* cp = line;
		boolean aliasFound = FALSE;
		boolean functionFound = FALSE;

		while (*cp != '\0')
		{
			/* jump over whitespace */
			while (isspace ((int)*cp))
				cp++;

			/* jump over strings */
			if (*cp == '"')
			{
				const unsigned char* prev = cp;
				cp++;
				while ((*cp != '"' || *prev == '\\') && *cp != '\0')
				{
					prev = cp;
					cp++;
				}
			}
			else if (*cp == '\'')
			{
				cp++;
				while (*cp != '\'' && *cp != '\0')
					cp++;
			}

			/* jump over comments */
			else if (*cp == '#')
				break;

			if (strncmp ((const char*) cp, "function", (size_t) 8) == 0  &&
				isspace ((int) cp [8]))
			{
				functionFound = TRUE;
				cp += 8;
				while (isspace ((int) *cp))
					++cp;
			}

			else if (strncmp ((const char*) cp, "alias", (size_t) 5) == 0  &&
				isspace ((int) cp [5]))
			{
				aliasFound = TRUE;
				cp += 5;
				while (isspace ((int) *cp))
					++cp;
			}

			// Get the name of the function or alias.
			if (! (isalnum ((int) *cp) || *cp == '_'))
			{
				aliasFound = FALSE;
				functionFound = FALSE;
				++cp;
				continue;
			}
			while (isalnum ((int) *cp) || *cp == '_')
			{
				vStringPut (name, (int) *cp);
				++cp;
			}
			vStringTerminate (name);

			while (isspace ((int) *cp))
				++cp;
			if (*cp++ == '(')
			{
				while (isspace ((int) *cp))
					++cp;
				if (*cp == ')'  && ! hackReject (name))
				{
					functionFound = TRUE;
					++cp;
				}
			}
			if (aliasFound)
			{
				makeSimpleTag (name, ShKinds, K_ALIAS);
				aliasFound = FALSE;
			}
			if (functionFound)
			{
				makeSimpleTag (name, ShKinds, K_FUNCTION);
				functionFound = FALSE;
			}
			vStringClear (name);
		}
	}
	vStringDelete (name);
}

extern parserDefinition* ShParser (void)
{
	static const char *const extensions [] = {
		"sh", "SH", "bsh", "bash", "ksh", "zsh", "ash", NULL
	};
	static const char *const aliases [] = {
		"sh", "bash", "ksh", "zsh", "ash", NULL
	};
	parserDefinition* def = parserNew ("Sh");
	def->kinds      = ShKinds;
	def->kindCount  = KIND_COUNT (ShKinds);
	def->extensions = extensions;
	def->aliases = aliases;
	def->parser     = findShTags;
	return def;
}

/* vi:set tabstop=4 shiftwidth=4: */
