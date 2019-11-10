/*
*   Copyright (c) 2000-2002, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for generating tags for Scheme language
*   files.
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
	K_FUNCTION, K_SET
} schemeKind;

static kindDefinition SchemeKinds [] = {
	{ true, 'f', "function", "functions" },
	{ true, 's', "set",      "sets" }
};

/*
*   FUNCTION DEFINITIONS
*/

/* Algorithm adapted from from GNU etags.
 * Scheme tag functions
 * look for (def... xyzzy
 * look for (def... (xyzzy
 * look for (def ... ((... (xyzzy ....
 * look for (set! xyzzy
 */
static void readIdentifier (vString *const name, const unsigned char *cp)
{
	const unsigned char *p;
	vStringClear (name);
	/* Go till you get to white space or a syntactic break */
	for (p = cp; *p != '\0' && *p != '(' && *p != ')' && !isspace (*p); p++)
		vStringPut (name, (int) *p);
}

static void findSchemeTags (void)
{
	vString *name = vStringNew ();
	const unsigned char *line;

	while ((line = readLineFromInputFile ()) != NULL)
	{
		const unsigned char *cp = line;

		if (cp [0] == '(' &&
			(cp [1] == 'D' || cp [1] == 'd') &&
			(cp [2] == 'E' || cp [2] == 'e') &&
			(cp [3] == 'F' || cp [3] == 'f'))
		{
			while (*cp != '\0'  &&  !isspace (*cp))
				cp++;
			/* Skip over open parens and white space */
			do {
				while (*cp != '\0' && (isspace (*cp) || *cp == '('))
					cp++;
				if (*cp == '\0')
					cp = line = readLineFromInputFile ();
				else
					break;
			} while (line);
			if (line == NULL)
				break;
			readIdentifier (name, cp);
			makeSimpleTag (name, K_FUNCTION);
		}
		if (cp [0] == '(' &&
			(cp [1] == 'S' || cp [1] == 's') &&
			(cp [2] == 'E' || cp [2] == 'e') &&
			(cp [3] == 'T' || cp [3] == 't') &&
			(cp [4] == '!') &&
			(isspace (cp [5]) || cp[5] == '\0'))
		{
			cp += 5;
			/* Skip over white space */
			do {
				while (*cp != '\0' && isspace (*cp))
					cp++;
				if (*cp == '\0')
					cp = line = readLineFromInputFile ();
				else
					break;
			} while (line);
			if (line == NULL)
				break;
			readIdentifier (name, cp);
			makeSimpleTag (name, K_SET);
		}
	}
	vStringDelete (name);
}

extern parserDefinition* SchemeParser (void)
{
	static const char *const extensions [] = {
		"SCM", "SM", "sch", "scheme", "scm", "sm", "rkt", NULL
	};
	static const char *const aliases [] = {
		"gosh", "guile", "racket", NULL
	};
	parserDefinition* def = parserNew ("Scheme");
	def->kindTable      = SchemeKinds;
	def->kindCount  = ARRAY_SIZE (SchemeKinds);
	def->extensions = extensions;
	def->aliases    = aliases;
	def->parser     = findSchemeTags;
	return def;
}
