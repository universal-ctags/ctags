/*
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

static boolean isIdentChar (int c)
{
	return (isalnum (c) || c == '_' || c == '-');
}

static const unsigned char *skipDoubleString (const unsigned char *cp)
{
	const unsigned char* prev = cp;
	cp++;
	while ((*cp != '"' || *prev == '\\') && *cp != '\0')
	{
		prev = cp;
		cp++;
	}
	return cp;
}

static const unsigned char *skipSingleString (const unsigned char *cp)
{
	cp++;
	while (*cp != '\'' && *cp != '\0')
		cp++;
	return cp;
}

static void findShTags (void)
{
	vString *name = vStringNew ();
	const unsigned char *line;
	vString *hereDocDelimiter = NULL;
	boolean hereDocIndented = FALSE;

	while ((line = fileReadLine ()) != NULL)
	{
		const unsigned char* cp = line;
		boolean aliasFound = FALSE;
		boolean functionFound = FALSE;

		if (hereDocDelimiter)
		{
			if (hereDocIndented)
			{
				while (*cp == '\t')
					cp++;
			}
			if (strcmp ((const char *) cp, vStringValue (hereDocDelimiter)) == 0)
			{
				vStringDelete (hereDocDelimiter);
				hereDocDelimiter = NULL;
			}
			continue;
		}

		while (*cp != '\0')
		{
			/* jump over whitespace */
			while (isspace ((int)*cp))
				cp++;

			/* jump over strings */
			if (*cp == '"')
				cp = skipDoubleString (cp);
			else if (*cp == '\'')
				cp = skipSingleString (cp);
			/* jump over comments */
			else if (*cp == '#')
				break;
			/* jump over here-documents */
			else if (cp[0] == '<' && cp[1] == '<')
			{
				const unsigned char *start, *end;
				boolean trimEscapeSequences = FALSE;
				boolean quoted = FALSE;
				cp += 2;
				/* an optional "-" strips leading tabulations from the heredoc lines */
				if (*cp != '-')
					hereDocIndented = FALSE;
				else
				{
					hereDocIndented = TRUE;
					cp++;
				}
				while (isspace (*cp))
					cp++;
				start = end = cp;
				/* the delimiter can be surrounded by quotes */
				if (*cp == '"')
				{
					start++;
					end = cp = skipDoubleString (cp);
					/* we need not to worry about variable substitution, they
					 * don't happen in heredoc delimiter definition */
					trimEscapeSequences = TRUE;
					quoted = TRUE;
				}
				else if (*cp == '\'')
				{
					start++;
					end = cp = skipSingleString (cp);
					quoted = TRUE;
				}
				else
				{
					while (isIdentChar ((int) *cp))
						cp++;
					end = cp;
				}
				if (end > start || quoted)
				{
					hereDocDelimiter = vStringNew ();
					for (; end > start; start++)
					{
						if (trimEscapeSequences && *start == '\\')
							start++;
						vStringPut (hereDocDelimiter, *start);
					}
				}
			}

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
			if (! isIdentChar ((int) *cp))
			{
				aliasFound = FALSE;
				functionFound = FALSE;
				++cp;
				continue;
			}
			while (isIdentChar ((int) *cp))
			{
				vStringPut (name, (int) *cp);
				++cp;
			}
			vStringTerminate (name);

			while (isspace ((int) *cp))
				++cp;
			if (*cp == '(')
			{
				++cp;
				while (isspace ((int) *cp))
					++cp;
				if (*cp == ')')
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
	if (hereDocDelimiter)
		vStringDelete (hereDocDelimiter);
}

extern parserDefinition* ShParser (void)
{
	static const char *const extensions [] = {
		"sh", "SH", "bsh", "bash", "ksh", "zsh", "ash", NULL
	};
	static const char *const aliases [] = {
		"sh", "bash", "ksh", "zsh", "ash",
		/* major mode name in emacs */
		"shell-script",
		NULL
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
