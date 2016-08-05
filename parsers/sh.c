/*
*   Copyright (c) 2000-2002, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for generating tags for scripts for the
*   Bourne shell (and its derivatives, the Korn and Z shells).
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include <string.h>

#include "kind.h"
#include "parse.h"
#include "read.h"
#include "routines.h"
#include "vstring.h"
#include "xtag.h"

/*
*   DATA DEFINITIONS
*/
typedef enum {
	K_NOTHING = -1,		/* place holder. Never appears on tags file. */
	K_ALIAS,
	K_FUNCTION,
	K_SOURCE,
} shKind;

typedef enum {
	R_SOURCE_GENERIC,
} shScriptRole;

static roleDesc ShScriptRoles [] = {
	{ TRUE, "loaded", "loaded" },
};

static kindOption ShKinds [] = {
	{ TRUE, 'a', "alias", "aliases"},
	{ TRUE, 'f', "function", "functions"},
	{ TRUE, 's', "script", "script files",
	  .referenceOnly = TRUE, ATTACH_ROLES (ShScriptRoles) },
};

/*
*   FUNCTION DEFINITIONS
*/

static boolean isFileChar  (int c)
{
	return (isalnum (c)
		|| c == '_' || c == '-'
		|| c == '/' || c == '.'
		|| c == '+' || c == '^'
		|| c == '%' || c == '@'
		|| c == '~');
}

static boolean isIdentChar (int c)
{
	return (isalnum (c) || c == '_' || c == '-');
}

/* bash allows all kinds of crazy stuff as the identifier after 'function' */
static boolean isBashFunctionChar (int c)
{
	return (c > 1 /* NUL and SOH are disallowed */ && c != 0x7f &&
	        /* blanks are disallowed, but VT and FF (and CR to some extent, but
	         * let's not fall into the pit of craziness) */
	        c != ' ' && c != '\t' && c != '\n' && c != '\r' &&
	        c != '"' && c != '\'' && c != '$' && c != '`' && c != '\\' &&
	        c != '&' && c != ';' &&
	        c != '(' && c != ')' &&
	        c != '<' && c != '>');
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
	boolean (* check_char)(int);

	while ((line = readLineFromInputFile ()) != NULL)
	{
		const unsigned char* cp = line;
		shKind found_kind = K_NOTHING;

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
					/* The input may be broken as a shell script but we need to avoid
					   memory leaking. */
					if (hereDocDelimiter)
						vStringClear(hereDocDelimiter);
					else
						hereDocDelimiter = vStringNew ();
					for (; end > start; start++)
					{
						if (trimEscapeSequences && *start == '\\')
							start++;
						vStringPut (hereDocDelimiter, *start);
					}
				}
			}

			check_char = isIdentChar;

			if (strncmp ((const char*) cp, "function", (size_t) 8) == 0  &&
				isspace ((int) cp [8]))
			{
				check_char = isBashFunctionChar;
				found_kind = K_FUNCTION;
				cp += 8;
			}
			else if (strncmp ((const char*) cp, "alias", (size_t) 5) == 0  &&
				isspace ((int) cp [5]))
			{
				found_kind = K_ALIAS;
				cp += 5;
			}
			else if (isXtagEnabled (XTAG_REFERENCE_TAGS)
				 && ShKinds [K_SOURCE].enabled)
			{
				if (cp [0] == '.'
				    && isspace((int) cp [1]))
				{
					found_kind = K_SOURCE;
					++cp;
				}
				else if (strncmp ((const char*) cp, "source", (size_t) 6) == 0
					 && isspace((int) cp [6]))
				{
					found_kind = K_SOURCE;
					cp += 6;
				}
				if (found_kind == K_SOURCE)
					check_char = isFileChar;
			}
			if (found_kind != K_NOTHING)
				while (isspace ((int) *cp))
					++cp;

			// Get the name of the function, alias or file to be read by source
			if (! check_char ((int) *cp))
			{
				found_kind = K_NOTHING;
				if (*cp != '\0')
					++cp;
				continue;
			}
			while (check_char ((int) *cp))
			{
				vStringPut (name, (int) *cp);
				++cp;
			}
			vStringTerminate (name);

			while (isspace ((int) *cp))
				++cp;

			if ((found_kind != K_SOURCE)
			    && *cp == '(')
			{
				++cp;
				while (isspace ((int) *cp))
					++cp;
				if (*cp == ')')
				{
					found_kind = K_FUNCTION;
					++cp;
				}
			}

			if (found_kind != K_NOTHING)
			{
				if (found_kind == K_SOURCE)
					makeSimpleRefTag (name, ShKinds, K_SOURCE, R_SOURCE_GENERIC);
				else
					makeSimpleTag (name, ShKinds, found_kind);
				found_kind = K_NOTHING;
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
	def->kindCount  = ARRAY_SIZE (ShKinds);
	def->extensions = extensions;
	def->aliases = aliases;
	def->parser     = findShTags;
	return def;
}
