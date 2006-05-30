/*
*   $Id$
*
*   Copyright (c) 2000-2003, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   This module contains functions for generating tags for PERL language
*   files.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include <string.h>

#include "options.h"
#include "read.h"
#include "routines.h"
#include "vstring.h"

/*
*   DATA DEFINITIONS
*/
typedef enum {
	K_NONE = -1,
	K_CONSTANT,
	K_LABEL,
	K_SUBROUTINE
} perlKind;

static kindOption PerlKinds [] = {
	{ TRUE, 'c', "constant",   "constants" },
	{ TRUE, 'l', "label",      "labels" },
	{ TRUE, 's', "subroutine", "subroutines" }
};

/*
*   FUNCTION DEFINITIONS
*/

static boolean isIdentifier1 (int c)
{
	return (boolean) (isalpha (c) || c == '_');
}

static boolean isIdentifier (int c)
{
	return (boolean) (isalnum (c) || c == '_');
}

static boolean isPodWord (const char *word)
{
	boolean result = FALSE;
	if (isalpha (*word))
	{
		const char *const pods [] = {
			"head1", "head2", "head3", "head4", "over", "item", "back",
			"pod", "begin", "end", "for"
		};
		const size_t count = sizeof (pods) / sizeof (pods [0]);
		const char *white = strpbrk (word, " \t");
		const size_t len = (white!=NULL) ? (size_t)(white-word) : strlen (word);
		char *const id = (char*) eMalloc (len + 1);
		size_t i;
		strncpy (id, word, len);
		id [len] = '\0';
		for (i = 0  ;  i < count  &&  ! result  ;  ++i)
		{
			if (strcmp (id, pods [i]) == 0)
				result = TRUE;
		}
		eFree (id);
	}
	return result;
}

/* Algorithm adapted from from GNU etags.
 * Perl support by Bart Robinson <lomew@cs.utah.edu>
 * Perl sub names: look for /^ [ \t\n]sub [ \t\n]+ [^ \t\n{ (]+/
 */
static void findPerlTags (void)
{
	vString *name = vStringNew ();
	vString *package = NULL;
	boolean skipPodDoc = FALSE;
	const unsigned char *line;

	while ((line = fileReadLine ()) != NULL)
	{
		boolean spaceRequired = FALSE;
		boolean qualified = FALSE;
		const unsigned char *cp = line;
		perlKind kind = K_NONE;

		if (skipPodDoc)
		{
			if (strncmp ((const char*) line, "=cut", (size_t) 4) == 0)
				skipPodDoc = FALSE;
			continue;
		}
		else if (line [0] == '=')
		{
			skipPodDoc = isPodWord ((const char*)line + 1);
			continue;
		}
		else if (strcmp ((const char*) line, "__DATA__") == 0)
			break;
		else if (strcmp ((const char*) line, "__END__") == 0)
			break;
		else if (line [0] == '#')
			continue;

		while (isspace (*cp))
			cp++;

		if (strncmp((const char*) cp, "sub", (size_t) 3) == 0)
		{
			cp += 3;
			kind = K_SUBROUTINE;
			spaceRequired = TRUE;
			qualified = TRUE;
		}
		else if (strncmp((const char*) cp, "use", (size_t) 3) == 0)
		{
			cp += 3;
			if (!isspace(*cp))
				continue;
			while (*cp && isspace (*cp))
				++cp;
			if (strncmp((const char*) cp, "constant", (size_t) 8) != 0)
				continue;
			cp += 8;
			kind = K_CONSTANT;
			spaceRequired = TRUE;
			qualified = TRUE;
		}
		else if (strncmp((const char*) cp, "package", (size_t) 7) == 0)
		{
			cp += 7;
			if (package == NULL)
				package = vStringNew ();
			else
				vStringClear (package);
			while (isspace (*cp))
				cp++;
			while ((int) *cp != ';'  &&  !isspace ((int) *cp))
			{
				vStringPut (package, (int) *cp);
				cp++;
			}
			vStringCatS (package, "::");
		}
		else
		{
			if (isIdentifier1 (*cp))
			{
				const unsigned char *p = cp;
				while (isIdentifier (*p))
					++p;
				if ((int) *p == ':')
					kind = K_LABEL;
			}
		}
		if (kind != K_NONE)
		{
			if (spaceRequired && !isspace (*cp))
				continue;

			while (isspace (*cp))
				cp++;
			while (isIdentifier (*cp))
			{
				vStringPut (name, (int) *cp);
				cp++;
			}
			vStringTerminate (name);
			if (vStringLength (name) > 0)
			{
				makeSimpleTag (name, PerlKinds, kind);
				if (Option.include.qualifiedTags && qualified &&
					package != NULL  && vStringLength (package) > 0)
				{
					vString *const qualifiedName = vStringNew ();
					vStringCopy (qualifiedName, package);
					vStringCat (qualifiedName, name);
					makeSimpleTag (qualifiedName, PerlKinds, kind);
					vStringDelete (qualifiedName);
				}
			}
			vStringClear (name);
		}
	}
	vStringDelete (name);
	if (package != NULL)
		vStringDelete (package);
}

extern parserDefinition* PerlParser (void)
{
	static const char *const extensions [] = { "pl", "pm", "plx", "perl", NULL };
	parserDefinition* def = parserNew ("Perl");
	def->kinds      = PerlKinds;
	def->kindCount  = KIND_COUNT (PerlKinds);
	def->extensions = extensions;
	def->parser     = findPerlTags;
	return def;
}

/* vi:set tabstop=4 shiftwidth=4: */
