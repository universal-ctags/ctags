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
#include "general.h"	/* must always come first */

#include <string.h>

#include "read.h"
#include "routines.h"
#include "vstring.h"

/*
*   DATA DEFINITIONS
*/
typedef enum {
    K_NONE = -1,
    K_SUBROUTINE,
    K_PACKAGE,
    K_CONSTANT
} perlKind;

static kindOption PerlKinds [] = {
    { TRUE, 's', "subroutine", "subroutines" },
    { TRUE, 'p', "package",    "packages" },
    { TRUE, 'c', "constant",   "constants" }
};

/*
*   FUNCTION DEFINITIONS
*/

static boolean isPodWord (const char *word)
{
    boolean result = FALSE;
    if (isalpha (*word))
    {
	const char *white = strchr (word, ' ');
	if (white == NULL)
	    white = strchr (word, '\t');
	if (white != NULL)
	{
	    const char *const pods [] = {
		"head1", "head2", "head3", "head4", "over", "item", "back",
		"pod", "begin", "end", "for"
	    };
	    const size_t count = sizeof (pods) / sizeof (pods [0]);
	    const size_t len = white - word;
	    char *id = (char*) eMalloc (len + 1);
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
    boolean skipPodDoc = FALSE;
    const unsigned char *line;

    while ((line = fileReadLine ()) != NULL)
    {
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
	}
	else if (strncmp((const char*) cp, "use", (size_t) 3) == 0)
	{
	    cp += 3;
	    if (!isspace(*cp))
		continue;
	    while (*cp && isspace(*cp))
		++cp;
	    if (strncmp((const char*) cp, "constant", (size_t) 8) != 0)
	    {
		cp += 8;
		continue;
	    }
	    cp += 8;
	    kind = K_CONSTANT;
	}
	else if (strncmp((const char*) cp, "package", (size_t) 7) == 0)
	{
	    cp += 7;
	    kind = K_PACKAGE;
	}
	if (kind != K_NONE)
	{
	    if (!isspace(*cp))		/* woops, not followed by a space */
	        continue;

	    while (isspace (*cp))
		cp++;
	    while (! isspace ((int) *cp) && *cp != '\0' &&
		   strchr ("{(;=", (int) *cp) == NULL)
	    {
		vStringPut (name, (int) *cp);
		cp++;
	    }
	    vStringTerminate (name);
	    if (vStringLength (name) > 0)
		makeSimpleTag (name, PerlKinds, kind);
	    vStringClear (name);
	}
    }
    vStringDelete (name);
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

/* vi:set tabstop=8 shiftwidth=4: */
