/*
*   $Id$
*
*   Copyright (c) 2000-2002, Darren Hiebert
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
#include "vstring.h"

/*
*   DATA DEFINITIONS
*/
typedef enum {
    K_SUBROUTINE,
    K_PACKAGE
} perlKind;

static kindOption PerlKinds [] = {
    { TRUE, 's', "subroutine", "subroutines" },
    { TRUE, 'p', "package", "packages" }
};

/*
*   FUNCTION DEFINITIONS
*/

/* Algorithm adapted from from GNU etags.
 * Perl support by Bart Robinson <lomew@cs.utah.edu>
 * Perl sub names: look for /^ [ \t\n]sub [ \t\n]+ [^ \t\n{ (]+/
 */
static void findPerlTags (void)
{
    vString *name = vStringNew ();
    boolean skipPodDoc = FALSE;
    const unsigned char *line;
    perlKind kind;

    while ((line = fileReadLine ()) != NULL)
    {
	const unsigned char *cp = line;

	if (skipPodDoc)
	{
	    if (strcmp ((const char*) line, "=cut") == 0)
		skipPodDoc = FALSE;
	    continue;
	}
	else if (line [0] == '=')
	{
	    skipPodDoc = (boolean) (strncmp (
			(const char*) line + 1, "cut", (size_t) 3) != 0);
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

	if (strncmp((const char*) cp, "sub", (size_t) 3) == 0 ||
	    strncmp((const char*) cp, "package", (size_t) 7) == 0)
	{
	    if (strncmp((const char*) cp, "sub", (size_t) 3) == 0)
	    {
	    	cp += 3;
		kind = K_SUBROUTINE;
	    } else {
	    	cp += 7;
		kind = K_PACKAGE;
	    }
	    if (!isspace(*cp))		/* woops, not followed by a space */
	        continue;

	    while (isspace (*cp))
		cp++;
	    while (! isspace ((int) *cp) && *cp != '\0' && *cp != '{' && *cp != '(')
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
    static const char *const extensions [] = { "pl", "pm", "perl", NULL };
    parserDefinition* def = parserNew ("Perl");
    def->kinds      = PerlKinds;
    def->kindCount  = KIND_COUNT (PerlKinds);
    def->extensions = extensions;
    def->parser     = findPerlTags;
    return def;
}

/* vi:set tabstop=8 shiftwidth=4: */
