/*
*   $Id$
*
*   Copyright (c) 2000-2002, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   This module contains functions for generating tags for makefiles.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"	/* must always come first */

#include <string.h>
#include <ctype.h>

#include "parse.h"
#include "read.h"
#include "vstring.h"

/*
*   DATA DEFINITIONS
*/
typedef enum {
    K_MACRO
} shKind;

static kindOption MakeKinds [] = {
    { TRUE, 'm', "macro", "macros"}
};

/*
*   FUNCTION DEFINITIONS
*/

static boolean isIdentifier (int c)
{
    return (boolean)(isalnum (c)  ||  c == '_');
}

static const unsigned char *readIdentifier (
	const unsigned char *p, vString *const id)
{
    vStringClear (id);
    while (isIdentifier ((int) *p))
    {
	vStringPut (id, (int) *p);
	++p;
    }
    vStringTerminate (id);
    return p;
}

static void findMakeTags (void)
{
    vString *name = vStringNew ();
    const unsigned char *line;
    boolean line_continuation = FALSE;
    boolean in_define = FALSE;

    while ((line = fileReadLine ()) != NULL)
    {
	const unsigned char* cp = line;
	boolean possible = TRUE;

	if (! line_continuation)
	{
	    if (! isIdentifier ((int) *cp))
		continue;
	}
	while (isspace ((int) *cp))
	    ++cp;
	if (*cp == '#')
	    continue;
	while (*cp != '\0')
	{
	    /*  We look for any sequence of identifier characters following
	     *  either a white space or a colon and followed by either = or :=
	     */
	    if (possible && isIdentifier ((int) *cp))
	    {
		cp = readIdentifier (cp, name);
		while (isspace ((int) *cp))
		    ++cp;
		if (strcmp (vStringValue (name), "endef") == 0)
		    in_define = FALSE;
		else if (in_define)
		    break;
		else if (strcmp (vStringValue (name), "define") == 0  &&
		    isIdentifier ((int) *cp))
		{
		    in_define = TRUE;
		    cp = readIdentifier (cp, name);
		    makeSimpleTag (name, MakeKinds, K_MACRO);
		}
		else
		{
		    if (strchr (":?", *cp) != NULL)
			++cp;
		    if (*cp == '=')
			makeSimpleTag (name, MakeKinds, K_MACRO);
		}
	    }
	    else if (isspace ((int) *cp) ||  *cp == ':')
		possible = TRUE;
	    else
		possible = FALSE;
	    if (*cp != '\0')
		++cp;
	}
	line_continuation = (boolean)
	    (cp > line + 2  &&  strcmp ((const char *) cp - 2, "\\\n") == 0);
    }
    vStringDelete (name);
}

extern parserDefinition* MakefileParser (void)
{
    static const char *const patterns [] = { "[Mm]akefile", NULL };
    static const char *const extensions [] = { "mak", "mk", NULL };
    parserDefinition* const def = parserNew ("Make");
    def->kinds      = MakeKinds;
    def->kindCount  = KIND_COUNT (MakeKinds);
    def->patterns   = patterns;
    def->extensions = extensions;
    def->parser     = findMakeTags;
    return def;
}

/* vi:set tabstop=8 shiftwidth=4: */
