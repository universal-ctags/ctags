/*
*   $Id$
*
*   Copyright (c) 2000-2002, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   This module contains functions for generating tags for TCL scripts.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"	/* must always come first */

#include <string.h>

#include "parse.h"
#include "read.h"
#include "vstring.h"

/*
*   DATA DEFINITIONS
*/
typedef enum {
    K_PROCEDURE
} tclKind;

static kindOption TclKinds [] = {
    { TRUE, 'p', "procedure", "procedures" }
};

/*
*   FUNCTION DEFINITIONS
*/

static void findTclTags (void)
{
    vString *name = vStringNew ();
    const unsigned char *line;

    while ((line = fileReadLine ()) != NULL)
    {
	int i;

	if (line [0] == '\0'  ||  line [0] == '#')
	    continue;

	if (strncmp ((const char*) line, "proc", (size_t) 4) == 0)
	{
	    const unsigned char *cp = line + 4;
	    while (isspace ((int) *cp))
		++cp;
	    while ((int) *cp != '\0'  &&  ! isspace ((int) *cp))
	    {
		vStringPut (name, (int) *cp);
		++cp;
	    }
	    vStringTerminate (name);
	    makeSimpleTag (name, TclKinds, K_PROCEDURE);
	    vStringClear (name);
	}
    }
    vStringDelete (name);
}

extern parserDefinition* TclParser (void)
{
    static const char *const extensions [] = { "tcl", "tk", "wish", NULL };
    parserDefinition* def = parserNew ("Tcl");
    def->kinds      = TclKinds;
    def->kindCount  = KIND_COUNT (TclKinds);
    def->extensions = extensions;
    def->parser     = findTclTags;
    return def;
}

/* vi:set tabstop=8 shiftwidth=4: */
