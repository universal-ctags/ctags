/*
*   $Id$
*
*   Copyright (c) 2000-2001, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   This module contains functions for generating tags for Python language
*   files.
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
    K_CLASS, K_FUNCTION
} pythonKind;

static kindOption PythonKinds [] = {
    { TRUE, 'c', "class",    "classes" },
    { TRUE, 'f', "function", "functions" }
};

/*
*   FUNCTION DEFINITIONS
*/

static void findPythonTags (void)
{
    vString *name = vStringNew ();
    const unsigned char *line;
    boolean inMultilineString = FALSE;

    while ((line = fileReadLine ()) != NULL)
    {
	const unsigned char *cp = line;

	while (*cp != '\0')
	{
	    if (*cp=='"' &&
		strncmp ((const char*) cp, "\"\"\"", (size_t) 3) == 0)
	    {
		inMultilineString = (boolean) !inMultilineString;
		cp += 3;
	    }
	    if (inMultilineString  ||  isspace ((int) *cp))
		++cp;
	    else if (*cp == '#')
		break;
	    else if (strncmp ((const char*) cp, "class", (size_t) 5) == 0)
	    {
		cp += 5;
		if (isspace ((int) *cp))
		{
		    while (isspace ((int) *cp))
			++cp;
		    while (isalnum ((int) *cp)  ||  *cp == '_')
		    {
			vStringPut (name, (int) *cp);
			++cp;
		    }
		    vStringTerminate (name);
		    makeSimpleTag (name, PythonKinds, K_CLASS);
		    vStringClear (name);
		}
	    }
	    else if (strncmp ((const char*) cp, "def", (size_t) 3) == 0)
	    {
		cp += 3;
		if (isspace ((int) *cp))
		{
		    while (isspace ((int) *cp))
			++cp;
		    while (isalnum ((int) *cp)  ||  *cp == '_')
		    {
			vStringPut (name, (int) *cp);
			++cp;
		    }
		    vStringTerminate (name);
		    makeSimpleTag (name, PythonKinds, K_FUNCTION);
		    vStringClear (name);
		}
	    }
	    else if (*cp != '\0')
	    {
		do
		    ++cp;
		while (isalnum ((int) *cp)  ||  *cp == '_');
	    }
	}
    }
    vStringDelete (name);
}

extern parserDefinition* PythonParser (void)
{
    static const char *const extensions [] = { "py", "python", NULL };
    parserDefinition* def = parserNew ("Python");
    def->kinds      = PythonKinds;
    def->kindCount  = KIND_COUNT (PythonKinds);
    def->extensions = extensions;
    def->parser     = findPythonTags;
    return def;
}

/* vi:set tabstop=8 shiftwidth=4: */
