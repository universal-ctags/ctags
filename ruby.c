/*
*   $Id$
*
*   Copyright (c) 2000-2001, Thaddeus Covert <sahuagin@mediaone.net>
*   Copyright (c) 2002 Matthias Veit <matthias_veit@yahoo.de>
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   This module contains functions for generating tags for Ruby language
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
    K_CLASS, K_METHOD, K_SINGLETON, K_MIXIN
} rubyKind;

static kindOption RubyKinds [] = {
    { TRUE, 'c', "class",  "classes" },
    { TRUE, 'f', "method", "methods" },
    { TRUE, 'F', "singleton method", "singleton methods" },
    { TRUE, 'm', "mixin",  "mixins" }
};


/*
*   FUNCTION DEFINITIONS
*/

static void findRubyTags (void)
{
    vString *name = vStringNew ();
    const unsigned char *line;
    boolean inMultilineString = FALSE;

    while ((line = fileReadLine ()) != NULL)
    {
        const unsigned char *cp = line;
        boolean is_singleton = FALSE;

        while (*cp != '\0')
	{
	    if (strncmp ((const char*) cp, "\"\"\"", (size_t) 3) == 0)
	    {
		cp += 3;
		inMultilineString = (boolean) !inMultilineString;
	    }
	    /* make sure you include the comments */
	    if (strncmp ((const char*) cp, "==begin", (size_t) 7) == 0)
	    {
		cp += 7;
		inMultilineString = (boolean) !inMultilineString;
	    }
	    /* mark the end of a comment */
	    if (strncmp ((const char*) cp, "==end", (size_t) 5) == 0)
	    {
		inMultilineString = FALSE;
		cp += 5;
	    }
	    if (inMultilineString  ||  isspace ((int) *cp))
		++cp;
	    else if (*cp == '#')
		break;
	    else if (strncmp ((const char*) cp, "module", (size_t) 6) == 0)
	    {
		cp += 6;
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
		    makeSimpleTag (name, RubyKinds, K_MIXIN);
		    vStringClear (name);
		}
	    }
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
		    makeSimpleTag (name, RubyKinds, K_CLASS);
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

		    /* Put the valid characters allowed in a variable name
			* in here. In ruby a variable name ENDING in ! means
			* it changes the underlying data structure in place.
			* A variable name ENDING in ? means that the function
			* returns a bool. Functions should not start with these
			* characters.
			*/
		    while (isalnum ((int) *cp)  ||
			(cp != '\0' && strchr ("_!?.", (int) cp) != NULL))
		    {
			/* classmethods are accesible only via class instance
			    * instead of object instance. This difference has to
			    * be outlined.
			    */
			if (*cp == '.')
			{
			    is_singleton = TRUE;
			    vStringTerminate (name);
			    vStringClear(name);
			}
			else
			{
			    vStringPut (name, (int) *cp);
			}
			++cp;
		    }
		    vStringTerminate (name);
		    if (is_singleton)
			makeSimpleTag (name, RubyKinds, K_SINGLETON);
		    else
			makeSimpleTag (name, RubyKinds, K_METHOD);
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

extern parserDefinition* RubyParser (void)
{
    static const char *const extensions [] = { "rb", NULL };
    parserDefinition* def = parserNew ("Ruby");
    def->kinds      = RubyKinds;
    def->kindCount  = KIND_COUNT (RubyKinds);
    def->extensions = extensions;
    def->parser     = findRubyTags;
    return def;
}
