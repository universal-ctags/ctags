/*
*   $Id$
*
*   Copyright (c) 2000-2001, Thaddeus Covert <sahuagin@mediaone.net>
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
    K_CLASS, K_FUNCTION
} rubyKind;

static kindOption RubyKinds [] = {
    { TRUE, 'c', "class",    "classes" },
    { TRUE, 'f', "function", "functions" },
    { TRUE, 'm', "mixin",    "mixins" }
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

	while (*cp != '\0')
	{
	    if (*cp=='"' &&
		strncmp ((const char*) cp, "\"\"\"", (size_t) 3) == 0)
	    {
		inMultilineString = (boolean) !inMultilineString;
		cp += 3;
	    }
	    /* make sure you include the comments */
	    if(*cp=='=' && strncmp((const char*)cp, "==begin", (size_t)7) == 0)
	      {
		inMultilineString = (boolean)!inMultilineString;
		cp +=3;
	      }
	    /* mark the end of a comment */
	    if( *cp=='=' && strncmp((const char*)cp, "==end", (size_t)5) == 0)
	      {
		inMultilineString = (boolean)0;
		cp+=5;
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
		    while (isalnum ((int) *cp)  ||  *cp == '_' ||
			   *cp == '!' || *cp =='?')
		    {
			vStringPut (name, (int) *cp);
			++cp;
		    }
		    vStringTerminate (name);
		    makeSimpleTag (name, RubyKinds, K_FUNCTION);
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
