/*
*   $Id$
*
*   Copyright (c) 2000, Jesus Castagnetto <jmcastagnetto@zkey.com>
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   This module contains functions for generating tags for the PHP web page
*   scripting language. Only recognizes functions and classes, not methods or
*   variables.
*
*   Parsing PHP defines by Pavel Hlousek <pavel.hlousek@seznam.cz>, Apr 2003.
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
    K_CLASS, K_DEFINE, K_FUNCTION, K_VARIABLE
} phpKind;

static kindOption PhpKinds [] = {
    { TRUE, 'c', "class",    "classes" },
    { TRUE, 'd', "define",   "constant definitions" },
    { TRUE, 'f', "function", "functions" },
    { TRUE, 'v', "variable", "variables" }
};

/*
*   FUNCTION DEFINITIONS
*/

static boolean isLetter(const int c)
{
    return (boolean)(isalpha(c) || (c >= 127  &&  c <= 255));
}

static boolean isVarChar1(const int c)
{
    return (boolean)(isLetter (c)  ||  c == '_');
}

static boolean isVarChar(const int c)
{
    return (boolean)(isVarChar1 (c) || isdigit (c));
}

static void findPhpTags (void)
{
    vString *name = vStringNew ();
    const unsigned char *line;

    while ((line = fileReadLine ()) != NULL)
    {
	const unsigned char *cp = line;

	while (isspace (*cp))
	    cp++;

	if (*(const char*)cp == '$'  &&  isVarChar1 (*(const char*)(cp+1)))
	{
	    cp += 1;
	    vStringClear (name);
	    while (isVarChar ((int) *cp))
	    {
		vStringPut (name, (int) *cp);
		++cp;
	    }
	    while (isspace ((int) *cp))
		++cp;
	    if (*(const char*) cp == '=')
	    {
		vStringTerminate (name);
		makeSimpleTag (name, PhpKinds, K_VARIABLE);
		vStringClear (name);
	    }
	}
	else if (strncmp ((const char*) cp, "function", (size_t) 8) == 0  &&
	    isspace ((int) cp [8]))
	{
	    cp += 8;

	    while (isspace ((int) *cp))
		++cp;

	    if (*cp == '&')	/* skip reference character */
		cp++;

	    vStringClear (name);
	    while (isalnum ((int) *cp)  ||  *cp == '_')
	    {
		vStringPut (name, (int) *cp);
		++cp;
	    }
	    vStringTerminate (name);
	    makeSimpleTag (name, PhpKinds, K_FUNCTION);
	    vStringClear (name);
	} 
	else if (strncmp ((const char*) cp, "class", (size_t) 5) == 0 &&
		 isspace ((int) cp [5]))
	{
	    cp += 5;

	    while (isspace ((int) *cp))
		++cp;
	    vStringClear (name);
	    while (isalnum ((int) *cp)  ||  *cp == '_')
	    {
		vStringPut (name, (int) *cp);
		++cp;
	    }
	    vStringTerminate (name);
	    makeSimpleTag (name, PhpKinds, K_CLASS);
	    vStringClear (name);
	}
	else if (strncmp ((const char*) cp, "define", (size_t) 6) == 0 &&
	         ! isalnum ((int) cp [6]))
	{
	    cp += 6;

	    while (isspace ((int) *cp))
		++cp;
	    if (*cp != '(')
		continue;
	    ++cp;

	    while (isspace ((int) *cp))
		++cp;
	    if ((*cp == '\'') || (*cp == '"'))
		++cp;
	    else if (! ((*cp == '_')  || isalnum ((int) *cp)))
		continue;
          
	    vStringClear (name);
	    while (isalnum ((int) *cp)  ||  *cp == '_')
	    {
		vStringPut (name, (int) *cp);
		++cp;
	    }
	    vStringTerminate (name);
	    makeSimpleTag (name, PhpKinds, K_DEFINE);
	    vStringClear (name);
	}
    }
    vStringDelete (name);
}

extern parserDefinition* PhpParser (void)
{
    static const char *const extensions [] = { "php", "php3", "phtml", NULL };
    parserDefinition* def = parserNew ("PHP");
    def->kinds      = PhpKinds;
    def->kindCount  = KIND_COUNT (PhpKinds);
    def->extensions = extensions;
    def->parser     = findPhpTags;
    return def;
}

/* vi:set tabstop=8 shiftwidth=4: */
