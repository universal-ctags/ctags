/*
*   $Id$
*
*   Copyright (c) 2000-2002, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   Thanks are due to Jay Glanville for significant improvements.
*
*   This module contains functions for generating tags for user-defined
*   functions for the Vim editor.
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
    K_FUNCTION,
    K_VARIABLE
} vimKind;

static kindOption VimKinds [] = {
    { TRUE,  'f', "function", "function definitions"},
    { TRUE,  'v', "variable", "variable definitions"},
};

/*
*   FUNCTION DEFINITIONS
*/

/* This function takes a char pointer, tries to find a scope separator in the
 * string, and if it does, returns a pointer to the character after the colon,
 * and the character defining the scope.
 * If a colon is not found, it returns the original pointer.
 */
static const unsigned char* skipColon (const unsigned char* name, int *scope)
{
    const unsigned char* result = name;
    if (scope != NULL)
	*scope = '\0';
    if (name[1] == ':')
    {
	if (scope != NULL)
	    *scope = *name;
	result = name + 2;
    }
    return result;
}

static void findVimTags (void)
{
    vString *name = vStringNew ();
    const unsigned char *line;
    boolean inFunction = FALSE;
    int scope;

    while ((line = fileReadLine ()) != NULL)
    {
	if (strncmp ((const char*) line, "fu", (size_t) 2) == 0)
	{
	    const unsigned char *cp = line + 1;
            inFunction = TRUE;

	    if ((int) *++cp == 'n'  &&  (int) *++cp == 'c'  &&
		(int) *++cp == 't'  &&  (int) *++cp == 'i'  &&
		(int) *++cp == 'o'  &&  (int) *++cp == 'n')
		    ++cp;
	    if ((int) *cp == '!')
		++cp;
	    if (isspace ((int) *cp))
	    {
		while (isspace ((int) *cp))
		    ++cp;
                cp = skipColon (cp, &scope);
		if (isupper ((int) *cp))
		{
		    do
		    {
			vStringPut (name, (int) *cp);
			++cp;
		    } while (isalnum ((int) *cp)  ||  *cp == '_');
		    vStringTerminate (name);
		    makeSimpleTag (name, VimKinds, K_FUNCTION);
		    vStringClear (name);
		}
	    }
	}

        if (strncmp ((const char*) line, "endf", (size_t) 4) == 0)
            inFunction = FALSE;

        if (!inFunction  &&
                strncmp ((const char*) line, "let", (size_t) 3) == 0)
	{
            /* we've found a variable declared outside of a function!! */
	    const unsigned char *cp = line + 3;
            /* get the name */
            if (isspace ((int) *cp))
	    {
                /* deal with spaces, $, @ and & */
                while (!isalnum ((int) *cp))
                    ++cp;
                cp = skipColon (cp, &scope);
                do
		{
                    vStringPut (name, (int) *cp);
                    ++cp;
                } while (isalnum ((int) *cp)  ||  *cp == '_');
                vStringTerminate (name);
                makeSimpleTag (name, VimKinds, K_VARIABLE);
                vStringClear (name);
            }
        }
    }
    vStringDelete (name);
}

extern parserDefinition* VimParser (void)
{
    static const char *const extensions [] = { "vim", NULL };
    parserDefinition* def = parserNew ("Vim");
    def->kinds      = VimKinds;
    def->kindCount  = KIND_COUNT (VimKinds);
    def->extensions = extensions;
    def->parser     = findVimTags;
    return def;
}

/* vi:set tabstop=8 shiftwidth=4: */
