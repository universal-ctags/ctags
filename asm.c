/*
*   $Id$
*
*   Copyright (c) 2000-2002, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   This module contains functions for generating tags for assembly language
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
    K_DEFINE, K_LABEL, K_MACRO
} asmKind;

/* indexed by asmKind */
static kindOption AsmKinds [] = {
    { TRUE, 'd', "define", "defines (names assigned a specified value)"},
    { TRUE, 'l', "label", "labels (names assigned an address)"},
    { TRUE, 'm', "macro", "macros"}
};

/*
*   FUNCTION DEFINITIONS
*/

/* Algorithm adapted from from GNU etags.
 * By Bob Weiner, Motorola Inc., 4/3/94
 * Unix and microcontroller assembly tag handling
 * look for '^ [a-zA-Z_.$] [a-zA_Z0-9_.$]*[: ^I^J]'
 */
static void findAsmTags (void)
{
    vString *name = vStringNew ();
    const unsigned char *line;

    while ((line = fileReadLine ()) != NULL)
    {
	const unsigned char *cp = line;
	int c = *cp;

	/*  If first char is alphabetic or one of [_.$], test for colon
	 *  following identifier.
	 */
	if (isalpha (c) || c == '_' || c == '.' || c == '$')
	{
	    vStringPut (name, c);
	    c = *++cp;
	    while (isalnum (c) || c == '_' || c == '.' || c == '$')
	    {
		vStringPut (name, c);
		c = *++cp;
	    }
	    vStringTerminate (name);
	    while (isspace (c))
		c = *++cp;
	    if (c == ':')
		makeSimpleTag (name, AsmKinds, K_LABEL);
	    else if (c == '=' ||
		     strncmp ((const char*) cp, "equ", (size_t) 3) == 0)
		makeSimpleTag (name, AsmKinds, K_DEFINE);
	    else if (strcmp (vStringValue (name), ".macro") == 0)
	    {
		vStringClear (name);
		while (isalnum (c) || c == '_')
		{
		    vStringPut (name, c);
		    c = *++cp;
		}
		vStringTerminate (name);
		if (vStringLength (name) > 0)
		    makeSimpleTag (name, AsmKinds, K_MACRO);
	    }
	    vStringClear (name);
	}
    }
    vStringDelete (name);
}

extern parserDefinition* AsmParser (void)
{
    static const char *const extensions [] = { "asm", "s", "S", NULL };
    parserDefinition* def = parserNew ("Asm");
    def->kinds      = AsmKinds;
    def->kindCount  = KIND_COUNT (AsmKinds);
    def->extensions = extensions;
    def->parser     = findAsmTags;
    return def;
}

/* vi:set tabstop=8 shiftwidth=4: */
