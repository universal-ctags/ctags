/*
*   $Id$
*
*   Copyright (c) 2000-2002, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   This module contains functions for generating tags for COBOL language
*   files.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"	/* must always come first */
#include "parse.h"

/*
*   FUNCTION DEFINITIONS
*/

static void installCobolRegex (const langType language)
{
   addTagRegex (language, "^[ \t]*[0-9]+[ \t]+([A-Z0-9][A-Z0-9-]*)[ \t]+(BLANK|OCCURS|IS|JUST|PIC|REDEFINES|RENAMES|SIGN|SYNC|USAGE|VALUE)",
	"\\1", "d,data", "i");
   addTagRegex (language, "^[ \t]*[FSR]D[ \t]+([A-Z0-9][A-Z0-9-]*)\\.",
	"\\1", "f,file", "i");
   addTagRegex (language, "^[ \t]*[0-9]+[ \t]+([A-Z0-9][A-Z0-9-]*)\\.",
	"\\1", "g,group", "i");
   addTagRegex (language, "^[ \t]*([A-Z0-9][A-Z0-9-]*)\\.",
	"\\1", "p,paragraph", "i");
   addTagRegex (language, "^[ \t]*PROGRAM-ID\\.[ \t]+([A-Z0-9][A-Z0-9-]*)\\.",
	"\\1", "P,program", "i");
   addTagRegex (language, "^[ \t]*([A-Z0-9][A-Z0-9-]*)[ \t]+SECTION\\.",
	"\\1", "s,section", "i");
}

extern parserDefinition* CobolParser ()
{
    static const char *const extensions [] = {
	    "cbl", "cob", "CBL", "COB", NULL };
    parserDefinition* def = parserNew ("Cobol");
    def->extensions = extensions;
    def->initialize = installCobolRegex;
    def->regex      = TRUE;
    return def;
}

/* vi:set tabstop=8 shiftwidth=4: */
