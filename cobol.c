/*
*   $Id$
*
*   Copyright (c) 2000-2001, Darren Hiebert
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
#include "read.h"
#include "vstring.h"

/*
*   DATA DEFINITIONS
*/
typedef enum eCobolKinds {
    K_PARAGRAPH
} cobolKind;

static kindOption CobolKinds [] = {
    { TRUE, 'p', "paragraph", "paragraphs" }
};

/*
*   FUNCTION DEFINITIONS
*/

/* Algorithm adapted from from GNU etags.
 * Idea by Corny de Souza
 * Cobol tag functions
 * We could look for anything that could be a paragraph name.
 * i.e. anything that starts in column 8 is one word and ends in a full stop.
 */
static void findCobolTags (void)
{
    vString *name = vStringNew ();
    const unsigned char *line;

    while ((line = fileReadLine ()) != NULL)
    {
	const unsigned char *cp = line;
	const unsigned char *dbp = cp + 7;

	/* If eoln, compiler option or comment ignore whole line. */
	if (dbp [-1] == ' '  &&  isalnum ((int) dbp [0]))
	{
	    for (cp = dbp  ;  isalnum ((int) *cp) || *cp == '-'  ;  cp++)
		vStringPut (name, (int) *cp);

	    if (*cp++ == '.')
	    {
		vStringPut (name, (int) *cp);
		makeSimpleTag (name, CobolKinds, K_PARAGRAPH);
	    }
	    vStringTerminate (name);
	    vStringClear (name);
	}
    }
    vStringDelete (name);
}

extern parserDefinition* CobolParser ()
{
    static const char *const extensions [] = { "cob", "COB", NULL };
    parserDefinition* def = parserNew ("Cobol");
    def->kinds      = CobolKinds;
    def->kindCount  = KIND_COUNT (CobolKinds);
    def->extensions = extensions;
    def->parser     = findCobolTags;
    return def;
}

/* vi:set tabstop=8 shiftwidth=4: */
