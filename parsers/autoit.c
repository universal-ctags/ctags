/*
*   Copyright (c) 2017, S
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for generating tags for AutoIt functions.
*   Homepage https://www.autoitscript.com/site/autoit/
*   Online Documentation https://www.autoitscript.com/autoit3/docs/
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include <string.h>

#include "parse.h"
#include "read.h"
#include "routines.h"
#include "vstring.h"

/*
*   DATA DEFINITIONS
*/
typedef enum {
	K_FUNCTION
} AutoItKind;

static kindDefinition AutoItKinds[] = {
	{ true, 'f', "func", "functions" }
};

/*
*   FUNCTION DEFINITIONS
*/
static void findAutoItTags (void)
{
	vString *name = vStringNew ();
	const unsigned char *line;

	while ((line = readLineFromInputFile ()) != NULL)
	{
		const unsigned char* cp = line;
		/* Min. string "func a()" == 8 */
		if (strlen ((const char *) line) >= 8 &&
			(line[0] == 'F' || line[0] == 'f') &&
			(line[1] == 'U' || line[1] == 'u') &&
			(line[2] == 'N' || line[2] == 'n') &&
			(line[3] == 'C' || line[3] == 'c') &&
			isspace ((int) line[4]))
		{
			cp += 4;
			while (isspace ((int) *cp))
				++cp;
			while (isalnum ((int) *cp) || *cp == '_')
			{
				vStringPut (name, (int) *cp);
				++cp;
			}
			while (isspace ((int) *cp))
				++cp;
			if (*cp == '(')
				makeSimpleTag (name, K_FUNCTION);
			vStringClear (name);
		}
	}
	vStringDelete (name);
}

parserDefinition *AutoItParser (void)
{
	static char const *extensions[] = { "au3", "AU3", NULL };
	parserDefinition* def = parserNew ("AutoIt");
	def->kindTable      = AutoItKinds;
	def->kindCount  = ARRAY_SIZE (AutoItKinds);
	def->extensions = extensions;
	def->parser     = findAutoItTags;
	return def;
}
