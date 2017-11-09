/*
*   Copyright (c) 2017, Alexey Olenich
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for generating tags for AutoIt functions.
*   Homepage             https://www.autoitscript.com/site/autoit/
*   Online Documentation https://www.autoitscript.com/autoit3/docs/
*
*   Functions: (?i)^func[ \t]{1,}([a-z0-9_]{1,}) >> \1
*   Regions:   (?i)^#region[ \t]*(.*?)           >> \1
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
	K_FUNCTION,
	K_REGION
} AutoItKind;

static kindDefinition AutoItKinds [] = {
	{ true, 'f', "func", "functions" },
	{ true, 'r', "region", "regions" }
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
		const unsigned char* p = line;
		if (p [0] == '#')
		{
			/* min. string "#region" > 7 */
			if ((p [1] == 'R' || p [1] == 'r') &&
				strlen ((const char *) p) > 8 &&
				(p [2] == 'E' || p [2] == 'e') &&
				(p [3] == 'G' || p [3] == 'g') &&
				(p [4] == 'I' || p [4] == 'i') &&
				(p [5] == 'O' || p [5] == 'o') &&
				(p [6] == 'N' || p [6] == 'n'))
			{
				p += 7;
				while (isspace ((int) *p))
					++p;
				while (*p != '\0')
				{
					vStringPut (name, (int) *p);
					++p;
				}

				if (vStringLength(name) > 0)
				{
					makeSimpleTag (name, K_REGION);
					vStringClear (name);
				}
			}
		}
		else
		{
			/* skip white space */
			while (isspace ((int) *p))
				++p;
			/* min. string "func a()" == 8 */
			if ((p [0] == 'F' || p [0] == 'f') &&
				strlen ((const char *) p) >= 8 &&
				(p [1] == 'U' || p [1] == 'u') &&
				(p [2] == 'N' || p [2] == 'n') &&
				(p [3] == 'C' || p [3] == 'c') &&
				isspace ((int) p [4]))
			{
				p += 5;
				while (isspace ((int) *p))
					++p;
				while (isalnum ((int) *p) || *p == '_')
				{
					vStringPut (name, (int) *p);
					++p;
				}
				while (isspace ((int) *p))
					++p;
				if (*p == '(' && (vStringLength(name) > 0))
				{
					makeSimpleTag (name, K_FUNCTION);
					vStringClear (name);
				}
			}
		}
	}
	vStringDelete (name);
}

parserDefinition *AutoItParser (void)
{
	static char const *extensions[] = { "au3", "AU3", "aU3", "Au3", NULL };
	parserDefinition* def = parserNew ("AutoIt");
	def->kindTable      = AutoItKinds;
	def->kindCount  = ARRAY_SIZE (AutoItKinds);
	def->extensions = extensions;
	def->parser     = findAutoItTags;
	return def;
}
