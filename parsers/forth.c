/*
*   Copyright (c) 2023, Eric Forgeot
*
*   Based on work by Jon Strait
*   from
*   https://sourceforge.net/p/geany/git/ci/master/tree/ctags/parsers/markdown.c
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your opinion) any later version.
*
*   This module contains functions for generating tags for Forth files
*   https://www.forth.com/starting-forth/1-forth-stacks-dictionary/
*/

/*
*   INCLUDE FILES
*/
#include "general.h"	/* must always come first */

#include <ctype.h>
#include <string.h>

#include "parse.h"
#include "read.h"
#include "vstring.h"
#include "routines.h"
#include "entry.h"

/*
*   DATA DEFINITIONS
*/

typedef enum {
//   K_SECTION,
   K_FUNCTION,
} ForthKind;

static kindDefinition ForthKinds[] = {
	{ true, 's', "word", "words" },
};

/*
*   FUNCTION DEFINITIONS
*/

static void findForthTags (void)
{
	vString *name = vStringNew();
	const unsigned char *line;

	while ((line = readLineFromInputFile()) != NULL)
	{
		if (line[0] == ':') {
			vStringCatS(name, (const char *) line);
//			makeSimpleTag(name, K_SECTION);
			makeSimpleTag(name, K_FUNCTION);
			vStringClear (name);
		}
	}
	vStringDelete (name);
}

extern parserDefinition* ForthParser (void)
{
	static const char *const patterns [] = { "*.fth", NULL };
	static const char *const extensions [] = { "forth", NULL };
	parserDefinition* const def = parserNew ("Forth");

	def->kindTable = ForthKinds;
	def->kindCount = ARRAY_SIZE (ForthKinds);
	def->patterns = patterns;
	def->extensions = extensions;
	def->parser = findForthTags;
	return def;
}

