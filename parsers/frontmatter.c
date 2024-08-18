/*
*
*   Copyright (c) 2022, Masatake YAMATO
*   Copyright (c) 2022, Red Hat, K.K.
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
* This module contains functions for extracting language objects in FrontMatter.
*
* https://gohugo.io/content-management/front-matter
*/

/*
*   INCLUDE FILES
*/
#include "general.h"	/* must always come first */

#include "x-frontmatter.h"

#include "entry.h"
#include "parse.h"
#include "promise.h"
#include "read.h"

#include <string.h>

/*
*   DATA DEFINITIONS
*/
static kindDefinition FrontMatterKinds [] = {
	{ true, 't', "title", "titles", },
};

/*
*   FUNCTION DEFINITIONS
*/
static void findFrontMatterTags (void)
{
	const unsigned char *line = readLineFromInputFile ();

	if (line == NULL)
		return;

#ifdef HAVE_LIBYAML
	if (strcmp("---", (const char *)line) == 0)
	{
		line = readLineFromInputFile ();
		if (line)
		{
			unsigned long endOffset = strlen((const char *)line);
			long startLineNum = getInputLineNumber ();
			while ((line = readLineFromInputFile()))
				endOffset = strlen((const char *)line);

			long endLineNum = getInputLineNumber ();

			makePromise ("YamlFrontMatter", startLineNum, 0,
						 endLineNum, endOffset, startLineNum);
		}
		return;
	}
#endif
}

extern parserDefinition* FrontMatterParser (void)
{
	parserDefinition* def = parserNew ("FrontMatter");
	def->kindTable      = FrontMatterKinds;
	def->kindCount  = ARRAY_SIZE (FrontMatterKinds);

	def->parser     = findFrontMatterTags;

	return def;
}
