/*
*   Copyright (c) 2003-2004, Ascher Stefan <stievie@utanet.at>
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for generating tags for R language files.
*   R is a programming language for statistical computing.
*   R is GPL Software, get it from http://www.r-project.org/
*/

/*
*   INCLUDE FILES
*/
#include "general.h"	/* must always come first */

#include <string.h>
#include <ctype.h>	/* to define isalpha(), isalnum(), isspace() */

#include "debug.h"
#include "entry.h"
#include "read.h"
#include "selectors.h"
#include "vstring.h"

#define SKIPSPACE(ch) while (isspace((int)*ch)) \
  ch++

typedef enum {
	K_FUNCTION,
	K_LIBRARY,
	K_SOURCE,
	K_GLOBALVAR,
	K_FUNCVAR,
	KIND_COUNT
} rKind;

static kindOption RKinds[KIND_COUNT] = {
	{TRUE, 'f', "function", "functions"},
	{TRUE, 'l', "library", "libraries"},
	{TRUE, 's', "source", "sources"},
	{TRUE, 'g', "globalVar", "global variables"},
	{TRUE, 'v', "functionVar", "function variables"},
};

static void makeRTag (const vString * const name, rKind kind)
{
	tagEntryInfo e;

	Assert (kind < KIND_COUNT);

	initTagEntry (&e, vStringValue (name), &(RKinds[kind]));

	makeTagEntry (&e);
}

static void createRTags (void)
{
	vString *vLine = vStringNew ();
	vString *name = vStringNew ();
	int ikind;
	const unsigned char *line;

	while ((line = readLineFromInputFile ()) != NULL)
	{
		const unsigned char *cp = (const unsigned char *) line;

		vStringClear (name);
		while ((*cp != '\0') && (*cp != '#'))
		{
			/* iterate to the end of line or to a comment */
			ikind = -1;
			switch (*cp)
			{
			case 'l':
			case 's':
				if (strncasecmp ((const char *) cp, "library", (size_t) 7) == 0)
				{
					/* load a library: library(tools) */
					cp += 7;
					SKIPSPACE (cp);
					if (*cp == '(')
						ikind = K_LIBRARY;
					else
						cp -= 7;
				}
				else if (strncasecmp ((const char *) cp, "source",
						(size_t) 6) == 0)
				{
					/* load a source file: source("myfile.r") */
					cp += 6;
					SKIPSPACE (cp);
					if (*cp == '(')
						ikind = K_SOURCE;
					else
						cp -= 6;
				}
				if (ikind != -1)
				{
					cp++;

					vStringClear (name);
					while ((!isspace ((int) *cp)) && *cp != '\0' && *cp != ')')
					{
						vStringPut (name, (int) *cp);
						cp++;
					}
					vStringTerminate (name);

					/* if the string really exists, make a tag of it */
					if (vStringLength (name) > 0)
						makeRTag (name, ikind);

					/* prepare for the next iteration */
					vStringClear (name);
				}
				else
				{
					vStringPut (name, (int) *cp);
					cp++;
				}
				break;
			case '<':
				cp++;
				if (*cp == '-')
				{
					/* assignment: ident <- someval */
					cp++;
					SKIPSPACE (cp);

					if (*cp == '\0')
					{
						/* not in this line, read next */
						/* sometimes functions are declared this way:
						 * ident <-
						 * function(...)
						 * {
						 * ...
						 * }
						 * I don't know if there is a reason to write the function keyword
						 * in a new line
						 */
						if ((line = readLineFromInputFile ()) != NULL)
						{
							cp = (const unsigned char *) line;
							SKIPSPACE (cp);
						}
						else
							break;
					}

					if (strncasecmp ((const char *) cp, "function",
							(size_t) 8) == 0)
					{
						/* it's a function: ident <- function(args) */
						cp += 8;
						vStringTerminate (name);
						/* if the string really exists, make a tag of it */
						if (vStringLength (name) > 0)
							makeRTag (name, K_FUNCTION);

						/* prepare for the next iteration */
						vStringClear (name);
						break;
					}
					else
					{
						/* it's a variable: ident <- value */
						vStringTerminate (name);
						/* if the string really exists, make a tag of it */
						if (vStringLength (name) > 0)
						{
							if (line[0] == ' ' || line[0] == '\t')
								makeRTag (name, K_FUNCVAR);
							else
								makeRTag (name, K_GLOBALVAR);
						}

						/* prepare for the next iteration */
						vStringClear (name);
						break;
					}
				}
			case ' ':
			case '\x009':
				/* skip whitespace */
				cp++;
				break;
			default:
				/* collect all characters that could be a part of an identifier */
				vStringPut (name, (int) *cp);
				cp++;
				break;
			}
		}
	}

	vStringDelete (name);
	vStringDelete (vLine);
}

extern parserDefinition *RParser (void)
{
	/* *.r;*.R: R files
	 * *.s;*.q: S files
	 */
	static const char *const extensions[] = { "r", "R", "s", "q", NULL };
	parserDefinition *const def = parserNew ("R");
	static selectLanguage selectors[] = { selectByArrowOfR,
					      NULL };
	def->extensions = extensions;
	def->kinds = RKinds;
	def->kindCount = KIND_COUNT;
	def->parser = createRTags;
	def->selectLanguage = selectors;
	return def;
}
