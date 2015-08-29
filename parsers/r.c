/*
*   Copyright (c) 2003-2004, Ascher Stefan <stievie@utanet.at>
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
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
#include "vstring.h"

#define R_REGEX

#define SKIPSPACE(ch) while (isspace((int)*ch)) \
  ch++

#ifndef R_REGEX
typedef enum {
	K_FUNCTION,
	K_LIBRARY,
	K_SOURCE,
	KIND_COUNT
} rKind;

static kindOption RKinds[KIND_COUNT] = {
	{TRUE, 'f', "function", "functions"},
	{TRUE, 's', "other", "libraries"},
	{TRUE, 's', "other", "sources"},
};
#endif

#ifdef R_REGEX
static void installRRegex (const langType language)
{
	/* This is a function, looks as follows:
	 * itent <- function(arg1, arg2) {
	 *   do_something;
	 * }
	 */
	addTagRegex (language,
		"^[ \t]*\"?([.a-zA-Z][.a-zA-Z0-9_]+)\"?[ \t]*<-[ \t]*function[ \t]*\\(", "\\1",
		"f,function,functions", NULL);
	/* Global variables */
	addTagRegex (language,
		"^\"?([.a-zA-Z][.a-zA-Z0-9_]*)\"?[ \t]*<-[ \t][^\(]+$", "\\1",
		"g,globalVar,global variables", NULL);
	/* Function local variable
	 * Assumes that code in functions is indented
	 */
	addTagRegex (language,
		"[ \t]\"?([.A-Za-z][.A-Za-z0-9_]*)\"?[ \t]*<-[ \t][^\(]+$", "\\1",
		"v,functionVar,function variables", NULL);
	/* This loads someting, e.g. a library, simply: library(libname) */
	addTagRegex (language,
		"^[ \t]*(library|source|load|data)[\\(]([a-zA-Z0-9_]+)[\\)]", "\\2",
		"s,other,library/source/load/data", NULL);
}
#else
static void makeRTag (const vString * const name, rKind kind)
{
	tagEntryInfo e;
	initTagEntry (&e, vStringValue (name));

	Assert (kind < KIND_COUNT);

	e.kindName = RKinds[kind].name;
	e.kind = RKinds[kind].letter;

	makeTagEntry (&e);
}

static void createRTags (void)
{
	vString *vLine = vStringNew ();
	vString *name = vStringNew ();
	int ikind;
	const unsigned char *line;

	while ((line = fileReadLine ()) != NULL)
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
						if ((line = fileReadLine ()) != NULL)
						{
							cp = (const unsigned char *) line;
							SKIPSPACE (cp);
						}
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
#endif

extern parserDefinition *RParser (void)
{
	/* *.r: R files
	 * *.s;*.q: S files
	 */
	static const char *const extensions[] = { "r", "s", "q", NULL };
	parserDefinition *const def = parserNew ("R");
#ifndef R_REGEX
	def->kinds = RKinds;
	def->kindCount = 4;
#endif
	def->extensions = extensions;
#ifndef R_REGEX
	def->parser = createRTags;
#else
	def->initialize = installRRegex;
	def->method = METHOD_NOT_CRAFTED | METHOD_REGEX;
#endif
	return def;
}

/* vi:set tabstop=4 shiftwidth=4 noexpandtab: */
