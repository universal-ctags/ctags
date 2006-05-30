/*
*   $Id$
*
*   Copyright (c) 2003, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   This module contains functions for generating tags for JavaScript language
*   files.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */
#include "parse.h"

/*
*   FUNCTION DEFINITIONS
*/

static void installJavaScriptRegex (const langType language)
{
	addTagRegex (language, "^[ \t]*function[ \t]*([A-Za-z0-9_]+)[ \t]*\\(",
		"\\1", "f,function,functions", NULL);
}

/* Create parser definition stucture */
extern parserDefinition* JavaScriptParser (void)
{
	static const char *const extensions [] = { "js", NULL };
	parserDefinition *const def = parserNew ("JavaScript");
	def->extensions = extensions;
	def->initialize = installJavaScriptRegex;
	def->regex      = TRUE;
	return def;
}

/* vi:set tabstop=4 shiftwidth=4: */
