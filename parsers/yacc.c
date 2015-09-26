/*
*   Copyright (c) 2001-2002, Nick Hibma <n_hibma@van-laarhoven.org>
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for generating tags for YACC language files.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include <string.h>
#include "parse.h"

/*
*   FUNCTION DEFINITIONS
*/

static void installYaccRegex (const langType language)
{
	addTagRegex (language,
		"^([A-Za-z][A-Za-z_0-9]+)[ \t]*:", "\\1", "l,label,labels", NULL);
}

extern parserDefinition* YaccParser (void)
{
	static const char *const extensions [] = { "y", NULL };
	parserDefinition* const def = parserNew ("YACC");
	def->extensions = extensions;
	def->initialize = installYaccRegex;
	def->method     = METHOD_NOT_CRAFTED|METHOD_REGEX;
	return def;
}

/* vi:set tabstop=4 shiftwidth=4: */
