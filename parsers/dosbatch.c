/*
*   Copyright (c) 2009, David Fishburn
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2.
*
*   This module contains functions for generating tags for DOS Batch language files.
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

static void installDosBatchRegex (const langType language)
{
	addTagRegex (language,
		"^:([A-Za-z_0-9]+)", "\\1", "l,label,labels", NULL);
	addTagRegex (language,
		"set[ \t]+([A-Za-z_0-9]+)[ \t]*=", "\\1", "v,variable,variables", NULL);
}

extern parserDefinition* DosBatchParser (void)
{
	static const char *const extensions [] = { "bat", "cmd", NULL };
	parserDefinition* const def = parserNew ("DosBatch");
	def->extensions = extensions;
	def->initialize = installDosBatchRegex;
	def->method     = METHOD_NOT_CRAFTED|METHOD_REGEX;
	return def;
}

/* vi:set tabstop=4 shiftwidth=4: */
