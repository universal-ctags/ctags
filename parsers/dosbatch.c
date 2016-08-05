/*
*   Copyright (c) 2009, David Fishburn
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for generating tags for DOS Batch language files.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include <string.h>
#include "parse.h"
#include "routines.h"
#include "selectors.h"

static tagRegexTable dosTagRegexTable [] = {
	{"^:([A-Za-z_0-9]+)", "\\1",
	 "l,label,labels", NULL},
	{"set[ \t]+([A-Za-z_0-9]+)[ \t]*=", "\\1",
	 "v,variable,variables", NULL},
};

/*
*   FUNCTION DEFINITIONS
*/

extern parserDefinition* DosBatchParser (void)
{
	static const char *const extensions [] = { "bat", "cmd", NULL };
	parserDefinition* const def = parserNew ("DosBatch");
	static selectLanguage selectors[] = { selectByRexxCommentAndDosbatchLabelPrefix,
					      NULL };

	def->extensions = extensions;
	def->tagRegexTable = dosTagRegexTable;
	def->tagRegexCount = ARRAY_SIZE (dosTagRegexTable);
	def->method     = METHOD_NOT_CRAFTED|METHOD_REGEX;
	def->selectLanguage = selectors;
	return def;
}
