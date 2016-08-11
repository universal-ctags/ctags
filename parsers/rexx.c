/*
*   Copyright (c) 2001-2003, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for generating tags for the REXX language
*   (http://www.rexxla.org, http://www2.hursley.ibm.com/rexx).
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* always include first */
#include "parse.h"    /* always include */
#include "routines.h"
#include "selectors.h"

static tagRegexTable rexxTagRegexTable [] = {
	{"^([A-Za-z0-9@#$\\.!?_]+)[ \t]*:", "\\1",
	 "s,subroutine,subroutines", NULL},
};

/*
*   FUNCTION DEFINITIONS
*/

extern parserDefinition* RexxParser (void)
{
	static const char *const extensions [] = { "cmd", "rexx", "rx", NULL };
	parserDefinition* const def = parserNew ("REXX");
	static selectLanguage selectors[] = { selectByRexxCommentAndDosbatchLabelPrefix,
					      NULL };
	def->extensions = extensions;
	def->tagRegexTable = rexxTagRegexTable;
	def->tagRegexCount = ARRAY_SIZE (rexxTagRegexTable);
	def->method     = METHOD_NOT_CRAFTED|METHOD_REGEX;
	def->selectLanguage = selectors;
	return def;
}
