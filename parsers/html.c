/*
*   Copyright (c) 2003, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for generating tags for HTML language
*   files.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */
#include "parse.h"
#include "routines.h"

static tagRegexTable htmlTagRegexTable [] = {
#define POSSIBLE_ATTRIBUTES "([ \t]+[a-z]+=\"?[^>\"]*\"?)*"
	{"<a"
	 POSSIBLE_ATTRIBUTES "[ \t]+name=\"?([^>\"]+)\"?" POSSIBLE_ATTRIBUTES
	 "[ \t]*>", "\\2",
	 "a,anchor,named anchors", "i"},
	{"^[ \t]*function[ \t]*([A-Za-z0-9_]+)[ \t]*\\(", "\\1",
	 "f,function,JavaScript functions", NULL},
};

/*
*   FUNCTION DEFINITIONS
*/

/* Create parser definition structure */
extern parserDefinition* HtmlParser (void)
{
	static const char *const extensions [] = { "htm", "html", NULL };
	parserDefinition *const def = parserNew ("HTML");
	def->extensions = extensions;
	def->tagRegexTable = htmlTagRegexTable;
	def->tagRegexCount = ARRAY_SIZE (htmlTagRegexTable);
	def->method     = METHOD_NOT_CRAFTED|METHOD_REGEX;
	return def;
}
