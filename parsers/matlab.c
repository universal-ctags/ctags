/*
*   Copyright (c) 2008, David Fishburn
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for generating tags for MATLAB language files.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include <string.h>
#include "parse.h"
#include "routines.h"
#include "selectors.h"

static tagRegexTable matlabTagRegexTable [] = {
	/* function [x,y,z] = asdf */
	{ "^[ \t]*function[ \t]*\\[.*\\][ \t]*=[ \t]*([a-zA-Z0-9_]+)",
	  "\\1", "f,function", NULL},
	/* function x = asdf */
	{"^[ \t]*function[ \t]*[a-zA-Z0-9_]+[ \t]*=[ \t]*([a-zA-Z0-9_]+)",
	 "\\1", "f,function", NULL},
	/* function asdf */
	{"^[ \t]*function[ \t]*([a-zA-Z0-9_]+)[^=]*$", "\\1",
	 "f,function", NULL},
	/* variables */
	{"^[ \t]*([a-zA-Z0-9_]+)[ \t]*=[ \t]", "\\1",
	 "v,variable", NULL},
	/* class definitions */
	{"^[ \t]*classdef[ \t]*([a-zA-Z0-9_]+)", "\\1",
	 "c,class", NULL},
};

/*
*   FUNCTION DEFINITIONS
*/
extern parserDefinition* MatLabParser (void)
{
	static const char *const extensions [] = { "m", NULL };
	static selectLanguage selectors [] = { selectByObjectiveCAndMatLabKeywords,
					       NULL };
	parserDefinition* const def = parserNew ("MatLab");
	def->extensions = extensions;
	def->tagRegexTable = matlabTagRegexTable;
	def->tagRegexCount = ARRAY_SIZE (matlabTagRegexTable);
	def->method     = METHOD_NOT_CRAFTED|METHOD_REGEX;
	def->selectLanguage = selectors;
	return def;
}
