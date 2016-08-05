/*
*   Copyright (c) 2000-2003, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for generating tags for COBOL language
*   files.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"	/* must always come first */
#include "parse.h"
#include "routines.h"

static tagRegexTable cobolTagRegexTable[] = {
	{"^[ \t]*[0-9]+[ \t]+([A-Z0-9][A-Z0-9-]*)[ \t]+("
	 "BLANK|OCCURS|IS|JUST|PIC|REDEFINES|RENAMES|SIGN|SYNC|USAGE|VALUE"
	 ")", "\\1",
	 "d,data,data items", "i"},
	{"^[ \t]*[FSR]D[ \t]+([A-Z0-9][A-Z0-9-]*)\\.", "\\1",
	 "f,file,file descriptions (FD, SD, RD)", "i"},
	{"^[ \t]*[0-9]+[ \t]+([A-Z0-9][A-Z0-9-]*)\\.", "\\1",
	 "g,group,group items", "i"},
	{"^[ \t]*([A-Z0-9][A-Z0-9-]*)\\.", "\\1",
	 "p,paragraph,paragraphs", "i"},
	{"^[ \t]*PROGRAM-ID\\.[ \t]+([A-Z0-9][A-Z0-9-]*)\\.", "\\1",
	 "P,program,program ids", "i"},
	{"^[ \t]*([A-Z0-9][A-Z0-9-]*)[ \t]+SECTION\\.", "\\1",
	 "s,section,sections", "i"},
};

/*
*   FUNCTION DEFINITIONS
*/

extern parserDefinition* CobolParser (void)
{
	static const char *const extensions [] = {
			"cbl", "cob", "CBL", "COB", NULL };
	parserDefinition* def = parserNew ("Cobol");
	def->extensions = extensions;
	def->tagRegexTable = cobolTagRegexTable;
	def->tagRegexCount = ARRAY_SIZE (cobolTagRegexTable);
	def->method     = METHOD_NOT_CRAFTED|METHOD_REGEX;
	return def;
}
