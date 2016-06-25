/*
 *   Copyright (c) 2000-2001, Francesc Rocher
 *
 *   Author: Francesc Rocher <f.rocher@computer.org>.
 *
 *   This source code is released for free distribution under the terms of the
 *   GNU General Public License version 2 or (at your option) any later version.
 *
 *   This module contains functions for generating tags for S-Lang files.
 */

/*
 *   INCLUDE FILES
 */
#include "general.h"  /* must always come first */
#include "parse.h"
#include "routines.h"

static tagRegexTable slangTagRegexTable [] = {
	{"^.*define[ \t]+([A-Z_][A-Z0-9_]*)[^;]*$", "\\1",
	 "f,function,functions", "i"},
	{"^[ \t]*implements[ \t]+\\([ \t]*\"([^\"]*)\"[ \t]*\\)[ \t]*;", "\\1",
	 "n,namespace,namespaces", NULL},
};

/*
 *   FUNCTION DEFINITIONS
 */

extern parserDefinition* SlangParser (void)
{
	static const char *const extensions [] = { "sl", NULL };
	parserDefinition* const def = parserNew ("SLang");
	def->extensions = extensions;
	def->tagRegexTable = slangTagRegexTable;
	def->tagRegexCount = ARRAY_SIZE (slangTagRegexTable);
	def->method     = METHOD_NOT_CRAFTED|METHOD_REGEX;
	return def;
}
