/*
 *   Copyright (c) 2016, Ori Bernstein
 *
 *   This source code is released for free distribution under the terms of the
 *   GNU General Public License version 2 or (at your option) any later version.
 *
 *   This module contains functions for generating tags for Myrddin language
 *   files. (https://myrlang.org)
 */

/*
 *   INCLUDE FILES
 */
#include "general.h"  /* must always come first */
#include "parse.h"

static tagRegexTable myrddinTagRegexTable [] = {
	/* const foo = {; function literal } */
	{"^([ \t]*extern)?[ \t]*const[ \t]+([a-zA-Z_][a-zA-Z0-9_]*)[ \t]*=[ \t]\\{",
		"\\2", "f,function,functions", NULL},
	/* const foo = initializer */
	{"^([ \t]*extern)?[ \t]*const[ \t]+([a-zA-Z_][a-zA-Z0-9_]*)[ \t]*(=[ \t][^{].*)?$",
		"\\2", "c,constant,constants", NULL},
	/* var foo = initializer */
	{"^([ \t]*extern)?[ \t]*var[ \t]+([a-zA-Z_][a-zA-Z0-9_]*)",
		"\\2", "v,var,variables", NULL},
	/* type foo = name */
	{"^[ \t]*type[ \t]+([a-zA-Z_][a-zA-Z0-9_]*)[ \t]*=",
		"\\1", "t,type,types", NULL},
	/* trait foo = trait-defn ;; */
	{"^[ \t]*trait[ \t]+([a-zA-Z_][a-zA-Z0-9_]*)[ \t]*=",
		"\\1", "r,trait,traits", NULL},
	/* pkg foo = declarations ;; */
	{"^[ \t]*pkg[ \t]+([a-zA-Z_][a-zA-Z0-9_]*)",
		"\\1", "p,pkg,packages", NULL},
};

/*
 *   FUNCTION DEFINITIONS
 */

extern parserDefinition *MyrddinParser (void)
{
	static const char *const extensions [] = { "myr", NULL };
	parserDefinition* const def = parserNew ("Myrddin");
	def->extensions = extensions;
	def->tagRegexTable = myrddinTagRegexTable;
	def->tagRegexCount = ARRAY_SIZE (myrddinTagRegexTable);
	def->method     = METHOD_NOT_CRAFTED|METHOD_REGEX;
	return def;
}
