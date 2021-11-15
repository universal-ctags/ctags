/*
*   Copyright (c) 2015, André Rivotti Casimiro <andre.r.casimiro@gmail.com>
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for generating tags for DTS language files.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"
#include "cpreprocessor.h"
#include "field.h"
#include "kind.h"
#include "parse.h"
#include "routines.h"

static tagRegexTable dtsTagRegexTable [] = {
	/* phandle = <0x00> */
	{"^[ \t]*phandle[ \t]+=[ \t]+<(0x[a-fA-F0-9]+)>", "\\1",
	 "p,phandler,phandlers", "{scope=ref}"},

	/* label: */
	{"^[ \t]*([a-zA-Z][a-zA-Z0-9_]*)[ \t]*:", "\\1",
	 "l,label,labels", "{scope=push}"},

	/* extras for tracking scopes  */
	{"^[ \t]*([a-zA-Z][a-zA-Z0-9_]*)[ \t]*\\{", "",
	 "", "{scope=push}{placeholder}"},
	{"\\}[ \t]*;", "",
	 "", "{scope=pop}{exclusive}"},
};

/*
*   FUNCTION DEFINITIONS
*/
static void runCppGetc (void)
{
	cppInit (false, false, false, false,
			 KIND_GHOST_INDEX, 0, 0,
			 KIND_GHOST_INDEX,
			 KIND_GHOST_INDEX, 0, 0,
			 FIELD_UNKNOWN);

	findRegexTagsMainloop (cppGetc);

	cppTerminate ();
}

extern parserDefinition* DTSParser (void)
{
	static const char *const extensions [] = { "dts", "dtsi", NULL };
	parserDefinition* const def = parserNew ("DTS");
	def->extensions = extensions;
	def->parser     = runCppGetc;
	def->tagRegexTable = dtsTagRegexTable;
	def->tagRegexCount = ARRAY_SIZE (dtsTagRegexTable);
	def->method     = METHOD_REGEX;
	def->requestAutomaticFQTag = true;
	return def;
}
