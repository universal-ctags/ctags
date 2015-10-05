/*
*   Copyright (c) 2015, André Rivotti Casimiro <andre.r.casimiro@gmail.com>
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for generating tags for YACC language files.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"
#include "get.h"
#include "parse.h"

typedef enum {
	DTS_MACRO, DTS_HEADER,
} dtsKind;

static kindOption DTSKinds [] = {
	{ TRUE,  'd', "macro",      "macro definitions"},
	{ FALSE, 'h', "header",     "included header files"},
};

static const tagRegexTable const dtsTagRegexTable [] = {
	/* phandle = <0x00> */
	{"^[ \t]*phandle[ \t]+=[ \t]+<(0x[a-fA-F0-9]+)>", "\\1",
	 "p,phandler,phandlers", "{scope=ref}"},

	/* label: */
	{"^[ \t]*([a-zA-Z][a-zA-Z0-9_]*)[ \t]*:", "\\1",
	 "l,label,labels", "{scope=push}"},

	/* extras for tracking scopes  */
	{"^[ \t]*([a-zA-Z][a-zA-Z0-9_]*)[ \t]*\\{", "",
	 "", "{scope=push}{placeholder}"},
	{"\\};", "",
	 "", "{scope=pop}{exclusive}"},
};

/*
*   FUNCTION DEFINITIONS
*/
static void runCppGetc (void)
{
	cppInit (0, FALSE, FALSE,
		 DTSKinds + DTS_MACRO,
		 DTSKinds + DTS_HEADER);

	findRegexTagsMainloop (cppGetc);

	cppTerminate ();
}

extern parserDefinition* DTSParser (void)
{
	static const char *const extensions [] = { "dts", "dtsi", NULL };
	parserDefinition* const def = parserNew ("DTS");
	def->kinds      = DTSKinds;
	def->kindCount  = COUNT_ARRAY (DTSKinds);
	def->extensions = extensions;
	def->parser     = runCppGetc;
	def->tagRegexTable = dtsTagRegexTable;
	def->tagRegexCount = COUNT_ARRAY (dtsTagRegexTable);
	def->method     = METHOD_REGEX;
	return def;
}

/* vi:set tabstop=4 shiftwidth=4: */
