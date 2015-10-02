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

/*
*   FUNCTION DEFINITIONS
*/

typedef enum {
	DTS_MACRO, DTS_HEADER,
} dtsKind;

static kindOption DTSKinds [] = {
	{ TRUE,  'd', "macro",      "macro definitions"},
	{ FALSE, 'h', "header",     "included header files"},
};

static void installDTSRegex (const langType language)
{
	/* phandle = <0x00> */
	addTagRegex (language,
		     "^[ \t]*phandle[ \t]+=[ \t]+<(0x[a-fA-F0-9]+)>", "\\1",
		     "p,phandler,phandlers",
		     "{scope=ref}");

	/* label: */
	addTagRegex (language,
		     "^[ \t]*([a-zA-Z][a-zA-Z0-9_]*)[ \t]*:", "\\1",
		     "l,label,labels",
		     "{scope=push}{exclusive}");

	/* extras for tracking scopes  */
	addTagRegex (language,
		     "^[ \t]*([a-zA-Z][a-zA-Z0-9_]*)[ \t]*\\{", "",
		     "",
		     "{scope=push}{placeholder}{exclusive}");
	addTagRegex (language,
		     "^[ \t]*\\};", "",
		     "",
		     "{scope=pop}{exclusive}");
}

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
	def->kindCount  = KIND_COUNT (DTSKinds);
	def->extensions = extensions;
	def->parser     = runCppGetc;
	def->initialize = installDTSRegex;
	def->method     = METHOD_REGEX;
	return def;
}

/* vi:set tabstop=4 shiftwidth=4: */
