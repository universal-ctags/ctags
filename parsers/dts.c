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
#include "lcpp.h"
#include "kind.h"
#include "parse.h"
#include "routines.h"

typedef enum {
	DTS_MACRO_KIND_UNDEF_ROLE,
} dtsMacroRole;

static roleDesc DTSMacroRoles [] = {
	RoleTemplateUndef,
};


typedef enum {
	DTS_HEADER_KIND_SYSTEM_ROLE,
	DTS_HEADER_KIND_LOCAL_ROLE,
} dtsHeaderRole;

static roleDesc DTSHeaderRoles [] = {
	RoleTemplateSystem,
	RoleTemplateLocal,
};


typedef enum {
	DTS_MACRO, DTS_HEADER,
} dtsKind;

static kindOption DTSKinds [] = {
	{ TRUE,  'd', "macro",      "macro definitions",
	  .referenceOnly = FALSE, ATTACH_ROLES(DTSMacroRoles)},
	{ TRUE, 'h', "header",     "included header files",
	  .referenceOnly = TRUE, ATTACH_ROLES(DTSHeaderRoles)},
};

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
	cppInit (0, FALSE, FALSE, FALSE,
		 DTSKinds + DTS_MACRO, DTS_MACRO_KIND_UNDEF_ROLE,
		 DTSKinds + DTS_HEADER, DTS_HEADER_KIND_SYSTEM_ROLE, DTS_HEADER_KIND_LOCAL_ROLE);

	findRegexTagsMainloop (cppGetc);

	cppTerminate ();
}

extern parserDefinition* DTSParser (void)
{
	static const char *const extensions [] = { "dts", "dtsi", NULL };
	parserDefinition* const def = parserNew ("DTS");
	def->kinds      = DTSKinds;
	def->kindCount  = ARRAY_SIZE (DTSKinds);
	def->extensions = extensions;
	def->parser     = runCppGetc;
	def->tagRegexTable = dtsTagRegexTable;
	def->tagRegexCount = ARRAY_SIZE (dtsTagRegexTable);
	def->method     = METHOD_REGEX;
	def->requestAutomaticFQTag = TRUE;
	return def;
}
