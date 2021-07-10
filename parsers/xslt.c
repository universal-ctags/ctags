/*
*
*   Copyright (c) 2016, Masatake YAMATO
*   Copyright (c) 2016, Red Hat, K.K.
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for generating tags for xslt 1.0.
*   Reference: https://www.w3.org/1999/11/xslt10.dtd
*
*/

#include "general.h"	/* must always come first */

#include <string.h>

#include "entry.h"
#include "options.h"
#include "parse.h"
#include "read.h"
#include "routines.h"
#include "selectors.h"
#include "xml.h"

typedef enum {
	K_STYLESHEET,
	K_PARAMETER,
	K_MATCHED_TEMPLATE,
	K_NAMED_TEMPLATE,
	K_VARIABLE,
	/* TODO: KEY, DECIMAL-FORMAT, ATTRIBUTE, ATTRIBUTE-SET, ... */
} xsltKind;

typedef enum {
	R_STYLESHEET_IMPORTED,
	R_STYLESHEET_INCLUDED,
} xsltStylesheetRole;
static roleDefinition XsltStylesheetRoles [] = {
	{ true, "imported", "imported" },
	{ true, "included", "included" },
};

typedef enum {
	R_PARAMETER_BOUND,
} xsltParameterRole;
static roleDefinition XsltParameterRoles [] = {
	{ true, "bound", "bound to value" },
};

typedef enum {
	R_MATCHED_TEMPLATE_APPLIED,
} xsltMatchedTemplateRole;
static roleDefinition XsltMatchedTemplateRoles [] = {
	{ true, "applied", "applied" },
};

typedef enum {
	R_NAMED_TEMPLATE_CALLED,
} xsltNamedTemplateRole;
static roleDefinition XsltNamedTemplateRoles [] = {
	{ true, "called", "called" },
};


static kindDefinition XsltKinds [] = {
	{ true,  's', "stylesheet", "stylesheets",
	  .referenceOnly = true, ATTACH_ROLES (XsltStylesheetRoles) },
	{ true,  'p', "parameter", "parameters",
	  .referenceOnly = false, ATTACH_ROLES (XsltParameterRoles) },
	{ true,  'm', "matchedTemplate", "matched template",
	  .referenceOnly = false, ATTACH_ROLES (XsltMatchedTemplateRoles) },
	{ true,  'n', "namedTemplate",   "named template",
	  .referenceOnly = false, ATTACH_ROLES (XsltNamedTemplateRoles) },
	{ true,  'v', "variable", "variables" },
};

static void makeTagRecursivelyWithVersionVerification (xmlNode *node,
						       const char *xpath,
						       const tagXpathRecurSpec *spec,
						       xmlXPathContext *ctx,
						       void *userData);
static void makeTagRecursively (xmlNode *node,
				const char *xpath,
				const tagXpathRecurSpec *spec,
				xmlXPathContext *ctx,
				void *userData);

static void makeTagWithProvidingScope (xmlNode *node,
				       const char *xpath,
				       const tagXpathMakeTagSpec *spec,
				       struct sTagEntryInfo *tag,
				       void *userData);

enum xsltXpathTable {
	TABLE_MAIN,
	TABLE_STYLESHEET,
	TABLE_VERSION_VERIFY,
	TABLE_TEMPLATE,
	TABLE_APPLIED_TEMPLATES,
	TABLE_CALL_TEMPLATE,
	TABLE_PARAM,
	TABLE_VARIABLE,
	TABLE_WITH_PARAM,
	TABLE_CALL_TEMPLATE_INTERNAL,
};

#define xsltXpathTemplateTableEntry(PREFIX)				\
	{ PREFIX "/*[local-name()='template']",				\
	  LXPATH_TABLE_DO_RECUR,					\
	  { .recurSpec = { makeTagRecursively, TABLE_TEMPLATE} }}

#define xsltXpathVariableTableEntry(PREFIX)				\
	{ PREFIX "/*[local-name()='variable']",				\
	  LXPATH_TABLE_DO_RECUR,					\
	  { .recurSpec = { makeTagRecursively, TABLE_VARIABLE} }}

#define xsltXpathParamTableEntry(PREFIX)			\
	{ PREFIX "/*[local-name()='param']",			\
	  LXPATH_TABLE_DO_RECUR,				\
	  { .recurSpec = { makeTagRecursively, TABLE_PARAM} }}

#define xsltXpathWithParamTableEntry(PREFIX)				\
	{ PREFIX "/*[local-name()='with-param']",			\
	  LXPATH_TABLE_DO_RECUR,					\
	  { .recurSpec = { makeTagRecursively, TABLE_WITH_PARAM} }}	\

static tagXpathTable xsltXpathMainTable[] = {
	{ "(/*[local-name()='stylesheet']|/*[local-name()='transform'])",
	  LXPATH_TABLE_DO_RECUR,
	  { .recurSpec = { makeTagRecursivelyWithVersionVerification } }},
};

static tagXpathTable xsltXpathStylesheetTable[] = {
	{ "./*[local-name()='import']/@href",
	  LXPATH_TABLE_DO_MAKE,
	  { .makeTagSpec = {K_STYLESHEET, R_STYLESHEET_IMPORTED} }},
	{ "./*[local-name()='include']/@href",
	  LXPATH_TABLE_DO_MAKE,
	  { .makeTagSpec = {K_STYLESHEET, R_STYLESHEET_INCLUDED} }},

	xsltXpathTemplateTableEntry ("."),
	xsltXpathVariableTableEntry ("."),
	xsltXpathParamTableEntry    ("."),
};

static tagXpathTable xsltXpathTemplateTable[] = {
	{ "./@match",
	  LXPATH_TABLE_DO_MAKE,
	  { .makeTagSpec = {K_MATCHED_TEMPLATE, ROLE_DEFINITION_INDEX,
			    makeTagWithProvidingScope } }},
	{ "./@name",
	  LXPATH_TABLE_DO_MAKE,
	  { .makeTagSpec = {K_NAMED_TEMPLATE, ROLE_DEFINITION_INDEX,
			    makeTagWithProvidingScope } }},
	{ ".",
	  LXPATH_TABLE_DO_RECUR,
	  { .recurSpec = { makeTagRecursively, TABLE_CALL_TEMPLATE_INTERNAL } }},
};

static tagXpathTable xsltXpathAppliedTemplatesTable [] = {
	{ "./@select",
	  LXPATH_TABLE_DO_MAKE,
	  { .makeTagSpec = {K_MATCHED_TEMPLATE, R_MATCHED_TEMPLATE_APPLIED,
			    makeTagWithProvidingScope } }},
	xsltXpathWithParamTableEntry("."),
};

static tagXpathTable xsltXpathCallTemplateTable [] = {
	{ "./@name",
	  LXPATH_TABLE_DO_MAKE,
	  { .makeTagSpec = {K_NAMED_TEMPLATE, R_NAMED_TEMPLATE_CALLED,
			    makeTagWithProvidingScope } }},
	xsltXpathWithParamTableEntry("."),
};

static tagXpathTable xsltXpathParamTable [] = {
	{ "./@name",
	  LXPATH_TABLE_DO_MAKE,
	  { .makeTagSpec = {K_PARAMETER, ROLE_DEFINITION_INDEX,
			    makeTagWithProvidingScope} }},
	xsltXpathTemplateTableEntry ("."),
};

static tagXpathTable xsltXpathVariableTable [] = {
	{ "./@name",
	  LXPATH_TABLE_DO_MAKE,
	  { .makeTagSpec = {K_VARIABLE, ROLE_DEFINITION_INDEX,
			    makeTagWithProvidingScope} }},
	xsltXpathTemplateTableEntry ("."),
};

static tagXpathTable xsltXpathWithParamTable [] = {
	{ "./@name",
	  LXPATH_TABLE_DO_MAKE,
	  { .makeTagSpec = {K_PARAMETER, R_PARAMETER_BOUND,
			    makeTagWithProvidingScope} }},
	xsltXpathTemplateTableEntry ("."),
};

static tagXpathTable xsltXpathTemplateInternalTable [] = {
	{ "./*[local-name()='apply-templates']",
	  LXPATH_TABLE_DO_RECUR,
	  { .recurSpec = { makeTagRecursively, TABLE_APPLIED_TEMPLATES } }},
	{ "./*[local-name()='call-template']",
	  LXPATH_TABLE_DO_RECUR,
	  { .recurSpec = { makeTagRecursively, TABLE_CALL_TEMPLATE } }},
	xsltXpathVariableTableEntry ("."),
	xsltXpathParamTableEntry ("."),
	{ "./node()[not ("
	  "local-name()='apply-templates'"
	  " or "
	  "local-name()='call-template'"
	  " or "
	  "local-name()='variable'"
	  " or "
	  "local-name()='param'"
	  ")]",
	  LXPATH_TABLE_DO_RECUR,
	  { .recurSpec = { makeTagRecursively, TABLE_CALL_TEMPLATE_INTERNAL } }},
};

static void verifyVersion (xmlNode *node,
			   const char *xpath CTAGS_ATTR_UNUSED,
			   const tagXpathRecurSpec *spec CTAGS_ATTR_UNUSED,
			   xmlXPathContext *ctx CTAGS_ATTR_UNUSED,
			   void *userData)
{
	bool *acceptable = userData;
	char *version = (char *)xmlNodeGetContent (node);

	if (version)
	{
		if (strcmp (version, "1.0") == 0)
		{
			verbose ("xslt: accept version: %s\n", version);
			*acceptable = true;
		}
		else
			verbose ("xslt: unsupported version: %s\n", version);

		eFree (version);
	}
	else
		verbose ("xslt: version unknown\n");
}

static tagXpathTable xsltXpathVersionVerifyTable [] = {
	{ "./@version",
	  LXPATH_TABLE_DO_RECUR,
	  { .recurSpec = { verifyVersion } }},
};


static tagXpathTableTable xsltXpathTableTable[] = {
	[TABLE_MAIN]              = { ARRAY_AND_SIZE (xsltXpathMainTable) },
	[TABLE_STYLESHEET]        = { ARRAY_AND_SIZE (xsltXpathStylesheetTable) },
	[TABLE_VERSION_VERIFY]    = { ARRAY_AND_SIZE (xsltXpathVersionVerifyTable) },
	[TABLE_TEMPLATE]          = { ARRAY_AND_SIZE (xsltXpathTemplateTable) },
	[TABLE_APPLIED_TEMPLATES] = { ARRAY_AND_SIZE (xsltXpathAppliedTemplatesTable) },
	[TABLE_CALL_TEMPLATE]     = { ARRAY_AND_SIZE (xsltXpathCallTemplateTable) },
	[TABLE_PARAM]             = { ARRAY_AND_SIZE (xsltXpathParamTable) },
	[TABLE_VARIABLE]          = { ARRAY_AND_SIZE (xsltXpathVariableTable) },
	[TABLE_WITH_PARAM]        = { ARRAY_AND_SIZE (xsltXpathWithParamTable) },
	[TABLE_CALL_TEMPLATE_INTERNAL] = { ARRAY_AND_SIZE (xsltXpathTemplateInternalTable) },
};


static void makeTagRecursivelyWithVersionVerification (xmlNode *node,
						       const char *xpath CTAGS_ATTR_UNUSED,
						       const tagXpathRecurSpec *spec CTAGS_ATTR_UNUSED,
						       xmlXPathContext *ctx,
						       void *userData)
{
	bool acceptable = false;
	int backup;

	findXMLTags (ctx, node, TABLE_VERSION_VERIFY, &acceptable);
	if (!acceptable)
		return;

	backup = *(int *)userData;
	findXMLTags (ctx, node, TABLE_STYLESHEET, userData);

	*(int *)userData = backup;
}

static void makeTagRecursively (xmlNode *node,
				const char *xpath CTAGS_ATTR_UNUSED,
				const tagXpathRecurSpec *spec,
				xmlXPathContext *ctx,
				void *userData)
{
	int backup = *(int *)userData;

	findXMLTags (ctx, node, spec->nextTable, userData);

	*(int *)userData = backup;
}

static void makeTagWithProvidingScope (xmlNode *node CTAGS_ATTR_UNUSED,
				       const char *xpath CTAGS_ATTR_UNUSED,
				       const tagXpathMakeTagSpec *spec CTAGS_ATTR_UNUSED,
				       struct sTagEntryInfo *tag,
				       void *userData)
{
	int *index = (int *)userData;

	tag->extensionFields.scopeIndex = *index;
	*index = makeTagEntry (tag);
}

static void
findXsltTags (void)
{
	scheduleRunningBaseparser (RUN_DEFAULT_SUBPARSERS);
}

static void
runXPathEngine(xmlSubparser *s,
			   xmlXPathContext *ctx, xmlNode *root)
{
	int corkIndex = CORK_NIL;

	findXMLTags (ctx, root, TABLE_MAIN, &corkIndex);
}

static xmlSubparser xsltSubparser = {
	.subparser = {
		.direction = SUBPARSER_BI_DIRECTION,
	},
	.runXPathEngine = runXPathEngine,
};

extern parserDefinition*
XsltParser (void)
{
	static const char *const extensions [] = { "xsl", "xslt", NULL };
	parserDefinition* const def = parserNew ("XSLT");
	static parserDependency dependencies [] = {
		[0] = { DEPTYPE_SUBPARSER, "XML", &xsltSubparser },
	};

	def->kindTable         = XsltKinds;
	def->kindCount     = ARRAY_SIZE (XsltKinds);
	def->extensions    = extensions;
	def->parser        = findXsltTags;
	def->tagXpathTableTable = xsltXpathTableTable;
	def->tagXpathTableCount = ARRAY_SIZE (xsltXpathTableTable);
	def->useCork = CORK_QUEUE;
	def->dependencies = dependencies;
	def->dependencyCount = ARRAY_SIZE (dependencies);

	return def;
}
