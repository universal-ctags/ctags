/*
*
*   Copyright (c) 2015, Masatake YAMATO
*   Copyright (c) 2015, Red Hat, K.K.
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for generating tags for diff files (based on Sh parser).
*/

#include "general.h"	/* must always come first */
#include "entry.h"
#include "debug.h"
#include "parse.h"
#include "read.h"
#include "routines.h"
#include "xml.h"


typedef enum {
	R_CLASS_WIDGET,
} gladeClassRole;

typedef enum {
	R_HANDLER_HANDLER,
} gladeHandleRoler;

static roleDefinition GladeClassRoles [] = {
	{ true, "widget", "specified as a widget constructor" },
};

static roleDefinition GladeHandlerRoles [] = {
	{ true, "handler", "specified as a callback for signal emission" },
};

typedef enum {
	K_CLASS, K_HANDLER,
} gladeKind;

static kindDefinition GladeKinds [] = {
	/* These two are appeared on names in C source code. */
	{ true,  'c', "class",	  "classes",
	  .referenceOnly = true, ATTACH_ROLES (GladeClassRoles) },
	{ true,  'h', "handler",  "handlers",
	  .referenceOnly = true, ATTACH_ROLES (GladeHandlerRoles) },
};

static tagXpathTable gladeXpathMainTable[] = {
	{ "///glade-interface//widget//@class",
	  LXPATH_TABLE_DO_MAKE,
	  { .makeTagSpec = { K_CLASS, R_CLASS_WIDGET } }
	},
	{ "///glade-interface//signal//@handler",
	  LXPATH_TABLE_DO_MAKE,
	  { .makeTagSpec = { K_HANDLER, R_HANDLER_HANDLER }}
	},
};

enum gladeXpathTable {
	TABLE_MAIN
};

static tagXpathTableTable gladeXpathTableTable[] = {
	[TABLE_MAIN] = { ARRAY_AND_SIZE(gladeXpathMainTable) },
};

static void
findGladeTags (void)
{
	scheduleRunningBaseparser (RUN_DEFAULT_SUBPARSERS);
}


static void
runXPathEngine(xmlSubparser *s,
			   xmlXPathContext *ctx, xmlNode *root)
{
	findXMLTags (ctx, root, TABLE_MAIN, NULL);
}

static xmlSubparser gladeSubparser = {
	.subparser = {
		.direction = SUBPARSER_BI_DIRECTION,
	},
	.runXPathEngine = runXPathEngine,
};

extern parserDefinition*
GladeParser (void)
{
	static const char *const extensions [] = { "glade", NULL };
	parserDefinition* const def = parserNew ("Glade");
	static parserDependency dependencies [] = {
		[0] = { DEPTYPE_SUBPARSER, "XML", &gladeSubparser },
	};

	def->kindTable         = GladeKinds;
	def->kindCount     = ARRAY_SIZE (GladeKinds);
	def->extensions    = extensions;
	def->parser        = findGladeTags;
	def->tagXpathTableTable  = gladeXpathTableTable;
	def->tagXpathTableCount  = ARRAY_SIZE (gladeXpathTableTable);
	def->dependencies = dependencies;
	def->dependencyCount = ARRAY_SIZE (dependencies);
	return def;
}
