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
#include "debug.h"
#include "options.h"
#include "parse.h"
#include "read.h"
#include "routines.h"


typedef enum {
	R_CLASS_WIDGET,
} gladeClassRole;

typedef enum {
	R_HANDLER_HANDLER,
} gladeHandleRoler;

static roleDesc GladeClassRoles [] = {
	{ TRUE, "widget", "specifed as a widget constructor" },
};

static roleDesc GladeHandlerRoles [] = {
	{ TRUE, "handler", "specifed as a callback for signal emission" },
};

typedef enum {
	K_ID, K_CLASS, K_HANDLER,
} gladeKind;

static kindOption GladeKinds [] = {
	{ TRUE,  'i', "id",	  "identifiers" },

	/* These two are appeared on names in C source code. */
	{ TRUE,  'c', "class",	  "classes",
	  .referenceOnly = TRUE, ATTACH_ROLES (GladeClassRoles) },
	{ TRUE,  'h', "handler",  "handlers",
	  .referenceOnly = TRUE, ATTACH_ROLES (GladeHandlerRoles) },
};

static tagXpathTable gladeXpathMainTable[] = {
	{ "///glade-interface//widget//@id",
	  LXPATH_TABLE_DO_MAKE,
	  { .makeTagSpec = { K_ID, ROLE_INDEX_DEFINITION } }
	},
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
	findXMLTags (NULL, NULL, gladeXpathTableTable + TABLE_MAIN, GladeKinds, NULL);
}

extern parserDefinition*
GladeParser (void)
{
	static const char *const extensions [] = { "glade", NULL };
	parserDefinition* const def = parserNew ("Glade");

	def->kinds         = GladeKinds;
	def->kindCount     = ARRAY_SIZE (GladeKinds);
	def->extensions    = extensions;
	def->parser        = findGladeTags;
	def->tagXpathTableTable  = gladeXpathTableTable;
	def->tagXpathTableCount  = ARRAY_SIZE (gladeXpathTableTable);
	return def;
}
