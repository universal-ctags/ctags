/*
*
*   Copyright (c) 2016, Masatake YAMATO
*   Copyright (c) 2016, Red Hat, K.K.
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for generating tags for
*   <!DOCTYPE node PUBLIC
*             "-//freedesktop//DTD D-BUS Object Introspection 1.0//EN"
*             "http://www.freedesktop.org/standards/dbus/1.0/introspect.dtd">
*   and
*
*	<!DOCTYPE node PUBLIC
*             "-//freedesktop//DTD D-BUS Introspection 0.1//EN"
*             "http://www.freedesktop.org/software/dbus/introspection.dtd">
**
*/

#include "general.h"	/* must always come first */
#include "entry.h"
#include "parse.h"
#include "read.h"
#include "routines.h"
#include "selectors.h"
#include "xml.h"

#include <string.h>


typedef enum {
	K_ARG, K_INTERFACE, K_METHOD, K_SIGNAL, K_PROPERTY, K_NODE,
} dbusIntrospectKind;

static kindDefinition DbusIntrospectKinds [] = {
	{ true,  'a', "arg",       "arguments"  },
	{ true,  'i', "interface", "interfaces" },
	{ true,  'm', "method",    "methods"    },
	{ true,  's', "signal",    "signals"    },
	{ true,  'p', "property",  "properties" },
	{ true,  'n', "node",      "nodes"      },
};

static void dbusIntrospectFindTagsUnderMain (xmlNode *node,
						  const char *xpath,
						  const struct sTagXpathRecurSpec *spec,
						  xmlXPathContext *ctx,
						  void *userData);
static void makeTagForMainName (xmlNode *node,
				     const char *xpath,
				     const struct sTagXpathMakeTagSpec *spec,
				     struct sTagEntryInfo *tag,
				     void *userData);
static void makeTagWithScope (xmlNode *node,
			      const char *xpath,
			      const struct sTagXpathMakeTagSpec *spec,
			      struct sTagEntryInfo *tag,
			      void *userData);
static int decideKindForMainName (xmlNode *node,
				  const char *xpath,
			      const struct sTagXpathMakeTagSpec *spec,
			      void *userData);

struct dbusIntrospectData {
	int scopeIndex;
	int kindForName;
};

static tagXpathTable dbusIntrospectXpathArgTable [] = {
	{ "arg",
	  LXPATH_TABLE_DO_RECUR,
	  { .recurSpec = { dbusIntrospectFindTagsUnderMain,
					   -1 } },
	},
};

enum dbusIntrospectXpathTable {
	TABLE_ROOT, TABLE_MAIN, TABLE_INTERFACE, TABLE_MAIN_NAME, TABLE_ARG,
};

static tagXpathTable dbusIntrospectXpathInterfaceTable [] = {
	{ "method",
	  LXPATH_TABLE_DO_RECUR,
	  { .recurSpec = { dbusIntrospectFindTagsUnderMain,
					   TABLE_ARG, } }
	},
	{ "signal",
	  LXPATH_TABLE_DO_RECUR,
	  { .recurSpec = { dbusIntrospectFindTagsUnderMain,
					   TABLE_ARG, } }
	},
	{ "property/@name",
	  LXPATH_TABLE_DO_MAKE,
	  { .makeTagSpec = { K_PROPERTY, ROLE_DEFINITION_INDEX,
						 makeTagWithScope, } }
	},
};

static tagXpathTable dbusIntrospectXpathMainTable [] = {
	{ "interface",
	  LXPATH_TABLE_DO_RECUR,
	  { .recurSpec = { dbusIntrospectFindTagsUnderMain,
					   TABLE_INTERFACE, } }
	},
	{ "node",
	  LXPATH_TABLE_DO_RECUR,
	  { .recurSpec = { dbusIntrospectFindTagsUnderMain,
					   TABLE_MAIN, } }
	},
};

static tagXpathTable dbusIntrospectXpathMainNameTable [] = {
	{ "@name",
	  LXPATH_TABLE_DO_MAKE,
	  { .makeTagSpec = { KIND_GHOST_INDEX, ROLE_DEFINITION_INDEX,
			     makeTagForMainName,
			     decideKindForMainName } }
	},
};

static tagXpathTable dbusIntrospectXpathRootTable [] = {
	{ "/node",
	  LXPATH_TABLE_DO_RECUR,
	  { .recurSpec = { dbusIntrospectFindTagsUnderMain,
					   TABLE_MAIN, } }
	},
};

static tagXpathTableTable dbusIntrospectXpathTableTable[] = {
	[TABLE_ROOT]      = { ARRAY_AND_SIZE (dbusIntrospectXpathRootTable)     },
	[TABLE_MAIN]      = { ARRAY_AND_SIZE (dbusIntrospectXpathMainTable)     },
	[TABLE_INTERFACE] = { ARRAY_AND_SIZE (dbusIntrospectXpathInterfaceTable)},
	[TABLE_MAIN_NAME] = { ARRAY_AND_SIZE (dbusIntrospectXpathMainNameTable) },
	[TABLE_ARG]       = { ARRAY_AND_SIZE (dbusIntrospectXpathArgTable)      },
};

static void dbusIntrospectFindTagsUnderMain (xmlNode *node,
						  const char *xpath,
						  const struct sTagXpathRecurSpec *spec,
						  xmlXPathContext *ctx,
						  void *userData)
{
	struct dbusIntrospectData *data = userData;
	int scopeIndex = data->scopeIndex;

	if (!strcmp(xpath, "interface"))
		data->kindForName = K_INTERFACE;
	else if (!strcmp (xpath, "method"))
		data->kindForName = K_METHOD;
	else if (!strcmp (xpath, "signal"))
		data->kindForName = K_SIGNAL;
	else if (!strcmp (xpath, "arg"))
		data->kindForName = K_ARG;
	else
		data->kindForName = K_NODE;

	findXMLTags (ctx, node, TABLE_MAIN_NAME, data);
	if (spec->nextTable >= 0)
		findXMLTags (ctx, node, spec->nextTable, data);
	data->scopeIndex = scopeIndex;
}

static void makeTagWithScope (xmlNode *node CTAGS_ATTR_UNUSED,
			      const char *xpath CTAGS_ATTR_UNUSED,
			      const struct sTagXpathMakeTagSpec *spec CTAGS_ATTR_UNUSED,
			      struct sTagEntryInfo *tag,
			      void *userData)
{
	struct dbusIntrospectData *data = userData;

	tag->extensionFields.scopeKindIndex = KIND_GHOST_INDEX;
	tag->extensionFields.scopeName  = NULL;
	tag->extensionFields.scopeIndex = data->scopeIndex;

	makeTagEntry (tag);
}

static void makeTagForMainName (xmlNode *node CTAGS_ATTR_UNUSED,
				     const char *xpath CTAGS_ATTR_UNUSED,
				     const struct sTagXpathMakeTagSpec *spec CTAGS_ATTR_UNUSED,
				     struct sTagEntryInfo *tag,
				     void *userData)
{
	struct dbusIntrospectData *data = userData;

	tag->extensionFields.scopeKindIndex = KIND_GHOST_INDEX;
	tag->extensionFields.scopeName  = NULL;
	tag->extensionFields.scopeIndex = data->scopeIndex;

	data->scopeIndex = makeTagEntry (tag);
}

static int decideKindForMainName (xmlNode *node CTAGS_ATTR_UNUSED,
			      const char *xpath CTAGS_ATTR_UNUSED,
			      const struct sTagXpathMakeTagSpec *spec CTAGS_ATTR_UNUSED,
			      void *userData)
{
	return ((struct dbusIntrospectData *)userData)->kindForName;
}

static void
findDbusIntrospectTags (void)
{
	scheduleRunningBaseparser (RUN_DEFAULT_SUBPARSERS);
}

static void
runXPathEngine(xmlSubparser *s,
			   xmlXPathContext *ctx, xmlNode *root)
{
	struct dbusIntrospectData data = {
		.scopeIndex = CORK_NIL,
		.kindForName = KIND_GHOST_INDEX,
	};
	findXMLTags (ctx, root, TABLE_ROOT, &data);
}

static xmlSubparser dbusIntrospectSubparser = {
	.subparser = {
		.direction = SUBPARSER_BI_DIRECTION,
	},
	.runXPathEngine = runXPathEngine,
};

extern parserDefinition*
DbusIntrospectParser (void)
{
	static const char *const extensions [] = { "xml", NULL };
	parserDefinition* const def = parserNew ("DBusIntrospect");
	static selectLanguage selectors[] = { selectByXpathFileSpec, NULL };

	static xpathFileSpec xpathFileSpecs[] = {
		{
			/* <!DOCTYPE node PUBLIC "-//freedesktop//DTD D-BUS Object Introspection 1.0//EN"
			   "http://www.freedesktop.org/standards/dbus/1.0/introspect.dtd">
			   <node ... */
			.externalID = "-//freedesktop//DTD D-BUS Object Introspection 1.0//EN",
			.systemID   = "http://www.freedesktop.org/standards/dbus/1.0/introspect.dtd",
		},
		{
			.externalID = "-//freedesktop//DTD D-BUS Introspection 0.1//EN",
			.systemID = "http://www.freedesktop.org/software/dbus/introspection.dtd",
		},
		/* TODO: the following rule is too strong; parsers may conflicts each other
		 * when we implement more xpath based parsers. */
		{
			.rootElementName = "node",
			.nameInDTD       = "",
			.externalID      = "",
			.systemID        = "",
			.rootNSPrefix    = "",
			.rootNSHref      = "",
		},
	};
	static parserDependency dependencies [] = {
		[0] = { DEPTYPE_SUBPARSER, "XML", &dbusIntrospectSubparser },
	};
	def->kindTable         = DbusIntrospectKinds;
	def->kindCount     = ARRAY_SIZE (DbusIntrospectKinds);
	def->extensions    = extensions;
	def->parser        = findDbusIntrospectTags;
	def->tagXpathTableTable = dbusIntrospectXpathTableTable;
	def->tagXpathTableCount = ARRAY_SIZE (dbusIntrospectXpathTableTable);
	def->useCork = CORK_QUEUE;
	def->selectLanguage = selectors;
	def->xpathFileSpecs = xpathFileSpecs;
	def->xpathFileSpecCount = ARRAY_SIZE (xpathFileSpecs);
	def->dependencies = dependencies;
	def->dependencyCount = ARRAY_SIZE (dependencies);

	return def;
}
