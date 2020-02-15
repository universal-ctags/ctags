/*
*   Copyright (c) 2008, David Fishburn
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for generating tags for Ant language files.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include <string.h>
#include "entry.h"
#include "parse.h"
#include "routines.h"
#ifdef HAVE_LIBXML
#include "read.h"
#include "selectors.h"
#include "xml.h"
#endif

#ifdef HAVE_LIBXML
/*
*   FUNCTION PROTOTYPES
*/
static void antFindTagsUnderProject (xmlNode *node,
				     const char *xpath,
				     const struct sTagXpathRecurSpec *spec,
				     xmlXPathContext *ctx,
				     void *userData);
static void antFindTagsUnderTask (xmlNode *node,
				  const char *xpath,
				  const struct sTagXpathRecurSpec *spec,
				  xmlXPathContext *ctx,
				  void *userData);
static void makeTagForProjectName (xmlNode *node,
				   const char *xpath,
				   const struct sTagXpathMakeTagSpec *spec,
				   struct sTagEntryInfo *tag,
				   void *userData);
static void makeTagForTargetName (xmlNode *node,
				  const char *xpath,
				  const struct sTagXpathMakeTagSpec *spec,
				  struct sTagEntryInfo *tag,
				  void *userData);
static void makeTagWithScope (xmlNode *node,
				  const char *xpath,
			      const struct sTagXpathMakeTagSpec *spec,
			      struct sTagEntryInfo *tag,
			      void *userData);
#endif

#ifdef HAVE_LIBXML
typedef enum {
	K_PROJECT, K_TARGET, K_PROPERTY, K_IMPORT,
} antKind;

typedef enum {
	R_IMPORT_GENERIC,
} antAntfileRole;

static roleDefinition AntAntfileRoles [] = {
        { true, "imported", "imported" },
};

static kindDefinition AntKinds [] = {
	{ true,  'p', "project",  "projects"   },
	{ true,  't', "target",   "targets"    },
	{ true,  'P', "property", "properties(global)" },
	{ true,  'i', "antfile",  "antfiles",
	  .referenceOnly = true, ATTACH_ROLES(AntAntfileRoles)},
};

enum antXpathTable {
	TABLE_MAIN, TABLE_PROJECT, TABLE_MAIN_NAME, TABLE_TARGET_NAME,
};

static tagXpathTable antXpathMainTable [] = {
	{ "///project",
	  LXPATH_TABLE_DO_RECUR,
	  { .recurSpec= { antFindTagsUnderProject } }
	},
};

static tagXpathTable antXpathProjectTable [] = {
	{ "target",
	  LXPATH_TABLE_DO_RECUR,
	  { .recurSpec= { antFindTagsUnderTask, TABLE_TARGET_NAME } }
	},
	{ "property/@name",
	  LXPATH_TABLE_DO_MAKE,
	  { .makeTagSpec = { K_PROPERTY, ROLE_DEFINITION_INDEX,
			     makeTagWithScope } }
	},
	{ "import/@file",
	  LXPATH_TABLE_DO_MAKE,
	  { .makeTagSpec = { K_IMPORT, R_IMPORT_GENERIC,
			     makeTagWithScope } }
	},
};

static tagXpathTable antXpathMainNameTable [] = {
	{ "@name",
	  LXPATH_TABLE_DO_MAKE,
	  { .makeTagSpec = { K_PROJECT, ROLE_DEFINITION_INDEX,
			     makeTagForProjectName } }
	},
};

static tagXpathTable antXpathTargetNameTable [] = {
	{ "@name",
	  LXPATH_TABLE_DO_MAKE,
	  { .makeTagSpec = { K_TARGET, ROLE_DEFINITION_INDEX,
			     makeTagForTargetName} }
	},
};

static tagXpathTableTable antXpathTableTable[] = {
	[TABLE_MAIN]        = { ARRAY_AND_SIZE (antXpathMainTable)       },
	[TABLE_PROJECT]     = { ARRAY_AND_SIZE (antXpathProjectTable)    },
	[TABLE_MAIN_NAME]   = { ARRAY_AND_SIZE (antXpathMainNameTable)   },
	[TABLE_TARGET_NAME] = { ARRAY_AND_SIZE (antXpathTargetNameTable) },
};

#else
static tagRegexTable antTagRegexTable [] = {
	{"^[ \t]*<[ \t]*project[^>]+name=\"([^\"]+)\".*", "\\1",
	 "p,project,projects", NULL},
	{"^[ \t]*<[ \t]*target[^>]+name=\"([^\"]+)\".*", "\\1",
	 "t,target,targets", NULL},
	{"^[ \t]*<[ \t]*property[^>]+name=\"([^\"]+)\".*", "\\1",
	 "P,property,property", NULL},
};
#endif

/*
*   FUNCTION DEFINITIONS
*/
#ifdef HAVE_LIBXML

static void
antFindTagsUnderProject (xmlNode *node,
			 const char *xpath CTAGS_ATTR_UNUSED,
			 const struct sTagXpathRecurSpec *spec CTAGS_ATTR_UNUSED,
			 xmlXPathContext *ctx,
			 void *userData CTAGS_ATTR_UNUSED)
{
	int corkIndex = CORK_NIL;

	findXMLTags (ctx, node, TABLE_MAIN_NAME, &corkIndex);
	findXMLTags (ctx, node, TABLE_PROJECT, &corkIndex);
}

static void antFindTagsUnderTask (xmlNode *node,
				  const char *xpath CTAGS_ATTR_UNUSED,
				  const struct sTagXpathRecurSpec *spec,
				  xmlXPathContext *ctx,
				  void *userData)
{
	int corkIndex = *(int *)userData;

	findXMLTags (ctx, node, spec->nextTable, &corkIndex);
}

static void makeTagForProjectName (xmlNode *node CTAGS_ATTR_UNUSED,
				   const char *xpath CTAGS_ATTR_UNUSED,
				   const struct sTagXpathMakeTagSpec *spec CTAGS_ATTR_UNUSED,
				   struct sTagEntryInfo *tag,
				   void *userData)
{
	int *corkIndex = userData;

	*corkIndex = makeTagEntry (tag);
}

static void makeTagForTargetName (xmlNode *node CTAGS_ATTR_UNUSED,
				  const char *xpath CTAGS_ATTR_UNUSED,
				  const struct sTagXpathMakeTagSpec *spec CTAGS_ATTR_UNUSED,
				  struct sTagEntryInfo *tag,
				  void *userData)
{
	int *corkIndex = (int *)userData;
	int parentIndex = *corkIndex;

	tag->extensionFields.scopeKindIndex = KIND_GHOST_INDEX;
	tag->extensionFields.scopeName  = NULL;
	tag->extensionFields.scopeIndex = parentIndex;

	*corkIndex = makeTagEntry (tag);
}

static void makeTagWithScope (xmlNode *node CTAGS_ATTR_UNUSED,
			      const char *xpath CTAGS_ATTR_UNUSED,
			      const struct sTagXpathMakeTagSpec *spec CTAGS_ATTR_UNUSED,
			      struct sTagEntryInfo *tag,
			      void *userData)
{
	tag->extensionFields.scopeKindIndex = KIND_GHOST_INDEX;
	tag->extensionFields.scopeName  = NULL;
	tag->extensionFields.scopeIndex = *(int *)userData;

	makeTagEntry (tag);
}

static void
findAntTags (void)
{
	scheduleRunningBaseparser (RUN_DEFAULT_SUBPARSERS);
}

static void
runXPathEngine(xmlSubparser *s,
			   xmlXPathContext *ctx, xmlNode *root)
{
	findXMLTags (ctx, root, TABLE_MAIN, NULL);
}

static xmlSubparser antSubparser = {
	.subparser = {
		.direction = SUBPARSER_BI_DIRECTION,
	},
	.runXPathEngine = runXPathEngine,
};
#endif

extern parserDefinition* AntParser (void)
{
	static const char *const extensions [] = { "build.xml", "ant",
#ifdef HAVE_LIBXML
				/* libxml based selector is needed to select a
				 * proper concrete xml parser.*/
						   "xml",
#endif
						   NULL };
	static const char *const patterns [] = { "build.xml", NULL };
	parserDefinition* const def = parserNew ("Ant");
#ifdef HAVE_LIBXML
	static selectLanguage selectors[] = { selectByXpathFileSpec, NULL };
	static xpathFileSpec xpathFileSpecs[] = {
		/* See http://ant.apache.org/faq.html#dtd */
		{
			.rootElementName = "project",
			.nameInDTD       = "",
			.externalID      = "",
			.systemID        = "",
			.rootNSPrefix    = "",
			.rootNSHref      = "",
		},
		{
			.rootElementName = "project",
			.nameInDTD       = "project",
			.externalID      = "",
			.systemID        = "",
			.rootNSPrefix    = "",
			.rootNSHref      = "",
		}
	};
	static parserDependency dependencies [] = {
		[0] = { DEPTYPE_SUBPARSER, "XML", &antSubparser },
	};
#endif
	def->extensions = extensions;
	def->patterns = patterns;
#ifdef HAVE_LIBXML
	def->kindTable = AntKinds;
	def->kindCount = ARRAY_SIZE (AntKinds);
	def->parser = findAntTags;
	def->tagXpathTableTable = antXpathTableTable;
	def->tagXpathTableCount = ARRAY_SIZE (antXpathTableTable);
	def->useCork = CORK_QUEUE;
	def->selectLanguage = selectors;
	def->xpathFileSpecs = xpathFileSpecs;
	def->xpathFileSpecCount = ARRAY_SIZE (xpathFileSpecs);
	def->dependencies = dependencies;
	def->dependencyCount = ARRAY_SIZE (dependencies);
#else
	def->tagRegexTable = antTagRegexTable;
	def->tagRegexCount = ARRAY_SIZE (antTagRegexTable);
	def->method     = METHOD_NOT_CRAFTED|METHOD_REGEX;
#endif
	return def;
}
