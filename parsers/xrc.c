/*
*
*   Copyright (c) 2023, Masatake YAMATO
*   Copyright (c) 2023, Red Hat, K.K.
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for generating tags for XML Based Resource System (XRC) files
*
*   Reference:
*   - https://docs.wxwidgets.org/3.0/overview_xrc.html
*/

/*
 *	 INCLUDE FILES
 */
#include "general.h"	/* must always come first */

#include "xml.h"

#include "entry.h"
#include "parse.h"

/*
 *	 DATA DECLARATIONS
 */

typedef enum {
	K_OBJECT,
} xrcKind;

/*
 *	 FUNCTION DECLARATIONS
 */

static void makeXRCTag (xmlNode *node,
						const char *xpath,
						const struct sTagXpathMakeTagSpec *spec,
						struct sTagEntryInfo *tag,
						void *userData);

/*
 *	DATA DEFINITIONS
 */

static kindDefinition XrcKinds [] = {
	{ true,  'o', "object",	  "objects", }
};

static tagXpathTable xrcXpathMainTable[] = {
	{ /* "//object/@name", */
		"/resource//object/@name",
		LXPATH_TABLE_DO_MAKE,
		{ .makeTagSpec = { K_OBJECT, ROLE_DEFINITION_INDEX, makeXRCTag, } }
	},
};

enum xrcXpathTable {
	TABLE_MAIN
};

static tagXpathTableTable xrcXpathTableTable[] = {
	[TABLE_MAIN] = { ARRAY_AND_SIZE(xrcXpathMainTable) },
};

/*
 *	 FUNCTION DEFINITIONS
 */

static void makeXRCTag (xmlNode *node,
						const char *xpath,
						const struct sTagXpathMakeTagSpec *spec,
						struct sTagEntryInfo *tag,
						void *userData)
{
	if (*tag->name == '\0')
		return;

	makeTagEntry (tag);
}

static void
findXrcTags (void)
{
	scheduleRunningBaseparser (0);
}

static void
runXPathEngine(xmlSubparser *s,
			   xmlXPathContext *ctx, xmlNode *root)
{
	findXMLTags (ctx, root, TABLE_MAIN, NULL);
}

static xmlSubparser xrcSubparser = {
	.subparser = {
		.direction = SUBPARSER_SUB_RUNS_BASE,
	},
	.runXPathEngine = runXPathEngine,
};

extern parserDefinition*
XrcParser (void)
{
	static const char *const extensions [] = { "xrc", NULL };
	parserDefinition* const def = parserNew ("XRC");
	static parserDependency dependencies [] = {
		[0] = { DEPTYPE_SUBPARSER, "XML", &xrcSubparser },
	};

	def->kindTable         = XrcKinds;
	def->kindCount     = ARRAY_SIZE (XrcKinds);
	def->extensions    = extensions;
	def->parser        = findXrcTags;
	def->tagXpathTableTable  = xrcXpathTableTable;
	def->tagXpathTableCount  = ARRAY_SIZE (xrcXpathTableTable);
	def->dependencies = dependencies;
	def->dependencyCount = ARRAY_SIZE (dependencies);
	return def;
}
