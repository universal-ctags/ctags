/*
*
*   Copyright (c) 2016, Masatake YAMATO
*   Copyright (c) 2016, Red Hat, K.K.
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for generating tags for svg.
*
*/

#include "general.h"	/* must always come first */
#include "entry.h"
#include "options.h"
#include "parse.h"
#include "read.h"
#include "routines.h"
#include "xml.h"

#include <string.h>

typedef enum {
	K_DEF,
} svgKind;

static kindDefinition SvgKinds [] = {
	{ true,  'd', "def", "ids in defs tags" },
};

static void
findSvgTags (void)
{
	scheduleRunningBaseparser (RUN_DEFAULT_SUBPARSERS);
}

static void
makeTagEntryWithNodeNotify (xmlSubparser *s,
							xmlNode *node,
							tagEntryInfo *xmlTag)
{
	if (node->type == XML_ATTRIBUTE_NODE
		&& (strcmp ((char *)node->name, "id") == 0)
		&& node->parent && node->parent->parent
		&& node->parent->parent->type == XML_ELEMENT_NODE
		&& (strcmp ((char *)node->parent->parent->name, "defs") == 0))
	{
		tagEntryInfo tag;
		initTagEntry (&tag, xmlTag->name, K_DEF);
		tag.filePosition = xmlTag->filePosition;
		tag.lineNumber = xmlTag->lineNumber;
		makeTagEntry (&tag);
	}
}

static xmlSubparser svgSubparser = {
	.subparser = {
		.direction = SUBPARSER_BI_DIRECTION,
	},
	.makeTagEntryWithNodeNotify = makeTagEntryWithNodeNotify,
};

extern parserDefinition*
SvgParser (void)
{
	static const char *const extensions [] = { "svg", NULL };
	parserDefinition* const def = parserNew ("SVG");

	static parserDependency dependencies [] = {
		[0] = { DEPTYPE_SUBPARSER, "XML", &svgSubparser },
	};

	def->dependencies = dependencies;
	def->dependencyCount = ARRAY_SIZE (dependencies);

	def->kindTable         = SvgKinds;
	def->kindCount     = ARRAY_SIZE (SvgKinds);
	def->extensions    = extensions;
	def->parser        = findSvgTags;
	return def;
}
