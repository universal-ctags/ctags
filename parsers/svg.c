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
#include "debug.h"
#include "options.h"
#include "parse.h"
#include "read.h"
#include "routines.h"


typedef enum {
	K_ID,
} svgKind;

static kindOption SvgKinds [] = {
	{ TRUE,  'i', "id", "id attributes" },
};

static tagXpathTable svgXpathMainTable [] = {
	{ "//*[local-name()='svg']//*/@id",
	  LXPATH_TABLE_DO_MAKE,
	  { .makeTagSpec = {K_ID, ROLE_INDEX_DEFINITION } }
	},
};

enum svgXpathTable {
	TABLE_MAIN,
};

static tagXpathTableTable svgXpathTableTable[] = {
	[TABLE_MAIN] = { ARRAY_AND_SIZE (svgXpathMainTable) },
};

static void
findSvgTags (void)
{
	findXMLTags (NULL, NULL,
		     svgXpathTableTable + TABLE_MAIN,
		     SvgKinds,
		     NULL);
}

extern parserDefinition*
SvgParser (void)
{
	static const char *const extensions [] = { "svg", NULL };
	parserDefinition* const def = parserNew ("SVG");

	def->kinds         = SvgKinds;
	def->kindCount     = ARRAY_SIZE (SvgKinds);
	def->extensions    = extensions;
	def->parser        = findSvgTags;
	def->tagXpathTableTable = svgXpathTableTable;
	def->tagXpathTableCount = ARRAY_SIZE (svgXpathTableTable);
	return def;
}
