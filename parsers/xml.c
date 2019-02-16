/*
*
*   Copyright (c) 2019, Masatake YAMATO
*   Copyright (c) 2019, Red Hat, K.K.
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for generating tags for id="..." in a XML file
*/

#include "general.h"	/* must always come first */
#include "entry.h"
#include "options.h"
#include "parse.h"
#include "read.h"
#include "routines.h"
#include "selectors.h"

typedef enum {
	K_ID,
} xmlKind;

static kindDefinition XmlKinds [] = {
	{ true, 'i', "id", "id attributes" },
};

enum xmlXpathTables {
	TABLE_MAIN,
};

static tagXpathTable XmlXpathMainTable [] = {
	{ "///@id",
	  LXPATH_TABLE_DO_MAKE,
	  { .makeTagSpec = { K_ID, ROLE_INDEX_DEFINITION } }
	},
};

static tagXpathTableTable xmlXpathTableTable[] = {
	[TABLE_MAIN] = { ARRAY_AND_SIZE (XmlXpathMainTable) },
};

static void
findXmlTags (void)
{
	findXMLTags (NULL, NULL, TABLE_MAIN, NULL);
}

extern parserDefinition*
XmlParser (void)
{
	static const char *const extensions [] = { "xml", NULL };
	parserDefinition* const def = parserNew ("XML");
	static selectLanguage selectors[] = { selectByXpathFileSpec, NULL };

	def->kindTable     = XmlKinds;
	def->kindCount     = ARRAY_SIZE (XmlKinds);
	def->extensions    = extensions;
	def->parser        = findXmlTags;
	def->tagXpathTableTable = xmlXpathTableTable;
	def->tagXpathTableCount = ARRAY_SIZE (xmlXpathTableTable);
	def->selectLanguage = selectors;
	return def;
}
