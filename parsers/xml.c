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

#ifndef HAVE_LIBXML
typedef void *xmlNsPtr;
#endif

typedef enum {
	K_ID,
	K_NSPREFIX,
} xmlKind;

static kindDefinition XmlKinds [] = {
	{ true, 'i', "id", "id attributes" },
	{ true, 'n', "nsprefix", "namespace prefixes" },
};

enum xmlXpathTables {
	TABLE_MAIN,
	TABLE_ID,
};

typedef enum {
	F_NS_URI,
} xmlField;

static fieldDefinition XmlFields [] = {
	{
		.name = "uri",
		.description = "uri associated with name prefix",
		.enabled = true,
	}
};

static void findNsPrefix (xmlNode *node,
					   const char *xpath,
					   const struct sTagXpathRecurSpec *spec,
					   xmlXPathContext *ctx,
					   void *userData);

static tagXpathTable XmlXpathMainTable [] = {
	{ "//*",
	  LXPATH_TABLE_DO_RECUR,
	  { .recurSpec = { .enter = findNsPrefix, .nextTable = TABLE_ID } }
	},
};

static tagXpathTable XmlXpathIdTable [] = {
	{ "./@id",
	  LXPATH_TABLE_DO_MAKE,
	  { .makeTagSpec = { K_ID, ROLE_INDEX_DEFINITION } }
	},
};

static tagXpathTableTable xmlXpathTableTable[] = {
	[TABLE_MAIN] = { ARRAY_AND_SIZE (XmlXpathMainTable) },
	[TABLE_ID]   = { ARRAY_AND_SIZE (XmlXpathIdTable) },
};

static int makeNsPrefixTag (const char *name, xmlNode *node, xmlNsPtr ns)
{
	int n = CORK_NIL;

#ifdef HAVE_LIBXML
	tagEntryInfo tag;

	initTagEntry (&tag, name, K_NSPREFIX);
	/* TODO
	 * - move this code block to lxpath.c.
	 * - adjust the line number for nsprefixes forward. */
	tag.lineNumber = XML_GET_LINE (node);
	tag.filePosition = getInputFilePositionForLine (tag.lineNumber);
	char *p = (char *)xmlGetNodePath (node);
	if (ns->href && *ns->href)
		attachParserField (&tag, XmlFields [F_NS_URI].ftype, (char *)ns->href);

	n = makeTagEntry (&tag);
	if (p)
		xmlFree (p);
#endif

	return n;
}

static void findNsPrefix (xmlNode *node,
					   const char *xpath,
					   const struct sTagXpathRecurSpec *spec,
					   xmlXPathContext *ctx,
					   void *userData)
{
#ifdef HAVE_LIBXML
	if (node->ns && node->ns->prefix == NULL)
	{
		vString *prefix = anonGenerateNew ("ns", K_NSPREFIX);
		makeNsPrefixTag (vStringValue (prefix), node, node->ns);
		vStringDelete (prefix);
	}

	for (xmlNsPtr ns = node->nsDef; ns; ns = ns->next)
	{
		if (ns->prefix)
			makeNsPrefixTag ((char *)ns->prefix, node, ns);
	}

	findXMLTags (ctx, node, spec->nextTable, userData);
#endif
}

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
	def->fieldTable = XmlFields;
	def->fieldCount = ARRAY_SIZE (XmlFields);

	return def;
}
