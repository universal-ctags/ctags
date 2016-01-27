/*
*   Copyright (c) 2015, Masatake YAMATO
*   Copyright (c) 2015, Red Hat, Inc.
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for applying regular expression matching.
*
*   The code for utilizing the Gnu regex package with regards to processing the
*   regex option and checking for regex matches was adapted from routines in
*   Gnu etags.
*/

#include "general.h"  /* must always come first */
#include "debug.h"
#include "entry.h"
#include "options.h"
#include "parse.h"
#include "read.h"
#include "routines.h"
#include "xtag.h"

#ifdef HAVE_LIBXML
#include <libxml/xpath.h>
#include <libxml/tree.h>

static void simpleXpathMakeTag (parserDefinition * parser,
				xmlNode *node,
				const tagXpathMakeTagSpec *spec,
				const kindOption* const kinds,
				const unsigned int passCount,
				void *userData)
{
	tagEntryInfo tag;
	xmlChar* str;
	const kindOption *kind;

	str = xmlNodeGetContent(node);
	if (str == NULL)
		return;

	kind = kinds + spec->kind;

	if (spec->role == ROLE_INDEX_DEFINITION)
		initTagEntry (&tag, (char *)str, kind);
	else if (isXtagEnabled(XTAG_REFERENCE_TAGS))
		initRefTagEntry (&tag, (char *)str,
				 kind,
				 spec->role);
	else
		goto out;


	tag.lineNumber = xmlGetLineNo (node);
	tag.filePosition = getInputFilePositionForLine (tag.lineNumber);

	if (spec->make)
		spec->make (parser, node, spec, &tag, passCount, userData);
	else
		makeTagEntry (&tag);

out:
	xmlFree (str);
}

extern void addTagXpath (const langType language, tagXpathTable *xpathTable)
{
	Assert (xpathTable->xpath);
	Assert (!xpathTable->xpathCompiled);

	verbose ("compile a xpath expression: %s\n", (xmlChar *)xpathTable->xpath);
	xpathTable->xpathCompiled = xmlXPathCompile ((xmlChar *)xpathTable->xpath);
	if (!xpathTable->xpathCompiled)
		error (WARNING, "Failed to compile the Xpath expression: %s", xpathTable->xpath);
}

static void findXMLTagsCore (parserDefinition * parser,
			     xmlXPathContext *ctx, xmlNode *root,
			     const tagXpathTableTable *xpathTableTable,
			     const kindOption* const kinds,
			     const unsigned int passCount,
			     void *userData)
{
	unsigned int i, j;
	xmlNode * node;

	Assert (root);
	Assert (xpathTableTable);

	for (i = 0; i < xpathTableTable->count; ++i)
	{
		xmlXPathObject *object;
		xmlNodeSet *set;
		const tagXpathTable *elt = xpathTableTable->table + i;

		if (! elt->xpathCompiled)
			continue;

#if 0
		/* Older version of libxml2 doesn't have xmlXPathSetContextNode. */
		if (xmlXPathSetContextNode (root, ctx) != 0)
		  {
		    error (WARNING, "Failed to set node to XpathContext");
		    return;
		  }
#else
		ctx->node = root;
#endif

		object = xmlXPathCompiledEval (elt->xpathCompiled, ctx);
		if (!object)
			continue;

		set = object->nodesetval;

		if (set)
		{
			for (j = 0; j < set->nodeNr; ++j)
			{
				node = set->nodeTab[j];
				if (elt->specType == LXPATH_TABLE_DO_MAKE)
					simpleXpathMakeTag (parser, node, &(elt->makeTagSpec), kinds,
							    passCount, userData);
				else
					elt->recurSpec.enter (parser, node, &(elt->recurSpec), ctx,
							      passCount, userData);
			}
		}
		xmlXPathFreeObject (object);
	}
}

extern rescanReason findXMLTags (parserDefinition *parser,
				 xmlXPathContext *ctx, xmlNode *root,
				 const tagXpathTableTable *xpathTableTable,
				 const kindOption* const kinds,
				 const unsigned int passCount,
				 void *userData)
{
	boolean usedAsEnterPoint = FALSE;
	xmlDocPtr doc = NULL;

	if (ctx == NULL)
	{
		usedAsEnterPoint = TRUE;

		findRegexTags (parser, passCount);
		doc = xmlParseFile(getInputFileName());
		if (doc == NULL)
		{
			verbose ("could not parse %s as a XML file\n", getInputFileName());
			return RESCAN_NONE;
		}

		ctx = xmlXPathNewContext (doc);
		if (ctx == NULL)
			error (FATAL, "failed to make a new xpath context for %s", getInputFileName());

		root = xmlDocGetRootElement(doc);
		if (root == NULL)
		{
			verbose ("could not get the root node for %s\n", getInputFileName());
			goto out;
		}
	}

	findXMLTagsCore (parser, ctx, root, xpathTableTable, kinds, passCount, userData);

out:
	if (usedAsEnterPoint)
	{
		xmlXPathFreeContext (ctx);
		xmlFreeDoc (doc);
	}

	return RESCAN_NONE;
}

#else

extern void addTagXpath (const langType language, tagXpathTable *xpathTable)
{
	xpathTable->xpathCompiled = NULL;
}

extern rescanReason findXMLTags (parserDefinition *parser,
				 xmlXPathContext *ctx, xmlNode *root,
				 const tagXpathTableTable *xpathTableTable,
				 const kindOption* const kinds,
				 const unsigned int passCount,
				 void *userData)
{
	return RESCAN_NONE;
}

#endif

/* vi:set tabstop=4 shiftwidth=4: */
