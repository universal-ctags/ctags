/*
*   Copyright (c) 2015, Masatake YAMATO
*   Copyright (c) 2015, Red Hat, Inc.
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for xpath meta parser.
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

static void simpleXpathMakeTag (xmlNode *node,
				const tagXpathMakeTagSpec *spec,
				const kindOption* const kinds,
				void *userData)
{
	tagEntryInfo tag;
	xmlChar* str;
	const kindOption *kind;
	char *path;

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

	path = (char *)xmlGetNodePath (node);
	tag.extensionFields.xpath = path;

	if (spec->make)
		spec->make (node, spec, &tag, userData);
	else
		makeTagEntry (&tag);

	if (path)
		xmlFree (path);
out:
	xmlFree (str);
}

extern void addTagXpath (const langType language __unused__, tagXpathTable *xpathTable)
{
	Assert (xpathTable->xpath);
	Assert (!xpathTable->xpathCompiled);

	verbose ("compile a xpath expression: %s\n", (xmlChar *)xpathTable->xpath);
	xpathTable->xpathCompiled = xmlXPathCompile ((xmlChar *)xpathTable->xpath);
	if (!xpathTable->xpathCompiled)
		error (WARNING, "Failed to compile the Xpath expression: %s", xpathTable->xpath);
}

static void findXMLTagsCore (xmlXPathContext *ctx, xmlNode *root,
			     const tagXpathTableTable *xpathTableTable,
			     const kindOption* const kinds,void *userData)
{
	unsigned int i;
	int j;
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
					simpleXpathMakeTag (node, &(elt->spec.makeTagSpec), kinds, userData);
				else
					elt->spec.recurSpec.enter (node, &(elt->spec.recurSpec), ctx, userData);
			}
		}
		xmlXPathFreeObject (object);
	}
}

static void suppressWarning (void *ctx __unused__, const char *msg __unused__, ...)
{
}

static xmlDocPtr makeXMLDoc (void)
{
	const unsigned char* data;
	size_t size;
	xmlDocPtr doc = NULL;

	doc = getInputFileUserData ();
	if (doc)
	{
		verbose ("reuse xml doc data\n");
		return doc;
	}

	data = getInputFileData (&size);
	if (data)
	{
		xmlLineNumbersDefault (1);
		doc = xmlParseMemory((const char*)data, size);
	}

	return doc;
}

extern void findXMLTags (xmlXPathContext *ctx, xmlNode *root,
			 const tagXpathTableTable *xpathTableTable,
			 const kindOption* const kinds,void *userData)
{
	boolean usedAsEntryPoint = FALSE;
	xmlDocPtr doc = NULL;

	if (ctx == NULL)
	{
		usedAsEntryPoint = TRUE;

		findRegexTags ();

		xmlSetGenericErrorFunc (NULL, suppressWarning);

		doc = makeXMLDoc ();

		if (doc == NULL)
		{
			verbose ("could not parse %s as a XML file\n", getInputFileName());
			return;
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

	findXMLTagsCore (ctx, root, xpathTableTable, kinds, userData);

out:
	if (usedAsEntryPoint)
	{
		xmlXPathFreeContext (ctx);

		if (doc != getInputFileUserData ())
			xmlFreeDoc (doc);
	}
}

#else

extern void addTagXpath (const langType language, tagXpathTable *xpathTable)
{
	xpathTable->xpathCompiled = NULL;
}

extern void findXMLTags (xmlXPathContext *ctx, xmlNode *root,
			 const tagXpathTableTable *xpathTableTable,
			 const kindOption* const kinds, void *userData)
{
}

#endif
