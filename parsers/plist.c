/*
*
*   Copyright (c) 2016, Masatake YAMATO
*   Copyright (c) 2016, Red Hat, K.K.
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for generating tags for property list defined
*   in http://www.apple.com/DTDs/PropertyList-1.0.dtd.
*/

#include "general.h"	/* must always come first */

#include <string.h>

#include "debug.h"
#include "options.h"
#include "parse.h"
#include "read.h"
#include "routines.h"
#include "strlist.h"


typedef enum {
	K_KEY,
} plistKind;

static kindOption PlistKinds [] = {
	{ TRUE,  'k', "key",	  "keys" },
};

static void plistFindTagsUnderKey (xmlNode *node,
				   const struct sTagXpathRecurSpec *spec,
				   xmlXPathContext *ctx,
				   void *userData);

static void makeTagWithScope (xmlNode *node,
			      const struct sTagXpathMakeTagSpec *spec,
			      struct sTagEntryInfo *tag,
			      void *userData);

static tagXpathTable plistXpathMainTable[] = {
	{ "///plist//dict/key",
	  LXPATH_TABLE_DO_RECUR,
	  .spec.recurSpec = {
			plistFindTagsUnderKey
		}
	},
};

static tagXpathTable plistXpathTextTable[] = {
	{ "text()",
	  LXPATH_TABLE_DO_MAKE,
	  .spec.makeTagSpec = {
			K_KEY,  ROLE_INDEX_DEFINITION,
			makeTagWithScope
		}
	},
};

enum plistXpathTable {
	TABLE_MAIN,
	TABLE_TEXT,
};

static tagXpathTableTable plistXpathTableTable[] = {
	[TABLE_MAIN] = { ARRAY_AND_SIZE(plistXpathMainTable) },
	[TABLE_TEXT] = { ARRAY_AND_SIZE(plistXpathTextTable) },
};

static bool isCompoundElement (xmlNode *node)
{
	return !! (node->name
		   && ((strcmp ((char *)(node->name), "dict") == 0)
		       || (strcmp ((char *)(node->name), "array") == 0)));
}

static xmlNode *getPrevKeyElement (xmlNode *node)
{
	xmlNode *prev;

	prev = xmlPreviousElementSibling (node);
	if (prev)
	{
		if (strcmp ((char *)prev->name, "key") == 0)
			return prev;
		else
			prev = NULL;
	}
	return prev;
}

static void plistFindTagsUnderKey (xmlNode *node,
				   const struct sTagXpathRecurSpec *spec,
				   xmlXPathContext *ctx,
				   void *userData)
{
	xmlNode *current;
	xmlNode *prev;
	stringList *queue;
	vString* path;
	vString* v;
	int c;

	queue = stringListNew ();
	current = node;
	for (current = node; current; current = current->parent)
	{
		if (isCompoundElement (current)
		    && (prev = getPrevKeyElement (current)))
		{
			char* parent = (char *)xmlNodeGetContent (prev);
			if (parent)
			{
				v = vStringNewInit (parent);
				stringListAdd (queue, v);
				xmlFree (parent);
			}
		}
	}

	path = vStringNew ();
	while ((c = stringListCount (queue)) > 0)
	{
		v = stringListLast (queue);
		vStringCat (path, v);
		vStringDelete (v);
		stringListRemoveLast (queue);
		if (c != 1)
			vStringPut (path, '.');
	}
	stringListDelete (queue);

	findXMLTags (ctx, node,
		     plistXpathTableTable + TABLE_TEXT,
		     PlistKinds,
		     (vStringLength (path) > 0)? vStringValue (path): NULL);

	vStringDelete (path);
}

static void makeTagWithScope (xmlNode *node,
			      const struct sTagXpathMakeTagSpec *spec,
			      struct sTagEntryInfo *tag,
			      void *userData)
{
	tag->extensionFields.scopeKind  = userData? PlistKinds + K_KEY: NULL;
	tag->extensionFields.scopeName  = userData;
	makeTagEntry (tag);
}

static void
findPlistTags (void)
{
	findXMLTags (NULL, NULL, plistXpathTableTable + TABLE_MAIN, PlistKinds, NULL);
}

extern parserDefinition*
PlistXMLParser (void)
{
	static const char *const extensions [] = { "plist", NULL };
	parserDefinition* const def = parserNew ("PlistXML");

	def->kinds         = PlistKinds;
	def->kindCount     = ARRAY_SIZE (PlistKinds);
	def->extensions    = extensions;
	def->parser        = findPlistTags;
	def->tagXpathTableTable  = plistXpathTableTable;
	def->tagXpathTableCount  = ARRAY_SIZE (plistXpathTableTable);
	return def;
}
