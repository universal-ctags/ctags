/*
*   Copyright (c) 2016, Masatake YAMATO
*   Copyright (c) 2016, Red Hat, Inc.
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   Xpath based parer API
*/
#ifndef CTAGS_LXPATH_PARSE_H
#define CTAGS_LXPATH_PARSE_H

#include "general.h"  /* must always come first */
#include "types.h"

#ifdef HAVE_LIBXML
#include <libxml/xpath.h>
#include <libxml/tree.h>
#else
#define xmlNode void
#define xmlXPathCompExpr void
#define xmlXPathContext void
#endif

typedef struct sTagXpathMakeTagSpec {
	int   kind;
	int   role;
	/* If make is NULL, just makeTagEntry is used instead. */
	void (*make) (xmlNode *node,
		      const struct sTagXpathMakeTagSpec *spec,
		      tagEntryInfo *tag,
		      void *userData);
} tagXpathMakeTagSpec;

typedef struct sTagXpathRecurSpec {
	void (*enter) (xmlNode *node,
		       const struct sTagXpathRecurSpec *spec,
		       xmlXPathContext *ctx,
		       void *userData);

	int  nextTable;		/* A parser can use this field any purpose.
				   main/lxpath part doesn't touch this. */

} tagXpathRecurSpec;

typedef struct sTagXpathTable
{
	const char *const xpath;
	enum  { LXPATH_TABLE_DO_MAKE, LXPATH_TABLE_DO_RECUR } specType;
	union {
		tagXpathMakeTagSpec makeTagSpec;
		tagXpathRecurSpec   recurSpec;
	} spec;
	xmlXPathCompExpr* xpathCompiled;
} tagXpathTable;

typedef struct sTagXpathTableTable {
	tagXpathTable *table;
	unsigned int   count;
} tagXpathTableTable;

typedef struct sXpathFileSpec {
	/*
	   NULL represents the associated field in DTD is not examined.
	   "" (an empty string) represents the associated field in DTD
	   (and root element) must not exist. */
	const char *rootElementName;
	const char *nameInDTD;
	const char *externalID;
	const char *systemID;
	const char *rootNSPrefix;
	const char *rootNSHref;
} xpathFileSpec;

/* Xpath interface */
extern void findXMLTags (xmlXPathContext *ctx, xmlNode *root,
			 const tagXpathTableTable *xpathTableTable,
			 const kindDefinition* const kinds, void *userData);
extern void addTagXpath (const langType language, tagXpathTable *xpathTable);

#endif  /* CTAGS_LXPATH_PARSE_H */
