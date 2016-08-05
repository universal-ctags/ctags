/*
*
*   Copyright (c) 2016, Masatake YAMATO
*   Copyright (c) 2016, Red Hat, K.K.
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for generating tags for RelaxNG.
*
*   https://www.oasis-open.org/committees/relax-ng/spec-20010811.html
*
*/

#include "general.h"	/* must always come first */
#include "options.h"
#include "parse.h"
#include "read.h"

typedef enum {
	K_ELEMENT,
	K_ATTRIBUTE,
	K_NAMED_PATTERN,
} relaxngKind;

static kindOption RelaxNGKinds [] = {
	{ TRUE,  'e', "element",     "elements"       },
	{ TRUE,  'a', "attribute",   "attributes"     },
	{ TRUE,  'n', "namedPattern", "named patterns" },
};

enum relaxngXpathTable {
	TABLE_MAIN, TABLE_ELEMENT_NAME, TABLE_PATTERN, TABLE_GRAMMAR, TABLE_DEFINE_NAME,
};

static void relaxngMakeAndFindTagsUnderElement (xmlNode *node,
						const struct sTagXpathRecurSpec *spec,
						xmlXPathContext *ctx,
						void *userData);
static void relaxngMakeAndFindTagsUnderDefine (xmlNode *node,
					       const struct sTagXpathRecurSpec *spec,
					       xmlXPathContext *ctx,
					       void *userData);

static void relaxngFindTags (xmlNode *node,
			     const struct sTagXpathRecurSpec *spec,
			     xmlXPathContext *ctx,
			     void *userData);


static tagXpathTable relaxngXpathMainTable [] = {
	{ "/*[local-name()='element']",
	  LXPATH_TABLE_DO_RECUR,
	  { .recurSpec = { relaxngMakeAndFindTagsUnderElement, TABLE_PATTERN } }
	},
	{ "/*[local-name()='grammar']",
	  LXPATH_TABLE_DO_RECUR,
	  { .recurSpec = { relaxngFindTags, TABLE_GRAMMAR } }
	},
};

static void makeTagWithScope (xmlNode *node,
			      const struct sTagXpathMakeTagSpec *spec,
			      struct sTagEntryInfo *tag,
			      void *userData);

static tagXpathTable relaxngXpathPatternTable [] = {
	{ "./*[local-name()='element']",
	  LXPATH_TABLE_DO_RECUR,
	  { .recurSpec = { relaxngMakeAndFindTagsUnderElement, TABLE_PATTERN } }
	},
	{ "./*[local-name()='attribute']/@name",
	  LXPATH_TABLE_DO_MAKE,
	  { .makeTagSpec = { K_ATTRIBUTE, ROLE_INDEX_DEFINITION,
			     makeTagWithScope } }
	},
	{ "./*[not(local-name()='element')][not(local-name()='attribute')]",
	  LXPATH_TABLE_DO_RECUR,
	  { .recurSpec = { relaxngFindTags, TABLE_PATTERN } }
	},
};


static void makeTagWithUpdatingScope (xmlNode *node,
				      const struct sTagXpathMakeTagSpec *spec,
				      struct sTagEntryInfo *tag,
				      void *userData);

static tagXpathTable relaxngXpathElementNameTable [] = {
	{ "@name",
	  LXPATH_TABLE_DO_MAKE,
	  { .makeTagSpec = { K_ELEMENT, ROLE_INDEX_DEFINITION,
			     makeTagWithUpdatingScope } }
	},
};

static tagXpathTable relaxngXpathGrammerTable [] = {
	{ "./*[local-name()='start']",
	  LXPATH_TABLE_DO_RECUR,
	  { .recurSpec = { relaxngFindTags, TABLE_PATTERN } }
	},
	{ "./*[local-name()='define']",
	  LXPATH_TABLE_DO_RECUR,
	  { .recurSpec = { relaxngMakeAndFindTagsUnderDefine, TABLE_PATTERN } }
	}
};

static tagXpathTable relaxngXpathDefineNameTable [] = {
	{ "@name",
	  LXPATH_TABLE_DO_MAKE,
	  { .makeTagSpec = { K_NAMED_PATTERN, ROLE_INDEX_DEFINITION,
			     makeTagWithUpdatingScope } }
	},
};

static tagXpathTableTable relaxngXpathTableTable[] = {
	[TABLE_MAIN]         = { ARRAY_AND_SIZE (relaxngXpathMainTable)        },
	[TABLE_ELEMENT_NAME] = { ARRAY_AND_SIZE (relaxngXpathElementNameTable) },
	[TABLE_PATTERN]      = { ARRAY_AND_SIZE (relaxngXpathPatternTable)     },
	[TABLE_GRAMMAR]      = { ARRAY_AND_SIZE (relaxngXpathGrammerTable)     },
	[TABLE_DEFINE_NAME]  = { ARRAY_AND_SIZE (relaxngXpathDefineNameTable)  },
};


static void
relaxngMakeAndFindTags(xmlNode *node,
		       const struct sTagXpathRecurSpec *spec,
		       xmlXPathContext *ctx,
		       int nextTable,
		       void *userData)
{
	int corkIndex = *(int *)userData;

	findXMLTags (ctx, node,
		     relaxngXpathTableTable + nextTable,
		     RelaxNGKinds,
		     &corkIndex);

	relaxngFindTags (node, spec, ctx, &corkIndex);
}

static void
relaxngMakeAndFindTagsUnderElement (xmlNode *node,
				    const struct sTagXpathRecurSpec *spec,
				    xmlXPathContext *ctx,
				    void *userData)
{
	relaxngMakeAndFindTags (node, spec, ctx, TABLE_ELEMENT_NAME, userData);
}

static void
relaxngMakeAndFindTagsUnderDefine (xmlNode *node,
				   const struct sTagXpathRecurSpec *spec,
				   xmlXPathContext *ctx,
				   void *userData)
{
	relaxngMakeAndFindTags (node, spec, ctx, TABLE_DEFINE_NAME, userData);
}

static void
relaxngFindTags (xmlNode *node,
		 const struct sTagXpathRecurSpec *spec,
		 xmlXPathContext *ctx,
		 void *userData)
{
	int corkIndex = *(int *)userData;

	findXMLTags (ctx, node,
		     relaxngXpathTableTable + spec->nextTable,
		     RelaxNGKinds,
		     &corkIndex);
}

static void
setScope (struct sTagEntryInfo *tag, int index)
{
	tag->extensionFields.scopeKind  = NULL;
	tag->extensionFields.scopeName  = NULL;
	tag->extensionFields.scopeIndex = index;

}

static void
makeTagWithUpdatingScope (xmlNode *node,
			  const struct sTagXpathMakeTagSpec *spec,
			  struct sTagEntryInfo *tag,
			  void *userData)
{
	int *corkIndex = userData;


	if (*corkIndex == CORK_NIL)
		/* mark tag as an entry point */
		;
	setScope (tag, *corkIndex);

	*corkIndex = makeTagEntry (tag);
}


static void
findRelaxNGTags (void)
{
	int corkIndex = CORK_NIL;

	findXMLTags (NULL, NULL,
		     relaxngXpathTableTable + TABLE_MAIN,
		     RelaxNGKinds,
		     &corkIndex);
}

static void
makeTagWithScope (xmlNode *node __unused__,
		  const struct sTagXpathMakeTagSpec *spec __unused__,
		  struct sTagEntryInfo *tag,
		  void *userData)
{
	setScope (tag, *(int *)userData);

	makeTagEntry (tag);
}

extern parserDefinition*
RelaxNGParser (void)
{
	static const char *const extensions [] = { "rng", NULL };
	parserDefinition* const def = parserNew ("RelaxNG");
	/* static selectLanguage selectors[] = { selectByDTD, NULL }; */

	def->kinds         = RelaxNGKinds;
	def->kindCount     = ARRAY_SIZE (RelaxNGKinds);
	def->extensions    = extensions;
	def->parser        = findRelaxNGTags;
	def->tagXpathTableTable = relaxngXpathTableTable;
	def->tagXpathTableCount = ARRAY_SIZE (relaxngXpathTableTable);
	def->useCork = TRUE;
	/* def->selectLanguage = selectors; */
	return def;
}
