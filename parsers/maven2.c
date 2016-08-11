/*
 *
 *   Copyright (c) 2016, Masatake YAMATO
 *   Copyright (c) 2016, Red Hat, K.K.
 *
 *   This source code is released for free distribution under the terms of the
 *   GNU General Public License version 2 or (at your option) any later version.
 *
 *   This module contains functions for generating tags for maven2 project model
 *   defined in http://maven.apache.org/POM/4.0.0,
 *              http://maven.apache.org/maven-v4_0_0.xsd.
 */

#include "general.h"	/* must always come first */

#include "debug.h"
#include "options.h"
#include "parse.h"
#include "read.h"
#include "routines.h"
#include "selectors.h"

#ifdef HAVE_LIBXML
#include <libxml/xpath.h>
#include <libxml/tree.h>
#endif

#include <string.h>

typedef enum {
	K_GROUP_ID, K_ARTIFACT_ID, K_PROPERTY, K_REPOSITORY_ID
} maven2Kind;

typedef enum {
	R_GROUP_ID_PARENT,
	R_GROUP_ID_DEPENDENCY,
} maven2GroupIdRole;

typedef enum {
	R_ARTIFACT_ID_PARENT,
	R_ARTIFACT_ID_DEPENDENCY,
} maven2ArtifactIdRole;

static roleDesc Maven2GroupIdRoles [] = {
	{ TRUE, "parent", "parent" },
	{ TRUE, "dependency", "dependency" },
};

static roleDesc Maven2ArtifactIdRoles [] = {
	{ TRUE, "parent", "parent" },
	{ TRUE, "dependency", "dependency" },
};

static kindOption Maven2Kinds [] = {
	{ TRUE,  'g', "groupId",    "group identifiers",
	  .referenceOnly = FALSE, ATTACH_ROLES (Maven2GroupIdRoles) },
	{ TRUE,  'a', "artifactId", "artifact identifiers",
	  .referenceOnly = FALSE, ATTACH_ROLES (Maven2ArtifactIdRoles) },
	{ TRUE,  'p', "property",   "properties" },
	{ TRUE,  'r', "repositoryId", "repository identifiers" },
};

static void makeTagWithScope (xmlNode *node,
			      const struct sTagXpathMakeTagSpec *spec,
			      struct sTagEntryInfo *tag,
			      void *userData);

static void makeTagRecursively (xmlNode *node,
				const struct sTagXpathRecurSpec *spec,
				xmlXPathContext *ctx,
				void *userData);

static void makeTagForProperties (xmlNode *node,
				  const struct sTagXpathRecurSpec *spec __unused__,
				  xmlXPathContext *ctx __unused__,
				  void *userData __unused__)
{
	const xmlChar* str;
	tagEntryInfo tag;

	str = node->name;
	initTagEntry (&tag, (char *)str, Maven2Kinds + K_PROPERTY);
	tag.lineNumber = xmlGetLineNo (node);
	tag.filePosition = getInputFilePositionForLine (tag.lineNumber);

	makeTagEntry (&tag);
}


enum maven2XpathTable {
	TABLE_MAIN,
	TABLE_PARENT,
	TABLE_DEPEDENCY,
};

static tagXpathTable maven2XpathMainTable[] = {
	{ "/*[local-name()='project']/*[local-name()='groupId']",
	  LXPATH_TABLE_DO_MAKE,
	  { .makeTagSpec = { K_GROUP_ID, ROLE_INDEX_DEFINITION,
			     makeTagWithScope } }
	},
	{ "/*[local-name()='project']/*[local-name()='parent']",
	  LXPATH_TABLE_DO_RECUR,
	  { .recurSpec = { makeTagRecursively, TABLE_PARENT } }
	},
	{ "/*[local-name()='project']/*[local-name()='dependencies']/*[local-name()='dependency']",
	  LXPATH_TABLE_DO_RECUR,
	  { .recurSpec = { makeTagRecursively, TABLE_DEPEDENCY } }
	},
	{ "/*[local-name()='project']/*[local-name()='artifactId']",
	  LXPATH_TABLE_DO_MAKE,
	  { .makeTagSpec = { K_ARTIFACT_ID, ROLE_INDEX_DEFINITION,
			     makeTagWithScope } }
	},
	{ "/*[local-name()='project']/*[local-name()='properties']/*",
	  LXPATH_TABLE_DO_RECUR,
	  { .recurSpec = { makeTagForProperties } }
	},
	{ "/*[local-name()='project']/*[local-name()='repositories']/*[local-name()='repository']/*[local-name()='id']",
	  LXPATH_TABLE_DO_MAKE,
	  { .makeTagSpec = { K_REPOSITORY_ID, ROLE_INDEX_DEFINITION, } }
	},
};

static tagXpathTable maven2XpathParentTable[] = {
	{ "./*[local-name()='groupId']",
	  LXPATH_TABLE_DO_MAKE,
	  { .makeTagSpec = { K_GROUP_ID, R_GROUP_ID_PARENT,
			     makeTagWithScope } }
	},
	{ "./*[local-name()='artifactId']",
	  LXPATH_TABLE_DO_MAKE,
	  { .makeTagSpec = { K_ARTIFACT_ID, R_ARTIFACT_ID_PARENT,
			     makeTagWithScope } }
	},
};

static tagXpathTable maven2XpathDependencyTable[] = {
	{ "./*[local-name()='groupId']",
	  LXPATH_TABLE_DO_MAKE,
	  { .makeTagSpec = { K_GROUP_ID, R_GROUP_ID_DEPENDENCY,
			     makeTagWithScope } }
	},
	{ "./*[local-name()='artifactId']",
	  LXPATH_TABLE_DO_MAKE,
	  { .makeTagSpec = { K_ARTIFACT_ID, R_ARTIFACT_ID_DEPENDENCY,
			     makeTagWithScope } }
	},
};

static tagXpathTableTable maven2XpathTableTable[] = {
	[TABLE_MAIN] = { ARRAY_AND_SIZE(maven2XpathMainTable) },
	[TABLE_PARENT] = { ARRAY_AND_SIZE(maven2XpathParentTable) },
	[TABLE_DEPEDENCY] = { ARRAY_AND_SIZE(maven2XpathDependencyTable) },
};

typedef enum {
	F_VERSION,
} maven2Field;

static fieldSpec Maven2Fields [] = {
	{
		.name = "version",
		.description = "version of artifact",
		.enabled = FALSE,
	}
};

static char* attachVersionIfExisting (struct sTagEntryInfo *tag, xmlNode *node)
{
	char *version = NULL;

#ifdef HAVE_LIBXML
	for (node = node->next; node != NULL; node = node->next)
	{
		if (strcmp ((char *)node->name, "version") == 0)
		{
			version = (char *)xmlNodeGetContent (node);
			break;
		}
	}
#endif
	if (version)
		attachParserField (tag, Maven2Fields [F_VERSION].ftype, version);
	return version;
}

static void makeTagWithScope (xmlNode *node,
			      const struct sTagXpathMakeTagSpec *spec,
			      struct sTagEntryInfo *tag,
			      void *userData)
{
	int *corkIndexes = userData;
	int i;
	char* version = NULL;

	if (tag->kind == Maven2Kinds + K_ARTIFACT_ID)
		version = attachVersionIfExisting (tag, node);

	i = makeTagEntry (tag);

	if (version)
		xmlFree (version);

	if ((tag->kind == Maven2Kinds + K_GROUP_ID)
	    || (tag->kind == Maven2Kinds + K_ARTIFACT_ID))
		corkIndexes [spec->kind] = i;
}

static void
findMaven2TagsForTable (enum maven2XpathTable tindex,
			xmlNode *node,
			xmlXPathContext *ctx)
{
	int corkIndexes [] = {
		[K_GROUP_ID]    = CORK_NIL,
		[K_ARTIFACT_ID] = CORK_NIL,
	};

	findXMLTags (ctx, node,
		     maven2XpathTableTable + tindex,
		     Maven2Kinds,
		     &corkIndexes);

	if ( corkIndexes [K_ARTIFACT_ID] != CORK_NIL
	     && corkIndexes [K_GROUP_ID] != CORK_NIL)
	{
		tagEntryInfo *tag = getEntryInCorkQueue (corkIndexes [K_ARTIFACT_ID]);
		tag->extensionFields.scopeIndex = corkIndexes [K_GROUP_ID];
	}
}

static void makeTagRecursively (xmlNode *node,
				  const struct sTagXpathRecurSpec *spec,
				  xmlXPathContext *ctx,
				  void *userData __unused__)
{
	findMaven2TagsForTable (spec->nextTable, node, ctx);
}

static void
findMaven2Tags (void)
{
	findMaven2TagsForTable (TABLE_MAIN, NULL, NULL);
}

extern parserDefinition*
Maven2Parser (void)
{
	static const char *const extensions [] = { "pom", NULL };
	static const char *const patterns [] =   { "pom.xml", NULL };
	parserDefinition* const def = parserNew ("Maven2");
	static selectLanguage selectors[] = { selectByDTD, NULL };

	def->kinds         = Maven2Kinds;
	def->kindCount     = ARRAY_SIZE (Maven2Kinds);
	def->extensions    = extensions;
	def->patterns      = patterns;
	def->parser        = findMaven2Tags;
	def->tagXpathTableTable  = maven2XpathTableTable;
	def->tagXpathTableCount  = ARRAY_SIZE (maven2XpathTableTable);
	def->useCork = TRUE;
	def->selectLanguage = selectors;
	def->fieldSpecs = Maven2Fields;
	def->fieldSpecCount = ARRAY_SIZE (Maven2Fields);
	return def;
}
