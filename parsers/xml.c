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
#include "subparser.h"
#include "xml.h"

static void makeTagWithNotification (xmlNode *node,
									 const char *xpath,
									 const tagXpathMakeTagSpec *spec,
									 tagEntryInfo *tag,
									 void *userData);

typedef enum {
	K_ID,
	K_NSPREFIX,
	K_ROOT,
} xmlKind;

#define ROOT_ELT_LETTER 'r'
static const char ROOT_ELT_LETTER_STR[2] = {ROOT_ELT_LETTER, '\0'};
static kindDefinition XmlKinds [] = {
	{ true, 'i', "id", "id attributes" },
	{ true, 'n', "nsprefix", "namespace prefixes" },
	{ true, ROOT_ELT_LETTER, "root", "root elements" },
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
	  { .makeTagSpec = { K_ID, ROLE_DEFINITION_INDEX,
						 .make = makeTagWithNotification,} }
	},
};

static tagXpathTableTable xmlXpathTableTable[] = {
	[TABLE_MAIN] = { ARRAY_AND_SIZE (XmlXpathMainTable) },
	[TABLE_ID]   = { ARRAY_AND_SIZE (XmlXpathIdTable) },
};

/* Pick up the root element specifier from "<!DOCTYPE...", and
 * run DTD parser for the "[" ... "]>" area.
 *
 * optlib2c translates the following .ctags elements:
 * ==========================================================
 * --langdef=Xml
 * --kinddef-Xml=r,root,root elements
 * --mline-regex-Xml=/<!DOCTYPE[[:space:]]+([a-zA-Z0-9]+)[[:space:]]+[^[]+\[((.|[\n])+)\]>/\1/r/{mgroup=1}{_guest=DTD,2start,2end}
 * ==========================================================
 */
static tagRegexTable XmlTagRegexTable [] = {
	{"<!DOCTYPE[[:space:]]+([a-zA-Z0-9]+)[[:space:]]+[^[]*\\[((.|[\n])+)\\]>", "\\1",
	 ROOT_ELT_LETTER_STR, "{mgroup=1}{_guest=DTD,2start,2end}", NULL, true},
};

static int makeTagWithNotificationCommon (tagEntryInfo *tag,
										   xmlNode *node)
{
	int n = makeTagEntry (tag);

	subparser *sub;
	foreachSubparser (sub, false)
	{
		xmlSubparser *xmlsub = (xmlSubparser *)sub;

		if (xmlsub->makeTagEntryWithNodeNotify)
		{
			enterSubparser(sub);
			xmlsub->makeTagEntryWithNodeNotify (xmlsub, node, tag);
			leaveSubparser();
		}
	}
	return n;
}

static void makeTagWithNotification (xmlNode *node,
									 const char *xpath CTAGS_ATTR_UNUSED,
									 const tagXpathMakeTagSpec *spec CTAGS_ATTR_UNUSED,
									 tagEntryInfo *tag,
									 void *userData CTAGS_ATTR_UNUSED)
{
	makeTagWithNotificationCommon (tag, node);
}

static int makeNsPrefixTag (const char *name, xmlNode *node, xmlNsPtr ns)
{
	int n = CORK_NIL;
	tagEntryInfo tag;
	vString *anon = NULL;

	if (name)
		initTagEntry (&tag, name, K_NSPREFIX);
	else
	{
		anon = anonGenerateNew ("ns", K_NSPREFIX);
		initTagEntry (&tag, vStringValue (anon), K_NSPREFIX);
		markTagExtraBit (&tag, XTAG_ANONYMOUS);
	}
	/* TODO
	 * - move this code block to lxpath.c.
	 * - adjust the line number for nsprefixes forward. */
	tag.lineNumber = XML_GET_LINE (node);
	tag.filePosition = getInputFilePositionForLine (tag.lineNumber);
	char *p = (char *)xmlGetNodePath (node);
	if (ns->href && *ns->href)
		attachParserField (&tag, false, XmlFields [F_NS_URI].ftype, (char *)ns->href);

	n = makeTagWithNotificationCommon (&tag, node);
	if (p)
		xmlFree (p);
	if (anon)
		vStringDelete (anon);

	return n;
}

static void findNsPrefix (xmlNode *node,
					   const char *xpath,
					   const struct sTagXpathRecurSpec *spec,
					   xmlXPathContext *ctx,
					   void *userData)
{
	for (xmlNsPtr ns = node->nsDef; ns; ns = ns->next)
		makeNsPrefixTag ((char *)ns->prefix, node, ns);

	findXMLTags (ctx, node, spec->nextTable, userData);
}

static void runAfter (xmlXPathContext *ctx, xmlNode *root, void *user_data)
{
	subparser *sub;
	foreachSubparser (sub, false)
	{
		xmlSubparser *xmlsub = (xmlSubparser *)sub;
		if (xmlsub->runXPathEngine)
		{
			enterSubparser(sub);
			xmlsub->runXPathEngine (xmlsub, ctx, root);
			leaveSubparser();
		}
	}
}

static void
findXmlTags (void)
{
	findXMLTagsFull (NULL, NULL, TABLE_MAIN, runAfter, NULL);
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
	def->tagRegexTable = XmlTagRegexTable;
	def->tagRegexCount = ARRAY_SIZE(XmlTagRegexTable);

	return def;
}
