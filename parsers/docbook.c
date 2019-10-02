/*
*
*   Copyright (c) 2019, Masatake YAMATO
*   Copyright (c) 2019, Red Hat, K.K.
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for generating tags for docbook XML.
*
*   References
*
*   * https://tdg.docbook.org/tdg/5.2/
*   * https://docbook.org/xml/5.1/
*   * https://docbook.org/xml/5.0.1/dtd/docbook.dtd
*
*   Design note
*
*   Usually, for the languages for documentation, Universal-ctags captures titles of
*   hierarchy structure like "chapter", "section", etc. This approach works well
*   for Markdown, ReStructuredText, and so on. Each of the parsers defines kinds
*   for the members of hierarchy structure:
*
*   $ u-ctags --list-kinds-full=ReStructuredText
*   #LETTER NAME          ENABLED REFONLY NROLES MASTER DESCRIPTION
*   S       subsection    yes     no      0      NONE   subsections
*   T       target        yes     no      0      NONE   targets
*   c       chapter       yes     no      0      NONE   chapters
*   s       section       yes     no      0      NONE   sections
*   t       subsubsection yes     no      0      NONE   subsubsections
*
*   This approach is not applied to this parser because the docbook format
*   has very complicated hierarchy structure. We have to define more than
*   90 kinds to apply the approach. The tags format can define only 51
*   ( = 26 * 2 - 1 ) kinds.
*
*   This parser captures the text in <title>...</title> with t/title kind.
*   The kind doesn't help users to know the context of a title in
*   the hierarchy structure of input.
*   One common field and two language specific fields tell the context instead.
*
*
*   xpath (common)
*       the xpath for the <title/> node.

*   parentTag (DocBook parser specific)
*       the XML tag of the parent node of the <title/> node.
*
*   parentId (DocBook parser specific)
*       the XML id attribute of the parent node of the <title/> node.
*
*
*   parentId field is printed only if the parent node has an id
*   attribute.
*
*   Let's see an example.
*
*   input.xml
*   --------------------------------------------------------------------
*   <book id="root">
*      <title>Universal-ctags</title>
*   ...
*   --------------------------------------------------------------------
*
*   For the input, this parser and the XML parser emit:
*   --------------------------------------------------------------------
*   Universal-ctags	input.xml	2;"	t	language:DocBook	parentTag:book	parentId:root
*   root	input.xml	1;"	i	language:XML
*   --------------------------------------------------------------------
*
*   We considered using scope kind instead of the parser specific tags like
*
*   scope:<parentTag>:<parentId>
*
*   However, it doens't fit well in following aspect:
*   A. <parentId> can be empty when the parent node has no id attribute,
*   B. both <parentTag> and <parentId> can contain colon chapters, and
*   C. <parentTag> is not a kind name defined in DocBook parser.
*
*   NOTE about B: the currently XML parser doesn't handle XML namespace well.
*   NOTE about C: I'm thinking about introducing new rule about the scope kind
*                 that the first sub field should be a kind name.
*/

/*
*   INCLUDE FILES
*/

#include "general.h"	/* must always come first */
#include "entry.h"
#include "debug.h"
#include "htable.h"
#include "parse.h"
#include "read.h"
#include "routines.h"
#include "selectors.h"
#include "xml.h"

/*
*   MACROS
*/


/*
*   DATA DECLARATIONS
*/

typedef enum {
	K_TITLE,
} docBookKind;

typedef enum {
	F_TAG,
	F_PARENT_ID,
} docBookField;

enum docBookXpathTable {
	TABLE_MAIN,
};

/*
*   FUNCTION PROTOTYPES
*/

static void makeTagWithXMLTag (xmlNode *node,
							   const char *xpath,
							   const struct sTagXpathMakeTagSpec *spec,
							   struct sTagEntryInfo *tag,
							   void *userData);

/*
*   DATA DEFINITIONS
*/

static kindDefinition DocBookKinds [] = {
	{ true, 't', "title", "titles" },
};

static fieldDefinition DocBookFields[] = {
	{ .name = "parentTag",
	  .description = "the name of the upper level XML tag having <title>...</title>",
	  .enabled = true,
	},
	{ .name = "parentId",
	  .description = "id given to the parent node",
	  .enabled = true,
	},
};

static tagXpathTable docBookXpathMainTable [] = {
	{ "//*[local-name()=\"title\"]",
	  LXPATH_TABLE_DO_MAKE,
	  { .makeTagSpec = { K_TITLE, ROLE_DEFINITION_INDEX, makeTagWithXMLTag} }
	},
};

static tagXpathTableTable docBookXpathTableTable[] = {
	[TABLE_MAIN] = { ARRAY_AND_SIZE (docBookXpathMainTable) },
};

static hashTable *node2id;


/*
*   FUNCTION DEFINITIONS
*/

static void makeTagWithXMLTag (xmlNode *node,
							   const char *xpath,
							   const struct sTagXpathMakeTagSpec *spec,
							   struct sTagEntryInfo *tag,
							   void *userData)
{
	if (node->type == XML_ELEMENT_NODE
		&& node->parent
		&& node->parent->type == XML_ELEMENT_NODE)
	{
		attachParserField (tag, DocBookFields[F_TAG].ftype,
						   (char *)node->parent->name);

		void *parent_id = hashTableGetItem (node2id, node->parent);
		if (parent_id)
		{
			tagEntryInfo *parent_tag  = getEntryInCorkQueue (HT_PTR_TO_INT(parent_id));
			if (parent_tag)
			{
				attachParserField (tag, DocBookFields[F_PARENT_ID].ftype,
								   (char *)parent_tag->name);
			}
		}
	}
	makeTagEntry (tag);
}

static void
findDocBookTags (void)
{
	Assert (node2id);
	hashTableClear (node2id);
	scheduleRunningBaseparser (RUN_DEFAULT_SUBPARSERS);
}


static void
runXPathEngine(xmlSubparser *s,
			   xmlXPathContext *ctx, xmlNode *root)
{
	findXMLTags (ctx, root, TABLE_MAIN, NULL);
}

static void makeTagEntryWithNodeNotify (xmlSubparser *s,
										xmlNode *node, int corKIndex)
{
	tagEntryInfo *e = getEntryInCorkQueue (corKIndex);
	Assert (e);

	if (e->kindIndex == KIND_XML_ID
		&& node->type == XML_ATTRIBUTE_NODE
		&& node->parent)
	{
		hashTablePutItem (node2id, node->parent, HT_INT_TO_PTR (corKIndex));
	}
}

static xmlSubparser docBookSubparser = {
	.subparser = {
		.direction = SUBPARSER_BI_DIRECTION,
	},
	.makeTagEntryWithNodeNotify = makeTagEntryWithNodeNotify,
	.runXPathEngine = runXPathEngine,
};

static void initialize (const langType language)
{
	node2id = hashTableNew (31, hashPtrhash, hashPtreq,
							NULL, NULL);
}

static void finalize (langType language CTAGS_ATTR_UNUSED, bool initialized)
{
	if (!initialized)
		return;

	hashTableDelete (node2id);
}

extern parserDefinition*
DocBookParser (void)
{
	static const char *const extensions [] = { "xml", "dbk", "docbook", NULL };
	parserDefinition* const def = parserNew ("DocBook");
	static selectLanguage selectors[] = { selectByXpathFileSpec, NULL };

	static xpathFileSpec xpathFileSpecs[] = {
		/*
		 * TODO
		 * - Implement pattern matching mechanism for defining specs, and
		 * - Provide the way to give specs from command line (--xmlmap-<LANG>=[+|-]spec).
		 */
#define docbookx(V) { .systemID = "http://www.oasis-open.org/docbook/xml/" #V "/docbookx.dtd", }
		docbookx(4.0),
		docbookx(4.1.2),
		docbookx(4.2),
		docbookx(4.3b),
		docbookx(4.3),
		docbookx(4.4),
		docbookx(4.5),
		{
			.systemID = "http://www.docbook.org/xml/4.4/docbookx.dtd",
		},
		{
			.systemID = "docbook.dtd",
		},
#define oasis(V) { .externalID = "-//OASIS//DTD DocBook V" #V "//EN", }
		oasis(3.1),
		oasis(4.0),
		oasis(4.1),
		oasis(4.5),
#define oasisx(V) { .externalID = "-//OASIS//DTD DocBook XML V" #V "//EN", }
		oasisx(4.0),
		oasisx(4.1.2),
		oasisx(4.2),
		oasisx(4.3RC),
		oasisx(4.3),
		oasisx(4.4),
		oasisx(4.5),
#if 0
		{
			.externalID = "-//GNOME//DTD DocBook PNG Variant V1.1//EN",
		}
#defie kde(V0,V1) { .externalID = "-//KDE//DTD DocBook XML V" #V0 "-Based Variant V" #v1 "//EN", }
		kde(4.1.2, 1.0),
		kde(4.1.2, 1.1),
		kde(4.1, 1.0),
		kde(4.2, 1.1),
		kde(4.5, 1.1),
#endif
	};
	static parserDependency dependencies [] = {
		[0] = { DEPTYPE_SUBPARSER, "XML", &docBookSubparser },
	};
	def->kindTable = DocBookKinds;
	def->kindCount = ARRAY_SIZE (DocBookKinds);
	def->extensions = extensions;
	def->parser = findDocBookTags;
	def->tagXpathTableTable = docBookXpathTableTable;
	def->tagXpathTableCount = ARRAY_SIZE (docBookXpathTableTable);
	def->useCork = true;
	def->selectLanguage = selectors;
	def->xpathFileSpecs = xpathFileSpecs;
	def->xpathFileSpecCount = ARRAY_SIZE (xpathFileSpecs);
	def->dependencies = dependencies;
	def->dependencyCount = ARRAY_SIZE (dependencies);
	def->fieldTable = DocBookFields;
	def->fieldCount = ARRAY_SIZE (DocBookFields);
	def->initialize = initialize;
	def->finalize = finalize;

	return def;
}
