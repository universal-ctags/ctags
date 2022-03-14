/*
 *
 *  Copyright (c) 2007-2011, Nick Treleaven
 *  Copyright (c) 2012, Lex Trotman
 *  Copyright (c) 2021, Jiri Techet
 *
 *   This source code is released for free distribution under the terms of the
 *   GNU General Public License version 2 or (at your option) any later version.
 *
 * This module contains functions for generating tags for markdown files.
 *
 * This parser was based on the asciidoc parser.
 *
 * Extended syntax like footnotes is described in
 * https://www.markdownguide.org/extended-syntax/
 */

/*
 *   INCLUDE FILES
 */
#include "general.h"	/* must always come first */

#include <ctype.h>
#include <string.h>

#include "debug.h"
#include "entry.h"
#include "parse.h"
#include "read.h"
#include "vstring.h"
#include "nestlevel.h"
#include "routines.h"
#include "promise.h"
#include "htable.h"

#include "markdown.h"

/*
 *   DATA DEFINITIONS
 */
typedef enum {
	K_CHAPTER = 0,
	K_SECTION,
	K_SUBSECTION,
	K_SUBSUBSECTION,
	K_LEVEL4SECTION,
	K_LEVEL5SECTION,
	K_SECTION_COUNT,
	K_FOOTNOTE = K_SECTION_COUNT,
} markdownKind;

static kindDefinition MarkdownKinds[] = {
	{ true, 'c', "chapter",       "chapters"},
	{ true, 's', "section",       "sections" },
	{ true, 'S', "subsection",    "level 2 sections" },
	{ true, 't', "subsubsection", "level 3 sections" },
	{ true, 'T', "l4subsection",  "level 4 sections" },
	{ true, 'u', "l5subsection",  "level 5 sections" },
	{ true, 'n', "footnote",      "footnotes" },
};

static fieldDefinition MarkdownFields [] = {
	{
	  .enabled     = false,
	  .name        = "sectionMarker",
	  .description = "character used for declaring section(#, ##, =, or -)",
	},
};

typedef enum {
	F_MARKER,
} markdownField;

static NestingLevels *nestingLevels = NULL;

/*
*   FUNCTION DEFINITIONS
*/

static NestingLevel *getNestingLevel (const int kind, unsigned long adjustmentWhenPop)
{
	NestingLevel *nl;
	tagEntryInfo *e;
	unsigned long line = getInputLineNumber ();

	line = (line > adjustmentWhenPop)? (line - adjustmentWhenPop): 0;

	while (1)
	{
		nl = nestingLevelsGetCurrent (nestingLevels);
		e = getEntryOfNestingLevel (nl);
		if ((nl && (e == NULL)) || (e && (e->kindIndex >= kind)))
			nestingLevelsPopFull (nestingLevels, HT_UINT_TO_PTR ((unsigned int)line));
		else
			break;
	}
	return nl;
}


static int makeMarkdownTag (const vString* const name, const int kind, const bool twoLine)
{
	int r = CORK_NIL;

	if (vStringLength (name) > 0)
	{
		const NestingLevel *const nl = getNestingLevel (kind, twoLine? 2: 1);
		tagEntryInfo *parent = getEntryOfNestingLevel (nl);
		tagEntryInfo e;

		initTagEntry (&e, vStringValue (name), kind);

		if (twoLine)
		{
			/* we want the line before the '---' underline chars */
			const unsigned long line = getInputLineNumber ();
			Assert (line > 0);
			if (line > 0)
			{
				e.lineNumber--;
				e.filePosition = getInputFilePositionForLine (line - 1);
			}
		}

		if (parent && (parent->kindIndex < kind))
			e.extensionFields.scopeIndex = nl->corkIndex;

		r = makeTagEntry (&e);
	}
	return r;
}


static int makeSectionMarkdownTag (const vString* const name, const int kind, const char *marker)
{
	int r = makeMarkdownTag (name, kind, marker[0] != '#');
	attachParserFieldToCorkEntry (r, MarkdownFields [F_MARKER].ftype, marker);

	nestingLevelsPush (nestingLevels, r);
	return r;
}


static vString *getHeading (const int kind, const unsigned char *line,
	const int lineLen, bool *delimited)
{
	int pos = 0;
	int start = kind + 1;
	int end = lineLen - 1;
	vString *name = vStringNew ();

	Assert (kind >= 0 && kind < K_SECTION_COUNT);
	Assert (lineLen > start);

	*delimited = false;
	while (isspace (line[pos])) ++pos;
	while (line[end] == line[pos] && end - 1 >= 0 && line[end - 1] != '\\')
	{
		--end;
		*delimited = true;
	}
	while (isspace (line[start])) ++start;
	while (isspace (line[end])) --end;

	if (start <= end)
		vStringNCatS (name, (const char*)(&(line[start])), end - start + 1);

	return name;
}


static int getFirstCharPos (const unsigned char *line, int lineLen, bool *indented)
{
	int indent = 0;
	int i;
	for (i = 0; i < lineLen && isspace (line[i]); i++)
		indent += line[i] == '\t' ? 4 : 1;
	*indented = indent >= 4;
	return i;
}


static void fillEndField (NestingLevel *nl, void *ctxData)
{
	tagEntryInfo *e = getEntryOfNestingLevel (nl);
	if (e)
	{
		unsigned long line = (unsigned long)(HT_PTR_TO_UINT (ctxData));
		e->extensionFields.endLine = line;
	}
}

static void getFootnoteMaybe (const char *line)
{
	const char *start = strstr (line, "[^");
	const char *end = start? strstr(start + 2, "]:"): NULL;

	if (! (start && end))
		return;
	if (! (end > (start + 2)))
		return;

	vString * footnote = vStringNewNInit (start + 2, end - (start + 2));
	const NestingLevel *const nl = nestingLevelsGetCurrent (nestingLevels);
	tagEntryInfo e;

	initTagEntry (&e, vStringValue (footnote), K_FOOTNOTE);
	if (nl)
		e.extensionFields.scopeIndex = nl->corkIndex;
	makeTagEntry (&e);

	vStringDelete (footnote);
}

static bool extractLanguageForCodeBlock (const char *langMarker,
										 vString *codeLang)
{
	subparser *s;
	bool r = false;

	foreachSubparser (s, false)
	{
		markdownSubparser *m = (markdownSubparser *)s;
		enterSubparser(s);
		if (m->extractLanguageForCodeBlock)
			r = m->extractLanguageForCodeBlock (m, langMarker, codeLang);
		leaveSubparser();
		if (r)
			break;
	}

	return r;
}

static void findMarkdownTags (void)
{
	vString *prevLine = vStringNew ();
	vString *codeLang = vStringNew ();
	const unsigned char *line;
	char inCodeChar = 0;
	long startSourceLineNumber = 0;
	long startLineNumber = 0;
	bool inPreambule = false;
	bool inComment = false;

	subparser *sub = getSubparserRunningBaseparser();
	if (sub)
		chooseExclusiveSubparser (sub, NULL);

	nestingLevels = nestingLevelsNewFull (0, fillEndField);

	while ((line = readLineFromInputFile ()) != NULL)
	{
		int lineLen = strlen ((const char*) line);
		bool lineProcessed = false;
		bool indented;
		int pos = getFirstCharPos (line, lineLen, &indented);
		const int lineNum = getInputLineNumber ();

		if (lineNum == 1 || inPreambule)
		{
			if (line[pos] == '-' && line[pos + 1] == '-' && line[pos + 2] == '-')
				inPreambule = !inPreambule;
		}

		if (inPreambule)
			continue;

		/* fenced code block */
		if (line[pos] == '`' || line[pos] == '~')
		{
			char c = line[pos];
			char otherC = c == '`' ? '~' : '`';
			int nSame;
			for (nSame = 1; line[nSame] == line[pos]; ++nSame);

			if (inCodeChar != otherC && nSame >= 3)
			{
				inCodeChar = inCodeChar ? 0 : c;
				if (inCodeChar == c && strstr ((const char *)(line + pos + nSame), "```") != NULL)
					inCodeChar = 0;
				else if (inCodeChar)
				{
					const char *langMarker = (const char *)(line + pos + nSame);
					startLineNumber = startSourceLineNumber = lineNum + 1;

					vStringClear (codeLang);
					if (! extractLanguageForCodeBlock (langMarker, codeLang))
					{
						vStringCopyS (codeLang, langMarker);
						vStringStripLeading (codeLang);
						vStringStripTrailing (codeLang);
					}
				}
				else
				{
					long endLineNumber = lineNum;
					if (vStringLength (codeLang) > 0
						&& startLineNumber < endLineNumber)
						makePromise (vStringValue (codeLang), startLineNumber, 0,
							endLineNumber, 0, startSourceLineNumber);
				}

				lineProcessed = true;
			}
		}
		/* XML comment start */
		else if (lineLen >= pos + 4 && line[pos] == '<' && line[pos + 1] == '!' &&
			line[pos + 2] == '-' && line[pos + 3] == '-')
		{
			if (strstr ((const char *)(line + pos + 4), "-->") == NULL)
				inComment = true;
			lineProcessed = true;
		}
		/* XML comment end */
		else if (inComment && strstr ((const char *)(line + pos), "-->"))
		{
			inComment = false;
			lineProcessed = true;
		}

		/* code block or comment */
		if (inCodeChar || inComment)
			lineProcessed = true;

		/* code block using indent */
		else if (indented)
			lineProcessed = true;

		/* if it's a title underline, or a delimited block marking character */
		else if (line[pos] == '=' || line[pos] == '-' || line[pos] == '#' || line[pos] == '>')
		{
			int nSame;
			for (nSame = 1; line[nSame] == line[pos]; ++nSame);

			/* quote */
			if (line[pos] == '>')
				;  /* just to make sure lineProcessed = true so it won't be in a heading */
			/* is it a two line title */
			else if (line[pos] == '=' || line[pos] == '-')
			{
				char marker[2] = { line[pos], '\0' };
				int kind = line[pos] == '=' ? K_CHAPTER : K_SECTION;
				bool whitespaceTerminated = true;

				for (int i = pos + nSame; i < lineLen; i++)
				{
					if (!isspace (line[i]))
					{
						whitespaceTerminated = false;
						break;
					}
				}

				vStringStripLeading (prevLine);
				vStringStripTrailing (prevLine);
				if (whitespaceTerminated && vStringLength (prevLine) > 0)
					makeSectionMarkdownTag (prevLine, kind, marker);
			}
			/* otherwise is it a one line title */
			else if (line[pos] == '#' && nSame <= K_SECTION_COUNT && isspace (line[nSame]))
			{
				int kind = nSame - 1;
				bool delimited = false;
				vString *name = getHeading (kind, line, lineLen, &delimited);
				if (vStringLength (name) > 0)
					makeSectionMarkdownTag (name, kind, delimited ? "##" : "#");
				vStringDelete (name);
			}

			lineProcessed = true;
		}

		vStringClear (prevLine);
		if (!lineProcessed)
		{
			getFootnoteMaybe ((const char *)line);
			vStringCatS (prevLine, (const char*) line);
		}
	}
	vStringDelete (prevLine);
	vStringDelete (codeLang);
	{
		unsigned int line = (unsigned int)getInputLineNumber ();
		nestingLevelsFreeFull (nestingLevels, HT_UINT_TO_PTR (line));
	}
}

extern parserDefinition* MarkdownParser (void)
{
	parserDefinition* const def = parserNew ("Markdown");
	static const char *const extensions [] = { "md", "markdown", NULL };

	def->enabled  = true;
	def->extensions = extensions;
	def->useCork = CORK_QUEUE;
	def->kindTable = MarkdownKinds;
	def->kindCount = ARRAY_SIZE (MarkdownKinds);
	def->fieldTable = MarkdownFields;
	def->fieldCount = ARRAY_SIZE (MarkdownFields);
	def->defaultScopeSeparator = "\"\"";
	def->parser = findMarkdownTags;

	return def;
}
