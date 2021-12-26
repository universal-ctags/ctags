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
} markdownKind;

static kindDefinition MarkdownKinds[] = {
	{ true, 'c', "chapter",       "chapters"},
	{ true, 's', "section",       "sections" },
	{ true, 'S', "subsection",    "level 2 sections" },
	{ true, 't', "subsubsection", "level 3 sections" },
	{ true, 'T', "l4subsection",  "level 4 sections" },
	{ true, 'u', "l5subsection",  "level 5 sections" },
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

static NestingLevel *getNestingLevel(const int kind, unsigned long adjustment_when_pop)
{
	NestingLevel *nl;
	tagEntryInfo *e;
	unsigned long line = getInputLineNumber();

	line = (line > adjustment_when_pop)? (line - adjustment_when_pop): 0;

	while (1)
	{
		nl = nestingLevelsGetCurrent(nestingLevels);
		e = getEntryOfNestingLevel (nl);
		if ((nl && (e == NULL)) || (e && (e->kindIndex >= kind)))
			nestingLevelsPopFull(nestingLevels, HT_UINT_TO_PTR((unsigned int)line));
		else
			break;
	}
	return nl;
}


static int makeMarkdownTag (const vString* const name, const int kind, const bool two_line)
{
	int r = CORK_NIL;

	if (vStringLength (name) > 0)
	{
		const NestingLevel *const nl = getNestingLevel(kind, two_line? 2: 1);
		tagEntryInfo *parent = getEntryOfNestingLevel (nl);
		tagEntryInfo e;

		initTagEntry (&e, vStringValue (name), kind);

		if (two_line)
		{
			/* we want the line before the '---' underline chars */
			const unsigned long line = getInputLineNumber();
			Assert (line > 0);
			if (line > 0)
			{
				e.lineNumber--;
				e.filePosition = getInputFilePositionForLine(line - 1);
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
	int r = makeMarkdownTag(name, kind, marker[0] != '#');
	attachParserFieldToCorkEntry (r, MarkdownFields [F_MARKER].ftype, marker);

	nestingLevelsPush(nestingLevels, r);
	return r;
}


static vString *get_heading(const int kind, const unsigned char *line,
	const int line_len, bool *delimited)
{
	int pos = 0;
	int start = kind + 1;
	int end = line_len - 1;
	vString *name = vStringNew();

	Assert (kind >= 0 && kind < K_SECTION_COUNT);
	Assert (line_len > start);

	*delimited = false;
	while (isspace(line[pos])) ++pos;
	while (line[end] == line[pos] && end - 1 >= 0 && line[end - 1] != '\\')
	{
		--end;
		*delimited = true;
	}
	while (isspace(line[start])) ++start;
	while (isspace(line[end])) --end;

	if (start <= end)
		vStringNCatS(name, (const char*)(&(line[start])), end - start + 1);

	return name;
}


static int get_first_char_pos(const unsigned char *line, int line_len, bool *indented)
{
	int indent = 0;
	int i;
	for (i = 0; i < line_len && isspace(line[i]); i++)
		indent += line[i] == '\t' ? 4 : 1;
	*indented = indent >= 4;
	return i;
}


static void fillEndField (NestingLevel *nl, void *ctxData)
{
	tagEntryInfo *e = getEntryOfNestingLevel (nl);
	if (e)
	{
		unsigned long line = (unsigned long)(HT_PTR_TO_UINT(ctxData));
		e->extensionFields.endLine = line;
	}
}


static void findMarkdownTags(void)
{
	vString *prev_line = vStringNew();
	vString *codeLang = vStringNew();
	const unsigned char *line;
	char in_code_char = 0;
	long startSourceLineNumber = 0;
	long startLineNumber = 0;
	bool inPreambule = false;

	nestingLevels = nestingLevelsNewFull(0, fillEndField);

	while ((line = readLineFromInputFile()) != NULL)
	{
		int line_len = strlen((const char*) line);
		bool line_processed = false;
		bool indented;
		int pos = get_first_char_pos(line, line_len, &indented);
		int lineNum = getInputLineNumber ();

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
			char other_c = c == '`' ? '~' : '`';
			int n_same;
			for (n_same = 1; line[n_same] == line[pos]; ++n_same);

			if (in_code_char != other_c && n_same >= 3)
			{
				in_code_char = in_code_char ? 0 : c;
				if (in_code_char == c && strstr((const char *)(line + pos + n_same), "```") != NULL)
					in_code_char = 0;
				else if (in_code_char)
				{
					startSourceLineNumber = getSourceLineNumber ();
					startLineNumber = getInputLineNumber ();
					vStringClear(codeLang);
					vStringCatS(codeLang, (const char *)(line + pos + n_same));
					vStringStripLeading(codeLang);
					vStringStripTrailing(codeLang);
				}
				else
				{
					long endLineNumber = getInputLineNumber () - 1;
					if (codeLang->size > 0)
						makePromise (vStringValue(codeLang), startLineNumber, 0,
							endLineNumber, 0, startSourceLineNumber);
				}

				line_processed = true;
			}
		}

		/* code block */
		if (in_code_char)
			line_processed = true;

		/* code block using indent */
		else if (indented)
			line_processed = true;

		/* if it's a title underline, or a delimited block marking character */
		else if (line[pos] == '=' || line[pos] == '-' || line[pos] == '#' || line[pos] == '>')
		{
			int n_same;
			for (n_same = 1; line[n_same] == line[pos]; ++n_same);

			/* quote */
			if (line[pos] == '>')
				;  /* just to make sure line_processed = true so it won't be in a heading */
			/* is it a two line title */
			else if (line[pos] == '=' || line[pos] == '-')
			{
				char marker[2] = { line[pos], '\0' };
				int kind = line[pos] == '=' ? K_CHAPTER : K_SECTION;
				bool whitespace_terminated = true;

				for (int i = pos + n_same; i < line_len; i++)
				{
					if (!isspace(line[i]))
					{
						whitespace_terminated = false;
						break;
					}
				}

				vStringStripLeading(prev_line);
				vStringStripTrailing(prev_line);
				if (whitespace_terminated && vStringLength(prev_line) > 0)
					makeSectionMarkdownTag(prev_line, kind, marker);
			}
			/* otherwise is it a one line title */
			else if (line[pos] == '#' && n_same <= K_SECTION_COUNT && isspace(line[n_same]))
			{
				int kind = n_same - 1;
				bool delimited = false;
				vString *name = get_heading(kind, line, line_len, &delimited);
				if (vStringLength(name) > 0)
					makeSectionMarkdownTag(name, kind, delimited ? "##" : "#");
				vStringDelete(name);
			}

			line_processed = true;
		}

		vStringClear(prev_line);
		if (!line_processed)
			vStringCatS(prev_line, (const char*) line);
	}
	vStringDelete(prev_line);
	vStringDelete(codeLang);
	{
		unsigned int line = (unsigned int)getInputLineNumber ();
		nestingLevelsFreeFull(nestingLevels, HT_UINT_TO_PTR(line));
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
	def->kindCount = ARRAY_SIZE(MarkdownKinds);
	def->fieldTable = MarkdownFields;
	def->fieldCount = ARRAY_SIZE(MarkdownFields);
	def->defaultScopeSeparator = "\"\"";
	def->parser = findMarkdownTags;

	return def;
}
