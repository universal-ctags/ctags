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

static NestingLevel *getNestingLevel(const int kind)
{
	NestingLevel *nl;
	tagEntryInfo *e;

	while (1)
	{
		nl = nestingLevelsGetCurrent(nestingLevels);
		e = getEntryOfNestingLevel (nl);
		if ((nl && (e == NULL)) || (e && (e->kindIndex >= kind)))
			nestingLevelsPop(nestingLevels);
		else
			break;
	}
	return nl;
}

static int makeMarkdownTag (const vString* const name, const int kind, const bool two_line)
{
	const NestingLevel *const nl = getNestingLevel(kind);
	int r = CORK_NIL;

	if (vStringLength (name) > 0)
	{
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


static bool process_name(vString *const name, const int kind,
						 const unsigned char *line, const int line_len)
{
	bool delimited = false;
	int start = kind + 1;
	int end = line_len - 1;

	Assert (kind >= 0 && kind < K_SECTION_COUNT);
	Assert (line_len > start);

	vStringClear(name);

	while (line[end] == line[0])
	{
		--end;
		delimited = true;
	}
	while (isspace(line[start])) ++start;
	while (isspace(line[end])) --end;

	if (start <= end)
		vStringNCatS(name, (const char*)(&(line[start])), end - start + 1);

	return delimited;
}


/* computes the length of an UTF-8 string
 * if the string doesn't look like UTF-8, return -1
 * FIXME consider East_Asian_Width Unicode property */
static int utf8_strlen(const char *buf, int buf_len)
{
	int len = 0;
	const char *end = buf + buf_len;

	for (len = 0; buf < end; len ++)
	{
		/* perform quick and naive validation (no sub-byte checking) */
		if (! (*buf & 0x80))
			buf ++;
		else if ((*buf & 0xe0) == 0xc0)
			buf += 2;
		else if ((*buf & 0xf0) == 0xe0)
			buf += 3;
		else if ((*buf & 0xf8) == 0xf0)
			buf += 4;
		else /* not a valid leading UTF-8 byte, abort */
			return -1;

		if (buf > end) /* incomplete last byte */
			return -1;
	}

	return len;
}


static void findMarkdownTags(void)
{
	vString *name = vStringNew();
	vString *codeLang = vStringNew();
	const unsigned char *line;
	char in_code_char = 0;
	long startSourceLineNumber = 0;
	long startLineNumber = 0;

	nestingLevels = nestingLevelsNew(0);

	while ((line = readLineFromInputFile()) != NULL)
	{
		int line_len = strlen((const char*) line);
		int name_len_bytes = vStringLength(name);
		int name_len = utf8_strlen(vStringValue(name), name_len_bytes);

		for (int i = 0; i < 2; i++)
		{
			char code_chars[] = { '`', '~' };
			char c = code_chars[i % 2];
			char other_c = code_chars[(i + 1) % 2];

			if (in_code_char != other_c && line_len >= 3 &&
				line[0] == c && line[1] == c && line[2] == c)
			{
				in_code_char = in_code_char ? 0 : c;
				if (in_code_char)
				{
					startSourceLineNumber = getSourceLineNumber ();
					startLineNumber = getInputLineNumber ();
					codeLang = vStringNewInit((const char *)(line + 3));
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
			}
		}

		if (in_code_char)
			continue;

		/* if the name doesn't look like UTF-8, assume one-byte charset */
		if (name_len < 0) name_len = name_len_bytes;

		/* if its a title underline, or a delimited block marking character */
		if (line[0] == '=' || line[0] == '-' || line[0] == '#')
		{
			int n_same;
			for (n_same = 1; line[n_same] == line[0]; ++n_same);

			/* is it a two line title */
			if (n_same == line_len)
			{
				if ((line[0] == '=' || line[0] == '-') && line_len >= name_len)
				{
					char marker[2] = { line[0], '\0' };
					int kind = line[0] == '=' ? K_CHAPTER : K_SECTION;
					makeSectionMarkdownTag(name, kind, marker);
					continue;
				}
			}

			/* otherwise is it a one line title */
			else if (line[0] == '#' && n_same <= K_SECTION_COUNT && isspace(line[n_same]))
			{
				int kind = n_same - 1;
				bool delimited = process_name(name, kind, line, line_len);
				makeSectionMarkdownTag(name, kind, delimited ? "##" : "#");
				continue;
			}
		}
		vStringClear(name);
		if (! isspace(*line))
			vStringCatS(name, (const char*) line);
	}
	vStringDelete(name);
	vStringDelete(codeLang);
	nestingLevelsFree(nestingLevels);
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
