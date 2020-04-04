/*
 *
 *  Copyright (c) 2007-2011, Nick Treleaven
 * 	Copyright (c) 2012, Lex Trotman
 *
 *   This source code is released for free distribution under the terms of the
 *   GNU General Public License version 2 or (at your option) any later version.
 *
 * This module contains functions for generating tags for asciidoc files.
 *
 * Based on Rest code by Nick Treleaven, see rest.c
 *
 * This code was ported from geany git commit 40396a3 at:
 *   https://github.com/geany/geany/blob/master/ctags/parsers/asciidoc.c
 * with the changes in geany's PR #1263, with some changes to work in uctags.
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

/*
 *   DATA DEFINITIONS
 */
typedef enum {
	K_CHAPTER = 0,
	K_SECTION,
	K_SUBSECTION,
	K_SUBSUBSECTION,
	K_LEVEL4SECTION,
	/* level-5 section not in here because it only works for one-line */
	SECTION_COUNT, /* this is the same as level-5 kind number */
	K_ANCHOR
} asciidocKind;

/*
 * The following kind letters are based on the markdown parser kinds,
 * and thus different than geany's.
 */
static kindDefinition AsciidocKinds[] = {
	{ true, 'c', "chapter",       "chapters"},
	{ true, 's', "section",       "sections" },
	{ true, 'S', "subsection",    "level 2 sections" },
	{ true, 't', "subsubsection", "level 3 sections" },
	{ true, 'T', "l4subsection",  "level 4 sections" },
	{ true, 'u', "l5subsection",  "level 5 sections" },
	{ true, 'a', "anchor",        "anchors" }
};

static char kindchars[SECTION_COUNT]={ '=', '-', '~', '^', '+' };

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

static int makeAsciidocTag (const vString* const name, const int kind, const bool two_line)
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
		{
			/*
			 * This doesn't use Cork, but in this case I think this is better,
			 * because Cork would record the scopes of all parents in the chain
			 * which is weird for text section identifiers, and also this is
			 * what the rst.c reStructuredText parser does.
			 */
			e.extensionFields.scopeKindIndex = parent->kindIndex;
			e.extensionFields.scopeName = parent->name;
		}

		r = makeTagEntry (&e);
	}
	return r;
}

static int makeSectionAsciidocTag (const vString* const name, const int kind, const bool two_line)
{
	int r = makeAsciidocTag(name, kind, two_line);
	nestingLevelsPush(nestingLevels, r);
	return r;
}


static int get_kind(char c)
{
	int i;

	for (i = 0; i < SECTION_COUNT; i++)
	{
		if (kindchars[i] == c)
			return i;
	}
	return -1;
}


static bool is_anchor(const unsigned char *line)
{
	/* must be at least "[#a]" */
	return line[0] == '[' && (line[1] == '#' || line[1] == '[');
}

static int capture_anchor(const unsigned char *const orig, int* captured_len)
{
	vString *name = vStringNew ();
	int r = CORK_NIL;
	const bool shorthand = orig[1] == '#' ? true : false;
	bool is_valid = false;
	bool seen_comma = false;
	const unsigned char *line = orig;

	Assert (line[0] == '[');
	Assert (line[1] == '#' || line[1] == '[');

	if (captured_len) *captured_len = 0;

	line += 2;

	while (*line != '\0')
	{
		if (*line == ']')
		{
			if (shorthand || line[1] == ']')
			{
				is_valid = true;
				if (shorthand) line++;
				else line += 2;
				break;
			}
			/* otherwise it's not the end, keep going */
		}

		if (*line == ',')
			seen_comma = true;

		if (!seen_comma)
			vStringPut (name, *line);

		line++;
	}

	if (is_valid && vStringLength (name) != 0)
	{
		r = makeAsciidocTag (name, K_ANCHOR, false);

		if (captured_len)
		{
			*captured_len = line - orig;
		}
	}

	vStringDelete (name);
	return r;
}


/* skips any leading anchor(s) in a one-line title, generating tags for them */
static int process_leading_anchors(const unsigned char *const begin)
{
	int captured_len = 0;
	const unsigned char *current = begin;

	while (is_anchor(current) && capture_anchor(current, &captured_len) != CORK_NIL)
	{
		/* minimum is "[#a]" */
		Assert (captured_len >= 4);
		current += captured_len;
		while (isspace(*current)) ++current;
	}

	return current - begin;
}

static int process_trailing_anchor(const unsigned char *const begin,
								   const unsigned char *const end)
{
	int captured_len = 0;
	const unsigned char *found = NULL;

	/* minimum is "[#a]" */
	if (*end == ']' && (end - begin) >= 4)
	{
		found = (const unsigned char*) strrchr((const char*) begin , '[');
		if (found && ((end - found) >= 4))
		{
			/* see if it's not shorthand [#a] but instead [[a]] */
			if (end[-1] == ']' && found > begin && found[-1] == '[')
				--found;

			if (is_anchor (found))
				capture_anchor(found, &captured_len);
		}
	}

	return captured_len;
}

static void process_name(vString *const name, const int kind,
						 const unsigned char *line, const int line_len)
{
	int start = kind + 1;
	int end = line_len - 1;

	Assert (kind >= 0 && kind < K_ANCHOR);
	Assert (line_len > start);

	vStringClear(name);

	while (line[end] == line[0]) --end;
	while (isspace(line[start])) ++start;
	while (isspace(line[end])) --end;

	if (start < end)
	{
		/* pop nesting levels, so that anchors get the parent's scope */
		getNestingLevel(kind);
		end -= process_trailing_anchor(line + start, line + end);
		start += process_leading_anchors(line + start);
	}

	while (isspace(line[end])) --end;

	if (start <= end)
		vStringNCatS(name, (const char*)(&(line[start])), end - start + 1);
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


static void findAsciidocTags(void)
{
	vString *name = vStringNew();
	const unsigned char *line;
	unsigned char in_block = '\0';  /* holds the block marking char or \0 if not in block */

	nestingLevels = nestingLevelsNew(0);

	while ((line = readLineFromInputFile()) != NULL)
	{
		if (is_anchor (line))
		{
			if (capture_anchor (line, NULL) != CORK_NIL)
			{
				vStringClear (name);
				continue;
			}
		}

		int line_len = strlen((const char*) line);
		int name_len_bytes = vStringLength(name);
		int name_len = utf8_strlen(vStringValue(name), name_len_bytes);

		/* if the name doesn't look like UTF-8, assume one-byte charset */
		if (name_len < 0) name_len = name_len_bytes;

		/* if its a title underline, or a delimited block marking character */
		if (line[0] == '=' || line[0] == '-' || line[0] == '~' ||
			line[0] == '^' || line[0] == '+' || line[0] == '.' ||
			line[0] == '*' || line[0] == '_' || line[0] == '/')
		{
			int n_same;
			for (n_same = 1; line[n_same] == line[0]; ++n_same);

			/* is it a two line title or a delimited block */
			if (n_same == line_len)
			{
				/* if in a block, can't be block start or title, look for block end */
				if (in_block)
				{
					if (line[0] == in_block) in_block = '\0';
				}

				/* if its a =_~^+ and the same length +-2 as the line before then its a title */
				/* (except in the special case its a -- open block start line) */
				else if ((line[0] == '=' || line[0] == '-' || line[0] == '~' ||
							line[0] == '^' || line[0] == '+') &&
						line_len <= name_len + 2 && line_len >= name_len - 2 &&
						!(line_len == 2 && line[0] == '-'))
				{
					int kind = get_kind((char)(line[0]));
					if (kind >= 0)
					{
						makeSectionAsciidocTag(name, kind, true);
						continue;
					}
				}

				/* else if its 4 or more /+-.*_= (plus the -- special case) its a block start */
				else if (((line[0] == '/' || line[0] == '+' || line[0] == '-' ||
						   line[0] == '.' || line[0] == '*' || line[0] == '_' ||
						   line[0] == '=') && line_len >= 4 )
						 || (line[0] == '-' && line_len == 2))
				{
					in_block = line[0];
				}
			}

			/* otherwise is it a one line title */
			else if (line[0] == '=' && n_same <= 6 && isspace(line[n_same]) &&
					!in_block)
			{
				int kind = n_same - 1;
				process_name(name, kind, line, line_len);
				makeSectionAsciidocTag(name, kind, false);
				continue;
			}
		}
		vStringClear(name);
		if (! isspace(*line))
			vStringCatS(name, (const char*) line);
	}
	vStringDelete(name);
	nestingLevelsFree(nestingLevels);
}

extern parserDefinition* AsciidocParser (void)
{
	static const char *const patterns [] = { "*.asc", "*.adoc", "*.asciidoc", NULL };
	static const char *const extensions [] = { "asc", "adoc", "asciidoc", NULL };

	parserDefinition* const def = parserNew ("Asciidoc");

	def->kindTable = AsciidocKinds;
	def->kindCount = ARRAY_SIZE (AsciidocKinds);
	def->patterns = patterns;
	def->extensions = extensions;
	def->parser = findAsciidocTags;
	/* do we even need to use Cork? */
	def->useCork = CORK_QUEUE;

	return def;
}
