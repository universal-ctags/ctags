/*
*
*   Copyright (c) 2007-2011, Nick Treleaven
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for generating tags for reStructuredText (reST) files.
*
*   This module was ported from geany.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"	/* must always come first */

#include <ctype.h>
#include <string.h>

#include "parse.h"
#include "read.h"
#include "vstring.h"
#include "nestlevel.h"
#include "entry.h"
#include "routines.h"
#include "field.h"

/*
*   DATA DEFINITIONS
*/
typedef enum {
	K_EOF = -1,
	K_CHAPTER = 0,
	K_SECTION,
	K_SUBSECTION,
	K_SUBSUBSECTION,
	K_CITATION,
	K_TARGET,
	K_SUBSTDEF,
	SECTION_COUNT
} rstKind;

static kindDefinition RstKinds[] = {
	{ true, 'c', "chapter",       "chapters"},
	{ true, 's', "section",       "sections" },
	{ true, 'S', "subsection",    "subsections" },
	{ true, 't', "subsubsection", "subsubsections" },
	{ true, 'C', "citation",      "citations"},
	{ true, 'T', "target",        "targets" },
	{ true, 'd', "substdef",      "substitute definitions" },
};

typedef enum {
	F_SECTION_MARKER,
} rstField;

static fieldDefinition RstFields [] = {
	{
		.name = "sectionMarker",
		.description = "character used for declaring section",
		.enabled = false,
	},
};

static char kindchars[SECTION_COUNT];

static NestingLevels *nestingLevels = NULL;

/*
*   FUNCTION DEFINITIONS
*/

static NestingLevel *getNestingLevel(const int kind)
{
	NestingLevel *nl;
	tagEntryInfo *e;

	int d = 0;

	if (kind > K_EOF)
	{
		d++;
		/* 1. we want the line before the '---' underline chars */
		d++;
		/* 2. we want the line before the next section/chapter title. */
	}

	while (1)
	{
		nl = nestingLevelsGetCurrent(nestingLevels);
		e = getEntryOfNestingLevel (nl);
		if ((nl && (e == NULL)) || (e && e->kindIndex >= kind))
		{
			if (e)
				e->extensionFields.endLine = (getInputLineNumber() - d);
			nestingLevelsPop(nestingLevels);
		}
		else
			break;
	}
	return nl;
}

static int makeTargetRstTag(const vString* const name, rstKind kindex)
{
	tagEntryInfo e;

	initTagEntry (&e, vStringValue (name), kindex);

	const NestingLevel *nl = nestingLevelsGetCurrent(nestingLevels);
	tagEntryInfo *parent = NULL;
	if (nl)
		parent = getEntryOfNestingLevel (nl);

	if (parent)
	{
		e.extensionFields.scopeKindIndex = parent->kindIndex;
		e.extensionFields.scopeName = parent->name;
	}

	return makeTagEntry (&e);
}

static void makeSectionRstTag(const vString* const name, const int kind, const MIOPos filepos,
		       char marker)
{
	const NestingLevel *const nl = getNestingLevel(kind);
	tagEntryInfo *parent;

	int r = CORK_NIL;

	if (vStringLength (name) > 0)
	{
		tagEntryInfo e;
		char m [2] = { [1] = '\0' };

		initTagEntry (&e, vStringValue (name), kind);

		e.lineNumber--;	/* we want the line before the '---' underline chars */
		e.filePosition = filepos;

		parent = getEntryOfNestingLevel (nl);
		if (parent && (parent->kindIndex < kind))
		{
#if 1
			e.extensionFields.scopeKindIndex = parent->kindIndex;
			e.extensionFields.scopeName = parent->name;
#else
			/* TODO

			   Following code makes the scope information full qualified form.
			   Do users want the full qualified form?
			   --- ./Units/rst.simple.d/expected.tags	2015-12-18 01:32:35.574255617 +0900
			   +++ /home/yamato/var/ctags-github/Units/rst.simple.d/FILTERED.tmp	2016-05-05 03:05:38.165604756 +0900
			   @@ -5,2 +5,2 @@
			   -Subsection 1.1.1	input.rst	/^Subsection 1.1.1$/;"	S	section:Section 1.1
			   -Subsubsection 1.1.1.1	input.rst	/^Subsubsection 1.1.1.1$/;"	t	subsection:Subsection 1.1.1
			   +Subsection 1.1.1	input.rst	/^Subsection 1.1.1$/;"	S	section:Chapter 1.Section 1.1
			   +Subsubsection 1.1.1.1	input.rst	/^Subsubsection 1.1.1.1$/;"	t	subsection:Chapter 1.Section 1.1.Subsection 1.1.1
			*/
			   e.extensionFields.scopeIndex = nl->corkIndex;
#endif
		}

		m[0] = marker;
		attachParserField (&e, false, RstFields [F_SECTION_MARKER].ftype, m);
		r = makeTagEntry (&e);
	}
	nestingLevelsPush(nestingLevels, r);
}


/* checks if str is all the same character */
static bool issame(const char *str)
{
	char first = *str;

	while (*str)
	{
		char c;

		str++;
		c = *str;
		if (c && c != first)
			return false;
	}
	return true;
}


static int get_kind(char c)
{
	int i;

	for (i = 0; i < SECTION_COUNT; i++)
	{
		if (kindchars[i] == c)
			return i;

		if (kindchars[i] == 0)
		{
			kindchars[i] = c;
			return i;
		}
	}
	return -1;
}


/* computes the length of an UTF-8 string
 * if the string doesn't look like UTF-8, return -1 */
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


static const unsigned char *is_markup_line (const unsigned char *line, char reftype)
{
	if ((line [0] == '.') && (line [1] == '.') && (line [2] == ' ')
		&& (line [3] == reftype))
		return line + 4;
	return NULL;
}

static int capture_markup (const unsigned char *target_line, char defaultTerminator, rstKind kindex)
{
	vString *name = vStringNew ();
	unsigned char terminator;
	int r = CORK_NIL;

	if (*target_line == '`')
		terminator = '`';
	else if (!isspace (*target_line) && *target_line != '\0')
	{
		/* "Simple reference names are single words consisting of
		 * alphanumerics plus isolated (no two adjacent) internal
		 * hyphens, underscores, periods, colons and plus signs; no
		 * whitespace or other characters are allowed."
		 * -- http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#reference-names
		 */
		vStringPut (name, *target_line);
		terminator = defaultTerminator;
	}
	else
		goto out;

	target_line++;


	bool escaped = false;
	while (*target_line != '\0')
	{
		if (escaped)
		{
			vStringPut (name, *target_line);
			escaped = false;
		}
		else
		{
			if (*target_line == '\\')
			{
				vStringPut (name, *target_line);
				escaped = true;
			}
			else if (*target_line == terminator)
				break;
			else
				vStringPut (name, *target_line);
		}
		target_line++;
	}

	if (vStringLength (name) == 0)
		goto out;

	r = makeTargetRstTag (name, kindex);

 out:
	vStringDelete (name);
	return r;
}

/* TODO: parse overlining & underlining as distinct sections. */
static void findRstTags (void)
{
	vString *name = vStringNew ();
	MIOPos filepos;
	const unsigned char *line;
	const unsigned char *markup_line;

	memset(&filepos, 0, sizeof(filepos));
	memset(kindchars, 0, sizeof kindchars);
	nestingLevels = nestingLevelsNew(0);

	while ((line = readLineFromInputFile ()) != NULL)
	{
		if ((markup_line = is_markup_line (line, '_')) != NULL)
		{
			/* Handle .. _target:
			 * http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#hyperlink-targets
			 */
			if (capture_markup (markup_line, ':', K_TARGET) != CORK_NIL)
			{
				vStringClear (name);
				continue;
			}
		}
		else if ((markup_line = is_markup_line (line, '[')) != NULL)
		{
			/* Handle .. [citation]
			 * https://docutils.sourceforge.io/docs/ref/rst/restructuredtext.html#citations
			 */
			if (capture_markup (markup_line, ']', K_CITATION) != CORK_NIL)
			{
				vStringClear (name);
				continue;
			}
		}
		else if ((markup_line = is_markup_line (line, '|')) != NULL)
		{
			/* Hanle .. |substitute definition|
			 * https://docutils.sourceforge.io/docs/ref/rst/restructuredtext.html#substitution-definitions
			 */
			if (capture_markup (markup_line, '|', K_SUBSTDEF) != CORK_NIL)
			{
				vStringClear (name);
				continue;
			}
		}

		int line_len = strlen((const char*) line);
		int name_len_bytes = vStringLength(name);
		/* FIXME: this isn't right, actually we need the real display width,
		 * taking into account double-width characters and stuff like that.
		 * But duh. */
		int name_len = utf8_strlen(vStringValue(name), name_len_bytes);

		/* if the name doesn't look like UTF-8, assume one-byte charset */
		if (name_len < 0)
			name_len = name_len_bytes;

		/* underlines must be the same length or more */
		if (line_len >= name_len && name_len > 0 &&
			ispunct(line[0]) && issame((const char*) line))
		{
			char c = line[0];
			int kind = get_kind(c);

			if (kind >= 0)
			{
				makeSectionRstTag(name, kind, filepos, c);
				continue;
			}
		}
		vStringClear (name);
		if (!isspace(*line))
		{
			vStringCatS(name, (const char*)line);
			filepos = getInputFilePosition();
		}
	}
	/* Force popping all nesting levels */
	getNestingLevel (K_EOF);
	vStringDelete (name);
	nestingLevelsFree(nestingLevels);
}

extern parserDefinition* RstParser (void)
{
	static const char *const extensions [] = { "rest", "reST", "rst", NULL };
	parserDefinition* const def = parserNew ("ReStructuredText");
	static const char *const aliases[] = {
		"rst",					/* The name of emacs's mode */
		NULL
	};

	def->kindTable = RstKinds;
	def->kindCount = ARRAY_SIZE (RstKinds);
	def->extensions = extensions;
	def->aliases = aliases;
	def->parser = findRstTags;

	def->fieldTable = RstFields;
	def->fieldCount = ARRAY_SIZE (RstFields);

	def->useCork = CORK_QUEUE;

	return def;
}
