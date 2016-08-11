/*
*
*   Copyright (c) 2007-2011, Nick Treleaven
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for generating tags for reStructuredText (reST) files.
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
	SECTION_COUNT
} rstKind;

static kindOption RstKinds[] = {
	{ TRUE, 'c', "chapter",       "chapters"},
	{ TRUE, 's', "section",       "sections" },
	{ TRUE, 'S', "subsection",    "subsections" },
	{ TRUE, 't', "subsubsection", "subsubsections" }
};

typedef enum {
	F_SECTION_MARKER,
} rstField;

static fieldSpec RstFields [] = {
	{
		.name = "sectionMarker",
		.description = "character used for declaring section",
		.enabled = FALSE,
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
		if ((nl && (e == NULL)) || (e && (e->kind - RstKinds) >= kind))
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

static void makeRstTag(const vString* const name, const int kind, const MIOPos filepos,
		       char marker)
{
	const NestingLevel *const nl = getNestingLevel(kind);
	tagEntryInfo *parent;

	int r = CORK_NIL;

	if (vStringLength (name) > 0)
	{
		tagEntryInfo e;
		char m [2] = { [1] = '\0' };

		initTagEntry (&e, vStringValue (name), &(RstKinds [kind]));

		e.lineNumber--;	/* we want the line before the '---' underline chars */
		e.filePosition = filepos;

		parent = getEntryOfNestingLevel (nl);
		if (parent && ((parent->kind - RstKinds) < kind))
		{
#if 1
			e.extensionFields.scopeKind = &(RstKinds [parent->kind - RstKinds]);
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
		attachParserField (&e, RstFields [F_SECTION_MARKER].ftype, m);
		r = makeTagEntry (&e);
	}
	nestingLevelsPush(nestingLevels, r);
}


/* checks if str is all the same character */
static boolean issame(const char *str)
{
	char first = *str;

	while (*str)
	{
		char c;

		str++;
		c = *str;
		if (c && c != first)
			return FALSE;
	}
	return TRUE;
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


/* TODO: parse overlining & underlining as distinct sections. */
static void findRstTags (void)
{
	vString *name = vStringNew ();
	MIOPos filepos;
	const unsigned char *line;

	memset(&filepos, 0, sizeof(MIOPos));
	memset(kindchars, 0, sizeof kindchars);
	nestingLevels = nestingLevelsNew(0);

	while ((line = readLineFromInputFile ()) != NULL)
	{
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
				makeRstTag(name, kind, filepos, c);
				continue;
			}
		}
		vStringClear (name);
		if (!isspace(*line))
		{
			vStringCatS(name, (const char*)line);
			filepos = getInputFilePosition();
		}
		vStringTerminate(name);
	}
	/* Force popping all nesting levels */
	getNestingLevel (K_EOF);
	vStringDelete (name);
	nestingLevelsFree(nestingLevels);
}

extern parserDefinition* RstParser (void)
{
	static const char *const extensions [] = { "rest", "reST", "rst", NULL };
	parserDefinition* const def = parserNew ("reStructuredText");

	def->kinds = RstKinds;
	def->kindCount = ARRAY_SIZE (RstKinds);
	def->extensions = extensions;
	def->parser = findRstTags;

	def->fieldSpecs = RstFields;
	def->fieldSpecCount = ARRAY_SIZE (RstFields);

	def->useCork = TRUE;

	return def;
}
