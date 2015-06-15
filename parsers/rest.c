/*
*
*   Copyright (c) 2007-2011, Nick Treleaven
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
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

/*
*   DATA DEFINITIONS
*/
typedef enum {
	K_CHAPTER = 0,
	K_SECTION,
	K_SUBSECTION,
	K_SUBSUBSECTION,
	SECTION_COUNT
} restKind;

static kindOption RestKinds[] = {
	{ TRUE, 'c', "chapter",       "chapters"},
	{ TRUE, 's', "section",       "sections" },
	{ TRUE, 'S', "subsection",    "subsections" },
	{ TRUE, 't', "subsubsection", "subsubsections" }
};

static char kindchars[SECTION_COUNT];

static NestingLevels *nestingLevels = NULL;

/*
*   FUNCTION DEFINITIONS
*/

static NestingLevel *getNestingLevel(const int kind)
{
	NestingLevel *nl;

	while (1)
	{
		nl = nestingLevelsGetCurrent(nestingLevels);
		if (nl && nl->type >= kind)
			nestingLevelsPop(nestingLevels);
		else
			break;
	}
	return nl;
}

static void makeRestTag(const vString* const name, const int kind, const fpos_t filepos)
{
	const NestingLevel *const nl = getNestingLevel(kind);

	if (vStringLength (name) > 0)
	{
		tagEntryInfo e;
		initTagEntry (&e, vStringValue (name));

		e.lineNumber--;	/* we want the line before the '---' underline chars */
		e.kindName = RestKinds [kind].name;
		e.kind = RestKinds [kind].letter;
		e.filePosition = filepos;

		if (nl && nl->type < kind)
		{
			e.extensionFields.scope [0] = RestKinds [nl->type].name;
			e.extensionFields.scope [1] = vStringValue (nl->name);
		}
		makeTagEntry (&e);
	}
	nestingLevelsPush(nestingLevels, name, kind);
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
static void findRestTags (void)
{
	vString *name = vStringNew ();
	fpos_t filepos = 0;
	const unsigned char *line;

	memset(kindchars, 0, sizeof kindchars);
	nestingLevels = nestingLevelsNew();

	while ((line = fileReadLine ()) != NULL)
	{
		int line_len = strlen((const char*) line);
		int name_len_bytes = vStringLength(name);
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
				makeRestTag(name, kind, filepos);
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
	vStringDelete (name);
	nestingLevelsFree(nestingLevels);
}

extern parserDefinition* RestParser (void)
{
	static const char *const patterns [] = { "*.rest", "*.reST", "*.rst", NULL };
	static const char *const extensions [] = { "rest", NULL };
	parserDefinition* const def = parserNew ("reStructuredText");

	def->kinds = RestKinds;
	def->kindCount = KIND_COUNT (RestKinds);
	def->patterns = patterns;
	def->extensions = extensions;
	def->parser = findRestTags;
	return def;
}

/* vi:set tabstop=8 shiftwidth=4: */
