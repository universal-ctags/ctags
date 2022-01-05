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
*
*   References:
*      https://docutils.sourceforge.io/docs/ref/rst/restructuredtext.html
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
#include "htable.h"
#include "debug.h"

/*
*   DATA DEFINITIONS
*/
typedef enum {
	K_EOF = -1,
	K_TITLE = 0,
	K_SUBTITLE,
	K_CHAPTER,
	K_SECTION,
	K_SUBSECTION,
	K_SUBSUBSECTION,
	SECTION_COUNT,
	K_CITATION = SECTION_COUNT,
	K_TARGET,
	K_SUBSTDEF,
} rstKind;

static kindDefinition RstKinds[] = {
	{ true, 'H', "title",         "titles"},
	{ true, 'h', "subtitle",      "sub titles" },
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
	F_SECTION_OVERLINE,
} rstField;

static fieldDefinition RstFields [] = {
	{
		.name = "sectionMarker",
		.description = "character used for declaring section",
		.enabled = false,
	},
	{
		.name = "overline",
		.description = "whether using overline & underline for declaring section",
		.enabled = false,
		.dataType = FIELDTYPE_BOOL
	},
};

static NestingLevels *nestingLevels = NULL;

struct sectionTracker {
	char kindchar;
	bool overline;
	int count;
};

struct olineTracker
{
	char c;
	size_t len;
};

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
	if (nl)
		e.extensionFields.scopeIndex = nl->corkIndex;

	return makeTagEntry (&e);
}

static void makeSectionRstTag(const vString* const name, const int kind, const MIOPos filepos,
		       char marker, bool overline)
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
			e.extensionFields.scopeIndex = nl->corkIndex;

		m[0] = marker;
		attachParserField (&e, false, RstFields [F_SECTION_MARKER].ftype, m);

		if (overline)
			attachParserField (&e, false, RstFields [F_SECTION_OVERLINE].ftype, "");

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


static int get_kind(char c, bool overline, struct sectionTracker tracker[])
{
	int i;

	for (i = 0; i < SECTION_COUNT; i++)
	{
		if (tracker[i].kindchar == c && tracker[i].overline == overline)
		{
			tracker[i].count++;
			return i;
		}

		if (tracker[i].count == 0)
		{
			tracker[i].count = 1;
			tracker[i].kindchar = c;
			tracker[i].overline = overline;
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

static void overline_clear(struct olineTracker *ol)
{
	ol->c = 0;
	ol->len = 0;
}

static void overline_set(struct olineTracker *ol, char c, size_t len)
{
	ol->c = c;
	ol->len = len;
}

static bool has_overline(struct olineTracker *ol)
{
	return (ol->c != 0);
}

static int getFosterEntry(tagEntryInfo *e, int shift)
{
	int r = CORK_NIL;

	while (shift-- > 0)
	{
		r = e->extensionFields.scopeIndex;
		Assert(r != CORK_NIL);
		e = getEntryInCorkQueue(r);
		Assert(e);
	}
	return r;
}

static void shiftKinds(int shift, rstKind baseKind)
{
	size_t count = countEntryInCorkQueue();
	hashTable *remapping_table = hashTableNew (count,
											   hashPtrhash,
											   hashPtreq, NULL, NULL);
	hashTableSetValueForUnknownKey(remapping_table, HT_INT_TO_PTR(CORK_NIL), NULL);

	for (int index = 0; index < count; index++)
	{
		tagEntryInfo *e = getEntryInCorkQueue(index);
		if (e && (baseKind <= e->kindIndex && e->kindIndex < SECTION_COUNT))
		{
			e->kindIndex += shift;
			if (e->kindIndex >= SECTION_COUNT)
			{
				markTagPlaceholder(e, true);

				int foster_parent = getFosterEntry(e, shift);
				Assert (foster_parent != CORK_NIL);
				hashTablePutItem(remapping_table, HT_INT_TO_PTR(index),
								 HT_INT_TO_PTR(foster_parent));
			}
		}
	}

	for (int index = 0; index < count; index++)
	{
		tagEntryInfo *e = getEntryInCorkQueue(index);
		if (e && e->extensionFields.scopeIndex != CORK_NIL)
		{
			void *remapping_to = hashTableGetItem (remapping_table,
												   HT_INT_TO_PTR(e->extensionFields.scopeIndex));
			if (HT_PTR_TO_INT(remapping_to) != CORK_NIL)
				e->extensionFields.scopeIndex = HT_PTR_TO_INT(remapping_to);
		}
	}
	hashTableDelete(remapping_table);
}

static void adjustSectionKinds(struct sectionTracker section_tracker[])
{
	if (section_tracker[K_TITLE].count > 1)
	{
		shiftKinds(2, K_TITLE);
		return;
	}

	if (section_tracker[K_TITLE].count == 1
		&& section_tracker[K_SUBTITLE].count > 1)
	{
		shiftKinds(1, K_SUBTITLE);
		return;
	}
}

static void inlineTagScope(tagEntryInfo *e, int parent_index)
{
	tagEntryInfo *parent = getEntryInCorkQueue (parent_index);
	if (parent)
	{
		e->extensionFields.scopeKindIndex = parent->kindIndex;
		e->extensionFields.scopeName = eStrdup(parent->name);
		e->extensionFields.scopeIndex = CORK_NIL;
	}
}

static void inlineScopes (void)
{
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
	size_t count = countEntryInCorkQueue();
	for (int index = 0; index < count; index++)
	{
		tagEntryInfo *e = getEntryInCorkQueue(index);

		if (e && e->extensionFields.scopeIndex != CORK_NIL)
			inlineTagScope(e, e->extensionFields.scopeIndex);
	}
}

static void findRstTags (void)
{
	vString *name = vStringNew ();
	MIOPos filepos;
	const unsigned char *line;
	const unsigned char *markup_line;
	struct sectionTracker section_tracker[SECTION_COUNT];
	struct olineTracker overline;

	memset(&filepos, 0, sizeof(filepos));
	memset(section_tracker, 0, sizeof section_tracker);
	overline_clear(&overline);
	nestingLevels = nestingLevelsNew(0);

	while ((line = readLineFromInputFile ()) != NULL)
	{
		if ((markup_line = is_markup_line (line, '_')) != NULL)
		{
			overline_clear(&overline);
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
			overline_clear(&overline);
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
			overline_clear(&overline);
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

		/* overline may come after an empty line (or begging of file). */
		if (name_len_bytes == 0 && line_len > 0 &&
			ispunct(line[0]) && issame((const char*) line))
		{
			overline_set(&overline, *line, line_len);
			continue;
		}

		/* underlines must be the same length or more */
		if (line_len >= name_len && name_len > 0 &&
			ispunct(line[0]) && issame((const char*) line))
		{
			char c = line[0];
			bool o = (overline.c == c && overline.len == line_len);
			int kind = get_kind(c, o, section_tracker);

			overline_clear(&overline);

			if (kind >= 0)
			{
				makeSectionRstTag(name, kind, filepos, c, o);
				vStringClear(name);
				continue;
			}
		}

		if (has_overline(&overline))
		{
			if (name_len > 0)
			{
				/*
				 * Though we saw an overline and a section title text,
				 * we cannot find the associated underline.
				 * In that case, we must reset the state of tracking
				 * overline.
				 */
				overline_clear(&overline);
			}

			/*
			 * We san an overline. The line is the candidate
			 * of a section title text. Skip the prefixed whitespaces.
			 */
			while (isspace(*line))
				line++;
		}

		vStringClear (name);
		if (!isspace(*line))
		{
			vStringCatS(name, (const char*)line);
			vStringStripTrailing (name);
			filepos = getInputFilePosition();
		}
	}
	/* Force popping all nesting levels */
	getNestingLevel (K_EOF);
	vStringDelete (name);
	nestingLevelsFree(nestingLevels);

	adjustSectionKinds(section_tracker);
	inlineScopes();
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
