/*
 *   Copyright (c) 2000-2006, Darren Hiebert, Elias Pschernig
 *
 *   This source code is released for free distribution under the terms of the
 *   GNU General Public License version 2 or (at your option) any later version.
 *
 *   This module contains functions for generating tags for BlitzBasic
 *   (BlitzMax), PureBasic and FreeBasic language files. For now, this is kept
 *   quite simple - but feel free to ask for more things added any time -
 *   patches are of course most welcome.
 */

/*
 *   INCLUDE FILES
 */
#include "general.h" /* must always come first */

#include <string.h>

#include "keyword.h"
#include "parse.h"
#include "read.h"
#include "routines.h"
#include "vstring.h"

/*
 *   DATA DEFINITIONS
 */
typedef enum {
	K_CONST,
	K_FUNCTION,
	K_LABEL,
	K_TYPE,
	K_VARIABLE,
	K_ENUM
} BasicKind;

static kindDefinition BasicKinds[] = {
	{true, 'c', "constant", "constants"},
	{true, 'f', "function", "functions"},
	{true, 'l', "label", "labels"},
	{true, 't', "type", "types"},
	{true, 'v', "variable", "variables"},
	{true, 'g', "enum", "enumerations"},
};

/* To force to trigger bugs, we make the orders of
 * enum eKeywordID and BasicKind different. */
enum eKeywordID {
	KEYWORD_ENUM,
	KEYWORD_CONST,
	KEYWORD_FUNCTION,
	KEYWORD_LABEL,
	KEYWORD_TYPE,
	KEYWORD_VARIABLE,
};
typedef int keywordId; /* to allow KEYWORD_NONE */

static const keywordTable BasicKeywordTable[] = {
	/* freebasic */
	{"const", KEYWORD_CONST},
	{"dim", KEYWORD_VARIABLE},
	{"common", KEYWORD_VARIABLE},
	{"function", KEYWORD_FUNCTION},
	{"sub", KEYWORD_FUNCTION},
	{"private sub", KEYWORD_FUNCTION},
	{"public sub", KEYWORD_FUNCTION},
	{"private function", KEYWORD_FUNCTION},
	{"public function", KEYWORD_FUNCTION},
	{"property", KEYWORD_FUNCTION},
	{"constructor", KEYWORD_FUNCTION},
	{"destructor", KEYWORD_FUNCTION},
	{"type", KEYWORD_TYPE},
	{"enum", KEYWORD_ENUM},

	/* blitzbasic, purebasic */
	{"global", KEYWORD_VARIABLE},

	/* purebasic */
	{"newlist", KEYWORD_VARIABLE},
	{"procedure", KEYWORD_FUNCTION},
	{"interface", KEYWORD_TYPE},
	{"structure", KEYWORD_TYPE},
};

static BasicKind keywordToKindMap[] = {
	[KEYWORD_ENUM]  = K_ENUM,
	[KEYWORD_CONST] = K_CONST,
	[KEYWORD_FUNCTION] = K_FUNCTION,
	[KEYWORD_LABEL] = K_LABEL,
	[KEYWORD_TYPE] = K_TYPE,
	[KEYWORD_VARIABLE] = K_VARIABLE,
};

/*
 *   FUNCTION DEFINITIONS
 */

static int keywordToKind (keywordId keywordId)
{
	if (keywordId == KEYWORD_NONE)
		return KIND_GHOST_INDEX;
	return keywordToKindMap [keywordId];
}

static const char *skipToMatching (char begin, char end, const char *pos)
{
	int counter = 1;
	pos++;
	while (*pos && counter > 0)
	{
		if (*pos == end)
			counter--;
		else if (*pos == begin)
			counter++;
		else if (*pos == '"')
			pos = skipToMatching ('"', '"', pos) - 1;
		pos++;
	}
	return pos;
}

static const char *nextPos (const char *pos)
{
	if (*pos == '\0')
		return pos;

	pos++;
	switch (*pos)
	{
		case '(':
			pos = skipToMatching ('(', ')', pos);
			break;
		case '"':
			pos = skipToMatching ('"', '"', pos);
			break;
	}
	return pos;
}

static bool isIdentChar (char c)
{
	return c && !isspace (c) && c != '(' && c != ',' && c != '=';
}

/* Match the name of a dim or const starting at pos. */
static void extract_dim (char const *pos, BasicKind kind)
{
	vString *name = vStringNew ();

	if (strncasecmp (pos, "shared", 6) == 0)
		pos += 6; /* skip keyword "shared" */

	while (isspace (*pos))
		pos++;

	/* capture "dim as String str" */
	if (strncasecmp (pos, "as", 2) == 0)
	{
			pos += 2; /* skip keyword "as" */

		while (isspace (*pos))
			pos++;
		while (!isspace (*pos) && *pos) /* skip next part which is a type */
			pos++;
		while (isspace (*pos))
			pos++;
		/* now we are at the name */
	}
	/* capture "dim as foo ptr bar" */
	if (strncasecmp (pos, "ptr", 3) == 0 && isspace(*(pos+3)))
	{
		pos += 3; /* skip keyword "ptr" */
		while (isspace (*pos))
			pos++;
	}
	/*	capture "dim as string * 4096 chunk" */
	if (strncmp (pos, "*", 1) == 0)
	{
		pos += 1; /* skip "*" */
		while (isspace (*pos) || isdigit(*pos) || ispunct(*pos))
			pos++;
	}

	for (; isIdentChar (*pos); pos++)
		vStringPut (name, *pos);
	makeSimpleTag (name, kind);

	/* if the line contains a ',', we have multiple declarations */
	while (*pos && strchr (pos, ','))
	{
		/* skip all we don't need(e.g. "..., new_array(5), " we skip "(5)") */
		while (*pos != ',' && *pos != '\'' && *pos)
			pos = nextPos (pos);

		if (*pos == '\'')
			break; /* break if we are in a comment */

		while (isspace (*pos) || *pos == ',')
			pos++;

		if (*pos == '\'')
			break; /* break if we are in a comment */

		vStringClear (name);
		for (; isIdentChar (*pos); pos++)
			vStringPut (name, *pos);
		makeSimpleTag (name, kind);
	}

	vStringDelete (name);
}

/* Match the name of a tag (function, variable, type, ...) starting at pos. */
static void extract_name (char const *pos, BasicKind kind)
{
	vString *name = vStringNew ();
	for (; isIdentChar (*pos); pos++)
		vStringPut (name, *pos);
	makeSimpleTag (name, kind);
	vStringDelete (name);
}

/* Match a keyword starting at p (case insensitive). */
static bool match_keyword (const char *p, keywordTable const *kw)
{
	size_t i;
	const char *old_p;
	for (i = 0; i < strlen (kw->name); i++)
	{
		if (tolower (p[i]) != kw->name[i])
			return false;
	}
	p += i;

	old_p = p;
	while (isspace (*p))
		p++;

	/* create tags only if there is some space between the keyword and the identifier */
	if (old_p == p)
		return false;

	int kind = keywordToKind (kw->id);
	if (kind == K_VARIABLE)
		extract_dim (p, kind); /* extract_dim adds the found tag(s) */
	else if (kind != KIND_GHOST_INDEX)
		extract_name (p, kind);
	return true;
}

/* Match a "label:" style label. */
static void match_colon_label (char const *p)
{
	char const *end = p + strlen (p) - 1;
	while (isspace (*end))
		end--;
	if (*end == ':')
	{
		vString *name = vStringNew ();
		vStringNCatS (name, p, end - p);
		makeSimpleTag (name, K_LABEL);
		vStringDelete (name);
	}
}

/* Match a ".label" style label. */
static void match_dot_label (char const *p)
{
	extract_name (p + 1, K_LABEL);
}

static void findBasicTags (void)
{
	const char *line;

	while ((line = (const char *) readLineFromInputFile ()) != NULL)
	{
		const char *p = line;

		while (isspace (*p))
			p++;

		/* Empty line? */
		if (!*p)
			continue;

		/* REM comment? */
		if (strncasecmp (p, "REM", 3) == 0  &&
			(isspace (*(p + 3)) || *(p + 3) == '\0'))
			continue;

		/* Single-quote comment? */
		if (*p == '\'')
			continue;

		/* In Basic, keywords always are at the start of the line. */
		for (size_t i = 0; i < ARRAY_SIZE(BasicKeywordTable); i++)
		{
			keywordTable const *kw = BasicKeywordTable + i;
			if (match_keyword (p, kw)) break;
		}

		/* Is it a label? */
		if (*p == '.')
			match_dot_label (p);
		else
			match_colon_label (p);
	}
}

parserDefinition *BasicParser (void)
{
	static char const *extensions[] = { "bas", "bi", "bm", "bb", "pb", NULL };
	parserDefinition *def = parserNew ("Basic");
	def->kindTable = BasicKinds;
	def->kindCount = ARRAY_SIZE (BasicKinds);
	def->extensions = extensions;
	def->parser = findBasicTags;
	return def;
}
