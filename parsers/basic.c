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

typedef struct {
	char const *token;
	BasicKind kind;
} KeyWord;

static kindDefinition BasicKinds[] = {
	{true, 'c', "constant", "constants"},
	{true, 'f', "function", "functions"},
	{true, 'l', "label", "labels"},
	{true, 't', "type", "types"},
	{true, 'v', "variable", "variables"},
	{true, 'g', "enum", "enumerations"}
};

static KeyWord basic_keywords[] = {
	/* freebasic */
	{"const", K_CONST},
	{"dim", K_VARIABLE},
	{"common", K_VARIABLE},
	{"function", K_FUNCTION},
	{"sub", K_FUNCTION},
	{"private sub", K_FUNCTION},
	{"public sub", K_FUNCTION},
	{"private function", K_FUNCTION},
	{"public function", K_FUNCTION},
	{"property", K_FUNCTION},
	{"constructor", K_FUNCTION},
	{"destructor", K_FUNCTION},
	{"type", K_TYPE},
	{"enum", K_ENUM},

	/* blitzbasic, purebasic */
	{"global", K_VARIABLE},

	/* purebasic */
	{"newlist", K_VARIABLE},
	{"procedure", K_FUNCTION},
	{"interface", K_TYPE},
	{"structure", K_TYPE},

	{NULL, 0}
};

/*
 *   FUNCTION DEFINITIONS
 */

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

	for (; *pos && !isspace (*pos) && *pos != '(' && *pos != ',' && *pos != '='; pos++)
		vStringPut (name, *pos);
	makeSimpleTag (name, kind);

	/* if the line contains a ',', we have multiple declarations */
	while (*pos && strchr (pos, ','))
	{
		/* skip all we don't need(e.g. "..., new_array(5), " we skip "(5)") */
		while (*pos != ',' && *pos != '\'' && *pos)
			pos++;

		if (*pos == '\'')
			break; /* break if we are in a comment */

		while (isspace (*pos) || *pos == ',')
			pos++;

		if (*pos == '\'')
			break; /* break if we are in a comment */

		vStringClear (name);
		for (; *pos && !isspace (*pos) && *pos != '(' && *pos != ',' && *pos != '='; pos++)
			vStringPut (name, *pos);
		makeSimpleTag (name, kind);
	}

	vStringDelete (name);
}

/* Match the name of a tag (function, variable, type, ...) starting at pos. */
static char const *extract_name (char const *pos, vString * name)
{
	while (isspace (*pos))
		pos++;
	vStringClear (name);
	for (; *pos && !isspace (*pos) && *pos != '(' && *pos != ',' && *pos != '='; pos++)
		vStringPut (name, *pos);
	return pos;
}

/* Match a keyword starting at p (case insensitive). */
static int match_keyword (const char *p, KeyWord const *kw)
{
	vString *name;
	size_t i;
	const char *old_p;
	for (i = 0; i < strlen (kw->token); i++)
	{
		if (tolower (p[i]) != kw->token[i])
			return 0;
	}
	p += i;

	old_p = p;
	while (isspace (*p))
		p++;

	/* create tags only if there is some space between the keyword and the identifier */
	if (old_p == p)
		return 0;

	if (kw->kind == K_VARIABLE)
	{
		extract_dim (p, kw->kind); /* extract_dim adds the found tag(s) */
		return 1;
	}

	name = vStringNew ();
	extract_name (p, name);
	makeSimpleTag (name, kw->kind);
	vStringDelete (name);
	return 1;
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
	if (*p == '.')
	{
		vString *name = vStringNew ();
		extract_name (p + 1, name);
		makeSimpleTag (name, K_LABEL);
		vStringDelete (name);
	}
}

static void findBasicTags (void)
{
	const char *line;

	while ((line = (const char *) readLineFromInputFile ()) != NULL)
	{
		const char *p = line;
		KeyWord const *kw;

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
		for (kw = basic_keywords; kw->token; kw++)
			if (match_keyword (p, kw)) break;

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
