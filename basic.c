/*
 *   $Id:$
 *
 *   Copyright (c) 2000-2006, Darren Hiebert, Elias Pschernig
 *
 *   This source code is released for free distribution under the terms of the
 *   GNU General Public License.
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

#include "options.h"
#include "parse.h"
#include "read.h"
#include "vstring.h"

/*
 *   DATA DEFINITIONS
 */
typedef enum {
	K_CONST,
	K_FUNCTION,
	K_LABEL,
	K_TYPE,
        K_VARIABLE
} BasicKind;

typedef struct {
	char const *token;
	BasicKind kind;
} KeyWord;

static kindOption BasicKinds[] = {
	{TRUE, 'c', "constant", "constants"},
	{TRUE, 'f', "function", "functions"},
	{TRUE, 'l', "label", "labels"},
	{TRUE, 't', "type", "types"},
	{TRUE, 'v', "variable", "variables"}
};

static KeyWord blitzbasic_keywords[] = {
	{"const", K_CONST},
	{"global", K_VARIABLE},
	{"dim", K_VARIABLE},
	{"function", K_FUNCTION},
	{"type", K_TYPE},
	{NULL, 0}
};

static KeyWord purebasic_keywords[] = {
	{"newlist", K_VARIABLE},
	{"global", K_VARIABLE},
	{"dim", K_VARIABLE},
	{"procedure", K_FUNCTION},
	{"interface", K_TYPE},
	{"structure", K_TYPE},
	{NULL, 0}
};

static KeyWord freebasic_keywords[] = {
	{"const", K_CONST},
	{"dim", K_VARIABLE},
	{"common", K_VARIABLE},
	{"function", K_FUNCTION},
	{"sub", K_FUNCTION},
	{"type", K_TYPE},
	{NULL, 0}
};

/*
 *   FUNCTION DEFINITIONS
 */

/* Match the name of a tag (function, variable, type, ...) starting at pos. */
static void extract_name (char const *pos, vString * name)
{
	while (isspace (*pos))
		pos++;
	vStringClear (name);
	for (; *pos && !isspace (*pos) && *pos != '(' && *pos != ','; pos++)
		vStringPut (name, *pos);
	vStringTerminate (name);
}

/* Match a keyword starting at p (case insensitive). */
static void match_keyword (const char *p, const char *keyword, BasicKind kind)
{
	vString *name;
	size_t i;
	for (i = 0; i < strlen (keyword); i++)
	{
		if (tolower (p[i]) != keyword[i])
			return;
	}
	name = vStringNew ();
	extract_name (p + i, name);
	makeSimpleTag (name, BasicKinds, kind);
	vStringDelete (name);
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
		makeSimpleTag (name, BasicKinds, K_LABEL);
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
		makeSimpleTag (name, BasicKinds, K_LABEL);
		vStringDelete (name);
	}
}

static void findBasicTags (KeyWord const keywords[],
	void (*label) (const char *))
{
	const char *line;

	while ((line = (const char *) fileReadLine ()) != NULL)
	{
		const char *p = line;
		KeyWord const *kw;

		while (isspace (*p))
			p++;

		/* Empty line? */
		if (!*p)
			continue;

		/* In Basic, keywords always are at the start of the line. */
		for (kw = keywords; kw->token; kw++)
			match_keyword (p, kw->token, kw->kind);

		/* Is it a label? */
		label (p);
	}
}

static void findBlitzBasicTags (void)
{
	findBasicTags (blitzbasic_keywords, match_dot_label);
}

static void findPureBasicTags (void)
{
	findBasicTags (purebasic_keywords, match_colon_label);
}

static void findFreeBasicTags (void)
{
	findBasicTags (freebasic_keywords, match_colon_label);
}

parserDefinition *BlitzBasicParser (void)
{
	static char const *extensions[] = { "bb", NULL };
	parserDefinition *def = parserNew ("BlitzBasic");
	def->kinds = BasicKinds;
	def->kindCount = KIND_COUNT (BasicKinds);
	def->extensions = extensions;
	def->parser = findBlitzBasicTags;
	return def;
}

parserDefinition *PureBasicParser (void)
{
	static char const *extensions[] = { "pb", NULL };
	parserDefinition *def = parserNew ("PureBasic");
	def->kinds = BasicKinds;
	def->kindCount = KIND_COUNT (BasicKinds);
	def->extensions = extensions;
	def->parser = findPureBasicTags;
	return def;
}

parserDefinition *FreeBasicParser (void)
{
	static char const *extensions[] = { "bas", "bi", NULL };
	parserDefinition *def = parserNew ("FreeBasic");
	def->kinds = BasicKinds;
	def->kindCount = KIND_COUNT (BasicKinds);
	def->extensions = extensions;
	def->parser = findFreeBasicTags;
	return def;
}

/* vi:set tabstop=4 shiftwidth=4: */
