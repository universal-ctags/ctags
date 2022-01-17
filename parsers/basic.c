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
 *
 *   FreeBasic:
 *   - https://www.freebasic.net/wiki/DocToc
 */

/*
 *   INCLUDE FILES
 */
#include "general.h" /* must always come first */

#include <string.h>

#include "entry.h"
#include "keyword.h"
#include "parse.h"
#include "read.h"
#include "routines.h"
#include "trace.h"
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
	K_ENUM,
	K_NAMESPACE,
} BasicKind;

typedef enum {
	BASIC_FUNCTION_DECL,
} basciFunctionRole;

static roleDefinition BasicFunctionRoles [] = {
	{ true, "decl", "declared" },
};

static kindDefinition BasicKinds[] = {
	{true, 'c', "constant", "constants"},
	{true, 'f', "function", "functions",
	 .referenceOnly = false, ATTACH_ROLES(BasicFunctionRoles)},
	{true, 'l', "label", "labels"},
	{true, 't', "type", "types"},
	{true, 'v', "variable", "variables"},
	{true, 'g', "enum", "enumerations"},
	{true, 'n', "namespace", "namespace"},
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
	KEYWORD_NAMESPACE,
	KEYWORD_END,
	KEYWORD_ACCESS,
	KEYWORD_DECLARE,
};
typedef int keywordId; /* to allow KEYWORD_NONE */

static const keywordTable BasicKeywordTable[] = {
	/* freebasic */
	{"const", KEYWORD_CONST},
	{"dim", KEYWORD_VARIABLE},
	{"common", KEYWORD_VARIABLE},
	{"function", KEYWORD_FUNCTION},
	{"sub", KEYWORD_FUNCTION},
	{"private", KEYWORD_ACCESS},
	{"public", KEYWORD_ACCESS},
	{"property", KEYWORD_FUNCTION},
	{"constructor", KEYWORD_FUNCTION},
	{"destructor", KEYWORD_FUNCTION},
	{"type", KEYWORD_TYPE},
	{"enum", KEYWORD_ENUM},
	{"namespace", KEYWORD_NAMESPACE},
	{"end", KEYWORD_END},
	{"declare", KEYWORD_DECLARE},

	/* blitzbasic, purebasic */
	{"global", KEYWORD_VARIABLE},

	/* purebasic */
	{"newlist", KEYWORD_VARIABLE},
	{"procedure", KEYWORD_FUNCTION},
	{"interface", KEYWORD_TYPE},
	{"structure", KEYWORD_TYPE},
};

struct BasicKeywordAttr {
	int kind;
} keywordAttrs [] = {
	[KEYWORD_ENUM]  = {
		.kind = K_ENUM,
	},
	[KEYWORD_CONST] = {
		.kind = K_CONST,
	},
	[KEYWORD_FUNCTION] = {
		.kind = K_FUNCTION,
	},
	[KEYWORD_LABEL] = {
		.kind = K_LABEL,
	},
	[KEYWORD_TYPE] = {
		.kind = K_TYPE,
	},
	[KEYWORD_VARIABLE] = {
		.kind = K_VARIABLE,
	},
	[KEYWORD_NAMESPACE] = {
		.kind = K_NAMESPACE,
	},
	[KEYWORD_END] = {
		.kind = KIND_GHOST_INDEX,
	},
	[KEYWORD_ACCESS] = {
		.kind = KIND_GHOST_INDEX,
	},
	[KEYWORD_DECLARE] = {
		.kind = KIND_GHOST_INDEX,
	},
};

struct matchState {
	const char *access;
	bool end;
	bool declaration;
};

static int currentScope;
/*
 *   FUNCTION DEFINITIONS
 */

static void pushScope (int corkIndex)
{
#ifdef DEBUG
	tagEntryInfo *e = getEntryInCorkQueue (corkIndex);
	TRACE_PRINT ("scope push: %s<%d>", e? e->name: "-", corkIndex);
#endif
	currentScope = corkIndex;
}

static void popScope (void)
{
	tagEntryInfo *e = getEntryInCorkQueue (currentScope);
#ifdef DEBUG
	TRACE_PRINT ("scope pop: %s<%d>", e? e->name: "-", currentScope);
#endif
	if (e)
	{
		e->extensionFields.endLine = getInputLineNumber ();
		currentScope = e->extensionFields.scopeIndex;
	}
	else
		currentScope = CORK_NIL;
}

static void updateScope (int corkIndex, int kindIndex, int keywordId)
{
	if (corkIndex != CORK_NIL && kindIndex == K_NAMESPACE)
		pushScope (corkIndex);
	else if (corkIndex == CORK_NIL && kindIndex == K_NAMESPACE)
		popScope ();
}

static int keywordToKind (keywordId keywordId)
{
	if (keywordId == KEYWORD_NONE)
		return KIND_GHOST_INDEX;
	return keywordAttrs [keywordId].kind;
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

static int makeBasicRefTag (vString *name, int kindIndex, int roleIndex)
{
	int r = makeSimpleRefTag (name, kindIndex, roleIndex);
	tagEntryInfo *e = getEntryInCorkQueue (r);
	if (e)
		e->extensionFields.scopeIndex = currentScope;
	return r;
}

static int makeBasicTag (vString *name, int kindIndex)
{
	return makeBasicRefTag (name, kindIndex, ROLE_DEFINITION_INDEX);
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
	makeBasicTag (name, kind);

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
		makeBasicTag (name, kind);
	}

	vStringDelete (name);
}

/* Match the name of a tag (function, variable, type, ...) starting at pos. */
static int extract_name (char const *pos, BasicKind kind, struct matchState *state)
{
	int r = CORK_NIL;
	vString *name = vStringNew ();
	for (; isIdentChar (*pos); pos++)
		vStringPut (name, *pos);
	if (state && state->declaration)
	{
		if (kind == K_FUNCTION)
			r = makeBasicRefTag (name, kind, BASIC_FUNCTION_DECL);
	}
	else
		r = makeBasicTag (name, kind);
	vStringDelete (name);

	tagEntryInfo *e = getEntryInCorkQueue (r);
	if (e && state && state->access)
		e->extensionFields.access = eStrdup (state->access);

	return r;
}

/* Match a keyword starting at p (case insensitive). */
static void match_state_reset (struct matchState *state)
{
	state->access = NULL;
	state->end = false;
	state->declaration =false;
}

static bool match_keyword (const char **cp, vString *buf,
						   struct matchState *state)
{
	const char *p;

	for (p = *cp; *p != '\0' && !isspace((unsigned char)*p); p++)
	{
		int c = tolower ((unsigned char)*p);
		vStringPut (buf, c);
	}

	int kw = lookupKeyword (vStringValue (buf), getInputLanguage ());
	if (kw == KEYWORD_NONE)
		return false;

	const char *old_p = p;
	while (isspace (*p))
		p++;

	if (kw == KEYWORD_ACCESS)
	{
		state->access = vStringValue(buf)[1] == 'r'? "private": "public";
		*cp = p;
		return true;
	}
	else if (kw == KEYWORD_END)
	{
		state->end = true;
		*cp = p;
		return true;
	}
	else if (kw == KEYWORD_DECLARE)
	{
		state->declaration = true;
		*cp = p;
		return true;
	}

	int kind = keywordToKind (kw);
	int index = CORK_NIL;
	if (!state->end)
	{
		/* create tags only if there is some space between the keyword and the identifier */
		if (kind != KIND_GHOST_INDEX && old_p == p)
			return false;

		if (kind == K_VARIABLE)
			extract_dim (p, kind); /* extract_dim adds the found tag(s) */
		else
			index = extract_name (p, kind, state);
	}

	updateScope (index, kind, kw);

	return false;
}

/* Match a "label:" style label. */
static void match_colon_label (char const *p)
{
	char const *end = p + strlen (p) - 1;
	while (isspace (*end))
		end--;
	if (*end == ':')
	{
		vString *name = vStringNewNInit (p, end - p);
		makeBasicTag (name, K_LABEL);
		vStringDelete (name);
	}
}

/* Match a ".label" style label. */
static void match_dot_label (char const *p)
{
	extract_name (p + 1, K_LABEL, NULL);
}

static void findBasicTags (void)
{
	const char *line;

	currentScope = CORK_NIL;
	vString *buf = vStringNew ();

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
		struct matchState state;
		match_state_reset (&state);
		do
			vStringClear (buf);
		while (match_keyword (&p, buf, &state));


		/* Is it a label? */
		if (*p == '.')
			match_dot_label (p);
		else
			match_colon_label (p);
	}
	vStringDelete (buf);
}

parserDefinition *BasicParser (void)
{
	static char const *extensions[] = { "bas", "bi", "bm", "bb", "pb", NULL };
	parserDefinition *def = parserNew ("Basic");
	def->kindTable = BasicKinds;
	def->kindCount = ARRAY_SIZE (BasicKinds);
	def->extensions = extensions;
	def->parser = findBasicTags;
	def->keywordTable = BasicKeywordTable;
	def->keywordCount = ARRAY_SIZE (BasicKeywordTable);
	def->useCork = CORK_QUEUE;
	return def;
}
