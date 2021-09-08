/*
*   Copyright (c) 2000-2001, Max Ischenko <mfi@ukr.net>.
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for generating tags for Lua language.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include <string.h>

#include "debug.h"
#include "entry.h"
#include "parse.h"
#include "read.h"
#include "routines.h"
#include "vstring.h"

/*
*   DATA DEFINITIONS
*/
typedef enum {
	K_FUNCTION,
	K_UNKNOWN,
} luaKind;

typedef enum {
	LUA_UNKNOWN_REFERENCED,
} luaUnknownRole;

static roleDefinition LuaUnknownRoles [] = {
	{ false, "referenced", "referenced somehow" },
};

static kindDefinition LuaKinds [] = {
	{ true, 'f', "function", "functions" },

	/* `unknown' is a kind just for making FQ tag for functions. */
	{ false, 'X', "unknown",  "unknown language object",
	  .referenceOnly = true, ATTACH_ROLES(LuaUnknownRoles) },
};

/*
*   FUNCTION DEFINITIONS
*/

/*
 * Helper function.
 * Returns 1 if line looks like a line of Lua code.
 *
 * TODO: Recognize UNIX bang notation.
 * (Lua treat first line as a comment if it starts with #!)
 *
 */
static bool is_a_code_line (const unsigned char *line)
{
	bool result;
	const unsigned char *p = line;
	while (isspace ((int) *p))
		p++;
	if (p [0] == '\0')
		result = false;
	else if (p [0] == '-' && p [1] == '-')
		result = false;
	else
		result = true;
	return result;
}

static bool isLuaIdentifier (char c)
{
	return (bool) !(isspace(c)  || c == '(' || c == ')' || c == '=' || c == '.' || c == ':');
}

static void set_scope (int child, int parent)
{
	if (parent == CORK_NIL || child == CORK_NIL)
		return;

	tagEntryInfo *e = getEntryInCorkQueue (child);
	if (!e)
		return;

	e->extensionFields.scopeIndex = parent;
}

static void extract_next_token (const char *begin, const char *end_sentinel, vString *name)
{
	if (begin == NULL || end_sentinel == NULL)
		return;

	Assert (begin <= end_sentinel);

	/* Both on '(' */
	if (begin == end_sentinel)
		return;

	/* Trim prefixed white spaces  */
	while (isspace ((int) *begin))
		begin++;

	/* Both on '(' */
	if (begin == end_sentinel)
		return;

	const char *end = end_sentinel - 1;

	/* Trim suffixed white spaces  */
	while (isspace ((int) *end))
		end--;

	Assert (begin <= end);

	int lastCorkIndx = CORK_NIL;
	for (const char *c = begin; c <= end; ++c)
	{
		if (*c == '.' || *c == ':')
		{
			int r = makeSimpleRefTag(name,
									 K_UNKNOWN, LUA_UNKNOWN_REFERENCED);
			set_scope(r, lastCorkIndx);
			lastCorkIndx = r;

			/* Do not include module names in function name */
			vStringClear (name);
		}
		else if (isLuaIdentifier (*c))
			vStringPut (name, (int) *c);
		else
		{
			/* An unexpected character is found
			 * between "function" and "(" */
			vStringClear (name);
			return;
		}
	}

	int d = makeSimpleTag (name, K_FUNCTION);
	set_scope(d, lastCorkIndx);
	vStringClear (name);
}

static void extract_prev_token (const char *end, const char *begin_sentinel, vString *name)
{
	const char *begin;

	if (end == NULL || begin_sentinel == NULL)
		return;

	if (! (begin_sentinel <= end))
		return;

	while (isspace ((int) *end))
	{
		end--;
		if (! (begin_sentinel <= end))
			return;
	}

	begin = end;
	while (begin_sentinel <= begin && isLuaIdentifier (*begin))
		begin--;

	int targetCorkIndex = CORK_NIL;
	if (end - begin)
	{
		vStringNCatS (name, begin + 1, end - begin);
		targetCorkIndex = makeSimpleTag (name, K_FUNCTION);
		vStringClear (name);
	}

	if (targetCorkIndex == CORK_NIL || begin_sentinel == begin)
		return;

	/* Fill the scope field of the function. */
	end = begin;
	while (begin_sentinel <= (begin + 1))
	{
		bool on_boundary = false;
		if (begin < begin_sentinel || !isLuaIdentifier (*begin))
		{
			if (end - begin)
			{
				vStringNCatS (name, begin + 1, end - begin);
				int r = makeSimpleRefTag (name,
										  K_UNKNOWN, LUA_UNKNOWN_REFERENCED);
				set_scope (targetCorkIndex, r);
				targetCorkIndex = r;
				vStringClear (name);
			}
			if (begin_sentinel <= begin && ! (*begin == ':' || *begin == '.'))
				break;
			on_boundary = true;
		}
		begin--;

		if(on_boundary)
			end = begin;
	}
}

static void findLuaTags (void)
{
	vString *name = vStringNew ();
	const unsigned char *line;

	while ((line = readLineFromInputFile ()) != NULL)
	{
		const char *p, *q;

		if (! is_a_code_line (line))
			continue;

		p = (const char*) strstr ((const char*) line, "function");
		if (p == NULL)
			continue;

		q = strchr ((const char*) line, '=');

		if (q == NULL) {
			p = p + 8;  /* skip the `function' word */

			/* We expect [ \t(] */
			if (! (*p == '(' || isspace ((int)*p)))
				continue;
			q = strchr ((const char*) p, '(');
			if (q)
				extract_next_token (p, q, name);
		} else if (
			   (*(q+1) != '=') /* ignore `if type(v) == "function" then ...' */
			   && (q < p)	   /* ignore "function" ~=  */
			   ) {
			p = (const char*) &line[0];
			if (p < q)
				extract_prev_token (q - 1, p, name);
		}
	}
	vStringDelete (name);
}

extern parserDefinition* LuaParser (void)
{
	static const char* const extensions [] = { "lua", NULL };
	parserDefinition* def = parserNew ("Lua");
	def->kindTable      = LuaKinds;
	def->kindCount  = ARRAY_SIZE (LuaKinds);
	def->extensions = extensions;
	def->parser     = findLuaTags;
	def->useCork    = CORK_QUEUE;
	def->requestAutomaticFQTag = true;
	return def;
}
