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
#include "parse.h"
#include "read.h"
#include "routines.h"
#include "vstring.h"

/*
*   DATA DEFINITIONS
*/
typedef enum {
	K_FUNCTION
} luaKind;

static kindDefinition LuaKinds [] = {
	{ true, 'f', "function", "functions" }
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
	return (bool) !(isspace(c)  || c == '(' || c == ')' || c == '=');
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

	for (const char *c = begin; c <= end; ++c)
	{
		if (isLuaIdentifier (*c))
			vStringPut (name, (int) *c);
		else
		{
			/* An unexpected character is found
			 * between "function" and "(" */
			vStringClear (name);
			return;
		}
	}

	makeSimpleTag (name, K_FUNCTION);
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

	if (end - begin)
	{
		vStringNCatS (name, begin + 1, end - begin);
		makeSimpleTag (name, K_FUNCTION);
		vStringClear (name);
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
	return def;
}
