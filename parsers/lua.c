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

#include "options.h"
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

static kindOption LuaKinds [] = {
	{ TRUE, 'f', "function", "functions" }
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
static boolean is_a_code_line (const unsigned char *line)
{
	boolean result;
	const unsigned char *p = line;
	while (isspace ((int) *p))
		p++;
	if (p [0] == '\0')
		result = FALSE;
	else if (p [0] == '-' && p [1] == '-')
		result = FALSE;
	else
		result = TRUE;
	return result;
}

static boolean isLuaIdentifier (char c)
{
	return (boolean) !(isspace(c)  || c == '(' || c == ')' || c == '=');
}

static void extract_next_token (const char *begin, const char *end_sentinel, vString *name)
{
	boolean found;

	if (begin == NULL || end_sentinel == NULL)
		return;

	if (! (begin < end_sentinel))
		return;

	while (isspace ((int) *begin))
	{
		begin++;
		if (! (begin < end_sentinel))
			return;
	}

	found = FALSE;
	while (begin != end_sentinel && isLuaIdentifier (*begin))
	{
		vStringPut (name, (int) *begin);
		begin++;
		found = TRUE;
	}

	if (found)
	{
		vStringTerminate (name);
		makeSimpleTag (name, LuaKinds, K_FUNCTION);
		vStringClear (name);
	}

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
		makeSimpleTag (name, LuaKinds, K_FUNCTION);
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
			p = p + 9;  /* skip the `function' word */
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
	def->kinds      = LuaKinds;
	def->kindCount  = ARRAY_SIZE (LuaKinds);
	def->extensions = extensions;
	def->parser     = findLuaTags;
	return def;
}
