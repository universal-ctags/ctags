/*
*   Copyright (c) 2001-2002, Nick Hibma <n_hibma@van-laarhoven.org>
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for generating tags for YACC language files.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include <string.h>
#include "parse.h"
#include "routines.h"
#include "read.h"
#include "mio.h"
#include "promise.h"


static bool not_in_grammar_rules = true;
static bool in_union;
static tagRegexTable yaccTagRegexTable [] = {
	{"^([A-Za-z][A-Za-z_0-9]+)[ \t]*:", "\\1",
	 "l,label,labels", NULL, &not_in_grammar_rules },
	{"^([A-Za-z][A-Za-z_0-9]+)[ \t]*$", "\\1",
	 "l,label,labels", NULL, &not_in_grammar_rules },
};

struct yaccParserState {
	unsigned long c_input;
	unsigned long c_source;
};

static  bool change_section (const char *line CTAGS_ATTR_UNUSED,
			     const regexMatch *matches CTAGS_ATTR_UNUSED,
			     unsigned int count CTAGS_ATTR_UNUSED,
			     void *data CTAGS_ATTR_UNUSED)
{
	not_in_grammar_rules = !not_in_grammar_rules;

	if (not_in_grammar_rules)
	{
		const unsigned char *tmp, *last;
		long endCharOffset;
		unsigned long c_start;
		unsigned long c_source_start;
		unsigned long c_end;

		readLineFromInputFile ();
		c_start = getInputLineNumber ();
		c_source_start = getSourceLineNumber();

		/* Skip the lines for finding the EOF. */
		endCharOffset = 0;
		last = NULL;
		while ((tmp = readLineFromInputFile ()))
			last = tmp;
		if (last)
			endCharOffset = strlen ((const char *)last);
		/* If `last' is too long, strlen returns a too large value
		   for the positive area of `endCharOffset'. */
		if (endCharOffset < 0)
			endCharOffset = 0;

		c_end = getInputLineNumber ();

		makePromise ("C", c_start, 0, c_end, endCharOffset, c_source_start);
	}
	return true;
}

static bool enter_c_prologue (const char *line CTAGS_ATTR_UNUSED,
			      const regexMatch *matches CTAGS_ATTR_UNUSED,
			      unsigned int count CTAGS_ATTR_UNUSED,
			      void *data)
{
	struct yaccParserState *cstart = data;


	readLineFromInputFile ();
	cstart->c_input  = getInputLineNumber ();
	cstart->c_source = getSourceLineNumber ();
	return true;
}

static bool leave_c_prologue (const char *line CTAGS_ATTR_UNUSED,
			      const regexMatch *matches CTAGS_ATTR_UNUSED,
			      unsigned int count CTAGS_ATTR_UNUSED,
			      void *data)
{
	struct yaccParserState *cstart = data;
	unsigned long c_end;

	c_end = getInputLineNumber ();
	makePromise ("C", cstart->c_input, 0, c_end, 0, cstart->c_source);
	memset (cstart, 0, sizeof (*cstart));

	return true;
}

static bool enter_union (const char *line CTAGS_ATTR_UNUSED,
			 const regexMatch *matches CTAGS_ATTR_UNUSED,
			 unsigned int count CTAGS_ATTR_UNUSED,
			 void *data)
{
	struct yaccParserState *cstart = data;

	if (not_in_grammar_rules)
	{
		in_union = true;
		cstart->c_input = getInputLineNumber ();
		cstart->c_source = getInputLineNumber ();
	}
	return true;
}

static bool leave_union (const char *line CTAGS_ATTR_UNUSED,
			 const regexMatch *matches CTAGS_ATTR_UNUSED,
			 unsigned int count CTAGS_ATTR_UNUSED,
			 void *data)
{
	struct yaccParserState *cstart = data;

	if (not_in_grammar_rules && in_union && cstart->c_input && cstart->c_source)
	{
		unsigned long c_end;

		c_end = getInputLineNumber ();

		makePromise ("C", cstart->c_input, strlen ("%"),
			     c_end, strlen ("}"),
			     cstart->c_source);

		memset (cstart, 0, sizeof (*cstart));
		in_union = false;
	}
	return true;
}

static void initializeYaccParser (langType language)
{
	/*
	   %{ ...
		C language
	   %}
		TOKE DEFINITIONS
	   %%
		SYNTAX
	   %%
		C language
	*/

	static struct yaccParserState state;

	memset (&state, 0, sizeof (state));

	addLanguageCallbackRegex (language, "^%\\{", "{exclusive}", enter_c_prologue, NULL, &state);
	addLanguageCallbackRegex (language, "^%\\}", "{exclusive}", leave_c_prologue, NULL, &state);

	addLanguageCallbackRegex (language, "^%%", "{exclusive}", change_section, NULL, NULL);

	addLanguageCallbackRegex (language, "^%union", "{exclusive}", enter_union, NULL, &state);
	addLanguageCallbackRegex (language, "^}",      "{exclusive}", leave_union, NULL, &state);
}

static void runYaccParser (void)
{
	in_union = false;
	not_in_grammar_rules = true;

	findRegexTags ();
}

extern parserDefinition* YaccParser (void)
{
	static const char *const extensions [] = { "y", NULL };
	parserDefinition* const def = parserNew ("YACC");
	def->extensions = extensions;
	def->initialize = initializeYaccParser;
	def->method     = METHOD_REGEX;
	def->parser     = runYaccParser;
	def->tagRegexTable = yaccTagRegexTable;
	def->tagRegexCount = ARRAY_SIZE (yaccTagRegexTable);
	return def;
}
