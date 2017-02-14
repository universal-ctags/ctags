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

static struct yaccParserState {
	bool not_in_grammar_rules;
	bool in_union;
	unsigned long c_input;
	unsigned long c_source;
} parserState;

static tagRegexTable yaccTagRegexTable [] = {
	{"^([A-Za-z][A-Za-z_0-9]+)[ \t]*:", "\\1",
	 "l,label,labels", NULL, &parserState.not_in_grammar_rules },
	{"^([A-Za-z][A-Za-z_0-9]+)[ \t]*$", "\\1",
	 "l,label,labels", NULL, &parserState.not_in_grammar_rules },
};

static  bool change_section (const char *line CTAGS_ATTR_UNUSED,
			     const regexMatch *matches CTAGS_ATTR_UNUSED,
			     unsigned int count CTAGS_ATTR_UNUSED,
			     void *data)
{
	struct yaccParserState *state = data;
	state->not_in_grammar_rules = !state->not_in_grammar_rules;

	if (state->not_in_grammar_rules)
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
	struct yaccParserState *state = data;


	readLineFromInputFile ();
	state->c_input  = getInputLineNumber ();
	state->c_source = getSourceLineNumber ();
	return true;
}

static bool leave_c_prologue (const char *line CTAGS_ATTR_UNUSED,
			      const regexMatch *matches CTAGS_ATTR_UNUSED,
			      unsigned int count CTAGS_ATTR_UNUSED,
			      void *data)
{
	struct yaccParserState *state = data;
	unsigned long c_end;

	c_end = getInputLineNumber ();
	makePromise ("C", state->c_input, 0, c_end, 0, state->c_source);
	state->c_input = 0;
	state->c_source = 0;
	return true;
}

static bool enter_union (const char *line CTAGS_ATTR_UNUSED,
			 const regexMatch *matches CTAGS_ATTR_UNUSED,
			 unsigned int count CTAGS_ATTR_UNUSED,
			 void *data)
{
	struct yaccParserState *state = data;

	if (state->not_in_grammar_rules)
	{
		state->in_union = true;
		state->c_input = getInputLineNumber ();
		state->c_source = getInputLineNumber ();
	}
	return true;
}

static bool leave_union (const char *line CTAGS_ATTR_UNUSED,
			 const regexMatch *matches CTAGS_ATTR_UNUSED,
			 unsigned int count CTAGS_ATTR_UNUSED,
			 void *data)
{
	struct yaccParserState *state = data;

	if (state->not_in_grammar_rules && state->in_union && state->c_input && state->c_source)
	{
		unsigned long c_end;

		c_end = getInputLineNumber ();

		makePromise ("C", state->c_input, strlen ("%"),
			     c_end, strlen ("}"),
			     state->c_source);

		state->c_input = 0;
		state->c_source = 0;
		state->in_union = false;
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

	addLanguageCallbackRegex (language, "^%\\{", "{exclusive}", enter_c_prologue, NULL, &parserState);
	addLanguageCallbackRegex (language, "^%\\}", "{exclusive}", leave_c_prologue, NULL, &parserState);

	addLanguageCallbackRegex (language, "^%%", "{exclusive}", change_section, NULL, &parserState);

	addLanguageCallbackRegex (language, "^%union", "{exclusive}", enter_union, NULL, &parserState);
	addLanguageCallbackRegex (language, "^}",      "{exclusive}", leave_union, NULL, &parserState);
}

static void runYaccParser (void)
{
	parserState.in_union = false;
	parserState.not_in_grammar_rules = true;
	parserState.c_input = 0;
	parserState.c_source = 0;

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
