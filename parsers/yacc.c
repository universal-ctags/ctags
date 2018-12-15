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

enum yaccParsingArea {
	YACC_TOP_LEVEL,
	YACC_C_PROLOGUE,
	YACC_UNION,
	YACC_GRAMMAR,
	YACC_C_EPILOGUE,
};

static struct yaccParserState {
	bool not_in_grammar_rules;
	enum yaccParsingArea area;
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
	if (state->area == YACC_GRAMMAR)
		state->area = YACC_C_EPILOGUE;
	else
		state->area = YACC_GRAMMAR;
	return true;
}

static bool enter_c_prologue (const char *line CTAGS_ATTR_UNUSED,
			      const regexMatch *matches CTAGS_ATTR_UNUSED,
			      unsigned int count CTAGS_ATTR_UNUSED,
			      void *data)
{
	struct yaccParserState *state = data;

	state->area = YACC_C_PROLOGUE;
	return true;
}

static bool leave_c_prologue (const char *line CTAGS_ATTR_UNUSED,
			      const regexMatch *matches CTAGS_ATTR_UNUSED,
			      unsigned int count CTAGS_ATTR_UNUSED,
			      void *data)
{
	struct yaccParserState *state = data;

	state->area = YACC_TOP_LEVEL;
	return true;
}

static bool enter_union (const char *line CTAGS_ATTR_UNUSED,
			 const regexMatch *matches CTAGS_ATTR_UNUSED,
			 unsigned int count CTAGS_ATTR_UNUSED,
			 void *data)
{
	struct yaccParserState *state = data;

	if (state->area == YACC_TOP_LEVEL)
		state->area = YACC_UNION;
	return true;
}

static bool leave_union (const char *line CTAGS_ATTR_UNUSED,
			 const regexMatch *matches CTAGS_ATTR_UNUSED,
			 unsigned int count CTAGS_ATTR_UNUSED,
			 void *data)
{
	struct yaccParserState *state = data;

	if (state->area == YACC_UNION)
		state->area = YACC_TOP_LEVEL;
	return true;
}

static void make_promise_for_epilogue (void)
{
	const unsigned char *tmp;
	long endCharOffset;
	unsigned long c_start;
	unsigned long c_source_start;
	unsigned long c_end;

	/* We are at line with %% so the next line is the start of epilogue */
	c_start = getInputLineNumber () + 1;
	c_source_start = getSourceLineNumber() + 1;

	/* Skip the lines for finding the EOF. */
	endCharOffset = 0;
	while ((tmp = readLineFromInputFile ()))
	{
		/* We want to get strlen() of the last line only but because
		 * readLineFromInputFile() invalidates the previous value, get
		 * endCharOffset here while the tmp variable is valid. */
		endCharOffset = strlen ((const char *)tmp);
	}
	/* If `last' is too long, strlen returns a too large value
	   for the positive area of `endCharOffset'. */
	if (endCharOffset < 0)
		endCharOffset = 0;

	c_end = getInputLineNumber ();
	makePromise ("C", c_start, 0, c_end, endCharOffset, c_source_start);
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
	enum yaccParsingArea last_area;

	unsigned long c_input = 0;
	unsigned long c_source = 0;

	parserState.not_in_grammar_rules = true;

	c_input = 0;
	c_source = 0;
	parserState.area = YACC_TOP_LEVEL;
	last_area = parserState.area;

	while (readLineFromInputFile () != NULL)
	{
		if (last_area == YACC_TOP_LEVEL &&
			parserState.area == YACC_C_PROLOGUE)
		{
			if (readLineFromInputFile ())
			{
				c_input  = getInputLineNumber ();
				c_source = getSourceLineNumber ();
			}
		}
		else if (last_area == YACC_C_PROLOGUE
				 && parserState.area == YACC_TOP_LEVEL)
		{
			unsigned long c_end = getInputLineNumber ();
			makePromise ("C", c_input, 0, c_end, 0, c_source);
			c_input = 0;
			c_source = 0;
		}
		else if (last_area == YACC_TOP_LEVEL
				 && parserState.area == YACC_UNION)
		{
			c_input = getInputLineNumber ();
			c_source = getInputLineNumber ();
		}
		else if (last_area == YACC_UNION
				 && parserState.area == YACC_TOP_LEVEL)
		{
			unsigned long c_end = getInputLineNumber ();
			makePromise ("C", c_input, strlen ("%"), c_end, strlen ("}"),
						 c_source);
			c_input = 0;
			c_source = 0;
		}
		else if (parserState.area == YACC_C_EPILOGUE)
		{
			make_promise_for_epilogue ();
		}
		last_area = parserState.area;
	}

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
