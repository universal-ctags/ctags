/*
 *   Copyright (c) 2018 Masatake YAMATO
 *   Copyright (c) 2018 Red Hat, Inc.
 *
 *   This source code is released for free distribution under the terms of the
 *   GNU General Public License version 2 or (at your option) any later version.
 *
 *   This module contains functions to generate tags for Fypp, a Python powered
 *   Fortran preprocessor.
 *   See: https://github.com/aradi/fypp
 */

#include "general.h"  /* must always come first */

#include "ctags.h"
#include "debug.h"
#include "entry.h"
#include "numarray.h"
#include "param.h"
#include "parse.h"
#include "promise.h"
#include "read.h"
#include "trace.h"

#include <string.h>

typedef enum {
	K_MACRO
} fyppKind;

static kindDefinition FyppKinds[] = {
	{ true, 'm', "macro", "macros" },
};

/* TODO
   If "guest" extra is disabled, we can reduce the codes to be processed. */

struct fyppParseCtx {
	int macro_cork_index;

	/* Areas surrounded by Fypp directive should be masked with white
	   spaces when running a guest parser like Fortran parser.

	   in_fypp_area field counts the depth of areas. Zero means the
	   current line is not part of a Fypp area; the line should passed
	   as is to the guest parser. Otherwise it should not.

	   #:if/#:elif/#:else/#:endif areas are handled in a special way.
	   #:if areas are passed to the guest parser.
	   #:elif/#:else/#:endif areas are not.
	   This approach follows what CPreProcessor parser does.
	   Quoted from ctags.1 man page:

	   If a preprocessor conditional is encountered within a statement
	   which defines a tag, ctags follows only the first branch of
	   that conditional (except in the special case of "#if 0", in
	   which case it follows only the last branch).

	   About the behavior about "#if 0" is not applicable to Fypp parser.
	   if_tracker is for tracking "the first branch of that conditional".
	   A scalar value is not enough to track the branch because branches
	   can be nested:

	   #:if COND0
	   ***
	   #:if COND1
	   ***
	   #:else
	   ...
	   #:endif
	   ...
	   #:else
	   ...
	   #:endif

	   In the above example, Fypp parser passes only `***` lines to the
	   guest parser.

	   if_tracker field tracks the branches. When the parser enters #:if area,
	   it appends 1 to if_tracker. When it enters #:elif or #:else area,
	   it repalces 1 with 0. When it reaches at #:endif, remove the last
	   element from the field. The product of if_tracker should be either
	   0 or 1. In the case that the product is 1, we can say the current line
	   is in the first if block.

	   if_cont field is for tracking '&' at the end of #:if directive lines.
	   If it is true, the last line is ended with &. We can know the
	   current line is part of Fypp directive that should not be
	   passed to the guest parser.

	   other_cont field has the same purpose as if_cont but
	   other_cont is for the other directives than #:if.
	*/
	int in_fypp_area;
	intArray *if_tracker;
	bool if_cont;
	bool other_cont;

	/* fypp_lines field records the line numbers for masking. */
	ulongArray *fypp_lines;
};

static vString *fyppGuestParser;

static bool fypp_does_line_continue(const char *line,
									const regexMatch *matches)
{
	if (matches[0].length == 0)
		return false;
	return *(line + matches[0].start + matches[0].length - 1) == '&';
}

static bool fypp_start_cb(const char *line,
						  const regexMatch *matches,
						  unsigned int count,
						  void *userData)
{
	struct fyppParseCtx *ctx = userData;

	TRACE_PRINT_PREFIX();
	TRACE_PRINT_FMT("%04d - %s", getInputLineNumber(), line);

	ulongArrayAdd (ctx->fypp_lines,
				   getInputLineNumber());
	ctx->in_fypp_area++;
	ctx->if_cont = false;
	ctx->other_cont = false;

	return true;
}

static bool fypp_end_cb(const char *line,
						const regexMatch *matches,
						unsigned int count,
						void *userData)
{
	struct fyppParseCtx *ctx = userData;

	TRACE_PRINT_PREFIX();
	TRACE_PRINT_FMT("%04d - %s", getInputLineNumber(), line);

	ulongArrayAdd (ctx->fypp_lines,
				   getInputLineNumber());
	ctx->in_fypp_area--;
	ctx->if_cont = false;
	ctx->other_cont = false;

	return true;
}

static bool fypp_line_cb (const char *line,
						  const regexMatch *matches,
						  unsigned int count,
						  void *userData)
{
	struct fyppParseCtx *ctx = userData;

	TRACE_PRINT_PREFIX();
	TRACE_PRINT_FMT("%04d - %s", getInputLineNumber(), line);

	ulongArrayAdd (ctx->fypp_lines,
				   getInputLineNumber());
	ctx->if_cont = false;
	ctx->other_cont = fypp_does_line_continue (line, matches);

	return true;
}

static bool macro_start_cb (const char *line,
							const regexMatch *matches,
							unsigned int count,
							void *userData)
{
	struct fyppParseCtx *ctx = userData;
	vString *macro = NULL;
	vString *signature = NULL;

	if (count > 0)
	{
		tagEntryInfo e;

		macro = vStringNew ();
		vStringNCopyS (macro,
					   line + matches[1].start,
					   matches[1].length);

		initTagEntry (&e, vStringValue (macro), K_MACRO);

		if (count > 1)
		{
			signature = vStringNew ();
			vStringNCopyS (signature,
						   line + matches[2].start,
						   matches[2].length);
			e.extensionFields.signature = vStringValue (signature);
		}

		ctx->macro_cork_index = makeTagEntry (&e);
	}

	if (macro)
		vStringDelete (macro);
	if (signature)
		vStringDelete (signature);

	fypp_start_cb (line, matches, count, userData);
	return true;
}

static bool macro_end_cb (const char *line,
						  const regexMatch *matches,
						  unsigned int count,
						  void *userData)
{
	struct fyppParseCtx *ctx = userData;

	tagEntryInfo *e = getEntryInCorkQueue (ctx->macro_cork_index);
	if (e)
		e->extensionFields.endLine = getInputLineNumber ();
	ctx->macro_cork_index = CORK_NIL;

	fypp_end_cb (line, matches, count, userData);
	return true;
}

static bool if_start_cb (const char *line,
						 const regexMatch *matches,
						 unsigned int count,
						 void *userData)
{
	struct fyppParseCtx *ctx = userData;

	if (ctx->if_tracker == NULL)
		ctx->if_tracker = intArrayNew ();

	intArrayAdd (ctx->if_tracker, 1);

	fypp_start_cb (line, matches, count, userData);

	ctx->if_cont = fypp_does_line_continue (line, matches);
	TRACE_PRINT("(count: %d, len: %d, ifCont: %d)",
				count, matches[1].length, ctx->if_cont);

	return true;
}

static bool if_else_cb (const char *line,
						const regexMatch *matches,
						unsigned int count,
						void *userData)
{
	struct fyppParseCtx *ctx = userData;

	if (ctx->if_tracker)
	{
		intArrayRemoveLast (ctx->if_tracker);
		intArrayAdd (ctx->if_tracker, 0);
	}

	fypp_line_cb(line, matches, count, userData);
	return true;
}

static bool if_end_cb (const char *line,
					   const regexMatch *matches,
					   unsigned int count,
					   void *userData)
{
	struct fyppParseCtx *ctx = userData;

	if (ctx->if_tracker)
	{
		intArrayRemoveLast (ctx->if_tracker);

		if (intArrayCount(ctx->if_tracker) == 0)
		{
			intArrayDelete (ctx->if_tracker);
			ctx->if_tracker = NULL;
		}
	}

	fypp_end_cb (line, matches, count, userData);
	return true;
}

static bool is_in_first_if_block (struct fyppParseCtx *ctx)
{
	int r = 1;

	for (unsigned int i = 0; i < intArrayCount (ctx->if_tracker); i++)
		r *= intArrayItem(ctx->if_tracker, i);

	return r;
}

static bool non_fypp_line_cb (const char *line,
							  const regexMatch *matches,
							  unsigned int count,
							  void *userData)
{
	struct fyppParseCtx *ctx = userData;

	if ((ctx->in_fypp_area > 0 && ((!ctx->if_tracker)
								  || (! is_in_first_if_block (ctx))
								  || ctx->if_cont))
		|| ctx->other_cont )
	{
		ulongArrayAdd (ctx->fypp_lines,
					   getInputLineNumber());

		TRACE_PRINT_PREFIX();
		TRACE_PRINT_FMT("%04d - %s", getInputLineNumber(), line);
		TRACE_PRINT_PREFIX();
		TRACE_PRINT_FMT("(inFyppArea: %d, ifTracker: %p, inFirstIfBlock: %d, ifCont: %d otherCont: %d/ ",
						ctx->in_fypp_area, ctx->if_tracker,
						ctx->if_tracker? is_in_first_if_block (ctx): -1,
						ctx->if_cont,
						ctx->other_cont);
#ifdef DO_TRACING
		if (ctx->if_tracker)
		{
			for (int i = 0; i < intArrayCount (ctx->if_tracker); i++)
				TRACE_PRINT_FMT("%d ", intArrayItem(ctx->if_tracker, i));
		}
#endif	/* DO_TRACING */
		TRACE_PRINT_FMT(")");
		TRACE_PRINT_NEWLINE();

		bool continued = fypp_does_line_continue(line, matches);
		if (ctx->if_cont)
		{
			ctx->if_cont = continued;
			TRACE_PRINT("(ifCont <= %d)", ctx->if_cont);
		}
		if (ctx->other_cont)
		{
			ctx->other_cont = continued;
			TRACE_PRINT("(otherCont <= %d)", ctx->other_cont);
		}
	}
	else
	{
		TRACE_PRINT_PREFIX();
		TRACE_PRINT_FMT("%04d + %s", getInputLineNumber(), line);
		TRACE_PRINT_PREFIX();
		TRACE_PRINT_FMT("(inFyppArea: %d, ifTracker: %p, ifCont: %d / ",
						ctx->in_fypp_area, ctx->if_tracker,
						ctx->if_cont);
		TRACE_PRINT_FMT(")");
		TRACE_PRINT_NEWLINE();
	}

	return true;
}


static struct fyppParseCtx parseCtx;

static void findFyppTags (void)
{
	int promise;

	parseCtx.macro_cork_index = CORK_NIL;
	parseCtx.if_tracker = NULL;

	if (fyppGuestParser)
		parseCtx.fypp_lines = ulongArrayNew ();

	findRegexTags ();


	if (fyppGuestParser)
	{
		promise = makePromise (vStringValue(fyppGuestParser),
							   1, 0,
							   getInputLineNumber(), 0,
							   0);
		if (promise >= 0)
			promiseAttachLineFiller (promise, parseCtx.fypp_lines);
		else
			ulongArrayDelete (parseCtx.fypp_lines);
	}

	if (parseCtx.if_tracker)
	{
		intArrayDelete (parseCtx.if_tracker);
		parseCtx.if_tracker = NULL;
	}
}

static void initializeFyppParser (langType language)
{
	addLanguageCallbackRegex (language,  "^[ \t]*#:[ \t]*def[ \t]*([a-zA-Z][a-zA-Z0-9_]*)[ \t]*(\\(.*\\))",
							  "{exclusive}", macro_start_cb, NULL, &parseCtx);
	addLanguageCallbackRegex (language,  "^[ \t]*#:[ \t]*enddef[ \t]*([a-zA-Z][a-zA-Z0-9_]*)[ \t]*$",
							  "{exclusive}", macro_end_cb, NULL, &parseCtx);
	addLanguageCallbackRegex (language,  "^[ \t]*#:[ \t]*for[ \t].*$",
							  "{exclusive}", fypp_start_cb, NULL, &parseCtx);
	addLanguageCallbackRegex (language,  "^[ \t]*#:[ \t]*endfor.*$",
							  "{exclusive}", fypp_end_cb, NULL, &parseCtx);
	addLanguageCallbackRegex (language,  "^[ \t]*#:[ \t]*call[ \t].*$",
							  "{exclusive}", fypp_start_cb, NULL, &parseCtx);
	addLanguageCallbackRegex (language,  "^[ \t]*#:[ \t]*endcall.*$",
							  "{exclusive}", fypp_end_cb, NULL, &parseCtx);

	addLanguageCallbackRegex (language,  "^[ \t]*#:[ \t]*if[ \t].*$",
							  "{exclusive}", if_start_cb, NULL, &parseCtx);
	addLanguageCallbackRegex (language,  "^[ \t]*#:[ \t]*el(se|if)[ \t].*$",
							  "{exclusive}", if_else_cb, NULL, &parseCtx);
	addLanguageCallbackRegex (language,  "^[ \t]*#:[ \t]*endif.*",
							  "{exclusive}", if_end_cb, NULL, &parseCtx);

	addLanguageCallbackRegex (language,  "^[ \t]*(#!|[#@$]:).*$",
							  "{exclusive}", fypp_line_cb, NULL, &parseCtx);
	addLanguageCallbackRegex (language,  "^.*$",
							  "{exclusive}", non_fypp_line_cb, NULL, &parseCtx);
}

static void finalizeFyppParser (langType language, bool initialized)
{
	if (fyppGuestParser)
	{
		vStringDelete (fyppGuestParser);
		fyppGuestParser = NULL;
	}
}

static void fyppSetGuestParser (const langType language CTAGS_ATTR_UNUSED,
								const char *optname CTAGS_ATTR_UNUSED, const char *arg)
{
	if (!strcmp (arg, RSV_NONE))
	{
		if (fyppGuestParser)
		{
			vStringDelete (fyppGuestParser);
			fyppGuestParser = NULL;
		}
		return;
	}

	langType lang = getNamedLanguage (arg, strlen(arg));
	if (lang == LANG_IGNORE)
		error (FATAL, "Unknown language: %s", arg);

	if (fyppGuestParser)
		vStringClear(fyppGuestParser);
	else
		fyppGuestParser = vStringNew();
	vStringCatS (fyppGuestParser, arg);
}

static parameterHandlerTable FyppParameterHandlerTable [] = {
	{ .name = "guest",
	  .desc = "parser run after Fypp parser parses the original input (\"NONE\" or a parser name [Fortran])" ,
	  .handleParameter = fyppSetGuestParser,
	},
};

extern parserDefinition* FyppParser (void)
{
	static const char *const extensions [] = { "fy", NULL };
	parserDefinition* const def = parserNew ("Fypp");
	def->kindTable = FyppKinds;
	def->kindCount = ARRAY_SIZE (FyppKinds);
	def->extensions = extensions;
	def->parser = findFyppTags;
	def->initialize = initializeFyppParser;
	def->finalize   = finalizeFyppParser;
	def->method     = METHOD_REGEX;

	def->parameterHandlerTable = FyppParameterHandlerTable;
	def->parameterHandlerCount = ARRAY_SIZE(FyppParameterHandlerTable);

	def->useCork = CORK_QUEUE;

	fyppGuestParser = vStringNewInit ("Fortran");

	return def;
}
