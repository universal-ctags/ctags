/*
*   Copyright (c) 2000-2003, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for applying regular expression matching.
*
*   The code for utilizing the Gnu regex package with regards to processing the
*   regex option and checking for regex matches was adapted from routines in
*   Gnu etags.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include <regex.h>
#include "lregex_p.h"

/*
*    FUNCTION DECLARATIONS
*/
static int match (struct regexBackend *backend,
				  void *code, const char *input, size_t size,
				  regmatch_t pmatch[BACK_REFERENCE_COUNT]);
static regexCompiledCode compile (struct regexBackend *backend,
								  const char *const regexp,
								  int flags);
static void delete_code (void *code);
static void set_icase_flag (int *flags);

/*
*    DATA DEFINITIONS
*/
static struct regexBackend defaultRegexBackend = {
	.fdefs = NULL,
	.fdef_count = 0,
	.set_icase_flag = set_icase_flag,
	.compile = compile,
	.match = match,
	.delete_code = delete_code,
};

/*
*    FUNCTOIN DEFINITIONS
*/
extern void basic_regex_flag_short (char c, void* data)
{
	struct flagDefsDescriptor *desc = data;

	if (desc->backend)
		error (FATAL, "regex backed is specified twice: %c", c);

	desc->backend = &defaultRegexBackend;
	desc->flags   = (desc->regptype == REG_PARSER_MULTI_TABLE)? 0: REG_NEWLINE;
}

extern void basic_regex_flag_long (const char* const s, const char* const unused CTAGS_ATTR_UNUSED, void* data)
{
	struct flagDefsDescriptor *desc = data;

	if (desc->backend)
		error (FATAL, "regex backed is specified twice: %s", s);

	basic_regex_flag_short ('b', data);
}

extern void extend_regex_flag_short (char c, void* data)
{
	struct flagDefsDescriptor *desc = data;

	if (desc->backend)
		error (FATAL, "regex backed is specified twice: %c", c);

	desc->backend = &defaultRegexBackend;
	desc->flags   = REG_EXTENDED;
	desc->flags  |= (desc->regptype == REG_PARSER_MULTI_TABLE)? 0: REG_NEWLINE;
}

extern void extend_regex_flag_long (const char* const s, const char* const unused CTAGS_ATTR_UNUSED, void* data)
{
	struct flagDefsDescriptor *desc = data;

	if (desc->backend)
		error (FATAL, "regex backed is specified twice: %s", s);

	extend_regex_flag_short('e', data);
}

static void delete_code (void *code)
{
	regex_t *regex_code = code;
	regfree (regex_code);
	eFree (regex_code);
}

static regexCompiledCode compile (struct regexBackend *backend,
								  const char *const regexp,
								  int flags)
{
	regex_t *regex_code = xMalloc (1, regex_t);
	int errcode = regcomp (regex_code, regexp, flags);
	if (errcode != 0)
	{
		char errmsg[256];
		regerror (errcode, regex_code, errmsg, 256);
		error (WARNING, "regcomp: %s", errmsg);
		regfree (regex_code);
		eFree (regex_code);
		return (regexCompiledCode) { .backend = NULL, .code = NULL };
	}
	return (regexCompiledCode) { .backend = &defaultRegexBackend, .code = regex_code };
}

static int match (struct regexBackend *backend,
				  void *code, const char *input, size_t size CTAGS_ATTR_UNUSED,
				  regmatch_t pmatch[BACK_REFERENCE_COUNT])
{
	return regexec ((regex_t *)code, input, BACK_REFERENCE_COUNT, pmatch, 0);
}

static void set_icase_flag (int *flags)
{
	*flags |= REG_ICASE;
}
