/*
*  Copyright (c) 2021, Red Hat, Inc.
*  Copyright (c) 2021, Masatake YAMATO
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

#ifdef HAVE_PCRE2
#define PCRE2_CODE_UNIT_WIDTH 8
#include <pcre2.h>
#endif

#include "lregex_p.h"
#include "trashbox.h"

#include <string.h>

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
static struct regexBackend pcre2RegexBackend = {
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
extern void pcre2_regex_flag_short (char c, void* data)
{
	struct flagDefsDescriptor *desc = data;

	if (desc->backend)
		error (FATAL, "regex backed is specified twice: %c", c);

	desc->backend = &pcre2RegexBackend;
	desc->flags   = (desc->regptype == REG_PARSER_MULTI_TABLE)? PCRE2_DOTALL: PCRE2_MULTILINE;
}

extern void pcre2_regex_flag_long (const char* const s, const char* const unused CTAGS_ATTR_UNUSED, void* data)
{
	struct flagDefsDescriptor *desc = data;

	if (desc->backend)
		error (FATAL, "regex backed is specified twice: %s", s);

	pcre2_regex_flag_short ('p', data);
}

static void delete_code (void *code)
{
	pcre2_code_free (code);
}

static regexCompiledCode compile (struct regexBackend *backend,
								  const char *const regexp,
								  int flags)
{
	int errornumber;
	PCRE2_SIZE erroroffset;
	pcre2_code *regex_code = pcre2_compile((PCRE2_SPTR)regexp,
										   PCRE2_ZERO_TERMINATED,
										   (uint32_t) flags,
										   &errornumber,
										   &erroroffset,
										   NULL);
	if (regex_code == NULL)
	{
		PCRE2_UCHAR buffer[256];
		pcre2_get_error_message(errornumber, buffer, sizeof(buffer));
		error (WARNING, "PCRE2 compilation failed at offset %d: %s", (int)erroroffset,
			   buffer);
		return (regexCompiledCode) { .backend = NULL, .code = NULL };
	}
	return (regexCompiledCode) { .backend = &pcre2RegexBackend, .code = regex_code };
}

static int match (struct regexBackend *backend,
				  void *code, const char *input, size_t size,
				  regmatch_t pmatch[BACK_REFERENCE_COUNT])
{
	static pcre2_match_data *match_data;
	if (match_data == NULL)
	{
		match_data = pcre2_match_data_create (BACK_REFERENCE_COUNT, NULL);
		DEFAULT_TRASH_BOX (match_data, pcre2_match_data_free);
	}

	int rc = pcre2_match (code, (PCRE2_SPTR)input, size,
						  0, 0, match_data, NULL);
	if (rc > 0)
	{
		PCRE2_SIZE *ovector = pcre2_get_ovector_pointer(match_data);
		if (ovector[0] <= ovector[1])
		{
			memset (pmatch, 0, sizeof(pmatch[0]) * BACK_REFERENCE_COUNT);
			for (int i = 0; i < BACK_REFERENCE_COUNT; i++)
			{
				pmatch [i].rm_so = (i < rc)? ovector [2*i]  : -1;
				pmatch [i].rm_eo = (i < rc)? ovector [2*i+1]: -1;

			}
			return 0;
		}
	}
	return 1;
}

static void set_icase_flag (int *flags)
{
	*flags |= PCRE2_CASELESS;
}
