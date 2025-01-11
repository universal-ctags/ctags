/*
*   Copyright (c) 2000-2002, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for generating tags for Scheme language
*   files.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include "entry.h"
#include "parse.h"

#include "x-lisp.h"

#include <ctype.h>
#include <string.h>

/*
*   DATA DEFINITIONS
*/
typedef enum {
	K_UNKNOWN,
	K_FUNCTION, K_SET
} schemeKind;

typedef enum {
	F_DEFINER,
} schemeField;

static fieldDefinition SchemeFields[] = {
	{ .name = "definer",
	  .description = "the name of the function or macro that defines the unknown/Y-kind object",
	  .enabled = true },
};

static kindDefinition SchemeKinds [] = {
	{ true, 'Y', "unknown", "unknown type of definitions" },
	{ true, 'f', "function", "functions" },
	{ true, 's', "set",      "sets" }
};

/*
*   FUNCTION DEFINITIONS
*/

/*
*   FUNCTION DEFINITIONS
*/
static bool scheme_is_def (struct lispDialect *dialect, const unsigned char *strp)
{
	bool cis = dialect->case_insensitive; /* Renaming for making code short */

	bool is_set = ( (strp [1] == 's' || (cis && strp [1] == 'S'))
					&& (strp [2] == 'e' || (cis && strp [2] == 'E'))
					&& (strp [3] == 't' || (cis && strp [3] == 'T'))
					&& (strp [4] == '!'));
	if (is_set)
		return true;

	return lispIsDef (dialect, strp);
}

static int  scheme_hint2kind (const vString *const hint, const char *namespace CTAGS_ATTR_UNUSED)
{
	int k = K_UNKNOWN;
	int n = vStringLength (hint) - 4;

	if (strncmp (vStringValue (hint) + 1, "SET!", 4) == 0)
		return K_SET;

	/* 4 means strlen("(def"). */
#define EQN(X) strncmp(vStringValue (hint) + 4, &X[3], n) == 0
	switch (n)
	{
	case 3:
		if (EQN("DEFINE"))
			k = K_FUNCTION;
		break;

	}
	return k;
}

static void findSchemeTags (void)
{
	struct lispDialect scheme_dialect = {
		.definer2kind = scheme_hint2kind,
		.case_insensitive = true,
		.namespace_sep = 0,
		.unknown_kind = K_UNKNOWN,
		.definer_field = SchemeFields + F_DEFINER,
		.skip_initial_spaces = false,
		.lambda_syntax_sugar = true,
		.is_def = scheme_is_def,
		.get_it = lispGetIt,
		.scope = CORK_NIL,
	};

	findLispTagsCommon (&scheme_dialect);
}

extern parserDefinition* SchemeParser (void)
{
	static const char *const extensions [] = {
		"SCM", "SM", "sch", "scheme", "scm", "sm", "rkt", NULL
	};
	static const char *const aliases [] = {
		"gosh", "guile", "racket", NULL
	};
	parserDefinition* def = parserNew ("Scheme");
	def->kindTable      = SchemeKinds;
	def->kindCount  = ARRAY_SIZE (SchemeKinds);
	def->fieldTable = SchemeFields;
	def->fieldCount = ARRAY_SIZE (SchemeFields);
	def->extensions = extensions;
	def->aliases    = aliases;
	def->parser     = findSchemeTags;
	def->useCork = CORK_QUEUE;
	def->versionCurrent = 0;
	def->versionAge = 1;
	return def;
}
