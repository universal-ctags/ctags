/*
*   Copyright (c) 2020, Masatake YAMATO
*   Copyright (c) 2020, Red Hat, Inc.
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*/

/*
 * INCLUDES
 */

#include "sorter.h"
#include "dsl.h"
#include "es.h"

#include <stdlib.h>
#include <string.h>


/*
 * MACROS
 */

#define DECLARE_ALT_VALUE_FN(N)									\
static EsObject* alt_value_##N (EsObject *args, DSLEnv *env)

#define DEFINE_ALT_VALUE_FN(N)											\
static EsObject* alt_value_##N (EsObject *args, DSLEnv *env)			\
{																		\
	if (!env->alt_entry)												\
		dsl_throw (NO_ALT_ENTRY, es_symbol_intern ("&" #N)); 			\
	return dsl_entry_##N (env->alt_entry);								\
}


/*
 * CONSTANTS
 */
static EsObject *LTN;
static EsObject *EQN;
static EsObject *GTN;

/*
 * FUNCTION DECLARATIONS
 */
static EsObject* sorter_alt_entry_ref (EsObject *args, DSLEnv *env);

DECLARE_ALT_VALUE_FN(name);
DECLARE_ALT_VALUE_FN(input);
DECLARE_ALT_VALUE_FN(pattern);
DECLARE_ALT_VALUE_FN(line);

DECLARE_ALT_VALUE_FN(access);
DECLARE_ALT_VALUE_FN(end);
DECLARE_ALT_VALUE_FN(extras);
DECLARE_ALT_VALUE_FN(file);
DECLARE_ALT_VALUE_FN(inherits);
DECLARE_ALT_VALUE_FN(implementation);
DECLARE_ALT_VALUE_FN(kind);
DECLARE_ALT_VALUE_FN(language);
DECLARE_ALT_VALUE_FN(scope);
DECLARE_ALT_VALUE_FN(scope_kind);
DECLARE_ALT_VALUE_FN(scope_name);
DECLARE_ALT_VALUE_FN(signature);
DECLARE_ALT_VALUE_FN(typeref);
DECLARE_ALT_VALUE_FN(typeref_kind);
DECLARE_ALT_VALUE_FN(typeref_name);
DECLARE_ALT_VALUE_FN(roles);
DECLARE_ALT_VALUE_FN(xpath);


static EsObject* sorter_proc_cmp (EsObject* args, DSLEnv *env);
static EsObject* sorter_proc_flip (EsObject* args, DSLEnv *env);
static EsObject* sorter_sform_cmp_or (EsObject* args, DSLEnv *env);

/*
 * DATA DEFINITIONS
 */

#define DSL_ERR_NO_ALT_ENTRY    (es_error_intern("the-alternative-entry-unavailable"))

static DSLProcBind pbinds [] = {
	{ "<>",              sorter_proc_cmp,          NULL, DSL_PATTR_CHECK_ARITY,     2,
	  .helpstr = "(<> <any:a> <any:b>) -> -1|0|1; compare a b. The types of a and b must be the same." },
	{ "*-",              sorter_proc_flip,         NULL, DSL_PATTR_CHECK_ARITY,     1,
	  .helpstr = "(*- <interger:n>) -> -<integer:n>; filp the result of comparison." },
	{ "<or>",            sorter_sform_cmp_or,      NULL, DSL_PATTR_CHECK_ARITY_OPT, 1,
	  .helpstr = "(<or> <any> ...) -> -1|0|1; evaluate arguments left to right till one of thme returns -1 or 1." },

	{ "&",               sorter_alt_entry_ref, NULL, DSL_PATTR_CHECK_ARITY,  1,
	  .helpstr = "(& <string:field>) -> #f|<string>" },
	{ "&name",           alt_value_name,           NULL, DSL_PATTR_MEMORABLE, 0UL,
	  .helpstr = "-> <string>"},
	{ "&input",          alt_value_input,          NULL, DSL_PATTR_MEMORABLE, 0UL,
	  .helpstr = "-> <string>"},
	{ "&pattern",        alt_value_pattern,        NULL, DSL_PATTR_MEMORABLE, 0UL,
	  .helpstr = "-> #f|<string>"},
	{ "&line",           alt_value_line,           NULL, DSL_PATTR_MEMORABLE, 0UL,
	  .helpstr = "-> #f|<integer>" },

	{ "&access",         alt_value_access,         NULL, DSL_PATTR_MEMORABLE, 0UL,
	  .helpstr = "-> #f|<string>" },
	{ "&end",            alt_value_end,            NULL, DSL_PATTR_MEMORABLE, 0UL,
	  .helpstr = "-> #f|<integer>"},
	{ "&extras",         alt_value_extras,         NULL, DSL_PATTR_MEMORABLE, 0UL,
	  .helpstr = "-> #f|<string>"},
	{ "&file",           alt_value_file,           NULL, DSL_PATTR_MEMORABLE, 0UL,
	  .helpstr = "-> <boolean>; whether the scope is limited in the file or not." },
	{ "&inherits",       alt_value_inherits,       NULL, DSL_PATTR_MEMORABLE, 0UL,
	  .helpstr = "-> <list>" },
	{ "&implementation", alt_value_implementation, NULL, DSL_PATTR_MEMORABLE, 0UL,
	  .helpstr = "-> #f|<string>" },
	{ "&kind",           alt_value_kind,           NULL, DSL_PATTR_MEMORABLE, 0UL,
	  .helpstr = "-> #f|<string>"},
	{ "&language",       alt_value_language,       NULL, DSL_PATTR_MEMORABLE, 0UL,
	  .helpstr = "-> #f|<string>" },
	{ "&scope",          alt_value_scope,          NULL, DSL_PATTR_MEMORABLE, 0UL,
	  .helpstr = "-> #f|<string>; $scope-kind:$scope-name"},
	{ "&scope-kind",     alt_value_scope_kind,     NULL, DSL_PATTR_MEMORABLE, 0UL,
	  .helpstr = "-> #f|<string>"},
	{ "&scope-name",     alt_value_scope_name,     NULL, DSL_PATTR_MEMORABLE, 0UL,
	  .helpstr = "-> #f|<string>"},
	{ "&signature",      alt_value_signature,      NULL, DSL_PATTR_MEMORABLE, 0UL,
	  .helpstr = "-> #f|<string>" },
	{ "&typeref",        alt_value_typeref,        NULL, DSL_PATTR_MEMORABLE, 0UL,
	  .helpstr = "-> #f|<string>"},
	{ "&typeref-kind",   alt_value_typeref_kind,   NULL, DSL_PATTR_MEMORABLE, 0UL,
	  .helpstr = "-> #f|<string>"},
	{ "&typeref-name",   alt_value_typeref_name,   NULL, DSL_PATTR_MEMORABLE, 0UL,
	  .helpstr = "-> #f|<string>"},
	{ "&roles",          alt_value_roles,          NULL, DSL_PATTR_MEMORABLE, 0UL,
	  .helpstr = "-> <list>" },
	{ "&xpath",         alt_value_xpath,           NULL, DSL_PATTR_MEMORABLE, 0UL,
	  .helpstr = "-> #f|<string>"},
};

/*
 * FUNCTION DEFINITIONS
 */

/*
 * Value functions
 */
DEFINE_ALT_VALUE_FN(name);
DEFINE_ALT_VALUE_FN(input);
DEFINE_ALT_VALUE_FN(pattern);
DEFINE_ALT_VALUE_FN(line);

DEFINE_ALT_VALUE_FN(access);
DEFINE_ALT_VALUE_FN(end);
DEFINE_ALT_VALUE_FN(extras);
DEFINE_ALT_VALUE_FN(file);
DEFINE_ALT_VALUE_FN(inherits);
DEFINE_ALT_VALUE_FN(implementation);
DEFINE_ALT_VALUE_FN(kind);
DEFINE_ALT_VALUE_FN(language);
DEFINE_ALT_VALUE_FN(scope);
DEFINE_ALT_VALUE_FN(scope_kind);
DEFINE_ALT_VALUE_FN(scope_name);
DEFINE_ALT_VALUE_FN(signature);
DEFINE_ALT_VALUE_FN(typeref);
DEFINE_ALT_VALUE_FN(typeref_kind);
DEFINE_ALT_VALUE_FN(typeref_name);
DEFINE_ALT_VALUE_FN(roles);
DEFINE_ALT_VALUE_FN(xpath);

/*
 * Special form(s)
 */
static EsObject* sorter_sform_cmp_or (EsObject* args, DSLEnv *env)
{
	EsObject *o = EQN;

	while (! es_null (args))
	{
		o = es_car (args);
		o = dsl_compile_and_eval (o, env);

		if (es_object_equal (o, LTN))
			return LTN;
		else if (es_object_equal (o, GTN))
			return GTN;
		else if (es_error_p (o))
			return o;

		args = es_cdr (args);
	}

	return o;
}


/*
 * Procs
 */
static EsObject* sorter_alt_entry_ref (EsObject *args, DSLEnv *env)
{
	EsObject *key = es_car(args);

	if (es_error_p (key))
		return key;
	else if (! es_string_p (key))
		dsl_throw (WRONG_TYPE_ARGUMENT,
				   es_symbol_intern ("&"));
	else
		return dsl_entry_xget_string (env->alt_entry, es_string_get (key));
}

static EsObject* sorter_proc_cmp (EsObject* args, DSLEnv *env)
{
	EsObject *a, *b;

	a = es_car (args);
	b = es_car (es_cdr (args));

	if (es_number_p (a))
	{
		if (!es_number_p (b))
			dsl_throw (NUMBER_REQUIRED, es_symbol_intern ("<>"));

		double ad = es_number_get (a);
		double bd = es_number_get (b);

		if (ad < bd)
			return LTN;
		else if (ad == bd)
			return EQN;
		else
			return GTN;
	}
	else if (es_string_p (a))
	{
		if (!es_string_p (b))
			dsl_throw (STRING_REQUIRED, es_symbol_intern ("<>"));

		const char *as = es_string_get (a);
		const char *bs = es_string_get (b);

		int tmp = strcmp (as, bs);
		if (tmp < 0)
			return LTN;
		else if (tmp > 0)
			return GTN;
		else
			return EQN;
	}
	else if (es_boolean_p (a))
	{
		if (!es_boolean_p (b))
			dsl_throw (BOOLEAN_REQUIRED, es_symbol_intern ("<>"));

		bool ab = es_boolean_get (a);
		bool bb = es_boolean_get (a);

		if (ab == bb)
			return EQN;
		else if ((int)ab < (int)bb)
			return LTN;
		else
			return GTN;
	}
	else
		dsl_throw (WRONG_TYPE_ARGUMENT, es_symbol_intern ("<>"));
}

static EsObject* sorter_proc_flip (EsObject* args, DSLEnv *env)
{
	EsObject *o;

	o = es_car (args);

	if (!es_integer_p (o))
		dsl_throw (INTEGER_REQUIRED, es_symbol_intern ("-*"));

	int i = es_integer_get (o);
	if (i < 0)
		return GTN;
	else if (i == 0)
		return EQN;
	else
		return LTN;
}

static int initialize (void)
{
	static int initialized;

	if (initialized)
		return 1;

	if (!dsl_init (DSL_SORTER, pbinds, sizeof(pbinds)/sizeof(pbinds [0])))
	{
		fprintf(stderr, "MEMORY EXHAUSTED\n");
		return 0;
	}

	LTN = es_integer_new (-1);
	EQN = es_integer_new (0);
	GTN = es_integer_new (1);

	initialized = 1;
	return 1;
}


/*
 * SCode
 */
struct sSCode
{
	DSLCode *dsl;
};

SCode *s_compile (EsObject *exp)
{
	SCode *code;

	if (!initialize ())
		exit (1);

	code = malloc (sizeof (SCode));
	if (code == NULL)
	{
		fprintf(stderr, "MEMORY EXHAUSTED\n");
		return NULL;
	}

	code->dsl = dsl_compile (DSL_SORTER, exp);
	if (code->dsl == NULL)
	{
		fprintf(stderr, "MEMORY EXHAUSTED or SYNTAX ERROR\n");
		free (code);
		return NULL;
	}
	return code;
}

int s_compare        (const tagEntry * a, const tagEntry * b, SCode *code)
{
	EsObject *r;
	int i;
	int exit_code = 0;

	DSLEnv env = {
		.engine = DSL_SORTER,
		.entry = a,
		.alt_entry = b,
	};
	es_autounref_pool_push ();
	r = dsl_eval (code->dsl, &env);

	if (es_integer_p (r))
	{
		int n = es_integer_get(r);

		if (n < 0)
			i = -1;
		else if (n == 0)
			i = 0;
		else
			i = 1;
		goto out;
	}
	else if (es_error_p (r))
	{
		dsl_report_error ("GOT ERROR in SORTING", r);
		exit_code = 1;
		goto out;
	}
	else
	{
		dsl_report_error ("Get unexpected value as the result of sorting",
						  r);
		exit_code = 1;
		goto out;
	}

 out:
	es_autounref_pool_pop ();

	dsl_cache_reset (DSL_SORTER);

	if (exit_code)
		exit (exit_code);

	return i;
}

void s_destroy        (SCode *code)
{
	dsl_release (DSL_SORTER, code->dsl);
	free (code);
}

void s_help           (FILE *fp)
{
	if (!initialize ())
		exit (1);
	dsl_help (DSL_SORTER, fp);
}
