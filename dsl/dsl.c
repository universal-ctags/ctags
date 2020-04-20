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
#include "dsl.h"
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

/*
 * FUNCTION DECLARATIONS
 */
static EsObject* builtin_null  (EsObject *args, DSLEnv *env);
static EsObject* builtin_begin (EsObject *args, DSLEnv *env);
static EsObject* builtin_begin0 (EsObject *args, DSLEnv *env);
static EsObject* builtin_and  (EsObject *args, DSLEnv *env);
static EsObject* builtin_or  (EsObject *args, DSLEnv *env);
static EsObject* builtin_not  (EsObject *args, DSLEnv *env);
static EsObject* builtin_eq  (EsObject *args, DSLEnv *env);
static EsObject* builtin_lt  (EsObject *args, DSLEnv *env);
static EsObject* builtin_gt  (EsObject *args, DSLEnv *env);
static EsObject* builtin_le  (EsObject *args, DSLEnv *env);
static EsObject* builtin_ge  (EsObject *args, DSLEnv *env);
static EsObject* builtin_prefix (EsObject *args, DSLEnv *env);
static EsObject* builtin_suffix (EsObject *args, DSLEnv *env);
static EsObject* builtin_substr (EsObject *args, DSLEnv *env);
static EsObject* builtin_member (EsObject *args, DSLEnv *env);
static EsObject* builtin_downcase (EsObject *args, DSLEnv *env);
static EsObject* builtin_upcase (EsObject *args, DSLEnv *env);
static EsObject* bulitin_debug_print (EsObject *args, DSLEnv *env);

/*
 * DATA DEFINITIONS
 */
static EsObject **engine_codes;

static DSLCode codes [] = {
	{ "null?",   builtin_null,   NULL, DSL_PATTR_CHECK_ARITY, 1 },
	{ "begin",   builtin_begin,  NULL, DSL_PATTR_SELF_EVAL,  0UL,
	  .helpstr = "(begin exp0 ... expN) -> expN" },
	{ "begin0",  builtin_begin0, NULL, DSL_PATTR_SELF_EVAL,  0UL,
	  .helpstr = "(begin0 exp0 ... expN) -> exp0" },
	{ "and",     builtin_and,    NULL, DSL_PATTR_SELF_EVAL },
	{ "or",      builtin_or,     NULL, DSL_PATTR_SELF_EVAL },
	{ "not",     builtin_not,    NULL, DSL_PATTR_CHECK_ARITY, 1},
	{ "eq?",     builtin_eq,     NULL, DSL_PATTR_CHECK_ARITY, 2 },
	{ "<",       builtin_lt,     NULL, DSL_PATTR_CHECK_ARITY, 2 },
	{ ">",       builtin_gt,     NULL, DSL_PATTR_CHECK_ARITY, 2 },
	{ "<=",      builtin_le,     NULL, DSL_PATTR_CHECK_ARITY, 2 },
	{ ">=",      builtin_ge,     NULL, DSL_PATTR_CHECK_ARITY, 2 },
	{ "prefix?", builtin_prefix, NULL, DSL_PATTR_CHECK_ARITY, 2,
	  .helpstr = "(prefix? TARGET<string> PREFIX<string>) -> <boolean>" },
	{ "suffix?", builtin_suffix, NULL, DSL_PATTR_CHECK_ARITY, 2,
	  .helpstr = "(suffix? TARGET<string> SUFFIX<string>) -> <boolean>" },
	{ "substr?", builtin_substr, NULL, DSL_PATTR_CHECK_ARITY, 2,
	  .helpstr = "(substr? TARGET<string> SUBSTR<string>) -> <boolean>" },
	{ "member",  builtin_member, NULL, DSL_PATTR_CHECK_ARITY, 2,
	  .helpstr = "(member ELEMENT LIST) -> #f|<list>" },
	{ "downcase", builtin_downcase, NULL, DSL_PATTR_CHECK_ARITY, 1,
	  .helpstr = "(downcase elt<string>|<list>) -> <string>|<list>" },
	{ "upcase", builtin_upcase, NULL, DSL_PATTR_CHECK_ARITY, 1,
	  .helpstr = "(upcate elt<string>|<list>) -> <string>|<list>" },
	{ "print",   bulitin_debug_print, NULL, DSL_PATTR_CHECK_ARITY, 1,
	  .helpstr = "(print OBJ) -> OBJ" },
};


/*
 * FUNCTION DEFINITIONS
 */
EsObject * dsl_code_define (DSLEngineType engine, DSLCode *code)
{
	EsObject *name = es_symbol_intern (code->name);
	if (name == es_nil)
		return es_nil;

	DSLCode **codes = es_symbol_get_data (name);
	if (!codes)
	{
		codes = calloc (DSL_ENGINE_COUNT, sizeof (codes[0]));
		if (!codes)
			return es_nil;
		es_symbol_set_data (name, codes);
	}

	codes [engine] = code;

	if (engine_codes == NULL)
	{
		engine_codes = calloc (DSL_ENGINE_COUNT, sizeof (engine_codes[0]));
		if (!engine_codes)
			return es_nil;
		for (int i = 0; i < DSL_ENGINE_COUNT; i++)
			engine_codes [i] = es_nil;
	}
	es_autounref_pool_push ();
	engine_codes [engine] = es_cons (es_object_autounref (name),
									 es_object_autounref (engine_codes [engine]));
	es_autounref_pool_pop ();

	return name;
}

int dsl_init (void)
{
	for (int i = 0; i < sizeof(codes)/sizeof(codes [0]); i++)
	{
		if (dsl_code_define (DSL_COMMON, codes + i) == NULL)
			return 0;
	}
	return 1;
}

DSLCode *dsl_code_lookup (DSLEngineType engine, EsObject *name)
{
	DSLCode **codes = es_symbol_get_data (name);
	if (!codes)
		return NULL;
	return codes [engine]? codes [engine]: codes [DSL_COMMON];
}

static void dsl_help0 (DSLEngineType engine, FILE *fp)
{
	EsObject *codes = engine_codes [engine];
	EsObject *rcodes = es_reverse (codes);
	EsObject *name, *cdr = rcodes;

	while (!es_null (cdr))
	{
		name = es_car (cdr);
		cdr = es_cdr (cdr);
		DSLCode *code = dsl_code_lookup (engine, name);
		assert (code);

		const char* hs = code->helpstr;
		fprintf(fp, "%15s: %s\n", code->name, hs? hs: "");
	}

	es_object_unref (rcodes);
}

void dsl_help (DSLEngineType engine, FILE *fp)
{
	dsl_help0 (DSL_COMMON, fp);
	dsl_help0 (engine, fp);
}

static void dsl_code_reset0 (DSLCode  *code)
{
	if (code->flags & DSL_PATTR_MEMORABLE)
		code->cache = NULL;
}

void dsl_code_reset (DSLCode  *code)
{
	if (code == NULL)
	{
		for (int i = 0; i < sizeof(codes)/sizeof(codes [0]); i++)
			dsl_code_reset0 (codes + i);
	}
	else
		dsl_code_reset (code);
}

static int length (EsObject *object)
{
	int i;
	for (i = 0; !es_null (object); i++)
		object = es_cdr (object);
	return i;
}

static EsObject *error_included (EsObject *object)
{
	while (!es_null (object))
	{
		if (es_error_p (es_car (object)))
			return es_car (object);
		object = es_cdr (object);
	}
	return es_false;
}

static EsObject *eval0 (EsObject *object, DSLEnv *env)
{
	if (es_null (object))
		return es_nil;
	else
		return es_object_autounref (
			es_cons(dsl_eval (es_car (object), env),
				eval0 (es_cdr (object), env))
			);
}

EsObject *dsl_eval (EsObject *object, DSLEnv *env)
{
	EsObject *r;
	DSLCode *code;

	if (es_null (object))
		return es_nil;
	else if (es_symbol_p (object))
	{
		code = dsl_code_lookup (env->engine, object);

		if (code)
		{
			if (code->cache)
				return code->cache;

			if (code->flags & DSL_PATTR_PURE)
				dsl_throw (UNBOUND_VARIABLE, object);

			r = code->proc (es_nil, env);
			if (code->flags & DSL_PATTR_MEMORABLE)
				code->cache = r;
			return r;
		}
		else
			dsl_throw (UNBOUND_VARIABLE, object);

	}
	else if (es_atom (object))
		return object;
	else
	{
		EsObject *car = es_car (object);
		EsObject *cdr = es_cdr (object);
		int l;

		code = dsl_code_lookup (env->engine, car);

		if (!code)
			dsl_throw (UNBOUND_VARIABLE, car);

		if (code->cache)
			return code->cache;

		if (code->flags & DSL_PATTR_CHECK_ARITY)
		{
			l = length (cdr);
			if (l < code->arity)
				dsl_throw (TOO_FEW_ARGUMENTS, car);
			else if (l > code->arity)
				dsl_throw (TOO_MANY_ARGUMENTS, car);
		}

		if (! (code->flags & DSL_PATTR_SELF_EVAL))
		{
			EsObject *err;

			cdr = eval0(cdr, env);

			err = error_included (cdr);
			if (!es_object_equal (err, es_false))
				return err;
		}

		r = code->proc (cdr, env);
		if (code->flags & DSL_PATTR_MEMORABLE)
			code->cache = r;
		return r;
	}
}

/*
 * Built-ins
 */
static EsObject* builtin_null  (EsObject *args, DSLEnv *env)
{
	return es_null(es_car (args))? es_true: es_false;
}

static EsObject* builtin_begin  (EsObject *args, DSLEnv *env)
{
	if (es_null (args))
		dsl_throw (TOO_FEW_ARGUMENTS,
				   es_symbol_intern ("begin"));

	EsObject *o = es_false;
	while (! es_null (args))
	{
		o = es_car (args);
		o = dsl_eval (o, env);
		if (es_error_p (o))
			return o;
		args = es_cdr (args);
	}
	return o;
}

static EsObject* builtin_begin0  (EsObject *args, DSLEnv *env)
{
	if (es_null (args))
		dsl_throw (TOO_FEW_ARGUMENTS, es_symbol_intern ("begin0"));

	int count = 0;
	EsObject *o, *o0 = es_false;
	while (! es_null (args))
	{
		o = es_car (args);
		o = dsl_eval (o, env);
		if (!count++)
			o0 = o;

		if (es_error_p (o))
			return o;
		args = es_cdr (args);
	}
	return o0;
}

static EsObject* builtin_and  (EsObject *args, DSLEnv *env)
{
	EsObject *o = es_true;

	while (! es_null (args))
	{
		o = es_car (args);
		o = dsl_eval (o, env);
		if (es_object_equal (o, es_false))
			return es_false;
		else if (es_error_p (o))
			return o;
		args = es_cdr (args);
	}

	return o;
}

static EsObject* builtin_or  (EsObject *args, DSLEnv *env)
{
	EsObject *o;

	while (! es_null (args))
	{
		o = es_car (args);
		o = dsl_eval (o, env);
		if (! es_object_equal (o, es_false))
			return o;
		else if (es_error_p (o))
			return o;
		args = es_cdr (args);
	}

	return es_false;
}

static EsObject* builtin_not  (EsObject *args, DSLEnv *env)
{
	if (es_object_equal (es_car(args), es_false))
		return es_true;
	else if (es_error_p (es_car(args)))
		return es_car(args);
	else
		return es_false;
}

#define DEFINE_OP_WITH_CHECK(N, X, C, E, O)				\
	static EsObject* builtin_##N  (EsObject *args, DSLEnv *env)	\
	{								\
		EsObject *a, *b;					\
									\
		a = es_car (args);					\
		b = es_car (es_cdr (args));				\
		if (!C (a)) dsl_throw(E, O);			\
		if (!C (b)) dsl_throw(E, O);			\
		if (X)							\
			return es_true;					\
		else							\
			return es_false;				\
	}

static EsObject* builtin_eq  (EsObject *args, DSLEnv *env)
{
	EsObject *a, *b;
	a = es_car (args);
	b = es_car (es_cdr (args));

	if (es_object_equal(a, b))
		return es_true;
	else
		return es_false;
}

DEFINE_OP_WITH_CHECK(lt, es_number_get (a) < es_number_get (b), es_number_p,  NUMBER_REQUIRED, es_symbol_intern ("<"));
DEFINE_OP_WITH_CHECK(gt, es_number_get (a) > es_number_get (b), es_number_p,  NUMBER_REQUIRED, es_symbol_intern (">"));
DEFINE_OP_WITH_CHECK(le, es_number_get (a) <= es_number_get (b), es_number_p, NUMBER_REQUIRED, es_symbol_intern ("<="));
DEFINE_OP_WITH_CHECK(ge, es_number_get (a) >= es_number_get (b), es_number_p, NUMBER_REQUIRED, es_symbol_intern (">="));

static EsObject* builtin_prefix (EsObject* args, DSLEnv *env)
{
	EsObject *target = es_car (args);
	EsObject *prefix = es_car (es_cdr (args));
	const char *ts;
	const char *ps;
	size_t tl;
	size_t pl;

	if ((! es_string_p (target))
	    || (! es_string_p (prefix)))
		dsl_throw (WRONG_TYPE_ARGUMENT, es_symbol_intern ("prefix?"));

	ts = es_string_get (target);
	ps = es_string_get (prefix);
	tl = strlen (ts);
	pl = strlen (ps);
	if (tl < pl)
		return es_false;
	return (strncmp (ts, ps, pl) == 0)? es_true: es_false;
}

static EsObject* builtin_suffix (EsObject* args, DSLEnv *env)
{
	EsObject *target = es_car (args);
	EsObject *suffix = es_car (es_cdr (args));
	const char *ts;
	const char *ss;
	size_t tl;
	size_t sl;
	unsigned int d;

	if ((! es_string_p (target))
	    || (! es_string_p (suffix)))
		dsl_throw (WRONG_TYPE_ARGUMENT, es_symbol_intern ("suffix?"));

	ts = es_string_get (target);
	ss = es_string_get (suffix);
	tl = strlen (ts);
	sl = strlen (ss);
	if (tl < sl)
		return es_false;
	d = tl - sl;
	return (strcmp (ts + d, ss) == 0)? es_true: es_false;
}

static EsObject* builtin_substr (EsObject* args, DSLEnv *env)
{
	EsObject *target = es_car (args);
	EsObject *substr = es_car (es_cdr (args));
	const char *ts;
	const char *ss;

	if ((! es_string_p (target))
	    || (! es_string_p (substr)))
		dsl_throw (WRONG_TYPE_ARGUMENT, es_symbol_intern("substr?"));
	ts = es_string_get (target);
	ss = es_string_get (substr);

	return strstr(ts, ss) == NULL? es_false: es_true;
}

static EsObject* builtin_member (EsObject *args, DSLEnv *env)
{
	EsObject *elt  = es_car (args);
	EsObject *lst = es_car (es_cdr (args));

	if (! es_list_p (lst))
		dsl_throw (WRONG_TYPE_ARGUMENT, es_symbol_intern ("member"));

	while (!es_null (lst))
	{
		if (es_object_equal (elt, es_car (lst)))
			return lst;
		lst = es_cdr (lst);
	}

	return es_false;
}

static EsObject* caseop (EsObject *o, int (*op)(int))
{
	if (es_string_p (o))
	{
		const char *s = es_string_get (o);
		char *r = strdup (s);

		for (char *tmp = r; *tmp != '\0'; tmp++)
			*tmp = op (*tmp);

		EsObject *q = es_object_autounref (es_string_new (r));
		free (r);
		return q;
	}
	else
		return o;
}

static EsObject* downcase (EsObject *o)
{
	return caseop (o, tolower);
}

static EsObject* upcase (EsObject *o)
{
	return caseop (o, toupper);
}

static EsObject* builtin_caseop0 (EsObject *o,
								  EsObject *(* op) (EsObject*))
{
	if (es_null (o)
		|| es_error_p (o))
		return o;
	else if (es_list_p (o))
	{
		EsObject *oa = es_car (o);
		EsObject *od = es_cdr (o);
		EsObject *da, *dd;

		da = op (oa);
		if (es_error_p (da))
			return da;
		dd = builtin_caseop0 (od, op);
		if (es_error_p (dd))
			return dd;

		return es_object_autounref (es_cons (da, dd));
	}
	else
		return op (o);
}

static EsObject* builtin_downcase (EsObject *args, DSLEnv *env)
{
	EsObject *o = es_car(args);
	return builtin_caseop0 (o, downcase);
}

static EsObject* builtin_upcase (EsObject *args, DSLEnv *env)
{
	EsObject *o = es_car(args);
	return builtin_caseop0 (o, upcase);
}

static MIO  *miodebug;
static EsObject* bulitin_debug_print (EsObject *args, DSLEnv *env)
{
	if (miodebug == NULL)
		miodebug = mio_new_fp (stderr, NULL);

	EsObject *o = es_car(args);
	es_print(o, miodebug);
	putc('\n', stderr);

	return o;
}
