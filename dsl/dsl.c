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
 * TYPES
 */
struct sDSLCode
{
	EsObject *expr;
};

struct sDSLEngine
{
	DSLProcBind *pbinds;
	int pbinds_count;
};
typedef struct sDSLEngine DSLEngine;

/*
 * MACROS
 */

#define DECLARE_VALUE_FN(N)									\
static EsObject* value_##N (EsObject *args, DSLEnv *env)

#define DEFINE_VALUE_FN(N)									\
static EsObject* value_##N (EsObject *args, DSLEnv *env)	\
{															\
	return dsl_entry_##N (env->entry);						\
}

/*
 * FUNCTION DECLARATIONS
 */

static EsObject *dsl_eval0 (EsObject *object, DSLEnv *env);
static EsObject *dsl_define (DSLEngineType engine, DSLProcBind *pbind);

static EsObject* builtin_null  (EsObject *args, DSLEnv *env);
static EsObject* sform_begin (EsObject *args, DSLEnv *env);
static EsObject* sform_begin0 (EsObject *args, DSLEnv *env);
static EsObject* sfrom_and  (EsObject *args, DSLEnv *env);
static EsObject* sform_or  (EsObject *args, DSLEnv *env);
static EsObject* sform_if  (EsObject *args, DSLEnv *env);
static EsObject* sform_cond  (EsObject *args, DSLEnv *env);
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
static EsObject* builtin_length (EsObject *args, DSLEnv *env);
static EsObject* bulitin_debug_print (EsObject *args, DSLEnv *env);
static EsObject* builtin_entry_ref (EsObject *args, DSLEnv *env);

static EsObject* builtin_string_append (EsObject *args, DSLEnv *env);
static EsObject* builtin_string2regexp (EsObject *args, DSLEnv *env);
static EsObject* builtin_regexp_quote (EsObject *args, DSLEnv *env);
static EsObject* builtin_add  (EsObject *args, DSLEnv *env);
static EsObject* builtin_sub  (EsObject *args, DSLEnv *env);

static EsObject* value_true (EsObject *args, DSLEnv *env);
static EsObject* value_false (EsObject *args, DSLEnv *env);
static EsObject* value_nil (EsObject *args, DSLEnv *env);

DECLARE_VALUE_FN(name);
DECLARE_VALUE_FN(input);
DECLARE_VALUE_FN(pattern);
DECLARE_VALUE_FN(line);

DECLARE_VALUE_FN(access);
DECLARE_VALUE_FN(end);
DECLARE_VALUE_FN(extras);
DECLARE_VALUE_FN(file);
DECLARE_VALUE_FN(inherits);
DECLARE_VALUE_FN(implementation);
DECLARE_VALUE_FN(kind);
DECLARE_VALUE_FN(language);
DECLARE_VALUE_FN(scope);
DECLARE_VALUE_FN(scope_kind);
DECLARE_VALUE_FN(scope_name);
DECLARE_VALUE_FN(signature);
DECLARE_VALUE_FN(typeref);
DECLARE_VALUE_FN(typeref_kind);
DECLARE_VALUE_FN(typeref_name);
DECLARE_VALUE_FN(roles);
DECLARE_VALUE_FN(xpath);

static EsObject* macro_string_append (EsObject *args);
static EsObject* macro_string2regexp (EsObject *args);
static EsObject* macro_regexp_quote (EsObject *args);
static EsObject* macro_debug_printX (EsObject *args);

/*
 * DATA DEFINITIONS
 */
static DSLEngine engines [DSL_ENGINE_COUNT];

static DSLProcBind pbinds_interanl_pseudo [] = {
	{ "#/PATTERN/", NULL, NULL, 0, 0,
	  .helpstr = "(#/patter/ <string>) -> <boolean>; regular expression matching" },
	{ "#/PATTERN/i", NULL, NULL, 0, 0,
	  .helpstr = "(#/patter/i <string>) -> <boolean>; in case insensitive way" },
};

static DSLProcBind pbinds [] = {
	{ "null?",   builtin_null,   NULL, DSL_PATTR_CHECK_ARITY, 1,
	  .helpstr = "(null? <any>) -> <boolean>" },
	{ "begin",   sform_begin,  NULL, DSL_PATTR_SELF_EVAL,  0UL,
	  .helpstr = "(begin <any:0> ... <any:n>) -> <any:n>" },
	{ "begin0",  sform_begin0, NULL, DSL_PATTR_SELF_EVAL,  0UL,
	  .helpstr = "(begin0 <any:0> ... <any:n>) -> <any:0>" },
	{ "and",     sfrom_and,    NULL, DSL_PATTR_SELF_EVAL,
	  .helpstr = "(and <any> ...) -> <boolean>" },
	{ "or",      sform_or,     NULL, DSL_PATTR_SELF_EVAL,
	  .helpstr = "(or <any> ...) -> <boolean>" },
	{ "if",      sform_if,       NULL, DSL_PATTR_SELF_EVAL|DSL_PATTR_CHECK_ARITY, 3,
	  .helpstr = "(if <any:cond> <any:true> <any:false>) -> <any:true>|<any:false>" },
	{ "cond",    sform_cond,     NULL, DSL_PATTR_SELF_EVAL, 0,
	  .helpstr = "(cond (<any:cond0> ... <any:expr0>) ... (<any:condN> ... <any:exprN>)) -> <any:exprI>|false" } ,
	{ "not",     builtin_not,    NULL, DSL_PATTR_CHECK_ARITY, 1,
	  .helpstr = "(not <any>) -> <boolean>" },
	{ "eq?",     builtin_eq,     NULL, DSL_PATTR_CHECK_ARITY, 2,
	  .helpstr = "(eq? <any> <any>) -> <boolean>" },
	{ "<",       builtin_lt,     NULL, DSL_PATTR_CHECK_ARITY, 2,
	  .helpstr = "(< <integer> <integer>) -> <boolean>" },
	{ ">",       builtin_gt,     NULL, DSL_PATTR_CHECK_ARITY, 2,
	  .helpstr = "(> <integer> <integer>) -> <boolean>" },
	{ "<=",      builtin_le,     NULL, DSL_PATTR_CHECK_ARITY, 2,
	  .helpstr = "(<= <integer> <integer>) -> <boolean>" },
	{ ">=",      builtin_ge,     NULL, DSL_PATTR_CHECK_ARITY, 2,
	  .helpstr = "(>= <integer> <integer>) -> <boolean>" },
	{ "prefix?", builtin_prefix, NULL, DSL_PATTR_CHECK_ARITY, 2,
	  .helpstr = "(prefix? <string:target> <string:prefix>) -> <boolean>" },
	{ "suffix?", builtin_suffix, NULL, DSL_PATTR_CHECK_ARITY, 2,
	  .helpstr = "(suffix? <string:target> <string:suffix>) -> <boolean>" },
	{ "substr?", builtin_substr, NULL, DSL_PATTR_CHECK_ARITY, 2,
	  .helpstr = "(substr? <string:target> string:substr>) -> <boolean>" },
	{ "member",  builtin_member, NULL, DSL_PATTR_CHECK_ARITY, 2,
	  .helpstr = "(member <any> <list>) -> #f|<list>" },
	{ "downcase", builtin_downcase, NULL, DSL_PATTR_CHECK_ARITY, 1,
	  .helpstr = "(downcase <string>|<list>) -> <string>|<list>" },
	{ "upcase", builtin_upcase, NULL, DSL_PATTR_CHECK_ARITY, 1,
	  .helpstr = "(upcase <string>|<list>) -> <string>|<list>" },
	{ "length",  builtin_length, NULL, DSL_PATTR_CHECK_ARITY, 1,
	  .helpstr = "(length <string>) -> <integer>" },
	{ "+",               builtin_add,          NULL, DSL_PATTR_CHECK_ARITY, 2,
	  .helpstr = "(+ <integer> <integer>) -> <integer>", },
	{ "-",               builtin_sub,          NULL, DSL_PATTR_CHECK_ARITY, 2,
	  .helpstr = "(- <integer> <integer>) -> <integer>", },
	{ "concat",   builtin_string_append,NULL, 0, 0,
	  .helpstr = "(concat <string> ...) -> <string>; an alias for string-append",
	  .macro = macro_string_append },
	{ "string-append",   builtin_string_append,NULL, 0, 0,
	  .helpstr = "(string-append <string> ...) -> <string>",
	  .macro = macro_string_append },
	{ "string->regexp",  builtin_string2regexp,NULL, 0, 0,
	  .helpstr = "((string->regexp <string:pattern>) <string:target>) -> <boolean>",
	  .macro = macro_string2regexp },
	{ "regexp-quote",    builtin_regexp_quote, NULL, DSL_PATTR_CHECK_ARITY, 1,
	  .helpstr = "(regexp-quote <string>) -> <string>",
	  .macro = macro_regexp_quote },
	{ "print",   bulitin_debug_print, NULL, DSL_PATTR_CHECK_ARITY, 1,
	  .helpstr = "(print OBJ) -> OBJ" },
	{ "printX", NULL, 0, 0,
	  .helpstr = "(printX EXPR) -> EXPR; do the same as `print' but this works before evaluating",
	  .macro = macro_debug_printX },
	{ "true",    value_true, NULL, 0, 0UL,
	  .helpstr = "-> #t" },
	{ "false",    value_false, NULL, 0, 0UL,
	  .helpstr = "-> #f" },
	{ "nil",    value_nil, NULL, 0, 0UL,
	  .helpstr = "-> ()" },
	{ "$",       builtin_entry_ref, NULL, DSL_PATTR_CHECK_ARITY, 1,
	  .helpstr = "($ <string:field>) -> #f|<string>" },
	{ "$name",           value_name,           NULL, DSL_PATTR_MEMORABLE, 0UL,
	  .helpstr = "-> <string>"},
	{ "$input",          value_input,          NULL, DSL_PATTR_MEMORABLE, 0UL,
	  .helpstr = "-> <string>; input file name" },
	{ "$pattern",        value_pattern,        NULL, DSL_PATTR_MEMORABLE, 0UL,
	  .helpstr = "-> #f|<string>"},
	{ "$line",           value_line,           NULL, DSL_PATTR_MEMORABLE, 0UL,
	  .helpstr = "-> #f|<integer>" },
	{ "$access",         value_access,         NULL, DSL_PATTR_MEMORABLE, 0UL,
	  .helpstr = "-> #f|<string>" },
	{ "$end",            value_end,            NULL, DSL_PATTR_MEMORABLE, 0UL,
	  .helpstr = "-> #f|<integer>"},
	{ "$extras",         value_extras,         NULL, DSL_PATTR_MEMORABLE, 0UL,
	  .helpstr = "-> #f|<string>"},
	{ "$file",           value_file,           NULL, DSL_PATTR_MEMORABLE, 0UL,
	  .helpstr = "-> <boolean>; whether the scope is limited in the file or not." },
	{ "$inherits",       value_inherits,       NULL, DSL_PATTR_MEMORABLE, 0UL,
	  .helpstr = "-> <list>" },
	{ "$implementation", value_implementation, NULL, DSL_PATTR_MEMORABLE, 0UL,
	  .helpstr = "-> #f|<string>" },
	{ "$kind",           value_kind,           NULL, DSL_PATTR_MEMORABLE, 0UL,
	  .helpstr = "-> #f|<string>"},
	{ "$language",       value_language,       NULL, DSL_PATTR_MEMORABLE, 0UL,
	  .helpstr = "-> #f|<string>" },
	{ "$scope",          value_scope,          NULL, DSL_PATTR_MEMORABLE, 0UL,
	  .helpstr = "-> #f|<string>; $scope-kind:$scope-name"},
	{ "$scope-kind",     value_scope_kind,     NULL, DSL_PATTR_MEMORABLE, 0UL,
	  .helpstr = "-> #f|<string>"},
	{ "$scope-name",     value_scope_name,     NULL, DSL_PATTR_MEMORABLE, 0UL,
	  .helpstr = "-> #f|<string>"},
	{ "$signature",      value_signature,      NULL, DSL_PATTR_MEMORABLE, 0UL,
	  .helpstr = "-> #f|<string>" },
	{ "$typeref",        value_typeref,        NULL, DSL_PATTR_MEMORABLE, 0UL,
	  .helpstr = "-> #f|<string>"},
	{ "$typeref-kind",   value_typeref_kind,   NULL, DSL_PATTR_MEMORABLE, 0UL,
	  .helpstr = "-> #f|<string>"},
	{ "$typeref-name",   value_typeref_name,   NULL, DSL_PATTR_MEMORABLE, 0UL,
	  .helpstr = "-> #f|<string>"},
	{ "$roles",          value_roles,          NULL, DSL_PATTR_MEMORABLE, 0UL,
	  .helpstr = "-> <list>" },
	{ "$xpath",         value_xpath,           NULL, DSL_PATTR_MEMORABLE, 0UL,
	  .helpstr = "-> #f|<string>"},
};


/*
 * FUNCTION DEFINITIONS
 */
static EsObject * dsl_define (DSLEngineType engine, DSLProcBind *pbind)
{
	EsObject *name = es_symbol_intern (pbind->name);
	if (name == es_nil)
		return es_nil;

	DSLProcBind **pbs = es_symbol_get_data (name);
	if (!pbs)
	{
		pbs = calloc (DSL_ENGINE_COUNT, sizeof (pbinds[0]));
		if (!pbs)
			return es_nil;
		es_symbol_set_data (name, pbs);
	}

	pbs [engine] = pbind;

	return name;
}

int dsl_init (DSLEngineType engine, DSLProcBind *engine_pbinds, int count)
{
	static int initialized = 0;

	if (!initialized)
	{
		engines [DSL_INTERNAL_PSEUDO].pbinds = pbinds_interanl_pseudo;
		engines [DSL_INTERNAL_PSEUDO].pbinds_count
			= sizeof(pbinds_interanl_pseudo)/sizeof(pbinds_interanl_pseudo [0]);

		for (int i = 0; i < sizeof(pbinds)/sizeof(pbinds [0]); i++)
		{
			if (dsl_define (DSL_COMMON, pbinds + i) == NULL)
				return 0;
		}
		engines [DSL_COMMON].pbinds = pbinds;
		engines [DSL_COMMON].pbinds_count = sizeof(pbinds)/sizeof(pbinds [0]);

		initialized = 1;
	}

	for (int i = 0; i < count; i++)
	{
		if (dsl_define (engine, engine_pbinds + i) == NULL)
			return 0;
	}

	engines [engine].pbinds = engine_pbinds;
	engines [engine].pbinds_count = count;

	return 1;
}

DSLProcBind *dsl_lookup (DSLEngineType engine, EsObject *name)
{
	DSLProcBind **pbs = es_symbol_get_data (name);
	if (!pbs)
		return NULL;
	return pbs [engine]? pbs [engine]: pbs [DSL_COMMON];
}

static void dsl_help0 (DSLEngineType engine, FILE *fp)
{
	DSLEngine *e = engines + engine;

	for (int i = 0; i < e->pbinds_count; i++)
	{
		const char* hs = e->pbinds [i].helpstr;
		fprintf(fp, "%15s: %s\n", e->pbinds [i].name, hs? hs: "");
	}
}

void dsl_help (DSLEngineType engine, FILE *fp)
{
	dsl_help0 (DSL_INTERNAL_PSEUDO, fp);
	dsl_help0 (DSL_COMMON, fp);
	dsl_help0 (engine, fp);
}

static void dsl_cache_reset0 (DSLProcBind  *pb)
{
	if (pb->flags & DSL_PATTR_MEMORABLE)
		pb->cache = NULL;
}

void dsl_cache_reset (DSLEngineType engine)
{
	for (int i = 0; i < sizeof(pbinds)/sizeof(pbinds [0]); i++)
		dsl_cache_reset0 (pbinds + i);

	DSLEngine *e = engines + engine;
	for (int i = 0; i < e->pbinds_count; i++)
		dsl_cache_reset0( e->pbinds + i);
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
			es_cons(dsl_eval0 (es_car (object), env),
				eval0 (es_cdr (object), env))
			);
}

static EsObject *dsl_eval0 (EsObject *object, DSLEnv *env)
{
	EsObject *r;
	DSLProcBind *pb;
	EsObject *car;

	if (es_null (object))
		return es_nil;
	else if (es_symbol_p (object))
	{
		pb = dsl_lookup (env->engine, object);

		if (pb)
		{
			if (pb->cache)
				return pb->cache;

			r = pb->proc (es_nil, env);
			if (pb->flags & DSL_PATTR_MEMORABLE)
				pb->cache = r;
			return r;
		}
		else
			dsl_throw (UNBOUND_VARIABLE, object);
	}
	else if (es_atom (object))
		return object;

	car = es_car (object);
	if (es_regex_p(car))
	{
		EsObject *cdr = es_cdr (object);
		int l = length (cdr);

		if (l < 1)
			dsl_throw (TOO_FEW_ARGUMENTS, car);
		else if (l > 1)
			dsl_throw (TOO_MANY_ARGUMENTS, car);

		cdr = eval0(cdr, env);
		EsObject *err;

		err = error_included (cdr);
		if (!es_object_equal (err, es_false))
			return err;

		EsObject *cadr = es_car (cdr);
		if (!es_string_p (cadr))
			dsl_throw (WRONG_TYPE_ARGUMENT, object);

		r = es_regex_exec (car, cadr);
		return r;
	}
	else if (es_error_p(car))
		return car;
	else if (es_cons_p (car))
	{
		car = dsl_eval0 (car, env);
		if (es_error_p(car))
			return car;

		object = es_object_autounref (es_cons (car, es_cdr (object)));
		return dsl_eval0 (object, env);
	}
	else if (es_symbol_p(car))
	{
		EsObject *cdr = es_cdr (object);
		int l;

		pb = dsl_lookup (env->engine, car);

		if (!pb)
			dsl_throw (UNBOUND_VARIABLE, car);

		if (pb->cache)
			return pb->cache;

		if (pb->flags & DSL_PATTR_CHECK_ARITY)
		{
			l = length (cdr);
			if (l < pb->arity)
				dsl_throw (TOO_FEW_ARGUMENTS, car);
			else if (l > pb->arity &&
					 !(pb->flags & DSL_PATTR_CHECK_ARITY_OPT))
				dsl_throw (TOO_MANY_ARGUMENTS, car);
		}

		if (! (pb->flags & DSL_PATTR_SELF_EVAL))
		{
			EsObject *err;

			cdr = eval0(cdr, env);

			err = error_included (cdr);
			if (!es_object_equal (err, es_false))
				return err;
		}

		r = pb->proc (cdr, env);
		if (pb->flags & DSL_PATTR_MEMORABLE)
			pb->cache = r;
		return r;
	}
	else
		dsl_throw (CALLABLE_REQUIRED, car);
}

EsObject *dsl_eval (DSLCode *code, DSLEnv *env)
{
	return dsl_eval0 (code->expr, env);
}

EsObject *dsl_compile_and_eval (EsObject *expr, DSLEnv *env)
{
	return dsl_eval0 (expr, env);
}

static EsObject *compile (EsObject *expr, void *engine)
{
	if (!es_cons_p (expr))
		return es_object_ref (expr);

	EsObject *head = es_car (expr);
	if (es_symbol_p (head))
	{
		DSLProcBind *pb = dsl_lookup (*((DSLEngineType *)engine),
									  head);
		if (!pb)
			dsl_throw (UNBOUND_VARIABLE, head);
		if (pb->macro)
		{
			EsObject *tail = compile (es_cdr (expr), engine);
			if (es_error_p (tail))
				return tail;
			expr = es_cons (head, tail);
			EsObject *r = pb->macro (expr);
			es_object_unref (expr);
			es_object_unref (tail);
			return r;
		}
	}
	return es_map (compile, expr, engine);
}

DSLCode *dsl_compile (DSLEngineType engine, EsObject *expr)
{
	DSLCode *code = malloc (sizeof (DSLCode));
	if (code == NULL)
		return NULL;

	code->expr = compile (expr, &engine);
	if (es_null (code->expr))
	{
		free (code);
		return NULL;
	}
	else if (es_error_p (code->expr))
	{
		dsl_report_error ("COMPILE ERROR", code->expr);
		free (code);
		return NULL;
	}
	return code;
}

void dsl_release (DSLEngineType engine, DSLCode *code)
{
	es_object_unref (code->expr);
	free (code);
}

/*
 * Built-ins
 */
static EsObject* builtin_null  (EsObject *args, DSLEnv *env)
{
	return es_null(es_car (args))? es_true: es_false;
}

static EsObject* sform_begin_common  (const char *fn, EsObject *args, DSLEnv *env)
{
	if (es_null (args))
		dsl_throw (TOO_FEW_ARGUMENTS,
				   es_symbol_intern (fn));

	EsObject *o = es_false;
	while (! es_null (args))
	{
		o = es_car (args);
		o = dsl_eval0 (o, env);
		if (es_error_p (o))
			return o;
		args = es_cdr (args);
	}
	return o;
}

static EsObject* sform_begin  (EsObject *args, DSLEnv *env)
{
	return sform_begin_common ("begin", args, env);
}

static EsObject* sform_begin0  (EsObject *args, DSLEnv *env)
{
	if (es_null (args))
		dsl_throw (TOO_FEW_ARGUMENTS, es_symbol_intern ("begin0"));

	int count = 0;
	EsObject *o, *o0 = es_false;
	while (! es_null (args))
	{
		o = es_car (args);
		o = dsl_eval0 (o, env);
		if (!count++)
			o0 = o;

		if (es_error_p (o))
			return o;
		args = es_cdr (args);
	}
	return o0;
}

static EsObject* sfrom_and  (EsObject *args, DSLEnv *env)
{
	EsObject *o = es_true;

	while (! es_null (args))
	{
		o = es_car (args);
		o = dsl_eval0 (o, env);
		if (es_object_equal (o, es_false))
			return es_false;
		else if (es_error_p (o))
			return o;
		args = es_cdr (args);
	}

	return o;
}

static EsObject* sform_or  (EsObject *args, DSLEnv *env)
{
	EsObject *o;

	while (! es_null (args))
	{
		o = es_car (args);
		o = dsl_eval0 (o, env);
		if (! es_object_equal (o, es_false))
			return o;
		else if (es_error_p (o))
			return o;
		args = es_cdr (args);
	}

	return es_false;
}

static EsObject* sform_if (EsObject *args, DSLEnv *env)
{
	EsObject *o;

	o = es_car (args);
	o = dsl_eval0 (o, env);
	if (!es_object_equal (o, es_false))
		return dsl_eval0 (es_car (es_cdr (args)), env);
	else if (es_error_p (o))
		return o;
	else
		return dsl_eval0 (es_car (es_cdr (es_cdr (args))), env);

	return es_false;
}

static EsObject* sform_cond (EsObject *args, DSLEnv *env)
{

	while (!es_null(args))
	{
		EsObject *o = es_car (args);
		args = es_cdr (args);

		if (!es_cons_p (o))
			dsl_throw (WRONG_TYPE_ARGUMENT, o);

		EsObject *condition = es_car(o);
		EsObject *actions   = es_cdr(o);

		EsObject *result = dsl_eval0 (condition, env);
		if (es_error_p (result))
			return result;

		if (!es_object_equal (result, es_false))
		{
			if (es_null (actions))
				return result;
			return sform_begin_common("cond", actions, env);
		}
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

static EsObject* builtin_length (EsObject *args, DSLEnv *env)
{
	EsObject *o = es_car(args);
	if (es_error_p (o))
		return o;
	if (!es_string_p (o))
		dsl_throw (WRONG_TYPE_ARGUMENT, es_symbol_intern ("length"));

	const char *cstr = es_string_get (o);
	size_t len = strlen (cstr);
	return es_object_autounref (es_integer_new ((int)len));
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

/*
 * Accessesors for tagEntry
 */

/*
 * Value functions
 */
DEFINE_VALUE_FN(name)
DEFINE_VALUE_FN(input)
DEFINE_VALUE_FN(pattern)
DEFINE_VALUE_FN(line)

DEFINE_VALUE_FN(access)
DEFINE_VALUE_FN(end)
DEFINE_VALUE_FN(extras)
DEFINE_VALUE_FN(file)
DEFINE_VALUE_FN(inherits)
DEFINE_VALUE_FN(implementation)
DEFINE_VALUE_FN(kind)
DEFINE_VALUE_FN(language)
DEFINE_VALUE_FN(scope)
DEFINE_VALUE_FN(scope_kind)
DEFINE_VALUE_FN(scope_name)
DEFINE_VALUE_FN(signature)
DEFINE_VALUE_FN(typeref)
DEFINE_VALUE_FN(typeref_kind)
DEFINE_VALUE_FN(typeref_name)
DEFINE_VALUE_FN(roles)
DEFINE_VALUE_FN(xpath)

static const char*entry_xget (const tagEntry *entry, const char* name)
{
	unsigned int i;
	unsigned short count = entry->fields.count;
	tagExtensionField *list = entry->fields.list;

	for (i = 0; i < count; ++i)
	{
		if (strcmp (list [i].key, name) == 0)
			return list [i].value;
	}
	return NULL;

}

EsObject* dsl_entry_xget_string (const tagEntry *entry, const char* name)
{
	const char* value = entry_xget (entry, name);
	if (value)
		return es_object_autounref (es_string_new (value));
	else
		return es_false;
}

/*
 * Accessesors for tagEntry
 */

static EsObject* builtin_entry_ref (EsObject *args, DSLEnv *env)
{
	EsObject *key = es_car(args);

	if (es_error_p (key))
		return key;
	else if (! es_string_p (key))
		dsl_throw (WRONG_TYPE_ARGUMENT, es_symbol_intern ("$"));
	else
		return dsl_entry_xget_string (env->entry, es_string_get (key));
}

EsObject* dsl_entry_name (const tagEntry *entry)
{
	return es_object_autounref (es_string_new (entry->name));
}

EsObject* dsl_entry_input (const tagEntry *entry)
{
	return es_object_autounref (es_string_new (entry->file));
}

EsObject* dsl_entry_access (const tagEntry *entry)
{
	return dsl_entry_xget_string (entry, "access");
}

EsObject* dsl_entry_file (const tagEntry *entry)
{
	return entry->fileScope? es_true: es_false;
}

EsObject* dsl_entry_language (const tagEntry *entry)
{
	return dsl_entry_xget_string (entry, "language");
}

EsObject* dsl_entry_implementation (const tagEntry *entry)
{
	return dsl_entry_xget_string (entry, "implementation");
}

EsObject* dsl_entry_signature (const tagEntry *entry)
{
	return dsl_entry_xget_string (entry, "signature");
}

EsObject* dsl_entry_line (const tagEntry *entry)
{
	unsigned long ln = entry->address.lineNumber;

	if (ln == 0)
		return es_false;
	else
		return es_object_autounref (es_integer_new (ln));
}

EsObject* dsl_entry_extras (const tagEntry *entry)
{
	return dsl_entry_xget_string (entry, "extras");
}

EsObject* dsl_entry_end (const tagEntry *entry)
{
	const char *end_str = entry_xget(entry, "end");
	EsObject *o;

	if (end_str)
	{
		o = es_read_from_string (end_str, NULL);
		if (es_integer_p (o))
			return es_object_autounref (o);
		else
			return es_false;
	}
	else
		return es_false;
}

EsObject* dsl_entry_kind (const tagEntry *entry)
{
	const char* kind;
	kind = entry->kind;

	if (kind)
		return es_object_autounref (es_string_new (entry->kind));
	else
		return es_false;
}

EsObject* dsl_entry_roles (const tagEntry *entry)
{
	return dsl_entry_xget_string(entry, "roles");
}

EsObject* dsl_entry_pattern (const tagEntry *entry)
{
	const char *pattern = entry->address.pattern;

	if (pattern == NULL)
		return es_false;
	else
		return es_object_autounref (es_string_new (pattern));
}

EsObject* dsl_entry_inherits (const tagEntry *entry)
{
	return dsl_entry_xget_string (entry, "inherits");
}

EsObject* dsl_entry_scope (const tagEntry *entry)
{
	return dsl_entry_xget_string (entry, "scope");
}

EsObject* dsl_entry_typeref (const tagEntry *entry)
{
	return dsl_entry_xget_string (entry, "typeref");
}

EsObject* dsl_entry_xpath (const tagEntry *entry)
{
	return dsl_entry_xget_string (entry, "xpath");
}

EsObject* dsl_entry_scope_kind (const tagEntry *entry)
{
	const char* scope = entry_xget (entry, "scope");
	const char* kind;
	EsObject *r;

	if (scope == NULL)
		return es_false;

	kind = strchr (scope, ':');
	if (kind == NULL)
		return es_false;

	r = es_object_autounref (es_string_newL (scope,
											 kind - scope));
	return r;
}

EsObject* dsl_entry_scope_name (const tagEntry *entry)
{
	const char* scope = entry_xget (entry, "scope");
	const char* kind;
	EsObject *r;

	if (scope == NULL)
		return es_false;

	kind = strchr (scope, ':');
	if (kind == NULL)
		return es_false;

	if (*(kind + 1) == '\0')
		return es_false;

	r = es_object_autounref (es_string_new (kind + 1));

	return r;
}

EsObject* dsl_entry_typeref_kind (const tagEntry *entry)
{
	const char* typeref = entry_xget (entry, "typeref");
	const char* kind;
	EsObject *r;

	if (typeref == NULL)
		return es_false;

	kind = strchr (typeref, ':');
	if (kind == NULL)
		return es_false;

	r = es_object_autounref (es_string_newL (typeref,
											 kind - typeref));
	return r;
}

EsObject* dsl_entry_typeref_name (const tagEntry *entry)
{
	const char* typeref = entry_xget (entry, "typeref");
	const char* kind;
	EsObject *r;

	if (typeref == NULL)
		return es_false;

	kind = strchr (typeref, ':');
	if (kind == NULL)
		return es_false;

	if (*(kind + 1) == '\0')
		return es_false;

	r = es_object_autounref (es_string_new (kind + 1));

	return r;
}

static EsObject* accumulate_length (EsObject *elt, void *data)
{
	size_t *len = data;
	if (!es_string_p (elt))
		dsl_throw (STRING_REQUIRED, elt);

	const char *s = es_string_get (elt);
	*len += strlen (s);

	return es_false;
}

static EsObject* string_accumulate (EsObject *elt, void *data)
{
	char **cursor = data;
	const char *s;
	char *t;

	for (s = es_string_get(elt), t = *cursor;
		 *s != '\0';
		 s++, t++)
		*t = *s;
	*cursor = t;
	return es_false;
}

static EsObject* builtin_string_append  (EsObject *args, DSLEnv *env)
{
	size_t len = 0;

	EsObject *r = es_foreach (accumulate_length, args, &len);
	if (!es_object_equal (r, es_false))
		return r;

	char *buf = malloc (len + 1);
	if (buf == NULL)
		return ES_ERROR_MEMORY;

	char *cursor = buf;
	r = es_foreach (string_accumulate, args, &cursor);
	if (!es_object_equal (r, es_false))
		goto out;
	*cursor = '\0';

	r = es_string_new (buf);

 out:
	free (buf);
	return es_object_autounref (r);
}

static EsObject* optimize_strings (EsObject *kar, EsObject *kdr, void *user_data)
{
	EsObject *r;
	if (!es_string_p (kar))
	{
		r = es_cons (kar, kdr);
		return r;
	}

	EsObject *kadr = es_car (kdr);
	if (!es_string_p (kadr))
	{
		r = es_cons (kar, kdr);
		return r;
	}

	const char *kar_str = es_string_get (kar);
	const char *kadr_str = es_string_get (kadr);
	size_t kar_len = strlen (kar_str);
	size_t kadr_len = strlen (kadr_str);
	char *buf = malloc (kar_len + kadr_len + 1);
	if (buf == NULL)
		return ES_ERROR_MEMORY;

	memcpy (buf, kadr_str, kadr_len);
	memcpy (buf + kadr_len, kar_str, kar_len);
	buf [kadr_len + kar_len] = '\0';

	EsObject *elt = es_object_autounref (es_string_new (buf));
	free (buf);

	if (es_error_p (elt))
		return ES_ERROR_MEMORY;

	r = es_cons (elt, es_cdr (kdr));

	return r;
}

static EsObject* macro_string_append (EsObject *expr)
{
	EsObject *new_expr;
	EsObject *list = es_cdr (expr);
	EsObject *r = es_fold (optimize_strings,
						   es_nil, list, NULL);

	if (es_error_p (r))
		return es_error_set_object (r, expr);

	EsObject *str = es_car (r);
	if (es_string_p (str) && es_null (es_cdr (r)))
		new_expr = es_object_ref (str);
	else
	{
		EsObject *kar = es_car (expr);
		EsObject* rr  = es_reverse (r);
		new_expr = es_cons (kar, rr);
		es_object_unref (rr);
	}
	es_object_unref (r);
	return new_expr;
}

static EsObject* builtin_add  (EsObject *args, DSLEnv *env)
{
	EsObject *a = es_car (args);
	if (!es_integer_p (a))
		dsl_throw (INTEGER_REQUIRED,
				   es_symbol_intern ("+"));

	EsObject *b = es_car (es_cdr (args));
	if (!es_integer_p (b))
		dsl_throw (INTEGER_REQUIRED,
				   es_symbol_intern ("+"));

	int ai = es_integer_get (a);
	int bi = es_integer_get (b);

	return es_object_autounref (es_integer_new (ai + bi));
}

static EsObject* builtin_sub  (EsObject *args, DSLEnv *env)
{
	EsObject *a = es_car (args);
	if (!es_integer_p (a))
		dsl_throw (INTEGER_REQUIRED,
				   es_symbol_intern ("-"));

	EsObject *b = es_car (es_cdr (args));
	if (!es_integer_p (b))
		dsl_throw (INTEGER_REQUIRED,
				   es_symbol_intern ("-"));

	int ai = es_integer_get (a);
	int bi = es_integer_get (b);

	return es_object_autounref (es_integer_new (ai - bi));
}

static EsObject* value_true (EsObject *args, DSLEnv *env)
{
	return es_true;
}

static EsObject* value_false (EsObject *args, DSLEnv *env)
{
	return es_false;
}

static EsObject* value_nil (EsObject *args, DSLEnv *env)
{
	return es_nil;
}

static EsObject* common_string2regexp (EsObject *args, DSLEnv *env,
									   EsObject *original_expr)
{
	static EsObject *self = es_nil;
	if (self == es_nil)
		self = es_symbol_intern ("string->regexp");

	if (!es_cons_p (args))
		dsl_throw(TOO_FEW_ARGUMENTS,
				  (original_expr == es_nil)
				  ? self
				  : es_car (original_expr));
	else if (!es_string_p (es_car (args)))
	{
		if (original_expr == es_nil)
			dsl_throw(STRING_REQUIRED, self);
		else
			return es_object_ref (original_expr);
	}
	else
	{
		static EsObject *case_fold_key = es_nil;
		if (case_fold_key == es_nil)
			case_fold_key = es_symbol_intern (":case-fold");
		static EsObject *false_val = es_nil;
		if (false_val == es_nil)
			false_val = es_symbol_intern ("false");

		EsObject *case_fold = es_car (es_cdr (args));
		EsObject *pattern = es_car (args);
		int icase = 0;

		if (!es_null (case_fold))
		{
			if (!es_object_equal (case_fold, case_fold_key))
				dsl_throw (WRONG_TYPE_ARGUMENT,
						   (original_expr == es_nil)? self: original_expr);

			case_fold = es_car (es_cdr (es_cdr (args)));
			if (es_null (case_fold))
				dsl_throw (TOO_FEW_ARGUMENTS,
						   (original_expr == es_nil)? self: original_expr);

			icase = ! (es_object_equal(case_fold, es_false)
					   /* TODO: remove the next condition. */
					   || es_object_equal(case_fold, false_val));
		}

		EsObject *r = es_regex_compile (es_string_get (pattern), icase);
		return (original_expr == es_nil)? es_object_autounref (r): r;
	}
}

static EsObject* builtin_string2regexp (EsObject *args, DSLEnv *env)
{
	return common_string2regexp (args, env, es_nil);
}

static EsObject* macro_string2regexp (EsObject *expr)
{
	return common_string2regexp (es_cdr (expr), NULL, expr);
}

static EsObject* common_regexp_quote (EsObject *args, DSLEnv *env,
									  EsObject *original_expr)
{
	static EsObject *self = es_nil;
	if (self == es_nil)
		self = es_symbol_intern ("regexp-quote");

	if (original_expr != es_nil && !es_cons_p (args))
		dsl_throw(TOO_FEW_ARGUMENTS,
				  es_car (original_expr));

	EsObject *unquoted_str = es_car (args);
	if (!es_string_p (unquoted_str))
	{
		if (original_expr  == es_nil)
			dsl_throw(STRING_REQUIRED, self);
		else
			return es_object_ref (original_expr);
	}

	const char *src = es_string_get (unquoted_str);
	if (src[0] == '\0')
		return (original_expr  == es_nil)
			? unquoted_str
			: es_object_ref (unquoted_str);

	size_t len = strlen (src);
	char *buf = malloc (len * 2 + 1);
	if (!buf)
		return ES_ERROR_MEMORY;

	char *dst = buf;
	for (size_t i = 0; i < len; i++)
	{
		if (strchr ("[{.*+]}^$()|?\\", src [i]))
			*dst++ = '\\';
		*dst++ = src[i];
	}
	*dst = '\0';
	EsObject *r = es_string_new (buf);
	free (buf);
	return (original_expr  == es_nil)? es_object_autounref (r): r;
}

static EsObject* builtin_regexp_quote (EsObject *args, DSLEnv *env)
{
	return common_regexp_quote (args, env, es_nil);
}

static EsObject* macro_regexp_quote (EsObject *expr)
{
	return common_regexp_quote (es_cdr (expr), NULL, expr);
}

static EsObject* macro_debug_printX (EsObject *expr)
{
	EsObject *code = es_cdr (expr);
	bulitin_debug_print (code, NULL);
	return es_object_ref(es_car (code));
}

void dsl_report_error (const char *msg, EsObject *obj)
{
	MIO  *mioerr = mio_new_fp (stderr, NULL);

	if (es_error_p (obj))
	{
		fprintf(stderr, "%s: %s: ", msg, es_error_name (obj));
		es_print(es_error_get_object(obj), mioerr);
	}
	else
	{
		fprintf(stderr, "%s: ", msg);
		es_print(obj, mioerr);
	}
	putc('\n', stderr);
	mio_unref(mioerr);
}
