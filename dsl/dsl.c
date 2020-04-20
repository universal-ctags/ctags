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

static EsObject **engine_codes;

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

DSLCode *dsl_code_lookup (DSLEngineType engine, EsObject *name)
{
	DSLCode **codes = es_symbol_get_data (name);
	if (!codes)
		return NULL;
	return codes [engine];
}

void dsl_help (DSLEngineType engine, FILE *fp)
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

void dsl_code_reset (DSLCode  *code)
{
	if (code->flags & DSL_PATTR_MEMORABLE)
		code->cache = NULL;
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
