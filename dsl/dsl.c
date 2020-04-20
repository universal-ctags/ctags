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
