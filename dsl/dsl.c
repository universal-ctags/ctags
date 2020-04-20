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
#include <stdlib.h>

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
	return name;
}

DSLCode *dsl_code_lookup (DSLEngineType engine, EsObject *name)
{
	DSLCode **codes = es_symbol_get_data (name);
	if (!codes)
		return NULL;
	return codes [engine];
}
