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

/*
 * FUNCTION DEFINITIONS
 */

EsObject *dsl_code_define (DSLCode *code)
{
	EsObject *name;

	name = es_symbol_intern (code->name);
	if (name == es_nil)
		return es_nil;
	es_symbol_set_data (name, code);
	return name;
}

DSLCode *dsl_code_lookup (EsObject *name)
{
	return es_symbol_get_data (name);
}
