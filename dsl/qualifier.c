/*
*   Copyright (c) 2016, Masatake YAMATO
*   Copyright (c) 2016, Red Hat, Inc.
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*/

/*
 * INCLUDES
 */
#include "qualifier.h"
#include "dsl.h"
#include "es.h"

#include <stdlib.h>

/*
 * TYPES
 */
struct sQCode
{
	DSLCode *dsl;
};


/*
 * DATA DEFINITIONS
 */


/*
 * FUNCTION DEFINITIONS
 */
static int initialize (void)
{
	static int initialized;

	if (initialized)
		return 1;

	if (!dsl_init (DSL_QUALIFIER, NULL, 0))
	{
		fprintf(stderr, "MEMORY EXHAUSTED\n");
		return 0;
	}

	initialized = 1;
	return 1;
}

QCode  *q_compile (EsObject *exp)
{
	QCode *code;

	if (!initialize ())
		exit (1);

	code = malloc (sizeof (QCode));
	if (code == NULL)
	{
		fprintf(stderr, "MEMORY EXHAUSTED\n");
		return NULL;
	}

	code->dsl = dsl_compile (DSL_QUALIFIER, exp);
	if (code->dsl == NULL)
	{
		fprintf(stderr, "MEMORY EXHAUSTED or SYNTAX ERROR\n");
		free (code);
		return NULL;
	}
	return code;
}

enum QRESULT q_is_acceptable  (QCode *code, tagEntry *entry)
{
	EsObject *r;
	int i;

	DSLEnv env = {
		.engine = DSL_QUALIFIER,
		.entry  = entry,
	};
	es_autounref_pool_push ();
	r = dsl_eval (code->dsl, &env);
	if (es_object_equal (r, es_false))
		i = Q_REJECT;
	else if (es_error_p (r))
	{
		dsl_report_error ("GOT ERROR in QUALIFYING", r);
		i = Q_ERROR;
	}
	else
		i = Q_ACCEPT;
	es_autounref_pool_pop ();

	dsl_cache_reset (DSL_QUALIFIER);

	return i;
}

void q_destroy (QCode *code)
{
	dsl_release (DSL_QUALIFIER, code->dsl);
	free (code);
}

void q_help (FILE *fp)
{
	if (!initialize ())
		exit (1);
	dsl_help (DSL_QUALIFIER, fp);
}
