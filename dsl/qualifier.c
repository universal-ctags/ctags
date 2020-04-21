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
#include "es-lang-c-stdc99.h"

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
static DSLProcBind pbinds [] = {
};


/*
 * FUNCTION DEFINITIONS
 */
static int initialize (void)
{
	static int initialized;

	if (initialized)
		return 1;

	if (!dsl_init ())
	{
		fprintf(stderr, "MEMORY EXHAUSTED\n");
		return 0;
	}

	int i;
	for (i = 0; i < sizeof(pbinds)/sizeof(pbinds [0]); i++)
	{
		if (dsl_define (DSL_QUALIFIER, pbinds + i) == NULL)
		{
			fprintf(stderr, "MEMORY EXHAUSTED\n");
			return 0;
		}
	}
	initialized = 1;
	return 1;
}

static void reset (void)
{
	int i;
	DSLProcBind *pb;

	dsl_cache_reset (NULL);
	for (i = 0; i < sizeof(pbinds)/sizeof(pbinds [0]); i++)
	{
		pb = pbinds + i;
		dsl_cache_reset (pb);
	}
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
		exit (1);
	}

	code->dsl = dsl_compile (DSL_QUALIFIER, exp);
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
		MIO  *mioerr = mio_new_fp (stderr, NULL);

		fprintf(stderr, "GOT ERROR in QUALIFYING: %s: ",
			 es_error_name (r));
		es_print(es_error_get_object(r), mioerr);
		putc('\n', stderr);
		i = Q_ERROR;

		mio_unref(mioerr);
	}
	else
		i = Q_ACCEPT;
	es_autounref_pool_pop ();

	reset ();

	return i;
}

void q_destroy (QCode *code)
{
	dsl_release (DSL_QUALIFIER, code->dsl);
	free (code);
}

void q_help (FILE *fp)
{
	initialize ();
	dsl_help (DSL_QUALIFIER, fp);
}
