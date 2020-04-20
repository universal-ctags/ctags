/*
*   Copyright (c) 2016, Masatake YAMATO
*   Copyright (c) 2016, Red Hat, Inc.
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for applying regular expression matching.
*
*   The code for utilizing the Gnu regex package with regards to processing the
*   regex option and checking for regex matches was adapted from routines in
*   Gnu etags.
*/

#include "qualifier.h"
#include "dsl.h"
#include "es-lang-c-stdc99.h"

#include <stdlib.h>
#include <string.h>
#include <limits.h>

/*
 * Types
 */


/*
 * Errors
 */


/*
 * Decls
 */
static DSLCode codes [] = {
};

static void initialize (void)
{
	static int initialized;

	if (initialized)
		return;

	if (!dsl_init ())
	{
		fprintf(stderr, "MEMORY EXHAUSTED\n");
		exit (1);
	}

	int i;
	for (i = 0; i < sizeof(codes)/sizeof(codes [0]); i++)
	{
		if (dsl_code_define (DSL_QUALIFIER, codes + i) == NULL)
		{
			fprintf(stderr, "MEMORY EXHAUSTED\n");
			exit (1);
		}
	}
	initialized = 1;
}

static void reset (void)
{
	int i;
	DSLCode *code;

	dsl_code_reset (NULL);
	for (i = 0; i < sizeof(codes)/sizeof(codes [0]); i++)
	{
		code = codes + i;
		dsl_code_reset (code);
	}
}


/*
 * QCode
 */

struct sQCode
{
	EsObject *es;
};

QCode  *q_compile (EsObject *exp)
{
	static QCode code;

	initialize ();

	code.es = es_object_ref (exp);
	return &code;
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
	r = dsl_eval (code->es, &env);
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

void    q_destroy (QCode *code)
{
	es_object_unref (code->es);
	free (code);
}

void         q_help           (FILE *fp)
{
	initialize ();
	dsl_help (DSL_QUALIFIER, fp);
}
