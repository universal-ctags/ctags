/*
*   Copyright (c) 2021, Masatake YAMATO
*   Copyright (c) 2021, Red Hat, Inc.
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*/

/*
 * INCLUDES
 */

#include "formatter.h"
#include "dsl.h"
#include "es.h"

#include <stdlib.h>
#include <string.h>

static EsObject* formatter_proc_list (EsObject* args, DSLEnv *env);

/*
 * DATA DEFINITIONS
 */
static DSLProcBind pbinds [] = {
	{ "list", formatter_proc_list, NULL, 0, 0UL,
	  .helpstr = "(list <any> ...) -> (<any> ...)" },
};

static EsObject* formatter_proc_list (EsObject* args, DSLEnv *env)
{
	return args;
}

/*
 * Procs
 */
static int initialize (void)
{
	static int initialized;

	if (initialized)
		return 1;

	if (!dsl_init (DSL_FORMATTER, pbinds, sizeof(pbinds)/sizeof(pbinds [0])))
	{
		fprintf(stderr, "MEMORY EXHAUSTED\n");
		return 0;
	}

	initialized = 1;
	return 1;
}

/*
 * FCode
 */
struct sFCode
{
	DSLCode *dsl;
};

FCode *f_compile (EsObject *exp)
{
	FCode *code;

	if (!initialize ())
		exit (1);

	code = malloc (sizeof (FCode));
	if (code == NULL)
	{
		fprintf(stderr, "MEMORY EXHAUSTED\n");
		return NULL;
	}

	code->dsl = dsl_compile (DSL_FORMATTER, exp);
	if (code->dsl == NULL)
	{
		fprintf(stderr, "MEMORY EXHAUSTED or SYNTAX ERROR\n");
		free (code);
		return NULL;
	}

	return code;
}

static int f_print0(EsObject *r, FILE *out)
{
	if (es_error_p (r))
	{
		dsl_report_error ("GOT ERROR in FORMATTING", r);
		return 1;
	}
	else if (es_string_p (r))
	{
		fputs (es_string_get(r), out);
		return 0;
	}
	else if (es_integer_p (r))
	{
		fprintf(out, "%d", es_integer_get(r));
		return 0;
	}
	else if (es_object_equal(r, es_false))
	{
		return 0;
	}
	else if (es_object_equal(r, es_true))
	{
		putc ('\n', out);
		return 0;
	}
	else if (es_cons_p (r))
	{
		EsObject *car = es_car (r);
		EsObject *cdr = es_cdr(r);

		for (; !es_null (car);
			 car = es_car (cdr), cdr = es_cdr (cdr))
		{
			if (f_print0 (car, out))
				return 1;
		}
		return 0;
	}
	else
	{
		dsl_report_error ("UNEXPECTED VALUE IN FORMATTING", r);
		return 1;
	}
}

int f_print (const tagEntry * entry, FCode *code, FILE *out)
{
	EsObject *r;
	int exit_code = 0;

	DSLEnv env = {
		.engine = DSL_FORMATTER,
		.entry = entry,
	};

	es_autounref_pool_push ();
	r = dsl_eval (code->dsl, &env);
	exit_code = f_print0 (r, out);
	es_autounref_pool_pop ();

	dsl_cache_reset (DSL_FORMATTER);

	if (exit_code)
		exit (exit_code);

	return 0;
}

void f_destroy        (FCode *code)
{
	dsl_release (DSL_FORMATTER, code->dsl);
	free (code);
}

void f_help           (FILE *fp)
{
	if (!initialize ())
		exit (1);
	dsl_help (DSL_FORMATTER, fp);
}
