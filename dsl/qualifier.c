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
static EsObject* builtin_entry_ref (EsObject *args, DSLEnv *env);
static EsObject* value_name (EsObject *args, DSLEnv *env);
static EsObject* value_input (EsObject *args, DSLEnv *env);
static EsObject* value_access (EsObject *args, DSLEnv *env);
static EsObject* value_file (EsObject *args, DSLEnv *env);
static EsObject* value_language (EsObject *args, DSLEnv *env);
static EsObject* value_implementation (EsObject *args, DSLEnv *env);
static EsObject* value_line (EsObject *args, DSLEnv *env);
static EsObject* value_kind (EsObject *args, DSLEnv *env);
static EsObject* value_roles (EsObject *args, DSLEnv *env);
static EsObject* value_pattern (EsObject *args, DSLEnv *env);
static EsObject* value_inherits (EsObject *args, DSLEnv *env);
static EsObject* value_scope_kind (EsObject *args, DSLEnv *env);
static EsObject* value_scope_name (EsObject *args, DSLEnv *env);
static EsObject* value_end (EsObject *args, DSLEnv *env);


static DSLCode codes [] = {
	{ "$",       builtin_entry_ref, NULL, DSL_PATTR_CHECK_ARITY, 1,
	  .helpstr = "($ NAME) -> #f|<string>" },
	{ "$name",           value_name,           NULL, DSL_PATTR_MEMORABLE, 0UL,},
	{ "$input",          value_input,          NULL, DSL_PATTR_MEMORABLE, 0UL,
	  .helpstr = "input file name" },
	{ "$access",         value_access,         NULL, DSL_PATTR_MEMORABLE, 0UL },
	{ "$file",           value_file,           NULL, DSL_PATTR_MEMORABLE, 0UL,
	  .helpstr = "file scope<boolean>" },
	{ "$language",       value_language,       NULL, DSL_PATTR_MEMORABLE, 0UL },
	{ "$implementation", value_implementation, NULL, DSL_PATTR_MEMORABLE, 0UL },
	{ "$line",           value_line,           NULL, DSL_PATTR_MEMORABLE, 0UL },
	{ "$kind",           value_kind,           NULL, DSL_PATTR_MEMORABLE, 0UL },
	{ "$roles",          value_roles,          NULL, DSL_PATTR_MEMORABLE, 0UL,
	  .helpstr = "<list>" },
	{ "$pattern",        value_pattern,        NULL, DSL_PATTR_MEMORABLE, 0UL },
	{ "$inherits",       value_inherits,       NULL, DSL_PATTR_MEMORABLE, 0UL,
	  .helpstr = "<list>" },
	{ "$scope-kind",     value_scope_kind,     NULL, DSL_PATTR_MEMORABLE, 0UL },
	{ "$scope-name",     value_scope_name,     NULL, DSL_PATTR_MEMORABLE, 0UL },
	{ "$end",            value_end,            NULL, DSL_PATTR_MEMORABLE, 0UL },
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

static EsObject* reverse (EsObject *object)
{
	EsObject *h;
	EsObject *r = es_nil;

	while (!es_null (object))
	{
		h = es_car (object);
		r = es_object_autounref (es_cons (h, r));
		object = es_cdr (object);
	}
	return r;
}

/*
 * Value
 */
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

static EsObject* entry_xget_string (const tagEntry *entry, const char* name)
{
	const char* value = entry_xget (entry, name);
	if (value)
		return es_object_autounref (es_string_new (value));
	else
		return es_false;
}

static EsObject* value_xget_csvlist (const tagEntry *entry, const char* field)
{
	const char* inherits = entry_xget (entry, field);

	if (inherits == NULL)
		return es_nil;
	else
	{
		EsObject *s = es_nil;
		char *d = strdup (inherits);
		char *h = d;
		char *t;

		if (h == NULL)
		{
			fprintf(stderr, "MEMORY EXHAUSTED\n");
			exit (1);
		}

		while ((t = strchr (h, ',')))
		{
			*t = '\0';
			s = es_cons (es_object_autounref (es_string_new (h)),
				     s);
			s = es_object_autounref (s);
			h = t + 1;
		}
		if (*h != '\0')
		{
			s = es_cons (es_object_autounref (es_string_new (h)),
				     s);
			s = es_object_autounref (s);
		}

		free (d);
		s = reverse (s);
		return s;
	}
}

static EsObject* builtin_entry_ref (EsObject *args, DSLEnv *env)
{
	EsObject *key = es_car(args);

	if (es_error_p (key))
		return key;
	else if (! es_string_p (key))
		dsl_throw (WRONG_TYPE_ARGUMENT, es_symbol_intern ("$"));
	else
		return entry_xget_string (env->entry, es_string_get (key));
}

static EsObject* value_name (EsObject *args, DSLEnv *env)
{
	return es_object_autounref (es_string_new (env->entry->name));
}

static EsObject* value_input (EsObject *args, DSLEnv *env)
{
	return es_object_autounref (es_string_new (env->entry->file));
}

static EsObject* value_access (EsObject *args, DSLEnv *env)
{
	return entry_xget_string (env->entry, "access");
}

static EsObject* value_file (EsObject *args, DSLEnv *env)
{
	return env->entry->fileScope? es_true: es_false;
}

static EsObject* value_language (EsObject *args, DSLEnv *env)
{
	return entry_xget_string (env->entry, "language");
}

static EsObject* value_implementation (EsObject *args, DSLEnv *env)
{
	return entry_xget_string (env->entry, "implementation");
}

static EsObject* value_line (EsObject *args, DSLEnv *env)
{
	unsigned long ln = env->entry->address.lineNumber;

	if (ln == 0)
		return es_false;
	else
		return es_object_autounref (es_integer_new (ln));
}

static EsObject* value_end (EsObject *args, DSLEnv *env)
{
	const char *end_str = entry_xget(env->entry, "end");
	EsObject *o;

	if (end_str)
	{
		o = es_read_from_string (end_str, NULL);
		if (es_error_p (o))
			return es_false;
		else
			return es_object_autounref (o);
	}
	else
		return es_false;
}

static EsObject* value_kind (EsObject *args, DSLEnv *env)
{
	const char* kind;
	kind = env->entry->kind;

	if (kind)
		return es_object_autounref (es_string_new (env->entry->kind));
	else
		return es_false;
}

static EsObject* value_roles (EsObject *args, DSLEnv *env)
{
	return value_xget_csvlist(env->entry, "roles");
}

static EsObject* value_pattern (EsObject *args, DSLEnv *env)
{
	const char *pattern = env->entry->address.pattern;

	if (pattern == NULL)
		return es_false;
	else
		return es_object_autounref (es_string_new (pattern));
}

static EsObject* value_inherits (EsObject *args, DSLEnv *env)
{
	return value_xget_csvlist (env->entry, "inherits");
}

static EsObject* value_scope_kind (EsObject *args, DSLEnv *env)
{
	const char* scope = entry_xget (env->entry, "scope");
	const char* kind;
	EsObject *r;

	if (scope == NULL)
		return es_false;

	kind = strchr (scope, ':');
	if (kind == NULL)
		return es_false;

	*(char *)kind = '\0';
	r = es_object_autounref (es_string_new (scope));
	*(char *)kind = ':';
	return r;
}

static EsObject* value_scope_name (EsObject *args, DSLEnv *env)
{
	const char* scope = entry_xget (env->entry, "scope");
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
