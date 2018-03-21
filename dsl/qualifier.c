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
#include "es-lang-c-stdc99.h"

#include <stdlib.h>
#include <string.h>
#include <limits.h>

/*
 * Types
 */
typedef struct sCode Code;
typedef EsObject* (* EsEntryProc)  (EsObject *args, tagEntry *entry);
enum ProcAttr {
	MEMORABLE   = 1UL << 0,
	PURE_PROC   = 1UL << 1,
	SELF_EVAL   = 1UL << 2,
	CHECK_ARITY = 1UL << 3,
};

/*
 * Errors
 */
#define ERR_UNBOUND_VARIABLE    (es_error_intern("unbound-variable"))
#define ERR_TOO_FEW_ARGUMENTS   (es_error_intern("too-few-arguments"))
#define ERR_TOO_MANY_ARGUMENTS  (es_error_intern("too-many-arguments"))
#define ERR_NUMBER_REQUIRED     (es_error_intern("number-required"))
#define ERR_WRONG_TYPE_ARGUMENT (es_error_intern("wrong-type-argument"))
#define throw(e,o)               return es_error_set_object(ERR_##e, o)

/*
 * Decls
 */
static EsObject* builtin_null  (EsObject *args, tagEntry *entry);
static EsObject* builtin_and  (EsObject *args, tagEntry *entry);
static EsObject* builtin_or  (EsObject *args, tagEntry *entry);
static EsObject* builtin_not  (EsObject *args, tagEntry *entry);
static EsObject* builtin_eq  (EsObject *args, tagEntry *entry);
static EsObject* builtin_lt  (EsObject *args, tagEntry *entry);
static EsObject* builtin_gt  (EsObject *args, tagEntry *entry);
static EsObject* builtin_le  (EsObject *args, tagEntry *entry);
static EsObject* builtin_ge  (EsObject *args, tagEntry *entry);
static EsObject* builtin_prefix (EsObject *args, tagEntry *entry);
static EsObject* builtin_suffix (EsObject *args, tagEntry *entry);
static EsObject* builtin_substr (EsObject *args, tagEntry *entry);
static EsObject* builtin_member (EsObject *args, tagEntry *entry);
static EsObject* builtin_entry_ref (EsObject *args, tagEntry *entry);


static EsObject* value_name (EsObject *args, tagEntry *entry);
static EsObject* value_input (EsObject *args, tagEntry *entry);
static EsObject* value_access (EsObject *args, tagEntry *entry);
static EsObject* value_file (EsObject *args, tagEntry *entry);
static EsObject* value_language (EsObject *args, tagEntry *entry);
static EsObject* value_implementation (EsObject *args, tagEntry *entry);
static EsObject* value_line (EsObject *args, tagEntry *entry);
static EsObject* value_kind (EsObject *args, tagEntry *entry);
static EsObject* value_roles (EsObject *args, tagEntry *entry);
static EsObject* value_pattern (EsObject *args, tagEntry *entry);
static EsObject* value_inherits (EsObject *args, tagEntry *entry);
static EsObject* value_scope_kind (EsObject *args, tagEntry *entry);
static EsObject* value_scope_name (EsObject *args, tagEntry *entry);
static EsObject* value_end (EsObject *args, tagEntry *entry);


struct sCode {
	const char *name;
	EsEntryProc proc;
	EsObject* cache;
	enum ProcAttr flags;
	int arity;
	const char* helpstr;
} codes [] = {
	{ "null?",    builtin_null,   NULL, CHECK_ARITY, 1 },
	{ "and",     builtin_and,    NULL, SELF_EVAL },
	{ "or",      builtin_or,     NULL, SELF_EVAL },
	{ "not",     builtin_not,    NULL, CHECK_ARITY, 1},
	{ "eq?",     builtin_eq,     NULL, CHECK_ARITY, 2 },
	{ "<",       builtin_lt,     NULL, CHECK_ARITY, 2 },
	{ ">",       builtin_gt,     NULL, CHECK_ARITY, 2 },
	{ "<=",      builtin_le,     NULL, CHECK_ARITY, 2 },
	{ ">=",      builtin_ge,     NULL, CHECK_ARITY, 2 },
	{ "prefix?", builtin_prefix, NULL, CHECK_ARITY, 2,
	  .helpstr = "(prefix? TARGET<string> PREFIX<string>) -> <boolean>" },
	{ "suffix?", builtin_suffix, NULL, CHECK_ARITY, 2,
	  .helpstr = "(suffix? TARGET<string> SUFFIX<string>) -> <boolean>" },
	{ "substr?", builtin_substr, NULL, CHECK_ARITY, 2,
	  .helpstr = "(substr? TARGET<string> SUBSTR<string>) -> <boolean>" },
	{ "member",  builtin_member, NULL, CHECK_ARITY, 2,
	  .helpstr = "(member ELEMENT LIST) -> #f|<list>'" },
	{ "$",       builtin_entry_ref, NULL, CHECK_ARITY, 1,
	  .helpstr = "($ NAME) -> #f|<string>'" },

	{ "$name",           value_name,           NULL, MEMORABLE, 0UL,},
	{ "$input",          value_input,          NULL, MEMORABLE, 0UL,
	  .helpstr = "input file name" },
	{ "$access",         value_access,         NULL, MEMORABLE, 0UL },
	{ "$file",           value_file,           NULL, MEMORABLE, 0UL,
	  .helpstr = "file scope<boolean>" },
	{ "$language",       value_language,       NULL, MEMORABLE, 0UL },
	{ "$implementation", value_implementation, NULL, MEMORABLE, 0UL },
	{ "$line",           value_line,           NULL, MEMORABLE, 0UL },
	{ "$kind",           value_kind,           NULL, MEMORABLE, 0UL },
	{ "$roles",          value_roles,          NULL, MEMORABLE, 0UL,
	  .helpstr = "<list>" },
	{ "$pattern",        value_pattern,        NULL, MEMORABLE, 0UL },
	{ "$inherits",       value_inherits,       NULL, MEMORABLE, 0UL,
	  .helpstr = "<list>" },
	{ "$scope-kind",     value_scope_kind,     NULL, MEMORABLE, 0UL },
	{ "$scope-name",     value_scope_name,     NULL, MEMORABLE, 0UL },
	{ "$end",            value_end,            NULL, MEMORABLE, 0UL },
};

static void define (Code *code)
{
	EsObject *name;

	name = es_symbol_intern (code->name);
	es_symbol_set_data (name, code);
}

static void initialize (void)
{
	int i;

	for (i = 0; i < sizeof(codes)/sizeof(codes [0]); i++)
		define (codes + i);
}

static void reset (void)
{
	int i;
	Code *code;

	for (i = 0; i < sizeof(codes)/sizeof(codes [0]); i++)
	{
		code = codes + i;
		if (code->flags & MEMORABLE)
			code->cache = NULL;
	}
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

static EsObject *eval (EsObject *object, tagEntry *entry);
static EsObject *eval0 (EsObject *object, tagEntry *entry)
{
	if (es_null (object))
		return es_nil;
	else
		return es_object_autounref (
			es_cons(eval (es_car (object), entry),
				eval0 (es_cdr (object), entry))
			);
}
static EsObject *eval (EsObject *object, tagEntry *entry)
{
	EsObject *r;
	Code *code;

	if (es_null (object))
		return es_nil;
	else if (es_symbol_p (object))
	{
		code = es_symbol_get_data (object);

		if (code)
		{
			if (code->cache)
				return code->cache;

			if (code->flags & PURE_PROC)
			  {
				  throw (UNBOUND_VARIABLE,
					 object);
			  }

			r = code->proc (es_nil, entry);
			if (code->flags & MEMORABLE)
				code->cache = r;
			return r;
		}
		else
			throw (UNBOUND_VARIABLE, object);

	}
	else if (es_atom (object))
		return object;
	else
	{
		EsObject *car = es_car (object);
		EsObject *cdr = es_cdr (object);
		int l;

		code = es_symbol_get_data (car);

		if (!code)
			throw (UNBOUND_VARIABLE, car);

		if (code->cache)
			return code->cache;

		if (code->flags & CHECK_ARITY)
		{
			l = length (cdr);
			if (l < code->arity)
				throw (TOO_FEW_ARGUMENTS, car);
			else if (l > code->arity)
				throw (TOO_MANY_ARGUMENTS, car);
		}

		if (! (code->flags & SELF_EVAL))
		{
			EsObject *err;

			cdr = eval0(cdr, entry);

			err = error_included (cdr);
			if (!es_object_equal (err, es_false))
				return err;
		}

		r = code->proc (cdr, entry);
		if (code->flags & MEMORABLE)
			code->cache = r;
		return r;
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
 * Built-ins
 */
static EsObject* builtin_null  (EsObject *args, tagEntry *entry)
{
	return es_null(es_car (args))? es_true: es_false;
}

static EsObject* builtin_and  (EsObject *args, tagEntry *entry)
{
	EsObject *o;

	while (! es_null (args))
	{
		o = es_car (args);
		o = eval (o, entry);
		if (es_object_equal (o, es_false))
			return es_false;
		else if (es_error_p (o))
			return o;
		args = es_cdr (args);
	}

	return es_true;
}

static EsObject* builtin_or  (EsObject *args, tagEntry *entry)
{
	EsObject *o;

	while (! es_null (args))
	{
		o = es_car (args);
		o = eval (o, entry);
		if (! es_object_equal (o, es_false))
			return es_true;
		else if (es_error_p (o))
			return o;
		args = es_cdr (args);
	}

	return es_false;
}

static EsObject* builtin_not  (EsObject *args, tagEntry *entry)
{
	if (es_object_equal (es_car(args), es_false))
		return es_true;
	else if (es_error_p (es_car(args)))
		return es_car(args);
	else
		return es_false;
}

#define DEFINE_OP_WITH_CHECK(N, X, C, E, O)				\
	static EsObject* builtin_##N  (EsObject *args, tagEntry *entry)	\
	{								\
		EsObject *a, *b;					\
									\
		a = es_car (args);					\
		b = es_car (es_cdr (args));				\
		if (!C (a)) throw(E, O);				\
		if (!C (b)) throw(E, O);				\
		if (X)							\
			return es_true;					\
		else							\
			return es_false;				\
	}

static EsObject* builtin_eq  (EsObject *args, tagEntry *entry)
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

static EsObject* builtin_prefix (EsObject* args, tagEntry *entry)
{
	EsObject *target = es_car (args);
	EsObject *prefix = es_car (es_cdr (args));
	const char *ts;
	const char *ps;
	size_t tl;
	size_t pl;

	if ((! es_string_p (target))
	    || (! es_string_p (prefix)))
		throw (WRONG_TYPE_ARGUMENT,
		       es_symbol_intern ("prefix?"));

	ts = es_string_get (target);
	ps = es_string_get (prefix);
	tl = strlen (ts);
	pl = strlen (ps);
	if (tl < pl)
		return es_false;
	return (strncmp (ts, ps, pl) == 0)? es_true: es_false;
}

static EsObject* builtin_suffix (EsObject* args, tagEntry *entry)
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
		throw (WRONG_TYPE_ARGUMENT,
		       es_symbol_intern ("suffix?"));

	ts = es_string_get (target);
	ss = es_string_get (suffix);
	tl = strlen (ts);
	sl = strlen (ss);
	if (tl < sl)
		return es_false;
	d = tl - sl;
	return (strcmp (ts + d, ss) == 0)? es_true: es_false;
}

static EsObject* builtin_substr (EsObject* args, tagEntry *entry)
{
	EsObject *target = es_car (args);
	EsObject *substr = es_car (es_cdr (args));
	const char *ts;
	const char *ss;

	if ((! es_string_p (target))
	    || (! es_string_p (substr)))
		throw (WRONG_TYPE_ARGUMENT, es_symbol_intern("substr?"));
	ts = es_string_get (target);
	ss = es_string_get (substr);

	return strstr(ts, ss) == NULL? es_false: es_true;
}

static EsObject* builtin_member (EsObject *args, tagEntry *entry)
{
	EsObject *elt  = es_car (args);
	EsObject *lst = es_car (es_cdr (args));

	if (! es_list_p (lst))
		throw (WRONG_TYPE_ARGUMENT, es_symbol_intern ("member"));

	while (!es_null (lst))
	{
		if (es_object_equal (elt, es_car (lst)))
			return lst;
		lst = es_cdr (lst);
	}

	return es_false;
}

/*
 * Value
 */
static const char*entry_xget (tagEntry *entry, const char* name)
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

static EsObject* entry_xget_string (tagEntry *entry, const char* name)
{
	const char* value = entry_xget (entry, name);
	if (value)
		return es_object_autounref (es_string_new (value));
	else
		return es_false;
}

static EsObject* value_xget_csvlist (tagEntry *entry, const char* field)
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

static EsObject* builtin_entry_ref (EsObject *args, tagEntry *entry)
{
	EsObject *key = es_car(args);

	if (es_error_p (key))
		return key;
	else if (! es_string_p (key))
		throw (WRONG_TYPE_ARGUMENT,
		       es_symbol_intern ("$"));
	else
		return entry_xget_string (entry, es_string_get (key));
}


static EsObject* value_name (EsObject *args, tagEntry *entry)
{
	return es_object_autounref (es_string_new (entry->name));
}

static EsObject* value_input (EsObject *args, tagEntry *entry)
{
	return es_object_autounref (es_string_new (entry->file));
}

static EsObject* value_access (EsObject *args, tagEntry *entry)
{
	return entry_xget_string (entry, "access");
}

static EsObject* value_file (EsObject *args, tagEntry *entry)
{
	return entry->fileScope? es_true: es_false;
}

static EsObject* value_language (EsObject *args, tagEntry *entry)
{
	return entry_xget_string (entry, "language");
}

static EsObject* value_implementation (EsObject *args, tagEntry *entry)
{
	return entry_xget_string (entry, "implementation");
}

static EsObject* value_line (EsObject *args, tagEntry *entry)
{
	unsigned long ln = entry->address.lineNumber;

	if (ln == 0)
		return es_false;
	else
		return es_object_autounref (es_integer_new (ln));
}

static EsObject* value_end (EsObject *args, tagEntry *entry)
{
	const char *end_str = entry_xget(entry, "end");
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

static EsObject* value_kind (EsObject *args, tagEntry *entry)
{
	const char* kind;
	kind = entry->kind;

	if (kind)
		return es_object_autounref (es_string_new (entry->kind));
	else
		return es_false;
}

static EsObject* value_roles (EsObject *args, tagEntry *entry)
{
	return value_xget_csvlist(entry, "roles");
}

static EsObject* value_pattern (EsObject *args, tagEntry *entry)
{
	const char *pattern = entry->address.pattern;

	if (pattern == NULL)
		return es_false;
	else
		return es_object_autounref (es_string_new (pattern));
}

static EsObject* value_inherits (EsObject *args, tagEntry *entry)
{
	return value_xget_csvlist (entry, "inherits");
}

static EsObject* value_scope_kind (EsObject *args, tagEntry *entry)
{
	const char* scope = entry_xget (entry, "scope");
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

static EsObject* value_scope_name (EsObject *args, tagEntry *entry)
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
	static int initialized;

	if (!initialized)
	{
		initialize ();
		initialized = 1;
	}

	code.es = es_object_ref (exp);
	return &code;
}

enum QRESULT q_is_acceptable  (QCode *code, tagEntry *entry)
{
	EsObject *r;
	int i;

	es_autounref_pool_push ();
	r = eval (code->es, entry);
	if (es_object_equal (r, es_false))
		i = Q_REJECT;
	else if (es_error_p (r))
	{
		MIO  *mioerr = mio_new_fp (stderr, NULL);;

		fprintf(stderr, "GOT ERROR in QUALIFYING: %s: ",
			 es_error_name (r));
		es_print(es_error_get_object(r), mioerr);
		putc('\n', stderr);
		i = Q_ERROR;

		mio_free(mioerr);
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
	int i;

	for (i = 0; i < sizeof (codes) / sizeof (codes[0]); i ++)
	{
		const char* hs = codes[i].helpstr;
		fprintf(fp, "%15s: %s\n", codes[i].name, hs? hs: "");
	}
}
