/*
  Copyright (c) 2009 Masatake YAMATO

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
  THE SOFTWARE. */

#if defined (HAVE_CONFIG_H)
# include <config.h>
#endif

#include "es.h"


#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <limits.h>

#include <regex.h>

static int es_debug = 0;

typedef struct _EsInteger EsInteger;
typedef struct _EsReal EsReal;
typedef struct _EsBoolean EsBoolean;
typedef struct _EsString EsString;
typedef struct _EsSingleton EsSingleton;
typedef struct _EsSymbol EsSymbol;
typedef struct _EsError EsError;
typedef struct _EsCons EsCons;
typedef struct _EsRegex EsRegex;
typedef struct _EsPointer EsPointer;

struct _EsObject
{
	EsType  type;
	union
	{
		int     ref_count;
		EsSingleton* next;
	};
};

struct _EsInteger
{
	EsObject base;
	int      value;
};

struct _EsReal
{
	EsObject base;
	double   value;
};

struct _EsBoolean
{
	EsObject base;
	int      value;
};

struct _EsString
{
	EsObject    base;
	char*       value;
};

struct _EsSingleton
{
	EsObject     base;
	char*        quark;
};

struct _EsSymbol
{
	EsSingleton base;
	void       *data;
};

struct _EsError
{
	EsSingleton base;
	EsObject   *object;
};

struct _EsCons
{
	EsObject base;
	EsObject* car;
	EsObject* cdr;
};

struct _EsRegex
{
	EsObject base;
	regex_t *code;
	char* literal;
	int   case_insensitive;
};

struct _EsPointer
{
	EsObject base;
	void *ptr;
	char  fat [];
};

enum EsObjectFlag
{
	ES_OBJECT_FLAG_ATOM = 1 << 0,
};

typedef struct _EsObjectClass EsObjectClass;
struct _EsObjectClass
{
	size_t         size;
	void           (* free)  (EsObject* object);
	int            (* equal) (const EsObject* self, const EsObject* other);
	void           (* print) (const EsObject* object, MIO* fp);
	unsigned       flags;
	EsSingleton  **obarray;
	const char*    name;
};


static void es_nil_free(EsObject* object);
static int  es_nil_equal(const EsObject* self, const EsObject* other);
static void es_nil_print(const EsObject* object, MIO* fp);

static void es_integer_free(EsObject* object);
static int  es_integer_equal(const EsObject* self, const EsObject* other);
static void es_integer_print(const EsObject* object, MIO* fp);

static void es_real_free(EsObject* object);
static int  es_real_equal(const EsObject* self, const EsObject* other);
static void es_real_print(const EsObject* object, MIO* fp);

static void es_boolean_free(EsObject* object);
static int  es_boolean_equal(const EsObject* self, const EsObject* other);
static void es_boolean_print(const EsObject* object, MIO* fp);

static void es_string_free(EsObject* object);
static int  es_string_equal(const EsObject* self, const EsObject* other);
static void es_string_print(const EsObject* self, MIO* fp);

static void es_symbol_free(EsObject* object);
static int  es_symbol_equal(const EsObject* self, const EsObject* other);
static void es_symbol_print(const EsObject* object, MIO* fp);

static void es_cons_free(EsObject* object);
static int  es_cons_equal(const EsObject* self, const EsObject* other);
static void es_cons_print(const EsObject* object, MIO* fp);

static void es_regex_free(EsObject* object);
static int  es_regex_equal(const EsObject* self, const EsObject* other);
static void es_regex_print(const EsObject* object, MIO* fp);

static void es_error_free(EsObject* object);
static int  es_error_equal(const EsObject* self, const EsObject* other);
static void es_error_print(const EsObject* object, MIO* fp);

static void es_pointer_free(EsObject* object);
static int  es_pointer_equal(const EsObject* self, const EsObject* other);
static void es_pointer_print(const EsObject* object, MIO* fp);

static EsSingleton* es_obarray_intern(EsType type, const char* name);
static const char*  es_singleton_get   (EsSingleton *singleton);
static unsigned int hash(const char* keyarg);
#define OBARRAY_SIZE    83
static EsSingleton  *symbol_obarray[OBARRAY_SIZE];
static EsSingleton  *error_obarray [OBARRAY_SIZE];

static EsObjectClass es_nil_class = {
	.size    = 0,
	.free    = es_nil_free,
	.equal   = es_nil_equal,
	.print   = es_nil_print,
	.flags   = ES_OBJECT_FLAG_ATOM,
	.obarray = NULL,
	.name    = "nil",
};

static EsObjectClass es_integer_class = {
	.size    = sizeof(EsInteger),
	.free    = es_integer_free,
	.equal   = es_integer_equal,
	.print   = es_integer_print,
	.flags   = ES_OBJECT_FLAG_ATOM,
	.obarray = NULL,
	.name    = "integer",
};

static EsObjectClass es_real_class = {
	.size    = sizeof(EsReal),
	.free    = es_real_free,
	.equal   = es_real_equal,
	.print   = es_real_print,
	.flags   = ES_OBJECT_FLAG_ATOM,
	.obarray = NULL,
	.name    = "real",
};

static EsObjectClass es_boolean_class = {
	.size    = sizeof(EsBoolean),
	.free    = es_boolean_free,
	.equal   = es_boolean_equal,
	.print   = es_boolean_print,
	.flags   = ES_OBJECT_FLAG_ATOM,
	.obarray = (void*)1,
	.name    = "boolean",
};

static EsObjectClass es_symbol_class = {
	.size    = sizeof(EsSymbol),
	.free    = es_symbol_free,
	.equal   = es_symbol_equal,
	.print   = es_symbol_print,
	.flags   = ES_OBJECT_FLAG_ATOM,
	.obarray = symbol_obarray,
	.name    = "symbol",
};

static EsObjectClass es_string_class = {
	.size    = sizeof(EsString),
	.free    = es_string_free,
	.equal   = es_string_equal,
	.print   = es_string_print,
	.flags   = ES_OBJECT_FLAG_ATOM,
	.obarray = NULL,
	.name    = "string",
};

static EsObjectClass es_cons_class = {
	.size    = sizeof(EsCons),
	.free    = es_cons_free,
	.equal   = es_cons_equal,
	.print   = es_cons_print,
	.flags   = 0,
	.obarray = NULL,
	.name    = "cons",
};

static EsObjectClass es_regex_class = {
	.size    = sizeof(EsRegex),
	.free    = es_regex_free,
	.equal   = es_regex_equal,
	.print   = es_regex_print,
	.flags   = ES_OBJECT_FLAG_ATOM,
	.obarray = NULL,
	.name    = "regex",
};

static EsObjectClass es_error_class = {
	.size    = sizeof(EsError),
	.free    = es_error_free,
	.equal   = es_error_equal,
	.print   = es_error_print,
	.flags   = ES_OBJECT_FLAG_ATOM,
	.obarray = error_obarray,
	.name    = "error",
};


#define ES_TYPE_CLASS_MAX 32
static int classes_count = ES_TYPE_FOREIGNER_START;
static EsObjectClass *classes[ES_TYPE_CLASS_MAX] = {
	[ES_TYPE_NIL]     = &es_nil_class,
	[ES_TYPE_INTEGER] = &es_integer_class,
	[ES_TYPE_REAL]    = &es_real_class,
	[ES_TYPE_BOOLEAN] = &es_boolean_class,
	[ES_TYPE_SYMBOL]  = &es_symbol_class,
	[ES_TYPE_STRING]  = &es_string_class,
	[ES_TYPE_CONS]    = &es_cons_class,
	[ES_TYPE_REGEX]   = &es_regex_class,
	[ES_TYPE_ERROR]   = &es_error_class,
};



static MIO *mio_stdout (void)
{
	static MIO  *out;

	if (out == NULL)
		out = mio_new_fp (stdout, NULL);

	return out;
}

static MIO *mio_stdin (void)
{
	static MIO  *out;

	if (out == NULL)
		out = mio_new_fp (stdin, NULL);

	return out;
}

static MIO *mio_stderr (void)
{
	static MIO  *out;

	if (out == NULL)
		out = mio_new_fp (stderr, NULL);

	return out;
}



static EsObjectClass*
class_of(const EsObject* object)
{
	return (classes[es_object_get_type(object)]);
}

static EsObject*
es_object_new(EsType type)
{
	EsObject* r;


	r = calloc(1, (classes[type])->size);
	if (r == NULL)
		return ES_ERROR_MEMORY;
	r->type = type;
	r->ref_count = 1;

	if (es_debug)
		mio_printf(mio_stderr(), ";; new{%s}: 0x%p\n",
				   (classes[type])->name,
				   r);

	return r;
}

static void
es_object_free(EsObject* object)
{
	memset(object, 0, class_of(object)->size);
	free(object);
}

static int
es_object_type_p(const EsObject* object, EsType type)
{
	return es_object_get_type(object) == type;
}

const char* es_type_get_name        (int t)
{
	return (classes[t]->name);
}

EsType
es_object_get_type      (const EsObject*      object)
{
	return object? object->type: ES_TYPE_NIL;
}

EsObject*
es_object_ref           (EsObject*       object)
{
	if (object)
    {
		if (class_of(object)->obarray)
			return object;

		if (es_debug)
			mio_printf(mio_stderr(), ";; ref{%s}: [%d]0x%p\n",
					   class_of(object)->name,
					   object->ref_count,
					   object);
		object->ref_count++;
    }
	return object;
}

void
es_object_unref         (EsObject*       object)
{

	if (object)
    {
		if (class_of(object)->obarray)
			return;

		if (object->ref_count == 0)
			if ((1 || es_debug))
			{
				mio_printf(mio_stderr(), "*** ref_count < 0: 0x%p ***\n", object);
				mio_printf(mio_stderr(), "*** BOOSTING while(1). ***\n");
				while (1);
			}

		object->ref_count--;
		if (es_debug)
			mio_printf(mio_stderr(), ";; unref{%s}: [%d]0x%p\n",
					   class_of(object)->name,
					   object->ref_count, object);
		if (object->ref_count == 0)
		{
			if (es_debug)
				mio_printf(mio_stderr(), ";; free{%s}: 0x%p\n",
						   class_of(object)->name,
						   object);
			class_of(object)->free(object);
		}
    }
}

void
es_object_unref_batch (EsObject*       array[],
					   unsigned int    count)
{
	unsigned int i;

	for (i = 0; i < count; i++)
    {
		es_object_unref(array[i]);
		array[i] = es_nil;
    }
}

int
es_object_equal         (const EsObject* self,
						 const EsObject* other)
{
	if (self == other)
		return 1;

	return class_of(self)->equal(self, other);
}


int
es_atom         (const EsObject* object)
{
	return class_of(object)->flags  & ES_OBJECT_FLAG_ATOM;
}


/*
 * Nil
 */
int
es_null(const EsObject* object)
{
	return (object == es_nil)? 1: 0;
}

static void
es_nil_free(EsObject* object)
{
	/* DO NOTHING */
}

static int
es_nil_equal(const EsObject* self, const EsObject* other)
{
	return es_null(other);
}

static void
es_nil_print(const EsObject* object, MIO* fp)
{
	mio_puts(fp, "()");
}

/*
 * Integer
 */
EsObject*
es_integer_new (int                value)
{
	EsObject* r;

	r = es_object_new(ES_TYPE_INTEGER);
	((EsInteger*)r)->value = value;
	return r;
}

int
es_integer_p   (const EsObject*   object)
{
	return es_object_type_p(object, ES_TYPE_INTEGER);
}

int
es_integer_get (const EsObject*   object)
{
	if (es_integer_p(object))
		return ((EsInteger *)object)->value;
	else
    {
		mio_printf(mio_stderr(), ";; es_integer_get, Wrong type argument: ");
		es_print(object, mio_stderr());
		mio_putc(mio_stderr(), '\n');
		return -1;
    }
}

static void
es_integer_free(EsObject* object)
{
	es_object_free(object);
}

static int
es_integer_equal(const EsObject* self, const EsObject* other)
{
	return ((es_integer_p(other))
			&& (es_integer_get(self) == es_integer_get(other)))? 1: 0;
}

static void
es_integer_print(const EsObject* object, MIO* fp)
{
	mio_printf(fp, "%d", es_integer_get(object));
}


/*
 * Real
 */
EsObject*
es_real_new (double                value)
{
	EsObject* r;

	r = es_object_new(ES_TYPE_REAL);
	((EsReal*)r)->value = value;
	return r;
}

int
es_real_p   (const EsObject*   object)
{
	return es_object_type_p(object, ES_TYPE_REAL);
}

double
es_real_get (const EsObject*   object)
{
	if (es_real_p(object))
		return ((EsReal *)object)->value;
	else
    {
		mio_printf(mio_stderr(), ";; es_real_get, Wrong type argument: ");
		es_print(object, mio_stderr());
		mio_putc(mio_stderr(), '\n');
		return -1;
    }
}

static void
es_real_free(EsObject* object)
{
	es_object_free(object);
}

static int
es_real_equal(const EsObject* self, const EsObject* other)
{
	return ((es_real_p(other))
			/* TODO: Too restricted? */
			&& (es_real_get(self) == es_real_get(other)))? 1: 0;
}

static void
es_real_print(const EsObject* object, MIO* fp)
{
	mio_printf(fp, "%f", es_real_get(object));
}

/*
 * Use Integer as Real
 */
int
es_number_p    (const EsObject*   object)
{
	return (es_integer_p(object) || es_real_p(object))? 1: 0;
}

double
es_number_get  (const EsObject*   object)
{
	double r;

	switch(es_object_get_type(object))
    {
    case ES_TYPE_INTEGER:
		r = (double)es_integer_get(object);
		break;
    case ES_TYPE_REAL:
		r = es_real_get(object);
		break;
    default:
		mio_printf(mio_stderr(), ";; es_number_get, Wrong type argument: ");
		es_print(object, mio_stderr());
		mio_putc(mio_stderr(), '\n');
		r = -1.0;
		break;
    }
	return r;
}


/*
 * Boolean
 */
EsObject*
es_boolean_new (int                value)
{
	static EsObject* T;
	static EsObject* F;

	if (!T)
    {
		T = es_object_new(ES_TYPE_BOOLEAN);
		((EsBoolean*)T)->value = 1;
    }
	if (!F)
    {
		F = es_object_new(ES_TYPE_BOOLEAN);
		((EsBoolean*)F)->value = 0;
    }

	return value? T: F;
}

int
es_boolean_p   (const EsObject*   object)
{
	return es_object_type_p(object, ES_TYPE_BOOLEAN);
}

int
es_boolean_get (const EsObject*   object)
{
	if (es_boolean_p(object))
		return ((EsBoolean *)object)->value;
	else
    {
		mio_printf(mio_stderr(), ";; es_boolean_get, Wrong type argument: ");
		es_print(object, mio_stderr());
		mio_putc(mio_stderr(), '\n');
		return -1;
    }
}

static void
es_boolean_free(EsObject* object)
{
	/* Do nothing */
}

static int
es_boolean_equal(const EsObject* self, const EsObject* other)
{
	return (self == other)? 1: 0;
}

static void
es_boolean_print(const EsObject* object, MIO* fp)
{
	mio_printf(fp, "#%c", (es_boolean_get(object)? 't': 'f'));
}

/*
 * Singleton
 */
static EsSingleton*
es_obarray_intern(EsType type, const char* name)
{
	unsigned int hv;
	EsSingleton** obarray;
	EsSingleton* s;
	EsSingleton* tmp;


	obarray = (classes[type])->obarray;
	if (!obarray)
		return NULL;

	hv = hash(name);
	tmp = obarray[hv];

	s = NULL;
	while (tmp)
    {
		if (!strcmp(tmp->quark, name))
		{
			s = tmp;
			break;
		}
		else
			tmp = ((EsObject *)tmp)->next;
    }

	if (!s)
    {
		s = (EsSingleton*) es_object_new(type);
		s->quark = strdup(name);
		tmp = obarray[hv];
		obarray[hv] = s;
		((EsObject *)s)->next = tmp;
    }

	return s;

}

static const char*
es_singleton_get   (EsSingleton *singleton)
{
	return singleton->quark;
}


/*
 * Symbol
 */
static unsigned char get_char_class(int c);


EsObject*
es_symbol_intern  (const char*       name)
{
	EsSingleton* r;

	r = es_obarray_intern(ES_TYPE_SYMBOL, name);
	return (EsObject*)r;
}

int
es_symbol_p    (const EsObject*   object)
{
	return es_object_type_p(object, ES_TYPE_SYMBOL);
}

const char*
es_symbol_get  (const EsObject*   object)
{
	if (es_symbol_p(object))
		return es_singleton_get((EsSingleton*)object);
	else
    {
		mio_printf(mio_stderr(), ";; es_symbol_get, Wrong type argument: ");
		es_print(object, mio_stderr());
		mio_putc(mio_stderr(), '\n');
		return NULL;
    }
}

void*        es_symbol_set_data (const EsObject*   object, void *data)
{
	if (es_symbol_p(object))
    {
		void* old_data;

		old_data = ((EsSymbol*)object)->data;
		((EsSymbol*)object)->data = data;
		return  old_data;
    }
	else
    {
		mio_printf(mio_stderr(), ";; es_symbol_set_data, Wrong type argument: ");
		es_print(object, mio_stderr());
		mio_putc(mio_stderr(), '\n');
		return NULL;
    }
}

void*        es_symbol_get_data (const EsObject*   object)
{
	if (es_symbol_p(object))
		return ((EsSymbol*)object)->data;
	else
    {
		mio_printf(mio_stderr(), ";; es_symbol_get_data, Wrong type argument: ");
		es_print(object, mio_stderr());
		mio_putc(mio_stderr(), '\n');
		return NULL;
    }
}

static void
es_symbol_free(EsObject* object)
{
	/* DO NOTHING */
}

static int
es_symbol_equal(const EsObject* self, const EsObject* other)
{
	return (self == other)? 1: 0;
}

static void
es_symbol_print(const EsObject* object, MIO* fp)
{
	const char* string;
	size_t len;
	char c;
	unsigned char cc;
	unsigned char mask;
	int needs_bar;
	int i;

	string = es_symbol_get(object);
	if (!string)
		return;

	len = strlen(string);
	if (len == 0)
		needs_bar = 1;

	c = string[0];
	cc = get_char_class(c);
	mask = 0x1;
	needs_bar = (cc & mask)? 1: 0;
	if (!needs_bar)
    {
		/* 0 => 1? */
		mask = 0x2;
		for (i = 0; i< len; i++)
		{
			c = string[i];
			cc = get_char_class(c);
			needs_bar = (cc & mask)? 1: 0;
			if (needs_bar)
				break;
		}

    }

	if (needs_bar)
		mio_printf(fp, "|");

	for (i = 0; i < len; i++)
    {
		c = string[i];
		if (c == '\\' || c == '|')
			mio_printf(fp, "\\");
		mio_printf(fp, "%c", c);
    }

	if (needs_bar)
		mio_printf(fp, "|");
}


/*
 * symbol.c - symbol implementation
 *
 *   Copyright (c) 2000-2007  Shiro Kawai  <shiro@acm.org>
 *
 *   Redistribution and use in source and binary forms, with or without
 *   modification, are permitted provided that the following conditions
 *   are met:
 *
 *   1. Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *
 *   2. Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions and the following disclaimer in the
 *      documentation and/or other materials provided with the distribution.
 *
 *   3. Neither the name of the authors nor the names of its contributors
 *      may be used to endorse or promote products derived from this
 *      software without specific prior written permission.
 *
 *   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 *   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 *   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 *   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 *   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 *   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
 *   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 *   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 *   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 *   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 *   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 *  $Id: symbol.c,v 1.40 2007/09/13 12:30:28 shirok Exp $
 */
/* table of special chars.
   bit 0: bad char for symbol to begin with
   bit 1: bad char for symbol to contain
   bit 2: bad char for symbol, and should be written as \nnn
   bit 3: bad char for symbol, and should be written as \c
   bit 4: may be escaped when case fold mode
*/
static char symbol_special[] = {
	/* NUL .... */
	7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
	/* .... */
	7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
	/*    !  "  #  $  %  &  '  (  )  *  +  ,  -  .  /  */
	3, 0, 3, 3, 0, 0, 0, 3, 3, 3, 0, 1, 3, 1, 1, 0,
	/* 0  1  2  3  4  5  6  7  8  9  :  ;  <  =  >  ?  */
	1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 3, 0, 0, 0, 0,
	/* @  A  B  C  D  E  F  G  H  I  J  K  L  M  N  O  */
	1, 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,
	/* P  Q  R  S  T  U  V  W  X  Y  Z  [  \  ]  ^  _  */
	16,16,16,16,16,16,16,16,16,16,16,3, 11,3, 0, 0,
	/* `  a  b  c  d  e  f  g  h  i  j  k  l  m  n  o  */
	3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	/* p  q  r  s  t  u  v  w  x  y  z  {  |  }  ~  ^? */
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 11,3, 0, 7
};

/* symbol_special[':'] was 1 in the symbol.c of Gauche.
   However I modified it to 0.
   Because a keyword is a just a symbol started from `:'
   in Es. */
static unsigned char
get_char_class(int c)
{
	return (c < 0)? 0xff: symbol_special[c];
}

/*
 * String
 */
EsObject*
es_string_new  (const char*        value)
{
	EsObject* r;

	r = es_object_new(ES_TYPE_STRING);
	((EsString*)r)->value = strdup(value);
	return r;
}

EsObject*
es_string_newL (const char* value, size_t len)
{
	EsObject* r;

	r = es_object_new(ES_TYPE_STRING);
	if (es_error_p (r))
		return r;

	void * v = malloc (len + 1);
	if (v == NULL)
	{
		((EsString*)r)->value = NULL;
		es_object_free (r);
		return ES_ERROR_MEMORY;
	}
	memcpy (v, value, len);
	((char *)v)[len] = '\0';
	((EsString*)r)->value = v;
	return r;
}

int
es_string_p    (const EsObject*   object)
{
	return es_object_type_p(object, ES_TYPE_STRING);
}

const char*
es_string_get  (const EsObject*   object)
{
	if (es_string_p(object))
		return ((EsString *)object)->value;
	else
    {
		mio_printf(mio_stderr(), ";; es_string_get, Wrong type argument: ");
		es_print(object, mio_stderr());
		mio_putc(mio_stderr(), '\n');
		return NULL;
    }
}

static void
es_string_free(EsObject* object)
{
	if (es_string_p(object))
    {
		free(((EsString*) object)->value);
		((EsString*) object)->value = NULL;
		es_object_free(object);
    }
	else
    {
		mio_printf(mio_stderr(), ";; Internal error: \n");
		mio_printf(mio_stderr(), ";;es_string_free, Wrong type argument: ");
		es_print(object, mio_stderr());
		mio_putc(mio_stderr(), '\n');
    }
}


static int
es_string_equal(const EsObject* self, const EsObject* other)
{
	if (es_string_p(other))
    {
		return (!strcmp(es_string_get(self), es_string_get(other)));
    }
	else
		return 0;
}

static void
es_string_print(const EsObject* object, MIO* fp)
{
	const char* string;
	char  c;
	size_t len;
	int      i;


	string = es_string_get(object);
	len    = strlen(string);

	mio_printf(fp, "\"");

	for (i = 0; i < len; i++)
    {
		char cc;

		c = string[i];
		switch (c)
		{
		case '\n':
			cc = 'n';
			break;
		case '\t':
			cc = 't';
			break;
		case '\r':
			cc = 'r';
			break;
		case '\f':
			cc = 'f';
			break;
		default:
			cc = 0;
			break;
		}
		if (cc)
		{
			mio_printf(fp, "\\");
			mio_printf(fp, "%c", cc);
			continue;
		}

		if (c == '\\' || c == '"')
			mio_printf(fp, "\\");
		mio_printf(fp, "%c", c);
    }

	mio_printf(fp, "\"");
}

/*
 * Cons
 */
EsObject*
es_cons        (EsObject* car, EsObject* cdr)
{
	EsObject* r;

	if (!es_list_p(cdr))
    {
		/* This library doesn't permit to dotted list. */
		return es_nil;
    }


	r = es_object_new(ES_TYPE_CONS);
	if (es_error_p (r))
		return r;
	if (es_debug)
    {
		mio_printf(mio_stderr(), ";; cons[0x%p] = (0x%p . 0x%p)\n", r, car, cdr);
		/* es_print(car, mio_stderr());
		   mio_putc('\n', mio_stderr());
		   es_print(cdr, mio_stderr());
		   mio_putc('\n', mio_stderr()); */
    }
	((EsCons*)r)->car = es_object_ref(car);
	((EsCons*)r)->cdr = es_object_ref(cdr);

	return r;
}

int
es_cons_p      (const EsObject* object)
{
	return es_object_type_p(object, ES_TYPE_CONS);
}

int
es_list_p      (const EsObject* object)
{
	EsType t;

	t = es_object_get_type(object);
	return (t == ES_TYPE_NIL || t == ES_TYPE_CONS);
}

EsObject*
es_car         (const EsObject* object)
{
	if (es_cons_p(object))
		return ((EsCons*)object)->car;
	else if (es_null(object))
		return es_nil;
	else
    {
		mio_printf(mio_stderr(), ";; es_car, Wrong type argument: ");
		es_print(object, mio_stderr());
		mio_putc(mio_stderr(), '\n');
		return es_nil;
    }
}

EsObject*
es_cdr         (const EsObject* object)
{
	if (es_cons_p(object))
		return ((EsCons*)object)->cdr;
	else if (es_null(object))
		return es_nil;
	else
    {
		mio_printf(mio_stderr(), ";; es_cdr, Wrong type argument: ");
		es_print(object, mio_stderr());
		mio_putc(mio_stderr(), '\n');
		return es_nil;
    }
}

static void
es_cons_free(EsObject* object)
{
	EsCons* cons;

	if (es_cons_p(object))
    {
		cons = ((EsCons*)object);

		es_object_unref(cons->car);
		cons->car = NULL;

		es_object_unref(cons->cdr);
		cons->cdr = NULL;
		es_object_free(object);
    }
	else if (es_null(object))
		;				/* DO NOTHING */
	else
    {
		mio_printf(mio_stderr(), ";; Internal error: \n");
		mio_printf(mio_stderr(), ";; es_cons_free, Wrong type argument: ");
		es_print(object, mio_stderr());
		mio_putc(mio_stderr(), '\n');
    }
}

static int
es_cons_equal(const EsObject* self, const EsObject* other)
{
	return (es_null(other)
			|| (!es_cons_p(other))
			|| (!es_object_equal(es_car(self), es_car(other)))
			|| (!es_object_equal(es_cdr(self), es_cdr(other))))
		? 0
		: 1;
}

static void
es_cons_print(const EsObject* object, MIO* fp)
{
	EsObject* car;
	EsObject* cdr;

	mio_printf(fp, "(");
	while(!es_null(object))
    {
		car = es_car(object);
		cdr = es_cdr(object);

		es_print(car, fp);
		if (es_cons_p(cdr))
			mio_putc(fp, ' ');
		else if (!es_null(cdr))
		{
			mio_printf(mio_stderr(), ";; es_cons_print, dotted list given: ");
			mio_putc(mio_stderr(), '\n');
		}
		object = cdr;
    }
	mio_printf(fp, ")");
}

static EsObject* es_cons_reverse_rec(EsObject* cdr,
									 EsObject* car,
									 EsObject* gathered);

static EsObject*
es_cons_reverse  (EsObject*        cons)
{
	/* g_return_val_if_fail (es_null(cons) || es_cons_p(cons), es_nil);
	   g_return_val_if_fail (!es_cproc_dotted_p(cons), es_nil); */

	if (es_null(cons))
		return es_nil;
	else
		return es_cons_reverse_rec(es_cdr(cons),
								   es_car(cons),
								   es_nil);
}

EsObject*
es_reverse  (EsObject* cons)
{
	return es_cons_reverse(cons);
}

static EsObject*
es_cons_reverse_rec(EsObject* cdr,
					EsObject* car,
					EsObject* gathered)
{
	EsObject* cons;
	EsObject* o;

	cons = es_cons(car, o = gathered);
	es_object_unref(o);

	if (es_null(cdr))
		return cons;
	else
		return es_cons_reverse_rec(es_cdr(cdr),
								   es_car(cdr),
								   cons);
}

/*
 * Regex
 */
EsObject*
es_regex_compile   (const char* pattern_literal, int case_insensitive)
{
	EsObject* r;
	regex_t *code;
	int err;
	int flag = REG_EXTENDED | REG_NEWLINE
		| (case_insensitive? REG_ICASE: 0);

	code = malloc(sizeof(regex_t));
	if (!code)
		return ES_ERROR_MEMORY;

	err = regcomp(code, pattern_literal,
				  flag);
	if (err)
	{
#if 0
/* TODO: This should be reported to caller. */
		char errmsg [256];
		regerror (err, code, errmsg, 256);
#endif
		regfree (code);
		free (code);
		return ES_ERROR_REGEX;
	}

	r = es_object_new(ES_TYPE_REGEX);
	((EsRegex*)r)->code = code;
	((EsRegex*)r)->literal = strdup(pattern_literal);
	if (!((EsRegex*)r)->literal)
	{
		regfree(((EsRegex*)r)->code);
		free(((EsRegex*)r)->code);
		es_object_free(r);
		return ES_ERROR_MEMORY;
	}
	((EsRegex*)r)->case_insensitive = case_insensitive;
	return r;
}

int
es_regex_p   (const EsObject*   object)
{
	return es_object_type_p(object, ES_TYPE_REGEX);
}

static void es_regex_free(EsObject* object)
{
	free(((EsRegex*)object)->literal);
	regfree(((EsRegex*)object)->code);
	free(((EsRegex*)object)->code);
	es_object_free(object);
}

static int
es_regex_equal(const EsObject* self, const EsObject* other)
{
	return (es_regex_p (other)
			&& (strcmp (((EsRegex*)self)->literal,
						((EsRegex*)other)->literal) == 0)
			&& (((EsRegex*)self)->case_insensitive ==
				((EsRegex*)other)->case_insensitive));
}

static void
es_regex_print(const EsObject* object, MIO* fp)
{
	mio_puts(fp, "#/");
	const char *s = ((EsRegex*)object)->literal;
	while (*s)
	{
		if (*s == '/')
			mio_putc(fp, '\\');
		mio_putc(fp, *s);
		s++;
	}
	mio_putc(fp, '/');
	if (((EsRegex*)object)->case_insensitive)
		mio_putc(fp, 'i');
}

EsObject*
es_regex_exec    (const EsObject* regex,
				  const EsObject* str)
{
	return regexec (((EsRegex*)regex)->code, es_string_get (str),
					0, NULL, 0)? es_false: es_true;
}

/*
 * Error
 */
EsObject*
es_error_intern  (const char*       name)
{
	EsSingleton* r;

	r = es_obarray_intern(ES_TYPE_ERROR, name);
	return (EsObject*)r;
}

int
es_error_p    (const EsObject*   object)
{
	return es_object_type_p(object, ES_TYPE_ERROR);
}

const char*
es_error_name  (const EsObject*   object)
{
	if (es_error_p(object))
		return es_singleton_get((EsSingleton *)object);
	else
    {
		mio_printf(mio_stderr(), ";; es_error_name, Wrong type argument: ");
		es_print(object, mio_stderr());
		mio_putc(mio_stderr(), '\n');
		return NULL;
    }
}

EsObject*
es_error_set_object (EsObject*   error, EsObject*   object)
{
	EsError *e = (EsError *)error;
	if (e->object)
		es_object_unref (e->object);

	e->object = es_object_ref (object);
	return error;
}

EsObject*
es_error_get_object (const EsObject*   error)
{
	EsError *e = (EsError *)error;
	return e->object;
}

static void
es_error_free(EsObject* object)
{
	/* DO NOTHING */
}

static int
es_error_equal(const EsObject* self, const EsObject* other)
{
	return (self == other)? 1: 0;
}

static void
es_error_print(const EsObject* object, MIO* fp)
{
	const char* string;
	EsError *e = (EsError *)object;

	string = es_error_name(object);
	mio_printf(fp, "#%s:", string);
	es_print (e->object, fp);
}

/*
 * Foreigner
 */
typedef struct _EsPointerClass EsPointerClass;
struct _EsPointerClass
{
	EsObjectClass base;

	size_t fat_size;
	EsObject *(* init_fat) (void *fat, void * ptr, void *extra);

	void (* free_ptr) (void *);
	int  (* equal_ptr) (const void*, const void*);
	void (* print_ptr) (const void*, MIO *);


	void (* free_fatptr) (void *, void *);
	int  (* equal_fatptr) (const void*, const void*,
						   const void*, const void*);
	void (* print_fatptr) (const void*, const void*, MIO *);
};

static EsType
es_type_define_pointer_full(const char *name,
							size_t fat_size,
							EsObject *(* initfat_fn) (void *fat, void * ptr, void *extra),
							void (* freefn) (void *),
							int  (* equalfn) (const void*, const void*),
							void (* printfn) (const void*, MIO *),
							void (* freefn_fat)  (void * ptr, void *fat),
							int  (* equalfn_fat) (const void* ptr_a, const void* fat_a,
												  const void* ptr_b, const void* fat_b),
							void (* printfn_fat) (const void* ptr, const void *fat, MIO *))
{
	EsType t = ES_TYPE_NIL;
	if (classes_count >= ES_TYPE_CLASS_MAX)
		return t;

	EsPointerClass *c = calloc (1, sizeof (EsPointerClass));
	if (c == NULL)
		return t;

	c->fat_size  = fat_size;
	c->init_fat = initfat_fn;
	c->free_ptr  = freefn;
	c->equal_ptr = equalfn;
	c->print_ptr = printfn;
	c->free_fatptr  = freefn_fat;
	c->equal_fatptr = equalfn_fat;
	c->print_fatptr = printfn_fat;

	c->base.size  = sizeof (EsPointer) + c->fat_size;
	c->base.free  = es_pointer_free;
	c->base.equal = es_pointer_equal;
	c->base.print = es_pointer_print;
	c->base.flags = ES_OBJECT_FLAG_ATOM;
	c->base.name  = strdup (name);
	if (c->base.name == NULL)
	{
		free (c);
		return t;
	}

	t = classes_count++;
	classes [t] = (EsObjectClass *)c;

	return t;
}

EsType
es_type_define_pointer(const char *name,
					   void (* freefn) (void *),
					   int  (* equalfn) (const void*, const void*),
					   void (* printfn) (const void*, MIO *))
{

	return es_type_define_pointer_full (name, 0, NULL,
										freefn, equalfn, printfn,
										NULL, NULL, NULL);
}

EsType
es_type_define_fatptr    (const char *name,
						  size_t fat_size,
						  EsObject *(* initfat_fn) (void *fat, void * ptr, void *extra),
						  void (* freefn) (void * ptr, void *fat),
						  int  (* equalfn) (const void* ptr_a, const void* fat_a,
											const void* ptr_b, const void* fat_b),
						  void (* printfn) (const void* ptr, const void *fat, MIO *))
{
	return es_type_define_pointer_full (name, fat_size, initfat_fn,
										NULL, NULL, NULL,
										freefn, equalfn, printfn);
}

static void es_pointer_free(EsObject* object)
{
	EsObjectClass *c = class_of(object);
	if (((EsPointer*)object)->ptr)
	{
		if (((EsPointerClass *)c)->free_fatptr)
			((EsPointerClass *)c)->free_fatptr (((EsPointer*)object)->ptr,
												((EsPointer*)object)->fat);
		else if (((EsPointerClass *)c)->free_ptr)
			((EsPointerClass *)c)->free_ptr (((EsPointer*)object)->ptr);
	}
	es_object_free (object);
}

static int  es_pointer_equal(const EsObject* self, const EsObject* other)
{
	if (es_object_get_type (self) != es_object_get_type (other))
		return 0;

	EsPointerClass *c = (EsPointerClass *)class_of(self);
	void *self_ptr  = ((EsPointer *)self)->ptr;
	void *other_ptr = ((EsPointer *)other)->ptr;

	if (c->fat_size == 0 && self_ptr == other_ptr)
		return 1;

	if (self_ptr == NULL)
		return 0;

	if (c->equal_fatptr)
		return c->equal_fatptr (self_ptr, ((EsPointer*)self)->fat,
								other_ptr, ((EsPointer*)other)->fat);
	else if (c->equal_ptr)
		return c->equal_ptr (self_ptr, other_ptr);
	return 0;
}

static void es_pointer_print(const EsObject* object, MIO* fp)
{
	EsObjectClass *c = class_of(object);
	if (((EsPointerClass *)c)->print_fatptr)
	{
		((EsPointerClass *)c)->print_fatptr (((EsPointer *)object)->ptr,
											 ((EsPointer *)object)->fat,
											 fp);
	}
	else if (((EsPointerClass *)c)->print_ptr)
	{
		((EsPointerClass *)c)->print_ptr (((EsPointer *)object)->ptr, fp);
	}
	else
	{
		mio_puts(fp, "#<");
		mio_puts(fp, c->name);
		mio_putc(fp, ' ');
		mio_printf(fp, "(%p, %p)", object, ((EsPointer *)object)->ptr);
		mio_putc(fp, '>');
	}
}

static EsObject*
es_pointer_new_common (EsType type, void *ptr)
{
	EsObject *r;

	r = es_object_new (type);
	if (es_error_p (r))
		return r;

	((EsPointer *)r)->ptr = ptr;
	return r;
}

/*
 * Pointer
 */
EsObject*
es_pointer_new (EsType type, void *ptr)
{
	EsObject *r = es_pointer_new_common (type, ptr);
	if (es_error_p (r))
		return r;

	if (((EsPointerClass *) (classes [type]))->fat_size > 0)
		memset(((EsPointer *)r)->fat, 0,
			   ((EsPointerClass *) (classes [type]))->fat_size);
	return r;
}

void*
es_pointer_get    (const EsObject *object)
{
	return ((EsPointer *)object)->ptr;
}

void*
es_pointer_take    (EsObject *object)
{
	void *r = ((EsPointer *)object)->ptr;
	((EsPointer *)object)->ptr = NULL;
	return r;
}

/*
 * Fat pointer
 */
EsObject*
es_fatptr_new (EsType type, void *ptr, void *extra)
{
	EsObject *r = es_pointer_new_common (type, ptr);
	if (es_error_p (r))
		return r;

	if (((EsPointerClass *) (classes [type]))->fat_size > 0)
	{
		if (((EsPointerClass *) (classes [type]))->init_fat)
		{
			EsObject *f = (* ((EsPointerClass *) (classes [type]))->init_fat)
				(((EsPointer *)r)->fat, ptr, extra);
			if (es_error_p (f))
			{
				es_object_free (r);
				return f;
			}
		}
		else if (extra)
			memcpy (((EsPointer *)r)->fat, extra,
					((EsPointerClass *) (classes [type]))->fat_size);
		else
			memset(((EsPointer *)r)->fat, 0,
				   ((EsPointerClass *) (classes [type]))->fat_size);
	}
	return r;
}

void*
es_fatptr_get     (const EsObject *object)
{
	EsObjectClass *c = class_of(object);
	if (((EsPointerClass *)c)->fat_size == 0)
		return NULL;

	return ((EsPointer *)object)->fat;
}



/* http://www.cse.yorku.ca/~oz/hash.html */
static unsigned long
djb2(unsigned char *str)
{
	unsigned long hash = 5381;
	int c;

	while ((c = *str++))
		hash = ((hash << 5) + hash) + c; /* hash * 33 + c */

	return hash;
}

static unsigned int hash(const char* keyarg)
{
	return ((unsigned int)djb2((unsigned char *)keyarg)) % OBARRAY_SIZE;
}

/*
 * Print
 */
void
es_print           (const EsObject* object,
					MIO*           out)
{
	class_of(object)->print(object, out? out: mio_stdout());
}


char*
es_print_to_string (EsObject*        object)
{
	char *bp;
	size_t size;
	MIO* out;


	out = mio_new_memory (NULL, 0, realloc, NULL);
	if (!out)
    {
		/* TODO: Report error */
		return NULL;
    }

	es_print(object, out);
	bp = (char *)mio_memory_get_data (out, &size);
	mio_unref(out);

	return bp;
}

static const char* comment_prefix = ";; ";
void
es_comment (const char* comment, MIO* out)
{
	const char* p;
	const char* c;

	p = comment_prefix? comment_prefix: ";; ";
	c = comment? comment: "";
	out = out? out: mio_stdout();

	/* ""
	   => ;;

	   "a"
	   => ;; a

	   "a\n"
	   => ;; a


	   "a\nb"
	   => ;; a
	   ;; b

	   "a\nb\n"
	   => ;; a
	   ;;b


	*/
	while (1)
    {
		mio_puts(out, p);

		while(1)
		{
			if (*c == '\0')
			{
				mio_putc(out, '\n');
				return;
			}
			else
			{
				mio_putc(out, *c++);
				if (*(c - 1) == '\n')
					break;
			}
		}
    }
}

char*
es_comment_to_string (const char* comment)
{
	char *bp;
	size_t size;
	MIO* out;

	out = mio_new_memory (NULL, 0, realloc, NULL);
	if (!out)
    {
		/* TODO: Report error */
		return NULL;
    }

	es_comment(comment, out);
	bp = (char *)mio_memory_get_data (out, &size);
	mio_unref(out);

	return bp;
}




/*
 * Read
 */
typedef struct _Token Token;
struct _Token
{
	char*  buffer;
	size_t filled;
	size_t allocated;
};
static Token* token_new   (char seed);
static void   token_free  (Token* token);
static Token* token_append(Token* token, char c);

static Token  eof_token;
#define EOF_TOKEN         (&eof_token)
static Token  open_paren_token;
#define OPEN_PAREN_TOKEN  (&open_paren_token)
static Token  close_paren_token;
#define CLOSE_PAREN_TOKEN (&close_paren_token)

static Token*   get_token      (MIO* in);
static void     skip_to_newline(MIO* in);
static int      is_whitespace    (char c);
static int      is_paren_open    (char c);
static int      is_paren_close   (char c);
static int      is_comment_start (char c);
static int      is_string_start  (char c);
static int      is_fence_start   (char c);
static int      is_reader_macro_prefix(char c);

typedef
int (*TerminalDetector) (int c);

static int is_string_end       (int c);
static int is_fence_end        (int c);
static int is_separator        (int c);

static Token* get_sequence      (MIO* fp,
								 Token* seed,
								 TerminalDetector is_terminator,
								 int              include_terminator);
static Token* get_string        (MIO* fp, char seed);
static Token* get_escaped_symbol(MIO* fp, char seed);
static Token* get_symbol        (MIO* fp, char seed);
static Token* get_regex         (MIO* fp);
static void   inject_regex_flag (Token* t, char c);

static EsObject* fill_list    (MIO*  fp);
static EsObject* make_atom    (Token* token);
static EsObject* make_string  (char* t);
static EsObject* make_symbol  (char* t,
							   int is_wrapped);
static EsObject* make_boolean (int b);
static int  is_integer   (const char* t,
						  int* i);
static EsObject* make_integer (int  i);
static int  is_real      (const char* t,
						  double* d);
static EsObject* make_real    (double d);
static EsObject* make_regex (const char *pat,
							 int case_insensitive);


EsObject*
es_read            (MIO* in)
{
	Token* t;
	EsObject* r;


	in = in? in: mio_stdin();

	t = get_token(in);

	if (t == NULL)
		return ES_READER_ERROR;
	else if (t == EOF_TOKEN)
		return ES_READER_EOF;
	else if (t == OPEN_PAREN_TOKEN)
		r = fill_list(in);
	else if (t == CLOSE_PAREN_TOKEN)
		return ES_READER_ERROR;
	else
		r = make_atom(t);

	token_free(t);

	return r;
}


static Token*
get_token(MIO* in)
{
	Token* t;

	int c;
	while (1)
    {
		c = mio_getc(in);

		if (c == EOF)
		{
			t = EOF_TOKEN;
			break;
		}
		else
		{
			char c0;

			c0 = (char)c;

			if (is_whitespace(c0))
				continue;
			else if (is_comment_start(c0))
			{
				skip_to_newline(in);
				/* TODO */
				continue;
			}
			else if (is_paren_open(c0))
			{
				t = OPEN_PAREN_TOKEN;
				break;
			}
			else if (is_paren_close(c0))
			{
				t = CLOSE_PAREN_TOKEN;
				break;
			}
			else if (is_string_start(c0))
			{
				t = get_string(in, c0);
				break;
			}
			else if (is_fence_start(c0))
			{
				t = get_escaped_symbol(in, c0);
				break;
			}
			else if (is_reader_macro_prefix(c0))
			{
				c = mio_getc(in);
				if (c == EOF)
				{
					t = get_symbol(in, c0);
					break;
				}
				else if (c == '/')
				{
					t = get_regex(in);
					break;
				}
				else
				{
					mio_ungetc (in, c);
					t = get_symbol(in, c0);
					break;
				}
			}
			else
			{
				t = get_symbol(in, c0);
				break;
			}
		}
    }

	return t;
}

static int
is_whitespace    (char c)
{
	static const char* const whitespace_chars = " \t\n\r\f";

	return strchr(whitespace_chars, c)? 1: 0;
}

static int
is_paren_open    (char c)
{
	return (c == '(')? 1: 0;
}

static int
is_paren_close   (char c)
{
	return (c == ')')? 1: 0;
}

static int
is_comment_start (char c)
{
	return (c == ';')? 1: 0;
}

static int
is_string_start  (char c)
{
	return (c == '"')? 1: 0;
}

static int
is_fence_start  (char c)
{
	return (c == '|')? 1: 0;
}

static int
is_reader_macro_prefix(char c)
{
	return (c == '#')? 1: 0;
}

static void
skip_to_newline  (MIO* fp)
{
	int c;


	while (1)
    {
		char c0;


		c = mio_getc(fp);
		if (c == EOF)
			break;

		c0 = (char)c;
		if (c0 == '\n')
			break;
    }
}

static int
is_string_end    (int c)
{
	return ((char)(c) == '"')? 1: 0;
}

static int
is_fence_end     (int c)
{
	return ((char)(c) == '|')? 1: 0;
}

static int
is_separator     (int c)
{
	if (c == EOF)
		return 1;
	else
    {
		char c0;


		c0 = (char)(c);
		if (is_whitespace(c0)
			|| is_comment_start(c0)
			|| is_paren_open(c0)
			|| is_paren_close(c0)
			|| is_string_start(c0)
			|| is_fence_start(c0))
			return 1;
    }

	return 0;
}

static Token*
get_string         (MIO* fp,
					char seed)
{
	Token* t;

	t = token_new(seed);
	return get_sequence(fp, t, is_string_end, 1);
}

static Token*
get_escaped_symbol (MIO* fp,
					char seed)
{
	Token* t;

	t = token_new(seed);
	return get_sequence(fp, t, is_fence_end, 1);
}

static Token*
get_symbol         (MIO* fp,
					char seed)
{
	Token* t;

	t = token_new(seed);
	return get_sequence(fp, t, is_separator, 0);
}

static Token*
get_regex (MIO* fp)
{
	Token *t;
	t = token_new('#');
	if (!t)
		return NULL;

	if (!token_append(t, '/'))
		return NULL;

	/* Inject a placeholder representing
	 * case-{in}sesitive. */
	if (!token_append(t, ' '))
		return NULL;

	int c;
	int in_escape = 0;
	while (1)
	{
		c = mio_getc(fp);
		if (EOF == c)
		{
			/* TODO: Propagate the error to upper layer. */
			mio_printf(mio_stderr(),
					   ";; unexpected termination during parsing regex pattern\n");
			token_free (t);
			return NULL;
		}

		char c0 = c;
		if (in_escape)
		{
			in_escape = 0;

			if (c0 == 'n')
				c0 = '\n';
			else if (c0 == 't')
				c0 = '\t';
			else if (c0 != '/')
			{
				if (!token_append(t, '\\'))
					return NULL;
			}

			if (!token_append(t, c0))
				return NULL;
		}
		else if (c0 == '\\')
			in_escape = 1;
		else if (c0 == '/')
		{
			c = mio_getc(fp);
			if (c == 'i')
				/* Refill the placeholder. */
				inject_regex_flag (t, 'i');
			else if (c != EOF)
				mio_ungetc (fp, c);
			break;
		}
		else
			if (!token_append(t, c0))
				return NULL;
	}
	return t;
}

static void
dump_token (MIO* stream, const char* prefix, Token* seed)
{
	const char* buf;
	int i;
	char  c;


	buf = seed->buffer;
	mio_printf(stream, "%s", prefix);
	for (i = 0; i < ( seed->filled - 1 ); i++)
    {
		c = buf[i];
		mio_putc(stream, c);
		if (buf[i] == '\n')
			mio_printf(stream, "%s", prefix);
    }
	mio_putc(mio_stderr(), '\n');
}

static Token*
get_sequence       (MIO* fp,
					Token* seed,
					TerminalDetector     is_terminator,
					int             include_terminator)
{
	int c;
	int in_escape;

	in_escape = 0;
	while (1)
    {
		c = mio_getc(fp);
		if (EOF == c)
		{
			if (in_escape)
			{
				/*
				  throw ReadError("no character after escape character: " + seed);
				*/
				mio_printf(mio_stderr(), ";; no character after escape character:\n");
				{
					seed = token_append(seed, '\\');
					dump_token(mio_stderr(), "; ", seed);
				}
				token_free(seed);
				return NULL;
			}
			else if (is_terminator(c))
				break;
			else
			{
				/*
				  throw ReadError("got EOF during reading a sequence: " + seed);
				*/
				mio_printf(mio_stderr(), ";; got EOF during reading a sequence: \n");
				dump_token(mio_stderr(), "; ", seed);
				token_free(seed);
				return NULL;
			}
		}
		else
		{
			char c0;


			c0 = (char)(c);
			if (in_escape)
			{
				switch (c0)
				{
				case 'n': c0 = '\n'; break;
				case 't': c0 = '\t'; break;
				case 'r': c0 = '\r'; break;
				case 'f': c0 = '\f'; break;
				default:  break;
				}
				seed = token_append(seed, c0);
				in_escape = 0;
				continue;
			}
			else if (c0 == '\\')
			{
				in_escape = 1;
				continue;
			}
			else if (is_terminator(c))
			{
				if (include_terminator)
					seed = token_append(seed, c0);
				else
				{
					if (mio_ungetc(fp, c) == EOF)
					{
						token_free(seed);
						return NULL;
					}
				}
				break;
			}
			else
			{
				seed = token_append(seed, c0);
				in_escape = 0;
				continue;
			}
		}
    }
	return seed;
}


/*
  (let ((total-length 0)
  (count-symbol 0))
  (mapatoms (lambda (s) (setq total-length (+ total-length (length (symbol-name s)))
  count-symbol (+ 1 count-symbol)
  )))
  (/ total-length count-symbol)) => 15
*/
#define DEFAULT_TOKEN_LENGHT 16
static Token*
token_new   (char seed)
{
	Token *t;


	t = malloc(sizeof(Token));
	if (!t)
		return NULL;

	t->buffer = calloc(1, sizeof(char) * DEFAULT_TOKEN_LENGHT);
	if (!t->buffer)
    {
		free(t);
		return NULL;
    }

	t->filled = 0;
	t->buffer[t->filled++] = seed;
	t->buffer[t->filled++]   = '\0';
	t->allocated = DEFAULT_TOKEN_LENGHT;

	return t;
}

static void
token_free  (Token* token)
{
	if ((token == NULL)
		|| (token == EOF_TOKEN)
		|| (token == OPEN_PAREN_TOKEN)
		|| (token == CLOSE_PAREN_TOKEN))
		return;


	free(token->buffer);
	token->buffer = NULL;
	free(token);
}

static Token*
token_append(Token* t, char c)
{
	size_t d;


	d = t->allocated - t->filled;
	if (d < 1)
    {
		char* tmp;

		tmp = t->buffer;
		t->buffer = realloc(t->buffer, t->allocated *= 2);
		if (!t->buffer)
		{
			t->buffer = tmp;
			token_free(t);
			return NULL;
		}
    }

	t->buffer[t->filled - 1] = c;
	t->buffer[t->filled++]   = '\0';

	return t;
}

/* We use the third character of buffer
 * as a flag representing an option for
 * regex pattern.
 *
 * 'i': case_insensitive
 */
static void
inject_regex_flag(Token* t, char c)
{
	t->buffer [2] = c;
}

static EsObject*
fill_list (MIO* fp)
{
	EsObject* r;
	Token*    t;

	r = es_nil;
	while(1)
    {
		t = get_token(fp);
		if (t == NULL)
		{
			es_object_unref(r);
			return ES_READER_ERROR;
		}
		else if (t == EOF_TOKEN)
		{
			es_object_unref(r);
			return ES_READER_ERROR;
		}
		else if (t == CLOSE_PAREN_TOKEN)
		{
			EsObject* tmp;

			tmp = es_cons_reverse(r);
			es_object_unref(r);
			r = tmp;
			break;
		}
		else if (t == OPEN_PAREN_TOKEN)
		{
			EsObject* car;
			EsObject* cdr;

			car = fill_list(fp);
			if (es_error_p(car))
			{
				es_object_unref(r);
				r = car;
				break;
			}

			cdr = r;
			r = es_cons(car, cdr);
			es_object_unref(car);
			es_object_unref(cdr);

			continue;
		}
		else
		{
			EsObject* car;
			EsObject* cdr;

			car = make_atom(t);
			token_free(t);

			if (es_error_p (car))
			{
				es_object_unref(r);
				r = car;
				break;
			}

			cdr = r;
			r = es_cons(car, cdr);
			es_object_unref(car);
			es_object_unref(cdr);

			continue;
		}
    }

	return r;
}


static EsObject*
make_atom          (Token*   token)
{
	EsObject* r;
	char* t;

	int i;
	double d;


	t = token->buffer;

	if (t[0] == '"')
		r = make_string(t);
	else if (t[0] == '|')
		r = make_symbol(t, 1);
	else if (strcmp(t, "#t") == 0)
		r = make_boolean(1);
	else if (strcmp(t, "#f") == 0)
		r = make_boolean(0);
	else if ((strncmp(t, "#/", 2) == 0)
			 && t[2] != '\0')
		r = make_regex (t + 3, (t[2] == 'i'));
	else if (is_integer(t, &i))
    {
		r = make_integer(i);
    }
	else if (is_real(t, &d))
    {
		r = make_real(d);
    }
	else
		r = make_symbol(t, 0);

	return r;
}

static EsObject*
make_string  (char* t)
{
	size_t len;


	len = strlen(t);
	t[(len - 1)] = '\0';
	return es_string_new(t + 1);
}

static EsObject*
make_symbol  (char* t,
			  int is_wrapped)
{
	if (is_wrapped)
    {
		size_t len;

		len = strlen(t);
		t[(len - 1)] = '\0';
		t = t + 1;
    }

	return es_symbol_intern(t);
}


static EsObject*
make_boolean (int b)
{
	return es_boolean_new(b);
}

static int
is_integer   (const char* cstr,
			  int* i)
{
	char* endptr;
	long  r;

	endptr = NULL;
	errno = 0;
	r = strtol(cstr, &endptr, 10);

	if (errno || (endptr == cstr))
		return 0;
	else if (*endptr != '\0')
		return 0;

	if ((r > INT_MAX) || r < INT_MIN)
    {
		/* TODO: What I should do?
		   TODO: Set error */
		/*
		  throw ReadError("Too large integer for `int': " + r);
		*/
		mio_printf(mio_stderr(), ";; is_integer, Integer out of range: %s\n", cstr);
		return 0;
    }

	*i = r;
	return 1;
}

static EsObject*
make_integer (int  i)
{
	return es_integer_new(i);
}

static int
is_real      (const char* cstr,
			  double* d)
{
	char* endptr;

	endptr = NULL;
	errno = 0;
	*d = strtod(cstr, &endptr);

	if (errno || (endptr == cstr))
		return 0;
	else if (*endptr != '\0')
		return 0;

	/* TODO: INF, NAN... */
	return 1;
}

static EsObject*
make_real (double d)
{
	return es_real_new(d);
}

static EsObject*
make_regex (const char *pat,
			int case_insensitive)
{
	return es_regex_compile(pat, case_insensitive);
}

EsObject*
es_read_from_string(const char* buf,
					const char** saveptr)
{
	MIO* in;
	EsObject* o;


	/* IN is opend in "r" mode and the stream pointed by
	   IN is short-lived here. */
	in = mio_new_memory((void *)buf, strlen(buf), NULL, NULL);
	o = es_read(in);
	if (saveptr)
		*saveptr = buf + mio_tell(in);
	mio_unref(in);

	return o;
}



typedef struct _EsAutounrefPool EsAutounrefPool;
typedef struct _EsChain EsChain;

struct _EsChain
{
	EsObject* object;
	EsChain*  next;
};

struct _EsAutounrefPool
{
	EsAutounrefPool * parent_pool;
	EsChain*          chain;
};

static EsAutounrefPool * currrent_pool;

static EsAutounrefPool* es_autounref_pool_new(void);
static void             es_autounref_pool_free(EsAutounrefPool* pool);
static EsChain*         es_chain_new(EsObject* object);
static void             es_chain_free(EsChain* chain);


void
es_autounref_pool_push(void)
{
	EsAutounrefPool* r;

	r = es_autounref_pool_new();
	r->parent_pool = currrent_pool;
	currrent_pool = r;
}

void
es_autounref_pool_pop (void)
{
	EsAutounrefPool *tmp;

	tmp = currrent_pool;
	currrent_pool = tmp->parent_pool;

	es_autounref_pool_free(tmp);
}

static void
es_autounref_pool_free(EsAutounrefPool* pool)
{
	pool->parent_pool = NULL;
	es_chain_free(pool->chain);
	pool->chain = NULL;

	free(pool);
}

EsObject*
es_object_autounref   (EsObject* object)
{
	EsChain* r;

	r = es_chain_new(object);
	r->next = currrent_pool->chain;
	currrent_pool->chain = r;

	return object;
}

static EsAutounrefPool*
es_autounref_pool_new(void)
{
	EsAutounrefPool* r;

	r = calloc(1, sizeof(EsAutounrefPool));
	return r;
}

static EsChain*
es_chain_new(EsObject *object)
{
	EsChain* r;

	r = calloc(1, sizeof(EsChain));
	r->object = object;
	return r;
}

static void
es_chain_free(EsChain *chain)
{
	EsChain *tmp;

	while(chain)
    {
		tmp = chain;
		chain = chain->next;

		es_object_unref(tmp->object);
		tmp->object = NULL;
		tmp->next = NULL;
		free(tmp);
    }
}


#include <stdarg.h>
static EsObject* es_list_va(EsObject* object, va_list *ap);

EsObject*
es_list(EsObject* object,...)
{
	EsObject* r;
	va_list ap;

	va_start(ap, object);
	r = es_list_va(object, &ap);
	va_end(ap);

	return r;
}

static EsObject*
es_list_va(EsObject* object, va_list *ap)
{
	EsObject* r;
	EsObject* p;
	EsObject* tmp;

	r = es_nil;
	p = object;
	es_autounref_pool_push();
	do {
		if (p == ES_READER_EOF)
			break;

		r = es_cons((p), es_object_autounref(r));
		p = va_arg(*ap, EsObject *);
	} while(1);
	es_autounref_pool_pop();

	tmp = r;
	r = es_cons_reverse(r);
	es_object_unref(tmp);

	return r;
}


static EsObject* es_append0(EsObject* tail, EsObject* body);
static EsObject* es_append1(EsObject* tail, EsObject* body0);

EsObject*
es_append(EsObject* list,...)
{
	EsObject *r;
	EsObject *tmp;
	EsObject *tail;
	EsObject *body;
	va_list ap;


	va_start(ap, list);
	r = es_list_va(list, &ap);
	va_end(ap);

	tmp = r;
	r = es_cons_reverse(r);
	es_object_unref(tmp);

	/* r */
	tail = es_car(r);
	body = es_cdr(r);
	tmp  = r;
	r = es_append0(tail, body);
	es_object_unref(tmp);

	return r;
}

static EsObject*
es_append0(EsObject* tail, EsObject* body)
{
	if (es_null(body))
		return tail;
	else
    {
		EsObject* car;

		car = es_cons_reverse(es_car(body));
		tail = es_append1(tail, car);
		es_object_unref(car);
		body = es_cdr(body);
		return es_append0(tail, body);
    }
}

static EsObject*
es_append1(EsObject* tail, EsObject* body0)
{
	if (es_null(body0))
		return es_object_ref(tail);
	else
    {
		EsObject* car;
		EsObject* r;

		car  = es_car(body0);
		tail = es_cons(car, tail);

		r = es_append1(tail, es_cdr(body0));
		es_object_unref(tail);
		return r;
    }
}



static EsObject* pattern_d         = NULL;
static EsObject* pattern_f         = NULL;
static EsObject* pattern_F         = NULL;
static EsObject* pattern_s         = NULL;
static EsObject* pattern_S         = NULL;
static EsObject* pattern_b         = NULL;
static EsObject* pattern_rest      = NULL;
static EsObject* pattern_unquote   = NULL;
static EsObject* pattern_splice    = NULL;

static EsObject* pattern_i_d       = NULL;
static EsObject* pattern_i_f       = NULL;
static EsObject* pattern_i_F       = NULL;
static EsObject* pattern_i_s       = NULL;
static EsObject* pattern_i_S       = NULL;
static EsObject* pattern_i_b       = NULL;
static EsObject* pattern_i_rest    = NULL;
static EsObject* pattern_i_unquote = NULL;

static void
pattern_init(void)
{
	if (!pattern_d) (pattern_d = es_symbol_intern("%d"));
	if (!pattern_f) (pattern_f = es_symbol_intern("%f"));
	if (!pattern_F) (pattern_F = es_symbol_intern("%F"));
	if (!pattern_s) (pattern_s = es_symbol_intern("%s"));
	if (!pattern_S) (pattern_S = es_symbol_intern("%S"));
	if (!pattern_b) (pattern_b = es_symbol_intern("%b"));
	if (!pattern_rest) (pattern_rest = es_symbol_intern("%@"));
	if (!pattern_unquote) (pattern_unquote = es_symbol_intern("%,"));
	if (!pattern_splice) (pattern_splice = es_symbol_intern("%,@"));

	if (!pattern_i_d) (pattern_i_d = es_symbol_intern("%_d"));
	if (!pattern_i_f) (pattern_i_f = es_symbol_intern("%_f"));
	if (!pattern_i_F) (pattern_i_F = es_symbol_intern("%_F"));
	if (!pattern_i_s) (pattern_i_s = es_symbol_intern("%_s"));
	if (!pattern_i_S) (pattern_i_S = es_symbol_intern("%_S"));
	if (!pattern_i_b) (pattern_i_b = es_symbol_intern("%_b"));
	if (!pattern_i_rest) (pattern_i_rest = es_symbol_intern("%_@"));
	if (!pattern_i_unquote) (pattern_i_unquote = es_symbol_intern("%_,"));
}

static EsObject*
es_vrealize_atom(EsObject* fmt_object, va_list *ap)
{
	if (fmt_object == pattern_d)
		return es_integer_new(va_arg(*ap, int));
	else if (fmt_object == pattern_f)
    {
		double x = va_arg(*ap, double);
		mio_printf(mio_stderr(), "=>%f\n", x);
		return es_real_new(x);
    }
	else if (fmt_object == pattern_s)
		return es_string_new(va_arg(*ap, char *));
	else if (fmt_object == pattern_S)
		return es_symbol_intern(va_arg(*ap, char *));
	else if (fmt_object == pattern_b)
		return es_boolean_new(va_arg(*ap, int));
	else if ((fmt_object == pattern_unquote)
			 || (fmt_object == pattern_splice))
		return es_object_ref(va_arg(*ap, EsObject*));
	else
		return es_object_ref(fmt_object);
}

static EsObject*
es_vrealize(EsObject* fmt_object, va_list *ap)
{
	pattern_init();

	if (es_cons_p(fmt_object))
    {
		EsObject* car;
		EsObject* cdr;
		EsObject* kar;
		EsObject* kdr;
		EsObject* r;

		car = es_car(fmt_object);

		if (car == pattern_rest)
			r = es_object_ref(va_arg(*ap, EsObject*));
		else
		{
			cdr = es_cdr(fmt_object);

			kar = es_vrealize(car, ap);
			kdr = es_vrealize(cdr, ap);

			if (car == pattern_splice)
			{
				if (es_cons_p(kar))
					r = es_append(kar, kdr, ES_READER_EOF);
				else
				{
					/* TODO: error */
					char *fmt;

					mio_printf(mio_stderr(),
							   ";; an atom is passed for splice format:\n");
					fmt = es_print_to_string(fmt_object);
					mio_printf(mio_stderr(), ";; => %s\n", fmt);
					free(fmt);
					r = es_nil;
				}
			}
			else
				r = es_cons(kar, kdr);

			es_object_unref(kar);
			es_object_unref(kdr);
		}
		return r;
    }
	else
		return es_vrealize_atom(fmt_object, ap);
}

EsObject*
es_realize   (EsObject* fmt_object,...)
{
	EsObject* object;
	va_list ap;

	if (es_error_p(fmt_object))
		return es_object_ref(fmt_object);

	va_start(ap, fmt_object);
	object = es_vrealize(fmt_object, &ap);
	va_end(ap);

	return object;
}

EsObject*
es_srealize  (const char* fmt,...)
{
	EsObject* fmt_object;
	EsObject* object;
	va_list ap;

	fmt_object = es_read_from_string(fmt, NULL);
	if (es_error_p(fmt_object))
		return fmt_object;

	va_start(ap, fmt);
	object = es_vrealize(fmt_object, &ap);
	va_end(ap);

	es_object_unref(fmt_object);

	return object;
}

EsObject* es_map   (EsObject * (*fn) (EsObject *, void *),
					EsObject *list, void *user_data)
{
	if (es_null (list))
		return list;

	EsObject *c = es_car (list);
	c = fn (c, user_data);
	if (es_error_p (c))
		return c;

	EsObject *r = es_map (fn, es_cdr (list), user_data);
	if (es_error_p (r))
	{
		es_object_unref (c);
		return r;
	}

	EsObject *o = es_cons (c, r);
	es_object_unref (r);
	es_object_unref (c);

	return o;
}

EsObject* es_foreach (EsObject * (*fn) (EsObject *, void *),
					  EsObject *list, void *user_data)
{
	if (es_null (list))
		return es_false;

	for (EsObject *c = list; !es_null (c); c = es_cdr (c))
	{
		EsObject *r = fn (es_car (c), user_data);
		if (!es_object_equal (r, es_false))
			return r;
	}

	return es_false;
}

EsObject* es_fold (EsObject * (*kons) (EsObject *, EsObject *, void *),
				   EsObject * knil, EsObject * list, void *user_data)
{
	EsObject *r = knil;

	es_autounref_pool_push();
	while (!es_null (list))
	{
		EsObject *e = es_car (list);
		list = es_cdr (list);

		r = (* kons) (e, (r == knil) ? r : es_object_autounref (r),
					  user_data);
		if (es_error_p (r))
			break;
	}
	es_autounref_pool_pop();

	return r;
}

static EsObject*
es_vmatch_atom_input(EsObject* input, EsObject* fmt_object, va_list *ap)
{
	return ES_READER_ERROR;
}

static EsObject*
es_vmatch_atom_fmt(EsObject* input, EsObject* fmt_object, va_list *ap)
{
	if (fmt_object == pattern_unquote)
		*(va_arg(*ap, EsObject**)) = /* es_object_ref */(input);
	else if (fmt_object == pattern_i_unquote)
		;
	else
		return ES_READER_ERROR;

	return fmt_object;
}

static EsObject*
es_vmatch_atom(EsObject* input, EsObject* fmt_object, va_list *ap)
{
	if (fmt_object == pattern_d)
    {
		if (es_integer_p(input))
			*(va_arg(*ap, int*)) = es_integer_get(input);
		else
			return ES_READER_ERROR;
    }
	else if (fmt_object == pattern_i_d)
    {
		if (es_integer_p(input))
			;
		else
			return ES_READER_ERROR;
    }
	else if (fmt_object == pattern_f)
    {
		if (es_real_p(input))
			*(va_arg(*ap, double*)) = es_real_get(input);
		else
			return ES_READER_ERROR;
    }
	else if (fmt_object == pattern_i_f)
    {
		if (es_real_p(input))
			;
		else
			return ES_READER_ERROR;
    }
	else if (fmt_object == pattern_F)
    {
		if (es_integer_p(input))
		{
			int i;

			i = es_integer_get(input);
			*(va_arg(*ap, double*)) = (double)i;
		}
		else if (es_real_p(input))
		{
			*(va_arg(*ap, double*)) = es_real_get(input);
		}
		else
			return ES_READER_ERROR;
    }
	else if (fmt_object == pattern_i_F)
    {
		if (es_integer_p(input) || es_real_p(input))
			;
		else
			return ES_READER_ERROR;
    }
	else if (fmt_object == pattern_s)
    {
		if (es_string_p(input))
			*(va_arg(*ap, const char**)) = /* strdup */(es_string_get(input));
		else
			return ES_READER_ERROR;
    }
	else if (fmt_object == pattern_i_s)
    {
		if (es_string_p(input))
			;
		else
			return ES_READER_ERROR;
    }
	else if (fmt_object == pattern_S)
    {
		if (es_symbol_p(input))
			*(va_arg(*ap, const char**)) = /* strdup */(es_symbol_get(input));
		else
			return ES_READER_ERROR;
    }
	else if (fmt_object == pattern_i_S)
    {
		if (es_symbol_p(input))
			;
		else
			return ES_READER_ERROR;
    }
	else if (fmt_object == pattern_b)
    {
		if (es_boolean_p(input))
			*(va_arg(*ap, int*)) = es_boolean_get(input);
		else
			return ES_READER_ERROR;
    }
	else if (fmt_object == pattern_i_b)
    {
		if (es_boolean_p(input))
			;
		else
			return ES_READER_ERROR;
    }
	else if (fmt_object == pattern_unquote)
		*(va_arg(*ap, EsObject**)) = /* es_object_ref */(input);
	else if (fmt_object == pattern_i_unquote)
		;
	else if (es_object_equal(fmt_object, input))
		;
	else
		return ES_READER_ERROR;

	return fmt_object;
}

static void
recover(EsObject* fmt_object, va_list *aq)
{
	if (es_cons_p(fmt_object))
    {
		recover(es_car(fmt_object), aq);
		recover(es_cdr(fmt_object), aq);
    }
	else
    {
		if (fmt_object == pattern_s
			|| fmt_object == pattern_S)
		{
			char **s;

			s = va_arg(*aq, char **);
			(void)/* free */(*s);

			*s = NULL;
		}
		else if (fmt_object == pattern_rest
				 || fmt_object == pattern_unquote)
		{
			EsObject** o;

			o = va_arg(*aq, EsObject**);
			(void)/* es_object_unref */(*o);
			*o = NULL;
		}
    }
}

static EsObject*
es_vmatch(EsObject* input, EsObject* fmt_object, va_list *ap)
{
	pattern_init();

	if (es_cons_p(fmt_object) && es_cons_p(input))
    {
		EsObject* fmt_car;
		EsObject* fmt_cdr;
		EsObject* i_car;
		EsObject* i_cdr;

		EsObject* r_car;
		EsObject* r_cdr;

		va_list   aq;

		fmt_car = es_car(fmt_object);

		if (fmt_car == pattern_rest)
		{
			*(va_arg(*ap, EsObject**)) = /* es_object_ref */(input);
			return fmt_car;
		}
		else if (fmt_car == pattern_i_rest)
		{
			return fmt_car;
		}

		fmt_cdr = es_cdr(fmt_object);

		i_car   = es_car(input);
		i_cdr   = es_cdr(input);

		va_copy(aq, *ap);
		r_car = es_vmatch(i_car, fmt_car, ap);
		if (es_error_p(r_car))
		{
			va_end(aq);
			return r_car;
		}

		r_cdr = es_vmatch(i_cdr, fmt_cdr, ap);
		if (es_error_p(r_cdr))
		{
			recover(fmt_car, &aq);
			va_end(aq);
			return r_cdr;
		}
		va_end(aq);
		return r_cdr;
    }
	else if (es_cons_p(fmt_object))
    {
		return es_vmatch_atom_input(input, fmt_object, ap);
    }
	else if (es_cons_p(input))
    {
		if (fmt_object == pattern_rest)
		{
			*(va_arg(*ap, EsObject**)) = /* es_object_ref */(input);
			return fmt_object;
		}
		else if (fmt_object == pattern_i_rest)
			return fmt_object;
		else
			return es_vmatch_atom_fmt(input, fmt_object, ap);
    }
	else
    {
		return es_vmatch_atom(input, fmt_object, ap);
    }
}

int
es_match(EsObject* input, EsObject* fmt_object,...)
{
	EsObject* object;
	va_list ap;

	va_start(ap, fmt_object);
	object = es_vmatch(input, fmt_object, &ap);
	va_end(ap);

	return !(es_error_p(object));
}

int
es_smatch   (EsObject* input, const char* fmt,...)
{
	int r;
	EsObject* object;
	EsObject* fmt_object;
	va_list ap;

	fmt_object = es_read_from_string(fmt, NULL);
	if (es_error_p(fmt_object))
		return 0;

	va_start(ap, fmt);
	object = es_vmatch(input, fmt_object, &ap);
	va_end(ap);

	r = !(es_error_p(object));
	es_object_unref(fmt_object);

	return r;
}

EsObject*
es_pget (EsObject* plist, EsObject* key, EsObject* default_value)
{
	if (es_cons_p(plist))
    {
		EsObject* car;
		EsObject* cdr;
		EsObject* cadr;
		EsObject* cddr;

		car = es_car(plist);
		cdr = es_cdr(plist);

		if (es_cons_p(cdr))
		{
			cadr = es_car(cdr);
			cddr = es_cdr(cdr);

			if (es_object_equal(car, key))
				return cadr;
			else
				return es_pget(cddr, key, default_value);
		}
		else
			return ES_READER_ERROR;
    }
	else
		return default_value;
}
