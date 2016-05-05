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

#ifndef __ES_LANG_C_STDC99_H__
#define __ES_LANG_C_STDC99_H__

#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif
#include <stdio.h>

#include "mio.h"

#ifdef  __cplusplus
extern "C" {
#endif

enum _EsType {
  ES_TYPE_NIL,
  ES_TYPE_INTEGER,
  ES_TYPE_REAL,
  ES_TYPE_BOOLEAN,
  ES_TYPE_SYMBOL,
  ES_TYPE_STRING,
  ES_TYPE_CONS,
  /* ... */
  ES_TYPE_ERROR
};
typedef enum _EsType EsType;

struct _EsObject;
typedef struct _EsObject EsObject;

EsType      es_object_get_type      (const EsObject*      object);



EsObject*   es_object_ref           (EsObject*       object);
void        es_object_unref         (EsObject*       object);
void        es_object_unref_batch   (EsObject*       array[],
				     unsigned int    count);
EsObject*   es_object_autounref     (EsObject*      object);

int         es_object_equal         (const EsObject* self,
				     const EsObject* other);

int         es_atom                 (const EsObject* object);


/*
 * Nil
 */
#define es_nil ((EsObject*)0)
int         es_null                 (const EsObject* object);

/*
 * Integer
 */
EsObject*    es_integer_new (int                value);
int          es_integer_p   (const EsObject*   object);
int          es_integer_get (const EsObject*   object);


/*
 * Real
 */
EsObject*    es_real_new    (double             value);
int          es_real_p      (const EsObject*   object);
double       es_real_get    (const EsObject*   object);


/*
 * Use Integer as Real
 */
int          es_number_p    (const EsObject*   object);
double       es_number_get  (const EsObject*   object);


/*
 * Boolean
 */
#define es_true             (es_boolean_new(1))
#define es_false            (es_boolean_new(0))
EsObject*    es_boolean_new (int                value);
int          es_boolean_p   (const EsObject*   object);
int          es_boolean_get (const EsObject*   object);

/*
 * String
 */
EsObject*    es_string_new  (const char*        value);
int          es_string_p    (const EsObject*   object);
const char*  es_string_get  (const EsObject*   object);


/*
 * Symbol
 */
EsObject*    es_symbol_intern  (const char*       name);
int          es_symbol_p    (const EsObject*   object);
const char*  es_symbol_get  (const EsObject*   object);

void*        es_symbol_set_data (const EsObject*   object, void *data);
void*        es_symbol_get_data (const EsObject*   object);

/*
 * Error
 */

EsObject*    es_error_intern (const char*        name);
int          es_error_p      (const EsObject*   object);
const char*  es_error_name   (const EsObject*   object);
EsObject*    es_error_set_object (EsObject*   error, EsObject*   object);
EsObject*    es_error_get_object (const EsObject*   error);


/*
 * Cons
 */
EsObject*    es_cons        (EsObject* car, EsObject* cdr);
int          es_cons_p      (const EsObject* object);
int          es_list_p      (const EsObject* object);
EsObject*    es_car         (const EsObject* object);
EsObject*    es_cdr         (const EsObject* object);



/*
 * Print
 */
void         es_print           (const EsObject* object,
				 MIO*           out);
char*        es_print_to_string (EsObject*        object);

/*
 * Read
 */
EsObject*    es_read            (MIO* in);
EsObject*    es_read_from_string(const char* in,
				 const char** saveptr);

#define      ES_READER_ERROR es_error_intern("READ-ERROR")
#define      ES_READER_EOF   es_error_intern("EOF")


/*
 * Comment
 */
void         es_comment           (const char* comment,
				   MIO*       out);
char*        es_comment_to_string (const char* comment);

/*
 * Autounref pool
 */
void es_autounref_pool_push(void);
void es_autounref_pool_pop (void);



/*
 * List builders
 */
EsObject* es_list     (EsObject* object,...);
EsObject* es_append   (EsObject* list,...);
EsObject* es_reverse  (EsObject* cons);

#define      ES_PROC_UNIMPLEMENTED es_error_intern("PROC-UNIMPLEMENTED")
EsObject* es_realize   (EsObject* fmt_object,...);
EsObject* es_srealize  (const char* fmt,...);

/*
 * Rich element accessors
 */
int        es_match        (EsObject* input, EsObject* fmt_object,...);
int        es_smatch       (EsObject* input, const char* fmt,...);


EsObject*  es_pget         (EsObject* plist, EsObject* key, EsObject* default_value);

#ifdef  __cplusplus
}
#endif

#endif /* Not def: __ES_LANG_C_STDC_H__ */
