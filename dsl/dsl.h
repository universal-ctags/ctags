/*
*   Copyright (c) 2020, Masatake YAMATO
*   Copyright (c) 2020, Red Hat, Inc.
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*/

#ifndef DSL_H
#define DSL_H

/*
 * INCLUDES
 */
#include "es-lang-c-stdc99.h"
#include "readtags.h"


/*
 * TYPES
 */
enum eDSLEngineType {
	DSL_COMMON,
	DSL_QUALIFIER,
	DSL_SORTER,
	DSL_ENGINE_COUNT
};
typedef enum eDSLEngineType DSLEngineType;

struct sDSLEnv {
	enum eDSLEngineType engine;
	const tagEntry *entry;
	const tagEntry *alt_entry;
};
typedef struct sDSLEnv DSLEnv;

typedef EsObject* (* DSLProc)  (EsObject *args, DSLEnv *env);


enum eDSLPAttr {
	DSL_PATTR_MEMORABLE   = 1UL << 0,
	DSL_PATTR_PURE        = 1UL << 1,
	DSL_PATTR_SELF_EVAL   = 1UL << 2,
	DSL_PATTR_CHECK_ARITY = 1UL << 3,
	DSL_PATTR_CHECK_ARITY_OPT = 1UL << 4 | DSL_PATTR_CHECK_ARITY,
};
typedef enum eDSLPAttr DSLPAttr;

typedef struct sDSLProcBind DSLProcBind;
struct sDSLProcBind {
	const char *name;
	DSLProc proc;
	EsObject* cache;
	DSLPAttr flags;
	int arity;
	const char* helpstr;
};

typedef struct sDSLCode DSLCode;

#define DSL_ERR_UNBOUND_VARIABLE    (es_error_intern("unbound-variable"))
#define DSL_ERR_TOO_FEW_ARGUMENTS   (es_error_intern("too-few-arguments"))
#define DSL_ERR_TOO_MANY_ARGUMENTS  (es_error_intern("too-many-arguments"))
#define DSL_ERR_STRING_REQUIRED     (es_error_intern("string-required"))
#define DSL_ERR_BOOLEAN_REQUIRED    (es_error_intern("boolean-required"))
#define DSL_ERR_INTEGER_REQUIRED    (es_error_intern("integer-required"))
#define DSL_ERR_NUMBER_REQUIRED     (es_error_intern("number-required"))
#define DSL_ERR_WRONG_TYPE_ARGUMENT (es_error_intern("wrong-type-argument"))
#define DSL_ERR_NO_ALT_ENTRY        (es_error_intern("the-alternative-entry-unavailable"))


/*
 * MACROS
 */
#define dsl_throw(e,o)               return es_error_set_object(DSL_ERR_##e, o)


/*
 * Function declarations
 */

/* Return 1 if no error. */
int            dsl_init        (DSLEngineType engine, DSLProcBind *engine_pbinds, int count);
DSLProcBind   *dsl_lookup      (DSLEngineType engine, EsObject *name);
void           dsl_help        (DSLEngineType engine, FILE *fp);
void           dsl_cache_reset (DSLEngineType engine);
DSLCode       *dsl_compile     (DSLEngineType engine, EsObject *expr);
EsObject      *dsl_eval        (DSLCode *code, DSLEnv *env);
void           dsl_release     (DSLEngineType engine, DSLCode *code);

/* This should be remove when we have a real compiler. */
EsObject *dsl_compile_and_eval (EsObject *expr, DSLEnv *env);


EsObject* dsl_entry_xget_string (const tagEntry *entry, const char* name);

EsObject* dsl_entry_name (const tagEntry *entry);
EsObject* dsl_entry_input (const tagEntry *entry);
EsObject* dsl_entry_pattern (const tagEntry *entry);
EsObject* dsl_entry_line (const tagEntry *entry);

EsObject* dsl_entry_access (const tagEntry *entry);
EsObject* dsl_entry_end (const tagEntry *entry);
EsObject* dsl_entry_file (const tagEntry *entry);
EsObject* dsl_entry_inherits (const tagEntry *entry);
EsObject* dsl_entry_implementation (const tagEntry *entry);
EsObject* dsl_entry_kind (const tagEntry *entry);
EsObject* dsl_entry_language (const tagEntry *entry);
EsObject* dsl_entry_scope_kind (const tagEntry *entry);
EsObject* dsl_entry_scope_name (const tagEntry *entry);
EsObject* dsl_entry_signature (const tagEntry *entry);
EsObject* dsl_entry_roles (const tagEntry *entry);

#endif
