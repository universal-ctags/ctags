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
	DSL_QUALIFIER,
	DSL_ENGINE_COUNT
};
typedef enum eDSLEngineType DSLEngineType;

struct sDSLEnv {
	enum eDSLEngineType engine;
	const tagEntry *entry;
};
typedef struct sDSLEnv DSLEnv;

typedef EsObject* (* DSLProc)  (EsObject *args, DSLEnv *env);


enum eDSLPAttr {
	DSL_PATTR_MEMORABLE   = 1UL << 0,
	DSL_PATTR_PURE        = 1UL << 1,
	DSL_PATTR_SELF_EVAL   = 1UL << 2,
	DSL_PATTR_CHECK_ARITY = 1UL << 3,
};
typedef enum eDSLPAttr DSLPAttr;

typedef struct sDSLCode DSLCode;
struct sDSLCode {
	const char *name;
	DSLProc proc;
	EsObject* cache;
	DSLPAttr flags;
	int arity;
	const char* helpstr;
};

#define DSL_ERR_UNBOUND_VARIABLE    (es_error_intern("unbound-variable"))
#define DSL_ERR_TOO_FEW_ARGUMENTS   (es_error_intern("too-few-arguments"))
#define DSL_ERR_TOO_MANY_ARGUMENTS  (es_error_intern("too-many-arguments"))
#define DSL_ERR_NUMBER_REQUIRED     (es_error_intern("number-required"))
#define DSL_ERR_WRONG_TYPE_ARGUMENT (es_error_intern("wrong-type-argument"))


/*
 * MACROS
 */
#define dsl_throw(e,o)               return es_error_set_object(DSL_ERR_##e, o)


/*
 * Function declarations
 */
EsObject *dsl_code_define (DSLEngineType engine, DSLCode *code);
DSLCode  *dsl_code_lookup (DSLEngineType engine, EsObject *name);
void      dsl_help        (DSLEngineType engine, FILE *fp);
void      dsl_code_reset  (DSLCode  *code);


#endif
