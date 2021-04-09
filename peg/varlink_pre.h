/*
 *   Copyright (c) 2019 Masatake YAMATO
 *   Copyright (c) 2019 Red Hat, Inc.
 *
 *   This source code is released for free distribution under the terms of the
 *   GNU General Public License version 2 or (at your option) any later version.
 *
 *   This module contains macros, data decls and prototypes to generate tags for Varlink.
 */

/*
*   INCLUDE FILES
*/
/* This one is not needed anymore because the inclusion is specified
   in Makefile.am
#include "general.h"
*/

#include "entry.h"
#include "numarray.h"
#include "kind.h"
#include "read.h"
#include "peg_common.h"


/*
*   MACROS
*/

/*
*   DATA DECLARATIONS
*/
typedef enum {
	K_INTERFACE,
	K_METHOD,
	K_IPARAM,
	K_OPARAM,
	K_STRUCT,
	K_FIELD,
	K_ENUM,
	K_ENUMERATION,
	K_ERROR,
	K_EDESC,
} varlinkKind;

static kindDefinition VarlinkKinds [] = {
	{ true,  'i', "interface",   "interfaces" },
	{ true,  'm', "method",      "methods" },
	{ true,  'I', "iparam",      "input parameters" },
	{ true,  'O', "oparam",      "output parameters" },
	{ true,  's', "struct",      "structs" },
	{ true,  'f', "field",       "fields" },
	{ true,  'g', "enum",        "enumeration names" },
	{ true,  'e', "enumerator",  "enumerators (values inside an enumeration)" },
	{ true,  'E', "error",       "errors" },
	{ true,  'd', "edesc",       "error descriptors" },
};

typedef enum  {
	METHOD_PARAM_INPUT,
	METHOD_PARAM_OUTPUT,
} methodParamState;

struct parserCtx {
	int scope_cork_index;
	intArray *kind_stack;
	methodParamState mparam_state;
	bool found_syntax_error;
};

/*
*   FUNCTION PROTOTYPES
*/
static void pushKind (struct parserCtx *auxil, int kind);
static void popKind (struct parserCtx *auxil, bool popScopeToo);
static void pushKindContextual (struct parserCtx *auxil);
static int peekKind (struct parserCtx *auxil);
static void setMethodParamState (struct parserCtx *auxil, methodParamState s);
static void reportError (struct parserCtx *auxil);
static int makeVarlinkTag (struct parserCtx *auxil, const char *name, long offset);
