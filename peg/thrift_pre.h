/*
 *   Copyright (c) 2021 Masatake YAMATO
 *
 *   This source code is released for free distribution under the terms of the
 *   GNU General Public License version 2 or (at your option) any later version.
 *
 *   This module contains macros, data decls and prototypes to generate tags for Thrift IDL.
 *   Reference: https://thrift.apache.org/docs/idl
 */

/*
*   INCLUDE FILES
*/
#include "kind.h"
#include "field.h"
#include "peg_common.h"

#include "parsers/cxx/cxx_tag.h"

/*
*   MACROS
*/

/*
*   DATA DECLARATIONS
*/
typedef enum {
	K_STRUCT,
	K_EXCEPTION,
	K_UNION,
	K_NAMESPACE,
	K_ENUMERATOR,
	K_ENUM,
	K_MEMBER,
	K_CONST,
	K_TYPEDEF,
	K_SERVICE,
	K_FUNCTION,
	K_PARAMETER,
	K_THROWSPARAM,
	K_THRIFTFILE,
	COUNT_KINDS
} thriftKind;

typedef enum {
	THRIFT_THRIFT_FILE_INCLUDED,
} thriftThriftFileRole;

static roleDefinition ThriftThriftFileRoles [] = {
	{ true, "included", "included file" },
};

static kindDefinition ThriftKinds [COUNT_KINDS] = {
	{ true,  's', "struct",       "structs"    },
	{ true,  'x', "exception",    "exceptions" },
	{ true,  'u', "union",        "unions"     },
	{ true,  'n', "namespace",    "namespaces" },
	{ true,  'e', "enumerator",   "enumerators (values inside an enumeration)" },
	{ true,  'g', "enum",         "enumeration names" },
	{ true,  'm', "member",       "members"    },
	{ true,  'C', "const",        "constants"  },
	{ true,  't', "typedef",      "typedefs"   },
	{ true,  'v', "service",      "services"   },
	{ true,  'f', "function",     "functions"  },
	{ false, 'z', "parameter",    "parameters" },
	{ false, 'Z', "throwsparam",  "parameters in throws list" },
	{ true,  'T', "thriftFile",   "thrift files",
	  .referenceOnly = true, ATTACH_ROLES(ThriftThriftFileRoles) },
};

typedef enum {
	F_THROWS,
	F_TARGET,
	COUNT_FIELDS
} thriftField;

static fieldDefinition ThriftFields[COUNT_FIELDS] = {
	{ .name        = "throws",
	  .description = "throws list of function",
	  .enabled     = true },
	{ .name        = "target",
	  .description = "the target language specified at \"namespace\"",
	  .enabled     = true },
};

struct parserCtx {
	struct parserBaseCtx base;
};

/*
*   FUNCTION PROTOTYPES
*/
#define USE_KIND_STACK KIND_GHOST_INDEX
static int makeThriftTagFull (struct parserCtx *auxil, const char *name, long offset, int kind, int role, bool pushScope);
static int makeThriftTag (struct parserCtx *auxil, const char *name, long offset, int kind, bool pushScope);
static vString* unliteral(const char *literal);
