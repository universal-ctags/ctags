/*
 *   Copyright (c) 2024 Masatake YAMATO
 *   Copyright (c) 2024 Red Hat, Inc.
 *
 *   This source code is released for free distribution under the terms of the
 *   GNU General Public License version 2 or (at your option) any later version.
 *
 *   This module contains macros, data decls and prototypes to generate tags for TOML.
 *
 *   references:
 *   - https://toml.io/en/v1.0.0
 */

/*
*   INCLUDE FILES
*/
#include "kind.h"
#include "peg_common.h"

#include "htable.h"
#include "ptrarray.h"

/*
*   MACROS
*/

/*
*   DATA DECLARATIONS
*/
typedef enum {
	K_KEY,
	K_TABLE,
	K_ARRAYTABLE,
	K_QKEY,
} tomlKind;

typedef enum {
	R_KEY_CHAINELT,
} tomlKeyRole;

static roleDefinition TomlKeyRoles [] = {
	{ false, "chainElt", "(EXPERIMENTAL)used as an element in a key name chain like a.b.c" },
};

static kindDefinition TomlKinds [] = {
	{ false, 'k', "key",        "keys",
	  .referenceOnly = false, ATTACH_ROLES(TomlKeyRoles) },
	{ true,  't', "table",      "tables"                 },
	{ true,  'a', "arraytable", "array tables"           },
	{ true,  'K', "qkey",       "qualified keys"         },
};

struct parserCtx {
	struct parserBaseCtx base;
	ptrArray *keyQueue;
	bool isArrayTable;
	long keyOffset;
	int lastTableIndex;
	hashTable *arrayTableCounters;
	uintArray *unwind_depths;
};

/*
*   FUNCTION PROTOTYPES
*/
static void tableKeyStart (struct parserCtx *auxil, bool isArrayTable, long offset);
static void tableKeyEnd (struct parserCtx *auxil);
static void keyvalStart (struct parserCtx *auxil, long offset);
static void keyvalKeyEnd (struct parserCtx *auxil);
static void keyvalValEnd (struct parserCtx *auxil);
static void queueKey (struct parserCtx *auxil, const char *name);
