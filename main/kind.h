/*
*   Copyright (c) 1998-2003, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*/
#ifndef CTAGS_MAIN_KIND_H
#define CTAGS_MAIN_KIND_H

/*
*   INCLUDE FILES
*/

#include "general.h"
#include "types.h"
#include "routines.h"

/*
*   DATA DECLARATIONS
*/

struct sRoleDefinition {
	bool enabled;
	char* name;		  /* role name */
	char* description;	  /* displayed in --help output */

	int id;
};

/*
 * Predefined kinds
 */
#define KIND_REGEX_DEFAULT_LETTER 'r'
#define KIND_REGEX_DEFAULT_NAME "regex"

#define KIND_NULL_LETTER    '\0'

/* GHOST kind can be used for a value for
 * initializing a variable holding a kind index,
 * or filling a struct member holding a kind index.
 *
 * Typical case is filling a scope related struct
 * member with GHOST to represent root name scope.
 *
 * input.c:
 *
 *     int main (void) { return 0; }
 *
 * Consider that tagging "main" in above input.
 * You may wonder what kind of value
 * should be used to fill tag.extensionFields.scopeKindIndex.
 * KIND_GHOST_INDEX can be used for the purpose.
 */
#define KIND_GHOST_INDEX -1
#define KIND_GHOST_LETTER   ' '
#define KIND_GHOST_NAME "ghost"

#define KIND_FILE_INDEX -2
#define KIND_FILE_DEFAULT_LETTER 'F'
#define KIND_FILE_DEFAULT_NAME "file"

#define KIND_WILDCARD_INDEX -3
#define KIND_WILDCARD_LETTER '*'

typedef struct sScopeSeparator {
	int parentKindIndex;
	const char *separator;
} scopeSeparator;

struct sKindDefinition {
	bool enabled;          /* are tags for kind enabled? */
	char  letter;               /* kind letter */
	char* name;		  /* kind name */
	char* description;	  /* displayed in --help output */
	bool referenceOnly;
	int nRoles;		/* The number of role elements. */
	roleDefinition *roles;
	scopeSeparator *separators;
	unsigned int separatorCount;

	int id;

	/* TODO:Following fields should be moved to kindObject. */
	/* Usage of `syncWith' field is a bit tricky.

	   If `LANG_AUTO' is specified to `syncWith' field of a kind
	   (target kind), the main part of ctags updates the field with
	   the id of a  parser (master parser) when initializing
	   parsers. It also updates `slave' and `master' fields.

	   If the value other than `LANG_AUTO' is specified,
	   the main part does nothing. */
	langType syncWith;
	kindDefinition *slave;
	kindDefinition *master;
};

#define ATTACH_ROLES(RS) .nRoles = ARRAY_SIZE(RS), .roles = RS
#define ATTACH_SEPARATORS(S) .separators = S, .separatorCount = ARRAY_SIZE(S)

/*
*   FUNCTION PROTOTYPES
*/

extern const char *scopeSeparatorFor (langType lang, int kindIndex, int parentKindIndex);

#endif	/* CTAGS_MAIN_KIND_H */
