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
#define KIND_REGEX_DEFAULT 'r'
#define KIND_REGEX_DEFAULT_LONG "regex"
/* We treat ' ' as a ghost kind.
   It will never be listed up in --list-kinds. */

#define KIND_NULL    '\0'

#define KIND_GHOST_INDEX -1
#define KIND_GHOST   ' '
#define KIND_GHOST_LONG "ghost"

#define KIND_FILE_INDEX -2
#define KIND_FILE_DEFAULT 'F'
#define KIND_FILE_DEFAULT_LONG "file"

#define KIND_WILDCARD_INDEX -3
#define KIND_WILDCARD '*'

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
