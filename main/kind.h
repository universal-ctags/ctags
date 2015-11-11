/*
*   Copyright (c) 1998-2003, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*/
#ifndef CTAGS_MAIN_KIND_H
#define CTAGS_MAIN_KIND_H

#include "general.h"
#include "vstring.h"

#define RoleTemplateGeneric { TRUE, "generic", "non-categorized generic role" }

typedef struct sRoleDesc {
	boolean enabled;
	const char* name;		  /* role name */
	const char* description;	  /* displayed in --help output */
} roleDesc;

struct sTagEntryInfo;
typedef struct sTagEntryInfo tagEntryInfo;
extern void printRole (const roleDesc* const role); /* for --help */
extern const char *renderRole (const roleDesc* const role, const tagEntryInfo *const tag, vString* b);

/*
 * Predefined kinds
 */
#define KIND_REGEX_DEFAULT 'r'
#define KIND_REGEX_DEFAULT_LONG "regex"
/* We treat ' ' as a ghost kind.
   It will never be listed up in --list-kinds. */

#define KIND_NULL    '\0'

#define KIND_GHOST   ' '
#define KIND_GHOST_LONG "ghost"

#define KIND_FILE_DEFAULT 'F'
#define KIND_FILE_DEFAULT_LONG "file"

#define KIND_FILE_ALT '!'

#define KIND_GENERIC_REFERENCE '@'
#define KIND_GENERIC_REFERENCE_DEFAULT_LONG "reference"

typedef struct sKindOption {
	boolean enabled;          /* are tags for kind enabled? */
	char  letter;               /* kind letter */
	const char* name;		  /* kind name */
	const char* description;	  /* displayed in --help output */
	boolean referenceOnly;
	int nRoles;		/* The number of role elements. */
	roleDesc *roles;
} kindOption;

#define ATTACH_ROLES(RS) .nRoles = ARRAY_SIZE(RS), .roles = RS
extern void printKind (const kindOption* const kind, boolean allKindFields, boolean indent);
#endif	/* CTAGS_MAIN_KIND_H */
