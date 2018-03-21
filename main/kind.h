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
#include "types.h"
#include "routines.h"
#include "vstring.h"

struct sRoleDefinition {
	bool enabled;
	char* name;		  /* role name */
	char* description;	  /* displayed in --help output */

	int id;
};

typedef void (* freeRoleDefFunc) (roleDefinition *);
extern const char *renderRole (const roleDefinition* const def, vString* b);

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

/* for the obsolete --list-kinds option */
extern void printKind (const kindDefinition* const kind, bool indent);

extern const char *scopeSeparatorFor (langType lang, int kindIndex, int parentKindIndex);

extern void enableKind (kindDefinition *kind, bool enable);

struct kindControlBlock;
typedef void (* freeKindDefFunc) (kindDefinition *);
extern struct kindControlBlock* allocKindControlBlock (parserDefinition *parser);
extern void freeKindControlBlock (struct kindControlBlock* kcb);
extern int  defineKind (struct kindControlBlock* kcb, kindDefinition *def,
						freeKindDefFunc freeKindDef);
extern int defineRole (struct kindControlBlock* kcb, int kindIndex,
					   roleDefinition *def, freeRoleDefFunc freeRoleDef);
extern bool isRoleEnabled (struct kindControlBlock* kcb, int kindIndex, int roleIndex);

extern unsigned int countKinds (struct kindControlBlock* kcb);
extern unsigned int countRoles (struct kindControlBlock* kcb, int kindIndex);
extern kindDefinition *getKind (struct kindControlBlock* kcb, int kindIndex);
extern kindDefinition *getKindForLetter (struct kindControlBlock* kcb, int letter);
extern kindDefinition *getKindForName (struct kindControlBlock* kcb, const char* name);
extern roleDefinition* getRole(struct kindControlBlock* kcb, int kindIndex, int roleIndex);
extern roleDefinition* getRoleForName(struct kindControlBlock* kcb, int kindIndex, const char* name);
extern void linkKindDependency (struct kindControlBlock *masterKCB,
								struct kindControlBlock *slaveKCB);

/* for --list-kinds-full option. LANGUAGE must be initialized. */
extern struct colprintTable * kindColprintTableNew (void);
extern void kindColprintAddLanguageLines (struct colprintTable *table,
										  struct kindControlBlock* kcb);
extern void kindColprintTablePrint (struct colprintTable *table, bool noparser,
									bool withListHeader, bool machinable, FILE *fp);

extern struct colprintTable * roleColprintTableNew (void);
extern void roleColprintAddRoles (struct colprintTable *table,
								  struct kindControlBlock* kcb,
								  const char *kindspecs);
extern void roleColprintTablePrint (struct colprintTable *table, bool noparser,
									bool withListHeader, bool machinable, FILE *fp);

#ifdef DEBUG
extern bool doesParserUseKind (struct kindControlBlock* kcb, char letter);
#endif

#endif	/* CTAGS_MAIN_KIND_H */
