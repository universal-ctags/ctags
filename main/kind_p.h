/*
*   Copyright (c) 1998-2003, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*/
#ifndef CTAGS_MAIN_KIND_PRIVATE_H
#define CTAGS_MAIN_KIND_PRIVATE_H

/*
*   INCLUDE FILES
*/

#include "general.h"
#include "kind.h"
#include "vstring.h"


/*
*   DATA DECLARATIONS
*/

struct kindControlBlock;
typedef void (* freeKindDefFunc) (kindDefinition *);
typedef void (* freeRoleDefFunc) (roleDefinition *);


/*
*   FUNCTION PROTOTYPES
*/
extern void enableKind (kindDefinition *kind, bool enable);
extern void enableRole (roleDefinition *role, bool enable);
extern const char *renderRole (const roleDefinition* const def, vString* b);

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
extern kindDefinition *getKindForLetter (struct kindControlBlock* kcb, char letter);
extern kindDefinition *getKindForName (struct kindControlBlock* kcb, const char* name);
extern int getKindIndexForLetter (struct kindControlBlock* kcb, char letter);
extern int getKindIndexForName (struct kindControlBlock* kcb, const char* name);
extern roleDefinition* getRole(struct kindControlBlock* kcb, int kindIndex, int roleIndex);
extern roleDefinition* getRoleForName(struct kindControlBlock* kcb, int kindIndex, const char* name);
extern void linkKindDependency (struct kindControlBlock *masterKCB,
								struct kindControlBlock *slaveKCB);

extern int defineScopeSeparator(struct kindControlBlock* kcb,
								int kindIndex,
								int parentKindIndex, const char *separator);
extern const  scopeSeparator *getScopeSeparator(struct kindControlBlock* kcb, int kindIndex, int parentKindIndex);

/* for the obsolete --list-kinds option */
extern void printKind (const kindDefinition* const kind, bool indent);

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

#endif	/* CTAGS_MAIN_KIND_PRIVATE_H */
