/*
*
*
*   Copyright (c) 2000-2003, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   Defines external interface to option processing.
*/
#ifndef CTAGS_MAIN_FLAGS_PRIVATE_H
#define CTAGS_MAIN_FLAGS_PRIVATE_H

#include "general.h"
#include "colprint_p.h"


#define LONG_FLAGS_OPEN  '{'
#define LONG_FLAGS_CLOSE '}'

typedef struct sFlagDefinition {
	char shortChar;
	const char *longStr;
	void (* shortProc) (char c,  void *data);
	void (* longProc)  (const char* const s, const char* const param, void *data);
	const char *paramName;
	const char *description;
} flagDefinition;

/* Return {{optscript}} part. */
extern const char*  flagsEval (const char* flags, flagDefinition* defs, unsigned int ndefs, void* data);
extern struct colprintTable * flagsColprintTableNew (void);
extern void flagsColprintAddDefinitions (struct colprintTable *table, flagDefinition* def, unsigned int ndefs);
extern void flagsColprintTablePrint (struct colprintTable *table,
									 bool withListHeader, bool machinable, FILE *fp);
#endif	/* CTAGS_MAIN_FLAGS_PRIVATE_H */
