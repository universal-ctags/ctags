/*
 *
 *  Copyright (c) 2016, Red Hat, Inc.
 *  Copyright (c) 2016, Masatake YAMATO
 *
 *  Author: Masatake YAMATO <yamato@redhat.com>
 *
 *   This source code is released for free distribution under the terms of the
 *   GNU General Public License version 2 or (at your option) any later version.
 *
 */
#ifndef CTAGS_MAIN_PARAM_PRIVATE_H
#define CTAGS_MAIN_PARAM_PRIVATE_H

/*
*   INCLUDE FILES
*/
#include "general.h"

#include "types.h"
#include "colprint_p.h"


/*
*   DATA DECLARATIONS
*/
struct paramControlBlock;
typedef void (* freeParamDefFunc) (paramDefinition *);


/*
*   FUNCTION PROTOTYPES
*/

extern void applyParameter (const langType language, const char *name, const char *args);

extern struct colprintTable * paramColprintTableNew (void);
extern void paramColprintAddParams (struct colprintTable *table,
									struct paramControlBlock* pcb);
extern void paramColprintTablePrint (struct colprintTable *table, bool noparser,
									bool withListHeader, bool machinable, FILE *fp);

extern struct paramControlBlock* allocParamControlBlock (parserDefinition *parser);
extern void freeParamControlBlock (struct paramControlBlock* pcb);
extern int  defineParam (struct paramControlBlock* pcb, paramDefinition *def,
						 freeParamDefFunc freeParamDef);
extern void applyParam (struct paramControlBlock* pcb, const char *name, const char *args);

#endif	/* CTAGS_MAIN_PARAM_PRIVATE_H */
