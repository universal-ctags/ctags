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
#ifndef CTAGS_MAIN_DEPENDENCY_PRIVATE_H
#define CTAGS_MAIN_DEPENDENCY_PRIVATE_H

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include "dependency.h"
#include "kind.h"
#include "types.h"

/*
*   MACROS
*/
#define foreachSlaveParser(VAR)			\
	VAR = NULL;								\
	while ((VAR = getNextSlaveParser (VAR)) != NULL)


/*
*   DATA DECLARATIONS
*/
struct slaveControlBlock;	/* Opaque data type for parse.c */

/*
*   FUNCTION PROTOTYPES
*/
extern void linkDependencyAtInitializeParsing (depType dtype,
						   parserDefinition *const master,
						   struct slaveControlBlock *masterSCB,
						   struct kindControlBlock *masterKCB,
						   parserDefinition *const slave,
						   struct kindControlBlock *slaveKCB,
						   void *data);

extern struct slaveControlBlock *allocSlaveControlBlock (parserDefinition *parser);
extern void freeSlaveControlBlock (struct slaveControlBlock *cb);
extern void initializeDependencies (parserDefinition *parser,
									struct slaveControlBlock *cb);
extern void finalizeDependencies (parserDefinition *parser,
								  struct slaveControlBlock *cb);

extern slaveParser *getFirstSlaveParser(struct slaveControlBlock *controlBlock);
extern slaveParser *getNextSlaveParser(slaveParser *last);

#endif	/* CTAGS_MAIN_DEPENDENCY_PRIVATE_H */
