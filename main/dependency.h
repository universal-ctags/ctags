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
#ifndef CTAGS_MAIN_DEPENDENCY_H
#define CTAGS_MAIN_DEPENDENCY_H

#include "general.h"

#include "types.h"
#include "kind.h"

typedef enum eDepType {
	DEPTYPE_KIND_OWNER,
	DEPTYPE_SUBPARSER,
	COUNT_DEPTYPES,
} depType;

typedef struct sParserDependency {
	depType type;
	const char *upperParser;
	void *data;
} parserDependency;

struct sSlaveParser {
	depType type;
	langType id;
	void *data;
	slaveParser *next;
};

struct slaveControlBlock;	/* Opaque data type for parse.c */


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
#define foreachSlaveParser(VAR)			\
	VAR = NULL;								\
	while ((VAR = getNextSlaveParser (VAR)) != NULL)

#endif	/* CTAGS_MAIN_DEPENDENCY_H */
