/*
 *
 *  Copyright (c) 2017, Red Hat, Inc.
 *  Copyright (c) 2017, Masatake YAMATO
 *
 *  Author: Masatake YAMATO <yamato@redhat.com>
 *
 *   This source code is released for free distribution under the terms of the
 *   GNU General Public License version 2 or (at your option) any later version.
 *
 */
#ifndef CTAGS_MAIN_SUBPARSER_H
#define CTAGS_MAIN_SUBPARSER_H

#include "general.h"

#include "colprint.h"
#include "dependency.h"
#include "types.h"


typedef enum eSubparserRunDirection {
	SUBPARSER_UNKNOWN_DIRECTION =  0,
	SUBPARSER_BASE_RUNS_SUB = 1 << 0,
	SUBPARSER_SUB_RUNS_BASE = 1 << 1,
	SUBPARSER_BI_DIRECTION  = SUBPARSER_BASE_RUNS_SUB|SUBPARSER_SUB_RUNS_BASE,
} subparserRunDirection;

struct sSubparser {
	/* private in the main part */
	slaveParser *slaveParser;
	subparser *next;
	bool schedulingBaseparserExplicitly;
	bool chosenAsExclusiveSubparser;

	/* public to the parser */
	subparserRunDirection direction;

	void (* inputStart) (subparser *s);
	void (* inputEnd) (subparser *s);
	void (* exclusiveSubparserChosenNotify) (subparser *s, void *data);
	void (* makeTagEntryNotify) (subparser *s, const tagEntryInfo *tag, int corkIndex);
};

/* A base parser doesn't have to call the following three functions.
   The main part calls them internally. */
extern void notifyInputStart (void);
extern void notifyInputEnd   (void);
extern void notifyMakeTagEntry (const tagEntryInfo *info, int corkIndex);

extern langType getSubparserLanguage (subparser *s);

/* Interface for Baseparser */
extern subparser *getNextSubparser(subparser *last, bool includingNoneCraftedParser);
#define foreachSubparser(VAR, INCLUDING_NONE_CRAFTED_PARSER)\
	VAR = NULL;								\
	while ((VAR = getNextSubparser (VAR, INCLUDING_NONE_CRAFTED_PARSER)) != NULL)

extern void enterSubparser(subparser *subparser);
extern void leaveSubparser(void);

extern subparser* getSubparserRunningBaseparser (void);
extern void chooseExclusiveSubparser (subparser *s, void *data);

/* Interface for Subparsers   */
#define RUN_DEFAULT_SUBPARSERS -1
extern void scheduleRunningBaseparser (int dependencyIndex);

extern subparser *getFirstSubparser(struct slaveControlBlock *controlBlock);
extern void useDefaultSubparsers (struct slaveControlBlock *controlBlock);
extern void useSpecifiedSubparser (struct slaveControlBlock *controlBlock, subparser *s);
extern void setupSubparsersInUse (struct slaveControlBlock *controlBlock);
extern subparser* teardownSubparsersInUse (struct slaveControlBlock *controlBlock);

extern struct colprintTable * subparserColprintTableNew (void);
extern void subparserColprintAddSubparsers (struct colprintTable *table,
											struct slaveControlBlock *scb);
extern void subparserColprintTablePrint (struct colprintTable *table,
										 bool withListHeader, bool machinable, FILE *fp);

#endif	/* CTAGS_MAIN_SUBPARSER_H */
