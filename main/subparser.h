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

#include "types.h"
#include "read.h"


typedef enum eSubparserRunDirection {
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
};

/* A base parser doesn't have to call the following two functions.
   The main part calls them internally. */
extern void notifyInputStart (void);
extern void notifyInputEnd   (void);
extern langType getSubparserLanguage (subparser *s);

/* Interface for Baseparser */
extern subparser *getNextSubparser(subparser *last);
#define foreachSubparser(VAR)				\
	VAR = NULL;								\
	while ((VAR = getNextSubparser (VAR)) != NULL)

#define enterSubparser(S) pushLanguage (getSubparserLanguage ((subparser *)S))
#define leaveSubparser()  popLanguage ()

extern subparser* getSubparserRunningBaseparser (void);
extern void chooseExclusiveSubparser (subparser *s, void *data);

/* Interface for Subparsers   */
#define RUN_DEFAULT_SUBPARSERS -1
extern void scheduleRunningBaseparser (int dependencyIndex);

extern void attachSubparser (langType base, subparser *sub);

#endif	/* CTAGS_MAIN_SUBPARSER_H */
