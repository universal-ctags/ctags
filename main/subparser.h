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

#include "dependency.h"
#include "types.h"

/*
*   MACROS
*/
#define foreachSubparser(VAR, INCLUDING_NONE_CRAFTED_PARSER)\
	VAR = NULL;								\
	while ((VAR = getNextSubparser (VAR, INCLUDING_NONE_CRAFTED_PARSER)) != NULL)

/*
*   DATA DECLARATIONS
*/
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

/*
*   FUNCTION PROTOTYPES
*/

/* Interface for Baseparser */
extern subparser *getNextSubparser(subparser *last, bool includingNoneCraftedParser);
extern void enterSubparser(subparser *subparser);
extern void leaveSubparser(void);
extern subparser* getSubparserRunningBaseparser (void);
extern void chooseExclusiveSubparser (subparser *s, void *data);

extern subparser *getLanguageSubparser (langType sublang, bool including_none_crafted_parser);

/* Interface for Subparsers   */
#define RUN_DEFAULT_SUBPARSERS -1
extern void scheduleRunningBaseparser (int dependencyIndex);

#endif	/* CTAGS_MAIN_SUBPARSER_H */
