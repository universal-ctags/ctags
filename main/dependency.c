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

#include "general.h"  /* must always come first */

#include "debug.h"
#include "dependency.h"
#include "options.h"
#include "parse.h"
#include "read.h"
#include "subparser.h"

#include <string.h>

struct slaveControlBlock {
	slaveParser *slaveParsers;	/* The parsers on this list must be initialized when
								   this parser is initialized. */
	subparser   *subparsersDefault;
	subparser   *subparsersInUse;
	langType     owner;
};

extern void linkDependencyAtInitializeParsing (depType dtype,
						   parserDefinition *const master,
						   struct slaveControlBlock *masterSCB,
						   struct kindControlBlock *masterKCB,
						   parserDefinition *const slave,
						   struct kindControlBlock *slaveKCB,
						   void *data)
{
	if (dtype == DEPTYPE_KIND_OWNER)
		linkKindDependency (masterKCB, slaveKCB);
	else if (dtype == DEPTYPE_SUBPARSER)
	{
		slaveParser *s = xMalloc (1, slaveParser);

		s->type = dtype;
		s->id = slave->id;
		s->data = data;

		s->next = masterSCB->slaveParsers;
		masterSCB->slaveParsers = s;
	}
}

static void attachSubparser (struct slaveControlBlock *base_sb, subparser *subparser)
{
	   subparser->next = base_sb->subparsersDefault;
	   base_sb->subparsersDefault = subparser;
}


extern struct slaveControlBlock *allocSlaveControlBlock (parserDefinition *parser)
{
	struct slaveControlBlock *cb;

	cb = xMalloc (1, struct slaveControlBlock);
	cb->slaveParsers = NULL;
	cb->subparsersDefault = NULL;
	cb->subparsersInUse = NULL;
	cb->owner = parser->id;

	return cb;
}

extern void freeSlaveControlBlock (struct slaveControlBlock *cb)
{
	eFree (cb);
}

extern void initializeDependencies (parserDefinition *parser,
									struct slaveControlBlock *cb)
{
	unsigned int i;
	slaveParser *sp;

	/* Initialize slaves */
	sp = cb->slaveParsers;
	while (sp != NULL)
	{
		if (sp->type == DEPTYPE_SUBPARSER)
		{
			subparser *sub;

			sub = (subparser *)sp->data;
			sub->slaveParser = sp;
		}

		if (sp->type == DEPTYPE_KIND_OWNER
			|| (sp->type == DEPTYPE_SUBPARSER &&
				(((subparser *)sp->data)->direction & SUBPARSER_BASE_RUNS_SUB)))
		{
			initializeParser (sp->id);
			if (sp->type == DEPTYPE_SUBPARSER
				&& isXtagEnabled (XTAG_TAGS_GENERATED_BY_SUBPARSER))
			{
				subparser *subparser = sp->data;
				attachSubparser (cb, subparser);
			}
		}
		sp = sp->next;
	}

	/* Initialize masters that act as base parsers. */
	for (i = 0; i < parser->dependencyCount; i++)
	{
		parserDependency *d = parser->dependencies + i;
		if (d->type == DEPTYPE_SUBPARSER &&
			((subparser *)(d->data))->direction & SUBPARSER_SUB_RUNS_BASE)
		{
			langType baseParser;
			baseParser = getNamedLanguage (d->upperParser, 0);
			Assert (baseParser != LANG_IGNORE);
			initializeParser (baseParser);
		}
	}
}

extern void finalizeDependencies (parserDefinition *parser,
								  struct slaveControlBlock *cb)
{
	while (cb->slaveParsers)
	{
		slaveParser *sp = cb->slaveParsers;
		cb->slaveParsers = sp->next;
		sp->next = NULL;
		eFree (sp);
	}
}

extern void notifyInputStart (void)
{
	subparser *s;

	foreachSubparser(s, false)
	{
		if (s->inputStart)
		{
			enterSubparser(s);
			s->inputStart (s);
			leaveSubparser();
		}
	}
}

extern void notifyInputEnd   (void)
{
	subparser *s;

	foreachSubparser(s, false)
	{
		if (s->inputEnd)
		{
			enterSubparser(s);
			s->inputEnd (s);
			leaveSubparser();
		}
	}
}

extern langType getSubparserLanguage (subparser *s)
{
	return s->slaveParser->id;
}

extern void chooseExclusiveSubparser (subparser *s, void *data)
{
	if (s->exclusiveSubparserChosenNotify)
	{
		s->chosenAsExclusiveSubparser = true;
		enterSubparser(s);
		s->exclusiveSubparserChosenNotify (s, data);
		verbose ("%s is chosen as exclusive subparser\n",
				 getLanguageName (getSubparserLanguage (s)));
		leaveSubparser();
	}
}

extern subparser *getFirstSubparser(struct slaveControlBlock *controlBlock)
{
	if (controlBlock)
		return controlBlock->subparsersInUse;
	return NULL;
}

extern void useDefaultSubparsers (struct slaveControlBlock *controlBlock)
{
	controlBlock->subparsersInUse = controlBlock->subparsersDefault;
}

extern void useSpecifiedSubparser (struct slaveControlBlock *controlBlock, subparser *s)
{
	s->schedulingBaseparserExplicitly = true;
	controlBlock->subparsersInUse = s;
}

extern void setupSubparsersInUse (struct slaveControlBlock *controlBlock)
{
	if (!controlBlock->subparsersInUse)
		useDefaultSubparsers(controlBlock);
}

extern subparser* teardownSubparsersInUse (struct slaveControlBlock *controlBlock)
{
	subparser *tmp;
	subparser *s = NULL;

	tmp = controlBlock->subparsersInUse;
	controlBlock->subparsersInUse = NULL;

	if (tmp && tmp->schedulingBaseparserExplicitly)
	{
		tmp->schedulingBaseparserExplicitly = false;
		s = tmp;
	}

	if (s)
		return s;

	while (tmp)
	{
		if (tmp->chosenAsExclusiveSubparser)
		{
			s = tmp;
		}
		tmp = tmp->next;
	}

	return s;
}


static int subparserDepth;

extern void enterSubparser(subparser *subparser)
{
	subparserDepth++;
	pushLanguage (getSubparserLanguage (subparser));
}

extern void leaveSubparser(void)
{
	popLanguage ();
	subparserDepth--;
}

extern bool doesSubparserRun (void)
{
	return subparserDepth;
}

extern slaveParser *getFirstSlaveParser (struct slaveControlBlock *scb)
{
	if (scb)
		return scb->slaveParsers;
	return NULL;
}

#define PR_SUBPARSER_WIDTH_NAME          30
#define PR_SUBPARSER_WIDTH_BASE_NAME     20
#define PR_SUBPARSER_WIDTH_DIRECTION 9

#define PR_SUBPARSER_STR(X) PR_SUBPARSER_WIDTH_##X
#define PR_SUBPARSER_FMT(X,T) "%-" STRINGIFY(PR_SUBPARSER_STR(X)) STRINGIFY(T)

#define MAKE_SUBPARSER_FMT()					\
	PR_SUBPARSER_FMT (NAME,s)					\
	" "											\
	PR_SUBPARSER_FMT (BASE_NAME,s)				\
	" "											\
	PR_SUBPARSER_FMT (DIRECTION,s)				\
	"\n"

extern void printSubparserListHeader (bool machinable)
{
	if (Option.withListHeader)
		printf ((machinable? "%s\t%s\t%s\n": MAKE_SUBPARSER_FMT()),
				"#NAME", "BASEPARSER", "DIRECTION");
}

extern void printSubparsers (struct slaveControlBlock *scb, bool machinable)
{
	slaveParser *tmp;

	pushLanguage (scb->owner);
	foreachSlaveParser(tmp)
	{
		if (tmp->type == DEPTYPE_SUBPARSER)
		{
			char *direction;

			switch (((subparser *)tmp->data)->direction)
			{
			case SUBPARSER_BASE_RUNS_SUB:
				direction = "base => sub {shared}";
				break;
			case SUBPARSER_SUB_RUNS_BASE:
				direction = "base <= sub {dedicated}";
				break;
			case SUBPARSER_BI_DIRECTION:
				direction = "base <> sub {bidirectional}";
				break;
			default:
				direction  = "UNKNOWN(INTERNAL BUG)";
				break;
			}

			printf ((machinable? "%s\t%s\t%s\n": MAKE_SUBPARSER_FMT()),
					getLanguageName (tmp->id),
					getLanguageName (scb->owner), direction);
		}
	}
	popLanguage ();
}
