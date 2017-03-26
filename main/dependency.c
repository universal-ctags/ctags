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


extern struct slaveControlBlock *allocSlaveControlBlock (void)
{
	struct slaveControlBlock *cb;

	cb = xMalloc (1, struct slaveControlBlock);
	cb->slaveParsers = NULL;
	cb->subparsersDefault = NULL;
	cb->subparsersInUse = NULL;

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
	slaveParser *sp = NULL;

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

	foreachSubparser(s)
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

	foreachSubparser(s)
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
