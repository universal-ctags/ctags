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
#include "subparser.h"

#include <string.h>

static void linkKinds (kindOption *masterKind, kindOption *slaveKind)
{
	kindOption *tail;

	slaveKind->master = masterKind;

	tail = slaveKind;
	while (tail->slave)
	{
		tail->enabled = masterKind->enabled;
		tail = tail->slave;
	}

	tail->slave = masterKind->slave;
	masterKind->slave = slaveKind;
}

static void linkKindDependency (parserDefinition *const masterParser,
				parserDefinition *const slaveParser)
{
	unsigned int k_slave, k_master;
	kindOption *kind_slave, *kind_master;

	for (k_slave = 0; k_slave < slaveParser->kindCount; k_slave++)
	{
		if (slaveParser->kinds [k_slave].syncWith == LANG_AUTO)
		{
			kind_slave = slaveParser->kinds + k_slave;
			for (k_master = 0; k_master < masterParser->kindCount; k_master++)
			{
				kind_master = masterParser->kinds + k_master;
				if ((kind_slave->letter == kind_master->letter)
				    && (strcmp (kind_slave->name, kind_master->name) == 0))
				{
					linkKinds (kind_master, kind_slave);
					kind_slave->syncWith = masterParser->id;
					kind_master->syncWith = masterParser->id;
					break;
				}
			}
		}
	}
}

extern void linkDependencyAtInitializeParsing (depType dtype,
						   parserDefinition *const master,
						   parserDefinition *const slave,
						   void *data)
{
	if (dtype == DEPTYPE_KIND_OWNER)
		linkKindDependency (master, slave);
	else if (dtype == DEPTYPE_SUBPARSER)
	{
		slaveParser *s = xMalloc (1, slaveParser);

		s->type = dtype;
		s->id = slave->id;
		s->data = data;
		s->next = master->slaveParsers;
		master->slaveParsers = s;
	}
}

extern void initializeDependencies (parserDefinition *parser)
{
	unsigned int i;
	slaveParser *sp;

	/* Initialize slaves */
	for (sp = parser->slaveParsers; sp; sp = sp->next)
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
			if (sp->type == DEPTYPE_SUBPARSER)
			{
				subparser *subparser = sp->data;
				subparser->next = parser->subparsersDefault;
				parser->subparsersDefault = subparser;
			}
		}
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

extern void finalizeDependencies (parserDefinition *parser)
{
	slaveParser *sp;
	slaveParser *tmp;

	for (sp = parser->slaveParsers; sp;)
	{
		tmp = sp;
		sp = sp->next;
		tmp->next = NULL;
		eFree (tmp);
	}
	parser->slaveParsers = NULL;
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
