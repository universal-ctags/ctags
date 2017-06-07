/*
 *
 *  Copyright (c) 2015, Red Hat, Inc.
 *  Copyright (c) 2015, Masatake YAMATO
 *
 *  Author: Masatake YAMATO <yamato@redhat.com>
 *
 *   This source code is released for free distribution under the terms of the
 *   GNU General Public License version 2 or (at your option) any later version.
 *
 */

#include "general.h"

#include <stdio.h>
#include <string.h>
#include "debug.h"
#include "kind.h"
#include "parse.h"
#include "options.h"

typedef struct sKindObject {
	kindDefinition *def;
	freeKindDefFunc free;
} kindObject;

struct kindControlBlock {
	kindObject *kind;
	unsigned int count;
	langType owner;
};

extern void printRole (const roleDesc* const role)
{
	if (role)
		printf ("%s\t%s\t%s\n", role->name, role->description, role->enabled? "on": "off");
}

extern const char *renderRole (const roleDesc* const role, vString* b)
{
	vStringCatS (b, role->name);
	return vStringValue (b);
}

#define PR_KIND_WIDTH_LETTER         7
#define PR_KIND_WIDTH_NAME          15
#define PR_KIND_WIDTH_DESCRIPTION   30
#define PR_KIND_WIDTH_ENABLED        8
#define PR_KIND_WIDTH_REFONLY        7
#define PR_KIND_WIDTH_NROLE          6
#define PR_KIND_WIDTH_MASTER	    10
#define MAKE_KIND_FMT(PREFIX,LETTER_SPEC,NROLL_SPEC)		\
	PREFIX							\
	PR_KIND_FMT (LETTER,LETTER_SPEC)			\
	" "							\
	PR_KIND_FMT (NAME,s)					\
	" "							\
	PR_KIND_FMT (ENABLED,s)					\
	" "							\
	PR_KIND_FMT (REFONLY,s)					\
	" "							\
	PR_KIND_FMT (NROLE,NROLL_SPEC)				\
	" "							\
	PR_KIND_FMT (MASTER,s)					\
	" "							\
	PR_KIND_FMT (DESCRIPTION,s)				\
	"\n"

extern void printKindListHeader (bool indent, bool tabSeparated)
{
#define KIND_HEADER_COMMON_FMT MAKE_KIND_FMT("%s", s, s)

	const char *fmt = tabSeparated
		? "%s%s%s\t%s\t%s\t%s\t%s\t%s\t%s\n"
		: (indent
		   ? PR_KIND_FMT (LANG,s) KIND_HEADER_COMMON_FMT
		   : "%s"                 KIND_HEADER_COMMON_FMT)
		;

	printf (fmt,
		(indent? "#PARSER": ""),
		(indent? (tabSeparated? "\t": " "): ""),
		(indent? "LETTER": "#LETTER"),
		"NAME",
		"ENABLED",
		"REFONLY",
		"NROLES",
		"MASTER",
		"DESCRIPTION");

#undef KIND_HEADER_COMMON_FMT
}

extern void printKind (const kindDefinition* const kind, bool allKindFields, bool indent,
		       bool tabSeparated)
{
#define KIND_FMT MAKE_KIND_FMT("", c, d)

	if (allKindFields)
	{
		printf ((tabSeparated
			 ?"%s%c\t%s\t%s\t%s\t%d\t%s\t%s\n"
			 :"%s" KIND_FMT),
			(indent? (tabSeparated? "\t": " "): ""),
			kind->letter,
			kind->name        != NULL ? kind->name        : "",
			kind->enabled             ? "on"              : "off",
			kind->referenceOnly       ? "TRUE"            : "FALSE",
			kind->nRoles,
			(kind->master
			 || kind->slave ) ? getLanguageName (kind->syncWith): "",
			kind->description != NULL ? kind->description : "");
	}
	else if (!kind->referenceOnly)
	{
		printf ("%s%c  %s%s\n", indent ? "    " : "", kind->letter,
			kind->description != NULL ? kind->description :
			(kind->name != NULL ? kind->name : ""),
			kind->enabled ? "" : " [off]");
	}

#undef KIND_FMT
}

const char *scopeSeparatorFor (const kindDefinition *kind, char parentLetter)
{
	scopeSeparator *table;
	Assert (kind);
	table = kind->separators;

	/* If no table is given, use the default generic separator ".".
	   The exception is if a root separator is looked up. In this case,
	   return NULL to notify there is no root separator to the caller. */

	if (table == NULL)
	{
		if (parentLetter == KIND_NULL)
			return NULL;
		else
			return ".";
	}

	while (table - kind->separators < kind->separatorCount)
	{
		/* KIND_WILDCARD cannot be used as a key for finding
		   a root separator.*/
		if ( (table->parentLetter == KIND_WILDCARD
		       && parentLetter != KIND_NULL)
		    || table->parentLetter == parentLetter)
			return table->separator;
		table++;
	}
	if (parentLetter == KIND_NULL)
		return NULL;
	else
		return ".";
}

extern void enableKind (kindDefinition *kind, bool enable)
{
	kindDefinition *slave;

	if (kind->master)
		enableKind (kind->master, enable);
	else
	{
		kind->enabled = enable;
		for (slave = kind->slave; slave; slave = slave->slave)
			slave->enabled = enable;
	}
}

extern struct kindControlBlock* allocKindControlBlock (parserDefinition *parser)
{
	unsigned int i;
	struct kindControlBlock *kcb;

	kcb = xMalloc (1, struct kindControlBlock);
	kcb->kind = xMalloc (parser->kindCount, kindObject);
	kcb->count = parser->kindCount;
	kcb->owner = parser->id;

	for (i = 0; i < parser->kindCount; ++i)
	{
		kcb->kind [i].def = parser->kindTable + i;
		kcb->kind [i].free = NULL;
		kcb->kind [i].def->id = i;
	}

	return kcb;
}

extern void freeKindControlBlock (struct kindControlBlock* kcb)
{
	unsigned int i;

	for (i = 0; i < kcb->count; ++i)
	{
		if (kcb->kind [i].free)
			kcb->kind [i].free (kcb->kind [i].def);
	}
	eFree (kcb->kind);
	eFree (kcb);
}

extern int  defineKind (struct kindControlBlock* kcb, kindDefinition *def,
						freeKindDefFunc freeKindDef)
{
	def->id = kcb->count++;
	kcb->kind = xRealloc (kcb->kind, kcb->count, kindObject);
	kcb->kind [def->id].def = def;
	kcb->kind [def->id].free = freeKindDef;

	verbose ("Add kind[%d] \"%c,%s,%s\" to %s\n", def->id,
			 def->letter, def->name, def->description,
			 getLanguageName (kcb->owner));

	return def->id;
}

extern unsigned int countKinds (struct kindControlBlock* kcb)
{
	return kcb->count;
}

extern kindDefinition *getKind (struct kindControlBlock* kcb, int kindIndex)
{
	return kcb->kind [kindIndex].def;
}

extern kindDefinition *getKindForLetter (struct kindControlBlock* kcb, int letter)
{
	unsigned int i;
	kindDefinition * kdef;

	for (i = 0;  i < countKinds (kcb);  ++i)
	{
		kdef = getKind (kcb, i);
		if (kdef->letter == letter)
			return kdef;
	}
	return NULL;
}

extern kindDefinition *getKindForName (struct kindControlBlock* kcb, const char* name)
{
	unsigned int i;
	kindDefinition * kdef;

	for (i = 0;  i < countKinds (kcb);  ++i)
	{
		kdef = getKind (kcb, i);
		Assert(kdef);
		if (kdef->name && (strcmp(kdef->name, name) == 0))
			return kdef;
	}
	return NULL;
}

static void linkKinds (langType master, kindDefinition *masterKind, kindDefinition *slaveKind)
{
	kindDefinition *tail;

	slaveKind->master = masterKind;

	tail = slaveKind;
	while (tail->slave)
	{
		tail->enabled = masterKind->enabled;
		tail = tail->slave;
	}

	tail->slave = masterKind->slave;
	masterKind->slave = slaveKind;

	masterKind->syncWith = master;
	slaveKind->syncWith = master;
}

extern void linkKindDependency (struct kindControlBlock *masterKCB,
								struct kindControlBlock *slaveKCB)
{
	unsigned int k_slave, k_master;
	kindDefinition *kind_slave, *kind_master;

	for (k_slave = 0; k_slave < countKinds (slaveKCB); k_slave++)
	{
		kind_slave = getKind(slaveKCB, k_slave);
		if (kind_slave->syncWith == LANG_AUTO)
		{
			for (k_master = 0; k_master < countKinds (masterKCB); k_master++)
			{
				kind_master = getKind(masterKCB, k_master);
				if ((kind_slave->letter == kind_master->letter)
				    && (strcmp (kind_slave->name, kind_master->name) == 0))
				{
					linkKinds (masterKCB->owner, kind_master, kind_slave);
					break;
				}
			}
		}
	}
}

#ifdef DEBUG
extern bool doesParserUseKind (struct kindControlBlock* kcb, char letter)
{
	unsigned int k;
	kindDefinition *kdef;

	for (k = 0; k < countKinds (kcb); k++)
	{
		kdef = getKind(kcb, k);
		if (kdef->letter == letter)
			return true;
	}
	return false;
}
#endif
