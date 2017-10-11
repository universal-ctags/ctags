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

#include "ctags.h"
#include "debug.h"
#include "kind.h"
#include "parse.h"
#include "options.h"
#include "vstring.h"

typedef struct sKindObject {
	kindDefinition *def;
	freeKindDefFunc free;
} kindObject;

struct kindControlBlock {
	kindObject *kind;
	unsigned int count;
	langType owner;
};

extern const char *renderRole (const roleDesc* const role, vString* b)
{
	vStringCatS (b, role->name);
	return vStringValue (b);
}

extern void printKind (const kindDefinition* const kind, bool indent)
{
	printf ("%s%c  %s%s\n", indent ? "    " : "", kind->letter,
			kind->description != NULL ? kind->description :
			(kind->name != NULL ? kind->name : ""),
			kind->enabled ? "" : " [off]");
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

extern bool isKindEnabled (struct kindControlBlock* kcb, int kindIndex)
{
	return kcb->kind [kindIndex].def->enabled;
}

extern bool isRoleEnabled (struct kindControlBlock* kcb, int kindIndex, int roleIndex)
{
	return kcb->kind [kindIndex].def->roles[roleIndex].enabled;
}

extern unsigned int countKinds (struct kindControlBlock* kcb)
{
	return kcb->count;
}

extern unsigned int countRoles (struct kindControlBlock* kcb, int kindIndex)
{
	return kcb->kind [kindIndex].def->nRoles;
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

extern struct colprintTable * kindColprintTableNew (void)
{
	return colprintTableNew ("L:LANGUAGE", "L:LETTER", "L:NAME", "L:ENABLED",
							 "L:REFONLY", "L:NROLES", "L:MASTER",
							 "L:DESCRIPTION",
							 NULL);
}

extern void kindColprintFillLine (struct colprintLine *line,
								  const char *lang,
								  kindDefinition *kdef)
{
	colprintLineAppendColumnCString (line, lang);
	colprintLineAppendColumnChar (line, kdef->letter);
	colprintLineAppendColumnCString (line, kdef->name
									 ? kdef->name
									 : "ThisShouldNotBePrintedKindNameMustBeGiven");
	colprintLineAppendColumnBool (line, kdef->enabled);
	colprintLineAppendColumnBool (line, kdef->referenceOnly);
	colprintLineAppendColumnInt (line, kdef->nRoles);
	colprintLineAppendColumnCString (line, (kdef->master
											|| kdef->slave ) ?
									 getLanguageName (kdef->syncWith): RSV_NONE);
	colprintLineAppendColumnCString (line, kdef->description? kdef->description: "NO DESCRIPTION GIVEN");
}

extern void kindColprintAddLanguageLines (struct colprintTable *table,
										  struct kindControlBlock* kcb)
{
	const char *lang = getLanguageName (kcb->owner);
	for (unsigned int i = 0; i < countKinds (kcb); i++)
	{
		kindDefinition *kdef = getKind (kcb, i);
		struct colprintLine *line = colprintTableGetNewLine(table);

		kindColprintFillLine (line, lang, kdef);
	}
}

static int kindColprintCompareLines (struct colprintLine *a , struct colprintLine *b)
{
	const char *a_parser = colprintLineGetColumn (a, 0);
	const char *b_parser = colprintLineGetColumn (b, 0);
	const char *a_letter;
	const char *b_letter;
	int r;

	r = strcmp (a_parser, b_parser);
	if (r != 0)
		return r;

	a_letter = colprintLineGetColumn (a, 1);
	b_letter = colprintLineGetColumn (b, 1);
	r = strcmp (a_letter, b_letter);
	if (r != 0)
		return r;

	return 0;
}

extern void kindColprintTablePrint (struct colprintTable *table, bool noparser,
									bool withListHeader, bool machinable, FILE *fp)
{
	colprintTableSort (table, kindColprintCompareLines);
	colprintTablePrint (table, noparser? 1: 0, withListHeader, machinable, fp);
}


extern struct colprintTable * roleColprintTableNew (void)
{
	return colprintTableNew ("L:LANGUAGE", "L:KIND(L/N)", "L:NAME",
							 "L:ENABLED", "L:DESCRIPTION", NULL);
}

extern void roleColprintAddRoles (struct colprintTable *table, struct kindControlBlock *kcb,
								  const char *kindspecs)
{
	const char* lang;
	vString *kind_l_and_n;

	lang = getLanguageName (kcb->owner);
	kind_l_and_n = vStringNew ();
	for (const char *c = kindspecs; *c != '\0'; c++)
	{
		const char *kname = NULL;
		size_t kname_len;

		if (*c == '{')
		{
			const char *start = c + 1;
			const char *end = strchr(c, '}');

			if (!end)
				error (FATAL, "'{' is not closed with '}' in \"%s\"", c);
			if (start == end)
				error (FATAL, "empty kind name is given in \"%s\"", c);

			kname = start;
			kname_len = end - start;
			c = end;
		}

		for (unsigned int i = 0; i < countKinds (kcb); i++)
		{
			const kindDefinition *k = getKind (kcb, i);

			if ((kname
				 && strlen (k->name) == kname_len
				 && strncmp (k->name, kname, kname_len) == 0)
				|| (!kname && *c == k->letter)
				|| (!kname && *c == KIND_WILDCARD))
			{
				for (int j = 0; j < k->nRoles; j++)
				{
					const roleDesc *r = k->roles + j;
					struct colprintLine *line = colprintTableGetNewLine(table);

					colprintLineAppendColumnCString (line, lang);

					vStringPut (kind_l_and_n, k->letter);
					vStringPut (kind_l_and_n, '/');
					vStringCatS (kind_l_and_n, k->name);
					colprintLineAppendColumnVString (line, kind_l_and_n);
					vStringClear (kind_l_and_n);

					colprintLineAppendColumnCString (line, r->name);
					colprintLineAppendColumnCString (line,
													 r->enabled ? "on" : "off");
					colprintLineAppendColumnCString (line, r->description);
				}
				if (! (!kname && *c == KIND_WILDCARD))
					break;
			}
		}
	}
	vStringDelete (kind_l_and_n);
#if 0
	if ((i == countKinds (kcb)) && (*c != KIND_WILDCARD) && (!allowMissingKind))
		error (FATAL, "No such letter kind in %s: %c\n", lang->name, *c);
#endif
}

static int roleColprintCompareLines(struct colprintLine *a, struct colprintLine *b)
{
	int r;

	const char *a_parser, *b_parser;
	a_parser = colprintLineGetColumn (a, 0);
	b_parser = colprintLineGetColumn (b, 0);

	r = strcmp(a_parser, b_parser);
	if (r != 0)
		return r;

	const char *a_kindln, *b_kindln;
	a_kindln = colprintLineGetColumn (a, 1);
	b_kindln = colprintLineGetColumn (b, 1);

	r = strcmp(a_kindln, b_kindln);
	if (r != 0)
		return r;

	const char *a_role, *b_role;
	a_role = colprintLineGetColumn (a, 2);
	b_role = colprintLineGetColumn (b, 2);

	return strcmp(a_role, b_role);
}

extern void roleColprintTablePrint (struct colprintTable *table, bool noparser,
									bool withListHeader, bool machinable, FILE *fp)
{
	colprintTableSort (table, roleColprintCompareLines);
	colprintTablePrint (table, noparser? 1: 0, withListHeader, machinable, fp);
}
