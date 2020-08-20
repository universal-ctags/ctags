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

#include <ctype.h>
#include <stdio.h>
#include <string.h>

#include "colprint_p.h"
#include "ctags.h"
#include "debug.h"
#include "entry.h"
#include "kind.h"
#include "parse_p.h"
#include "options.h"
#include "ptrarray.h"
#include "routines.h"
#include "vstring.h"

typedef struct sRoleObject {
	roleDefinition *def;
	freeRoleDefFunc free;
} roleObject;

struct roleControlBlock {
	roleObject *role;
	unsigned int count;
	int owner;
};

typedef struct sKindObject {
	kindDefinition *def;
	freeKindDefFunc free;
	struct roleControlBlock *rcb;
	ptrArray * dynamicSeparators;
} kindObject;

struct kindControlBlock {
	kindObject *kind;
	unsigned int count;
	langType owner;
	scopeSeparator defaultScopeSeparator;
	scopeSeparator defaultRootScopeSeparator;
};

extern const char *renderRole (const roleDefinition* const role, vString* b)
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

extern void enableRole (roleDefinition *role, bool enable)
{
	role->enabled = enable;
}

static void initRoleObject (roleObject *robj, roleDefinition *rdef, freeRoleDefFunc freefunc, int roleId)
{
#ifdef DEBUG
	size_t len = strlen (rdef->name);
	for (int i = 0; i < len; i++)
		Assert (isalnum (rdef->name [i]));
#endif
	robj->def = rdef;
	robj->free = freefunc;
	robj->def->id = roleId;
}

static struct roleControlBlock* allocRoleControlBlock (kindObject *kind)
{
	unsigned int j;
	struct roleControlBlock* rcb;

	rcb = xMalloc(1, struct roleControlBlock);
	rcb->count = kind->def->nRoles;
	rcb->owner = kind->def->id;
	rcb->role = xMalloc(rcb->count, roleObject);
	for (j = 0; j < rcb->count; j++)
		initRoleObject (rcb->role + j, kind->def->roles + j, NULL, j);

	return rcb;
}

extern struct kindControlBlock* allocKindControlBlock (parserDefinition *parser)
{
	unsigned int i;
	struct kindControlBlock *kcb;

	kcb = xMalloc (1, struct kindControlBlock);
	kcb->kind = xMalloc (parser->kindCount, kindObject);
	kcb->count = parser->kindCount;
	kcb->owner = parser->id;

	kcb->defaultScopeSeparator.parentKindIndex = KIND_WILDCARD_INDEX;
	kcb->defaultScopeSeparator.separator = NULL;
	if (parser->defaultScopeSeparator)
		kcb->defaultScopeSeparator.separator = eStrdup (parser->defaultScopeSeparator);

	kcb->defaultRootScopeSeparator.parentKindIndex = KIND_GHOST_INDEX;
	kcb->defaultRootScopeSeparator.separator = NULL;
	if (parser->defaultRootScopeSeparator)
		kcb->defaultRootScopeSeparator.separator = eStrdup (parser->defaultRootScopeSeparator);

	for (i = 0; i < parser->kindCount; ++i)
	{
		kindObject *kind = kcb->kind + i;
		kind->def = parser->kindTable + i;
		kind->free = NULL;
		kind->def->id = i;
		kind->rcb = allocRoleControlBlock (kind);
		kind->dynamicSeparators = NULL;
	}

	return kcb;
}

static void freeRoleControlBlock (struct roleControlBlock *rcb)
{
	unsigned int i;
	for (i = 0; i < rcb->count; ++i)
	{
		if (rcb->role[i].free)
			rcb->role [i].free (rcb->role [i].def);
	}
	eFreeNoNullCheck (rcb->role);
	eFree (rcb);
}

extern void freeKindControlBlock (struct kindControlBlock* kcb)
{
	unsigned int i;

	for (i = 0; i < kcb->count; ++i)
	{
		if (kcb->kind [i].free)
			kcb->kind [i].free (kcb->kind [i].def);
		freeRoleControlBlock (kcb->kind [i].rcb);
		if (kcb->kind [i].dynamicSeparators)
			ptrArrayDelete(kcb->kind [i].dynamicSeparators);
	}

	if (kcb->defaultRootScopeSeparator.separator)
		eFree((char *)kcb->defaultRootScopeSeparator.separator);
	if (kcb->defaultScopeSeparator.separator)
		eFree((char *)kcb->defaultScopeSeparator.separator);

	if (kcb->kind)
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
	kcb->kind [def->id].rcb = allocRoleControlBlock(kcb->kind + def->id);
	kcb->kind [def->id].dynamicSeparators = NULL;

	verbose ("Add kind[%d] \"%c,%s,%s\" to %s\n", def->id,
			 def->letter, def->name, def->description,
			 getLanguageName (kcb->owner));

	return def->id;
}

extern int defineRole (struct kindControlBlock* kcb, int kindIndex,
					   roleDefinition *def, freeRoleDefFunc freeRoleDef)
{
	struct roleControlBlock *rcb = kcb->kind[kindIndex].rcb;
	int roleIndex = rcb->count++;

	if (roleIndex == ROLE_MAX_COUNT)
	{
		rcb->count--;
		error (FATAL, "Too many role definition for kind \"%s\" of language \"%s\" (> %d)",
			   kcb->kind[kindIndex].def->name,
			   getLanguageName (kcb->owner),
			   (int)(ROLE_MAX_COUNT - 1));
	}

	rcb->role = xRealloc (rcb->role, rcb->count, roleObject);
	initRoleObject (rcb->role + roleIndex, def, freeRoleDef, roleIndex);

	return roleIndex;
}

extern bool isRoleEnabled (struct kindControlBlock* kcb, int kindIndex, int roleIndex)
{
	roleDefinition *rdef = getRole (kcb, kindIndex, roleIndex);
	return rdef->enabled;
}

extern unsigned int countKinds (struct kindControlBlock* kcb)
{
	return kcb->count;
}

extern unsigned int countRoles (struct kindControlBlock* kcb, int kindIndex)
{
	return kcb->kind [kindIndex].rcb->count;
}

extern kindDefinition *getKind (struct kindControlBlock* kcb, int kindIndex)
{
	return kcb->kind [kindIndex].def;
}

extern kindDefinition *getKindForLetter (struct kindControlBlock* kcb, char letter)
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

extern int getKindIndexForLetter (struct kindControlBlock* kcb, char letter)
{
	unsigned int i;
	kindDefinition * kdef;

	for (i = 0;  i < countKinds (kcb);  ++i)
	{
		kdef = getKind (kcb, i);
		if (kdef->letter == letter)
			return (unsigned int)i;
	}
	return KIND_GHOST_INDEX;
}

extern int getKindIndexForName (struct kindControlBlock* kcb, const char* name)
{
	unsigned int i;
	kindDefinition * kdef;

	for (i = 0;  i < countKinds (kcb);  ++i)
	{
		kdef = getKind (kcb, i);
		Assert(kdef);
		if (kdef->name && (strcmp(kdef->name, name) == 0))
			return (int)i;
	}
	return KIND_GHOST_INDEX;
}

extern roleDefinition* getRole(struct kindControlBlock* kcb, int kindIndex, int roleIndex)
{
	struct roleControlBlock *rcb = kcb->kind[kindIndex].rcb;
	return rcb->role [roleIndex].def;
}

extern roleDefinition* getRoleForName(struct kindControlBlock* kcb,
									  int kindIndex, const char* name)
{
	unsigned int i;
	roleDefinition *rdef;

	for (i = 0; i < countRoles (kcb, kindIndex); ++i)
	{
		rdef = getRole(kcb, kindIndex, i);
		Assert(rdef);
		if (rdef->name && (strcmp(rdef->name, name) == 0))
			return rdef;
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

static void scopeSeparatorDelete (void *data)
{
	scopeSeparator *sep = data;
	eFree ((void *)sep->separator);
	sep->separator = NULL;
	eFree (sep);
}

extern int defineScopeSeparator(struct kindControlBlock* kcb,
								int kindIndex,
								int parentKindIndex, const char *separator)
{
	if (kindIndex == KIND_WILDCARD_INDEX)
	{
		if (parentKindIndex == KIND_WILDCARD_INDEX)
		{
			if (kcb->defaultScopeSeparator.separator)
				eFree ((char *)kcb->defaultScopeSeparator.separator);
			verbose ("Installing default separator for %s: %s\n",
					 getLanguageName (kcb->owner), separator);
			kcb->defaultScopeSeparator.separator = eStrdup (separator);
		}
		else if (parentKindIndex == KIND_GHOST_INDEX)
		{
			if (kcb->defaultRootScopeSeparator.separator)
				eFree ((char *)kcb->defaultRootScopeSeparator.separator);
			verbose ("Installing default root separator for %s: %s\n",
					 getLanguageName (kcb->owner),
					 separator);
			kcb->defaultRootScopeSeparator.separator = eStrdup (separator);
		}
		else
			error (FATAL,
				   "Don't specify a real kind as parent when defining a default scope separator: %d",
				   parentKindIndex);
		return 0;
	}
	Assert (kcb->count > kindIndex);
	kindObject *kind = kcb->kind + kindIndex;

	if (!kind->dynamicSeparators)
		kind->dynamicSeparators = ptrArrayNew (scopeSeparatorDelete);

	scopeSeparator *sep = xMalloc (1, scopeSeparator);
	sep->parentKindIndex = parentKindIndex;
	sep->separator = eStrdup(separator);
	ptrArrayAdd (kind->dynamicSeparators, sep);

	return 0;
}

static scopeSeparator *getScopeSeparatorDynamic(kindObject *kobj, int parentKindIndex)
{
	scopeSeparator *sep;

	if (kobj->dynamicSeparators)
	{
		for (unsigned int i = ptrArrayCount (kobj->dynamicSeparators); 0 < i ; i--)
		{
			sep = ptrArrayItem (kobj->dynamicSeparators, i - 1);
			if (sep->parentKindIndex == parentKindIndex)
				return sep;
		}
	}
	return NULL;
}

static const scopeSeparator *getScopeSeparatorStatic(kindDefinition *kdef, int parentKindIndex)
{
	scopeSeparator *table = kdef->separators;

	if (table == NULL)
		return NULL;

	while (table - kdef->separators < (int)kdef->separatorCount)
	{
		if (table->parentKindIndex == parentKindIndex)
			return table;

		/* If a caller wants a root separator for kdef,
		   we should not return a wildcard table. */
		if (parentKindIndex != KIND_GHOST_INDEX
			&& table->parentKindIndex == KIND_WILDCARD_INDEX)
			return table;

		table++;
	}

	return NULL;
}

extern const scopeSeparator *getScopeSeparator(struct kindControlBlock* kcb,
											   int kindIndex, int parentKindIndex)
{
	Assert (kindIndex != KIND_GHOST_INDEX);
	Assert (kindIndex != KIND_FILE_INDEX);
	Assert (kindIndex != KIND_WILDCARD_INDEX);

	Assert (parentKindIndex != KIND_WILDCARD_INDEX);
	Assert (parentKindIndex != KIND_FILE_INDEX);
	/* A caller specifies KIND_GHOST_INDEX for parentKindIndex when it
	 * wants root separator. */

	Assert (kcb->count > kindIndex);
	kindObject *kobj = kcb->kind + kindIndex;
	const scopeSeparator *sep;

	sep = getScopeSeparatorDynamic (kobj, parentKindIndex);
	if (sep)
		return sep;

	sep = getScopeSeparatorStatic (kobj->def, parentKindIndex);
	if (sep)
		return sep;

	/* Cannot find a suitable sep definition.
	 * Use default one. */
	if (parentKindIndex == KIND_GHOST_INDEX)
	{
		if (kcb->defaultRootScopeSeparator.separator)
			return &kcb->defaultRootScopeSeparator;
		return NULL;
	}
	else
	{
		if (kcb->defaultScopeSeparator.separator)
			return &kcb->defaultScopeSeparator;

		static scopeSeparator defaultSeparator = {
			.separator = ".",
			.parentKindIndex = KIND_WILDCARD_INDEX,
		};
		return &defaultSeparator;
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

static void kindColprintFillLine (struct colprintLine *line,
								  const char *langName,
								  kindDefinition *kdef)
{
	langType lang = getNamedLanguage (langName, 0);
	unsigned int count = countLanguageRoles(lang, kdef->id);
	colprintLineAppendColumnCString (line, langName);
	colprintLineAppendColumnChar (line, kdef->letter);
	colprintLineAppendColumnCString (line, kdef->name
									 ? kdef->name
									 : "ThisShouldNotBePrintedKindNameMustBeGiven");
	colprintLineAppendColumnBool (line, kdef->enabled);
	colprintLineAppendColumnBool (line, kdef->referenceOnly);
	colprintLineAppendColumnInt (line, count);
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
				|| (!kname && *c == KIND_WILDCARD_LETTER))
			{
				unsigned int nRoles = countRoles(kcb, i);
				for (unsigned int j = 0; j < nRoles; j++)
				{
					const roleDefinition *r = getRole (kcb, i, j);
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
				if (! (!kname && *c == KIND_WILDCARD_LETTER))
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
