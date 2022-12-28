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

#include "general.h"
#include "options.h"
#include "param.h"
#include "param_p.h"
#include "parse.h"

#include <string.h>

typedef struct sParamObject {
	paramDefinition *def;
	freeParamDefFunc free;
} paramObject;

struct paramControlBlock {
	paramObject *param;
	unsigned int count;
	langType owner;
};

extern struct paramControlBlock* allocParamControlBlock (parserDefinition *parser)
{
	struct paramControlBlock *pcb;

	pcb = xMalloc (1, struct paramControlBlock);
	pcb->param = xMalloc (parser->paramCount, paramObject);
	pcb->count = parser->paramCount;
	pcb->owner = parser->id;

	for (unsigned int i = 0; i < parser->paramCount; ++i)
	{
		paramObject *param = pcb->param + i;
		param->def = parser->paramTable + i;
		param->free = NULL;
	}

	return pcb;
}

extern void freeParamControlBlock (struct paramControlBlock* pcb)
{
	for (unsigned int i = 0; i< pcb->count; ++i)
	{
		if (pcb->param [i].free)
			pcb->param [i].free (pcb->param [i].def);
	}
	if (pcb->param)
		eFree (pcb->param);
	eFree (pcb);
}

extern int  defineParam (struct paramControlBlock* pcb, paramDefinition *def,
						 freeParamDefFunc freeParamDef)
{
	unsigned int id = pcb->count++;
	pcb->param = xRealloc (pcb->param, pcb->count, paramObject);
	pcb->param [id].def = def;
	pcb->param [id].free = freeParamDef;

	verbose ("Add param[%d] \"%s,%s\" to %s\n", id,
			 def->name, def->desc,
			 getLanguageName (pcb->owner));

	return id;
}

extern void applyParam (struct paramControlBlock* pcb, const char *name, const char *args)
{
	for (unsigned int i = 0; i < pcb->count; i++)
	{
		paramDefinition *pdef = pcb->param[i].def;
		if (strcmp(pdef->name, name) == 0)
		{
			pdef->handleParam (pcb->owner, name, args);
			return;
		}
	}
	const char *lang = getLanguageName (pcb->owner);
	error (FATAL, "no such parameter in %s: %s", lang, name);
}

extern struct colprintTable * paramColprintTableNew (void)
{
	return colprintTableNew ("L:LANGUAGE", "L:NAME","L:DESCRIPTION", NULL);
}

extern void paramColprintAddParams (struct colprintTable *table,
									struct paramControlBlock* pcb)
{
	const char *lang = getLanguageName (pcb->owner);
	for (unsigned int i = 0; i < pcb->count; i++)
	{
		paramDefinition *pdef = pcb->param [i].def;
		struct colprintLine *line = colprintTableGetNewLine(table);

		colprintLineAppendColumnCString (line, lang);
		colprintLineAppendColumnCString (line, pdef->name);
		colprintLineAppendColumnCString (line, pdef->desc);
	}
}

static int paramColprintCompareLines (struct colprintLine *a , struct colprintLine *b)
{
	const char *a_parser = colprintLineGetColumn (a, 0);
	const char *b_parser = colprintLineGetColumn (b, 0);

	int r;
	r = strcmp (a_parser, b_parser);
	if (r != 0)
		return r;

	const char *a_name = colprintLineGetColumn (a, 1);
	const char *b_name = colprintLineGetColumn (b, 1);

	return strcmp(a_name, b_name);
}

extern void paramColprintTablePrint (struct colprintTable *table, bool noparser,
									bool withListHeader, bool machinable, FILE *fp)
{
	colprintTableSort (table, paramColprintCompareLines);
	colprintTablePrint (table, noparser? 1: 0, withListHeader, machinable, fp);
}
