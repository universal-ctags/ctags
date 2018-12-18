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
#include "param.h"
#include "param_p.h"
#include "parse.h"

#include <string.h>


extern struct colprintTable * paramColprintTableNew (void)
{
	return colprintTableNew ("L:LANGUAGE", "L:NAME","L:DESCRIPTION", NULL);
}

extern void paramColprintAddParameter (struct colprintTable *table,
									   langType language,
									   const parameterHandlerTable *const paramHandler)
{
	struct colprintLine *line = colprintTableGetNewLine(table);

	colprintLineAppendColumnCString (line, getLanguageName (language));
	colprintLineAppendColumnCString (line, paramHandler->name);
	colprintLineAppendColumnCString (line, paramHandler->desc);
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
