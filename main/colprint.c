/*
*   Copyright (c) 2017 Masatake YAMATO
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*/
#include "general.h"  /* must always come first */

#include "colprint_p.h"
#include "ptrarray.h"
#include "routines.h"
#include "strlist.h"
#include "vstring.h"

#include <stdarg.h>
#include <stdio.h>
#include <string.h>


enum colprintJustification {
	COLPRINT_LEFT,				/* L:... */
	COLPRINT_RIGHT,				/* R:... */
	COLPRINT_LAST,
};

struct colprintHeaderColumn {
	vString *value;
	enum colprintJustification justification;
	unsigned int maxWidth;
	bool needPrefix;
};

struct colprintTable {
	ptrArray *header;
	ptrArray *lines;
};

static void fillWithWhitespaces (int i, FILE *fp)
{
	while (i-- > 0)
	{
		fputc(' ', fp);
	}
}

static struct colprintHeaderColumn * colprintHeaderColumnNew (const char* spec)
{
	int offset = 2;
	struct colprintHeaderColumn *headerCol = xCalloc (1, struct colprintHeaderColumn);

	if (strstr(spec, "L:") == spec)
		headerCol->justification = COLPRINT_LEFT;
	else if (strstr(spec, "R:") == spec)
		headerCol->justification = COLPRINT_RIGHT;
	else
	{
		headerCol->justification = COLPRINT_LEFT;
		offset = 0;
	}

	headerCol->value = vStringNewInit(spec + offset);
	headerCol->maxWidth = vStringLength(headerCol->value);
	return headerCol;
}

static void colprintHeaderColumnDelete (struct colprintHeaderColumn * headerCol)
{
	vStringDelete (headerCol->value);
	eFree (headerCol);
}

struct colprintTable *colprintTableNew (const char* columnHeader, ... /* NULL TERMINATED */)
{
	char *tmp;
	va_list ap;
	struct colprintTable *table;
	struct colprintHeaderColumn *headerCol;


	table = xCalloc (1, struct colprintTable);
	table->header = ptrArrayNew ((ptrArrayDeleteFunc)colprintHeaderColumnDelete);
	table->lines  = ptrArrayNew ((ptrArrayDeleteFunc)stringListDelete);

	headerCol = colprintHeaderColumnNew(columnHeader);
	ptrArrayAdd (table->header, headerCol);

	va_start(ap, columnHeader);
	while (1)
	{
		tmp = va_arg(ap, char*);
		if (tmp)
		{
			headerCol = colprintHeaderColumnNew(tmp);
			ptrArrayAdd (table->header, headerCol);
		}
		else
			break;
	}
	va_end(ap);

	struct colprintHeaderColumn *last_col =	ptrArrayLast (table->header);
	if (last_col)
		last_col->justification = COLPRINT_LAST;

	return table;
}

void colprintTableDelete (struct colprintTable *table)
{
	ptrArrayDelete(table->header);
	table->header = NULL;

	ptrArrayDelete(table->lines);
	table->header = NULL;

	eFree (table);
}

static void colprintColumnPrintGeneric (vString *column, struct colprintHeaderColumn *spec, bool machinable, FILE *fp)
{
	int maxWidth = spec->maxWidth + (spec->needPrefix? 1: 0);

	if ((column == spec->value) && (spec->needPrefix))
	{
		fputc('#', fp);
		maxWidth--;
	}

	if (machinable)
	{
		fputs (vStringValue (column), fp);
		if (spec->justification != COLPRINT_LAST)
			fputc ('\t', fp);
	}
	else
	{
		int padLen = maxWidth - vStringLength (column);
		if (spec->justification == COLPRINT_LEFT
			|| spec->justification == COLPRINT_LAST)
		{
			fputs (vStringValue (column), fp);
			if (spec->justification != COLPRINT_LAST)
			{
				fillWithWhitespaces (padLen, fp);
				fputc (' ', fp);
			}
		}
		else
		{
			fillWithWhitespaces (padLen, fp);
			fputs (vStringValue (column), fp);
			fputc (' ', fp);
		}
	}
}

static void colprintHeaderColumnPrint (struct colprintHeaderColumn *headerCol, bool machinable, FILE* fp)
{
	colprintColumnPrintGeneric (headerCol->value, headerCol, machinable, fp);
}

static void colprintHeaderPrint (ptrArray *header, unsigned int startFrom, bool withHeader, bool machinable, FILE *fp)
{
	unsigned int i;

	if (!withHeader)
		return;

	for (i = startFrom; i < ptrArrayCount(header); i++)
	{
		struct colprintHeaderColumn *headerCol = ptrArrayItem (header, i);
		colprintHeaderColumnPrint (headerCol, machinable, fp);
	}
	fputc('\n', fp);
}

static void colprintLinePrint  (stringList *line, unsigned int startFrom, ptrArray *header, bool machinable, FILE *fp)
{
	unsigned int i;

	for (i = startFrom; i < stringListCount (line); i++)
	{
		vString *value = stringListItem(line, i);
		struct colprintHeaderColumn *spec = ptrArrayItem (header, i);
		colprintColumnPrintGeneric(value, spec, machinable, fp);
	}
}
static void colprintLinesPrint (ptrArray *lines, unsigned int startFrom, ptrArray *header, bool machinable, FILE *fp)
{
	unsigned int i;

	for (i = 0; i < ptrArrayCount (lines); i++)
	{
		stringList *line = ptrArrayItem (lines, i);
		colprintLinePrint (line, startFrom, header, machinable, fp);
		fputc('\n', fp);
	}
}

static void colprintUpdateMaxWidths (ptrArray *header, ptrArray *lines, unsigned int startFrom)
{
	for (unsigned int c = 0; c < ptrArrayCount(header); c++)
	{
		struct colprintHeaderColumn *spec = ptrArrayItem (header, c);

		if (c == startFrom)
			spec->needPrefix = true;
		else
			spec->needPrefix = false;
	}

	for (unsigned int c = 0; c < ptrArrayCount(header); c++)
	{
		struct colprintHeaderColumn *spec = ptrArrayItem (header, c);

		for (unsigned int l = 0; l < ptrArrayCount(lines); l++)
		{
			struct colprintLine *line = ptrArrayItem(lines, l);
			vString *column = ptrArrayItem((ptrArray *)line, c);
			if (spec->maxWidth < vStringLength(column))
				spec->maxWidth = vStringLength(column);
		}
	}
}

void colprintTablePrint (struct colprintTable *table, unsigned int startFrom, bool withHeader, bool machinable, FILE *fp)
{
	colprintUpdateMaxWidths (table->header, table->lines, startFrom);

	colprintHeaderPrint (table->header, startFrom, withHeader, machinable, fp);
	colprintLinesPrint (table->lines, startFrom, table->header, machinable, fp);
}

void colprintTableSort  (struct colprintTable *table, int (* compareFn) (struct colprintLine *, struct colprintLine *))
{
	ptrArraySort (table->lines, (int (*) (const void *, const void *))compareFn);
}

struct colprintLine *colprintTableGetNewLine (struct colprintTable *table)
{
	stringList *line = stringListNew ();

	ptrArrayAdd (table->lines, line);
	return (struct colprintLine *)line;
}

static void colprintLineAppendColumn (struct colprintLine *line, vString *column)
{
	stringList *slist = (stringList *)line;
	stringListAdd (slist, column);
}

void colprintLineAppendColumnCString (struct colprintLine *line, const char *column)
{
	vString* vcol = vStringNewInit (column? column: "");
	colprintLineAppendColumn (line, vcol);
}

void colprintLineAppendColumnVString (struct colprintLine *line, vString* column)
{
	colprintLineAppendColumnCString(line, vStringValue (column));
}

void colprintLineAppendColumnChar (struct colprintLine *line, char column)
{
	vString* vcol = vStringNew ();
	vStringPut (vcol, column);
	colprintLineAppendColumn (line, vcol);
}

void colprintLineAppendColumnInt  (struct colprintLine *line, unsigned int column)
{
	char buf[12];

	snprintf(buf, 12, "%u", column);
	colprintLineAppendColumnCString	(line, buf);
}

void colprintLineAppendColumnBool  (struct colprintLine *line, bool column)
{
	colprintLineAppendColumnCString	(line, column? "yes": "no");
}

const char *colprintLineGetColumn (struct colprintLine *line, unsigned int column)
{
	stringList *slist = (stringList *)line;
	if (column <= stringListCount(slist))
	{
		vString *vstr = stringListItem (slist, column);
		return vStringValue (vstr);
	}
	else
		return NULL;
}
