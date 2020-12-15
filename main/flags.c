/*
*
*   Copyright (c) 2000-2003, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions to process command line options.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include <string.h>
#include <stdio.h>

#include "ctags.h"
#include "flags_p.h"
#include "vstring.h"
#include "routines.h"

extern const char *flagsEval (const char* flags_original, flagDefinition* defs, unsigned int ndefs, void* data)
{
	unsigned int i, j;
	char *flags;
	const char *optscript = NULL;

	if (!flags_original)
		return NULL;

	flags = eStrdup (flags_original);
	for (i = 0 ; flags [i] != '\0' ; ++i)
	{
		if (flags [i] == LONG_FLAGS_OPEN && flags [i + 1] == LONG_FLAGS_OPEN)
		{
			optscript = flags_original + i;
			break;
		}
		else if (flags [i] == LONG_FLAGS_OPEN)
		{
			const char* aflag = flags + i + 1;
			char* needle_close_paren = strchr(aflag, LONG_FLAGS_CLOSE);
			const char* param;
			char* needle_equal;

			if (needle_close_paren == NULL)
			{
				error (WARNING, "long flags specifier opened with `%c' is not closed `%c': \"%s\"",
				       LONG_FLAGS_OPEN, LONG_FLAGS_CLOSE, flags_original);
				break;
			}

			*needle_close_paren = '\0';
			needle_equal = strchr(aflag, '=');
			if ((needle_equal == NULL || (needle_equal >= needle_close_paren)))
			{
				needle_equal = NULL;
				param = NULL;
			}
			else
			{
				param = needle_equal + 1;
				*needle_equal = '\0';
			}

			for ( j = 0 ; j < ndefs ; ++j )
				if (defs[j].longStr && (strcmp(aflag, defs[j].longStr) == 0))
					defs[j].longProc(aflag, param, data);

			if (needle_equal)
				*needle_equal = '=';
			*needle_close_paren = LONG_FLAGS_CLOSE;

			i = needle_close_paren - flags;
		}
		else for (j = 0 ; j < ndefs ; ++j)
			if (flags[i] == defs[j].shortChar)
				defs[j].shortProc(flags[i], data);
	}
	eFree (flags);
	return optscript;
}

extern struct colprintTable * flagsColprintTableNew (void)
{
	return colprintTableNew ("L:LETTER", "L:NAME", "L:DESCRIPTION", NULL);
}

extern void flagsColprintAddDefinitions (struct colprintTable *table, flagDefinition* def,
										 unsigned int ndefs)
{
	vString *longName = vStringNew ();

	for (unsigned int i = 0; i < ndefs; i++)
	{
		struct colprintLine * line;
		char shortChar;
		const char *paramName;
		const char *description;


		line = colprintTableGetNewLine(table);

		shortChar = def[i].shortChar;
		if (shortChar == '\0')
			shortChar = '-';
		colprintLineAppendColumnChar (line, shortChar);

		vStringCopyS (longName, def[i].longStr? def[i].longStr: RSV_NONE);
		paramName = def[i].paramName;
		if (paramName)
		{
			vStringPut (longName, '=');
			vStringCatS (longName, paramName);
		}
		colprintLineAppendColumnVString (line, longName);
		vStringClear(longName);

		description = def[i].description? def[i].description: "";
		colprintLineAppendColumnCString (line, description);
	}

	vStringDelete(longName);
}

static int flagsColprintCompareLines(struct colprintLine *a , struct colprintLine *b)
{
	const char *a_letter = colprintLineGetColumn (a, 0);
	const char *b_letter = colprintLineGetColumn (b, 0);

	if (a_letter[0] != '-' && b_letter[0] == '-')
		return -1;
	else if (a_letter[0] == '-' && b_letter[0] != '-')
		return 1;
	else if (a_letter[0] != '-' && b_letter[0] != '-')
		return strcmp(a_letter, b_letter);


	const char *a_name = colprintLineGetColumn (a, 1);
	const char *b_name = colprintLineGetColumn (b, 1);

	if (a_name[0] != '_' && b_name[0] == '_')
		return -1;
	else if (a_name[0] == '_' && b_name[0] != '_')
		return 1;

	return strcmp(a_name, b_name);
}

extern void flagsColprintTablePrint (struct colprintTable *table,
									 bool withListHeader, bool machinable, FILE *fp)
{
	colprintTableSort (table, flagsColprintCompareLines);
	colprintTablePrint (table, 0, withListHeader, machinable, fp);
}
