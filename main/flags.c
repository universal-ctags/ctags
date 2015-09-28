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

#include "flags.h"
#include "vstring.h"
#include "routines.h"

void flagsEval (const char* flags_original, flagDefinition* defs, unsigned int ndefs, void* data)
{
	unsigned int i, j;
	char *flags;

	if (!flags_original)
		return;
	if (!defs)
		return;

	flags = eStrdup (flags_original);
	for (i = 0 ; flags [i] != '\0' ; ++i)
	{
		if (flags [i] == LONG_FLAGS_OPEN)
		{
			const char* aflag = flags + i + 1;
			char* needle_close_paren = strchr(aflag, LONG_FLAGS_CLOSE);
			const char* param;
			char* needle_eqaul;

			if (needle_close_paren == NULL)
			{
				error (WARNING, "long flags specifier opened with `%c' is not closed `%c'",
				       LONG_FLAGS_OPEN, LONG_FLAGS_CLOSE);
				break;
			}

			*needle_close_paren = '\0';
			needle_eqaul = strchr(aflag, '=');
			if ((needle_eqaul == NULL || (needle_eqaul >= needle_close_paren)))
			{
				needle_eqaul = NULL;
				param = NULL;
			}
			else
			{
				param = needle_eqaul + 1;
				*needle_eqaul = '\0';
			}

			for ( j = 0 ; j < ndefs ; ++j )
				if (defs[j].longStr && (strcmp(aflag, defs[j].longStr) == 0))
					defs[j].longProc(aflag, param, data);

			if (needle_eqaul)
				*needle_eqaul = '=';
			*needle_close_paren = LONG_FLAGS_CLOSE;

			i = needle_close_paren - flags;
		}
		else for (j = 0 ; j < ndefs ; ++j)
			if (flags[i] == defs[j].shortChar)
				defs[j].shortProc(flags[i], data);
	}
	eFree (flags);
}

void  flagPrintHelp (flagDefinition* def, unsigned int ndefs)
{

	unsigned int i;
	const char *longStr;
	const char *description;
	const char *paramName;
	char shortChar[3];
	for ( i = 0; i < ndefs; ++i )
	{
		longStr = def[i].longStr? def[i].longStr: "";
		description = def[i].description? def[i].description: "";
		paramName = def[i].paramName;

		if (def[i].shortChar == '\0')
			strcpy (shortChar, "\\0");
		else
		{
			shortChar[0] = def[i].shortChar;
			shortChar[1] = '\0';
		}

		if (paramName)
			printf ("%s\t%s=%s\t%s\n", shortChar, longStr, paramName, description);
		else
			printf ("%s\t%s\t%s\n", shortChar, longStr, description);
	}
}
