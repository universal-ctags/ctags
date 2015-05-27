/*
*
*   Copyright (c) 2000-2003, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
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

#define LONG_FLAG_OPEN  '{'
#define LONG_FLAG_CLOSE '}'

void flagsEval (const char* flags, flagDefinition* defs, unsigned int ndefs, void* data)
{
	unsigned int i, j;

	if (!flags)
		return;
	if (!defs)
		return;

	for (i = 0 ; flags [i] != '\0' ; ++i)
	{
		if (flags [i] == LONG_FLAG_OPEN)
		{
			const char* aflag = flags + i + 1;
			char* needle_close_paren = strchr(aflag, LONG_FLAG_CLOSE);
			const char* param;
			char* needle_eqaul;

			if (needle_close_paren == NULL)
			{
				error (WARNING, "long flags specifier opened with `%c' is not closed `%c'",
				       LONG_FLAG_OPEN, LONG_FLAG_CLOSE);
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
			*needle_close_paren = LONG_FLAG_CLOSE;

			i = needle_close_paren - flags;
		}
		else for (j = 0 ; j < ndefs ; ++j)
			if (flags[i] == defs[j].shortChar)
				defs[j].shortProc(flags[i], data);
	}
}
