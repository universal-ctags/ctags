/*
*
*   Copyright (c) 2000-2001, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   This module contains functions for generating tags for diff files (based on Sh parser).
*/

/*
*   INCLUDE FILES
*/
#include "general.h"	/* must always come first */

#include <ctype.h>
#include <string.h>

#include "parse.h"
#include "read.h"
#include "vstring.h"

/*
*   DATA DEFINITIONS
*/
typedef enum {
	K_MODIFIED_FILE,
	K_NEW_FILE,
	K_HUNK,
} diffKind;

static kindOption DiffKinds [] = {
	{ TRUE, 'm', "modified file", "modified files"},
};

enum {
	DIFF_DELIM_MINUS = 0,
	DIFF_DELIM_PLUS
};

static const char *DiffDelims[2] = {
	"--- ",
	"+++ "
};

/*
*   FUNCTION DEFINITIONS
*/

static const unsigned char *stripAbsolute (const unsigned char *filename)
{
	const unsigned char *tmp;

	/* strip any absolute path */
	if (*filename == '/' || *filename == '\\')
	{
		boolean skipSlash = TRUE;

		tmp = (const unsigned char*) strrchr ((const char*) filename,  '/');
		if (tmp == NULL)
		{	/* if no / is contained try \ in case of a Windows filename */
			tmp = (const unsigned char*) strrchr ((const char*) filename, '\\');
			if (tmp == NULL)
			{	/* last fallback, probably the filename doesn't contain a path, so take it */
				tmp = filename;
				skipSlash = FALSE;
			}
		}

		/* skip the leading slash or backslash */
		if (skipSlash)
			tmp++;
	}
	else
		tmp = filename;

	return tmp;
}

static void findDiffTags (void)
{
	vString *filename = vStringNew ();
	const unsigned char *line, *tmp;
	int delim = DIFF_DELIM_MINUS;

	while ((line = fileReadLine ()) != NULL)
	{
		const unsigned char* cp = line;

		if (strncmp ((const char*) cp, DiffDelims[delim], 4u) == 0)
		{
			cp += 4;
			if (isspace ((int) *cp)) continue;
			/* when original filename is /dev/null use the new one instead */
			if (delim == DIFF_DELIM_MINUS &&
				strncmp ((const char*) cp, "/dev/null", 9u) == 0 &&
				(cp[9] == 0 || isspace (cp[9])))
			{
				delim = DIFF_DELIM_PLUS;
				continue;
			}

			tmp = stripAbsolute (cp);

			if (tmp != NULL)
			{
				while (! isspace(*tmp) && *tmp != '\0')
				{
					vStringPut(filename, *tmp);
					tmp++;
				}

				vStringTerminate(filename);
				makeSimpleTag (filename, DiffKinds, K_MODIFIED_FILE);
				vStringClear (filename);
			}

			/* restore default delim */
			delim = DIFF_DELIM_MINUS;
		}
	}
	vStringDelete (filename);
}

extern parserDefinition* DiffParser (void)
{
	static const char *const extensions [] = { "diff", "patch", NULL };
	parserDefinition* const def = parserNew ("Diff");
	def->kinds      = DiffKinds;
	def->kindCount  = KIND_COUNT (DiffKinds);
	def->extensions = extensions;
	def->parser     = findDiffTags;
	return def;
}

/* vi:set tabstop=8 shiftwidth=4: */
