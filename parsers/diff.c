/*
*
*   Copyright (c) 2000-2001, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for generating tags for diff files (based on Sh parser).
*/

/*
*   INCLUDE FILES
*/
#include "general.h"	/* must always come first */

#include <ctype.h>
#include <string.h>

#include "entry.h"
#include "parse.h"
#include "routines.h"
#include "read.h"
#include "vstring.h"

/*
*   DATA DEFINITIONS
*/
typedef enum {
	K_MODIFIED_FILE,
	K_NEW_FILE,
	K_DELETED_FILE,
	K_HUNK,
} diffKind;

static kindDefinition DiffKinds [] = {
	{ true, 'm', "modifiedFile",  "modified files"},
	{ true, 'n', "newFile",       "newly created files"},
	{ true, 'd', "deletedFile",   "deleted files"},
	{ true, 'h', "hunk",          "hunks"},
};

enum {
	DIFF_DELIM_MINUS = 0,
	DIFF_DELIM_PLUS
};

static const char *DiffDelims[2] = {
	"--- ",
	"+++ "
};

static const char *HunkDelim[2] = {
	"@@ ",
	" @@",
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
		bool skipSlash = true;

		tmp = (const unsigned char*) strrchr ((const char*) filename,  '/');
		if (tmp == NULL)
		{	/* if no / is contained try \ in case of a Windows filename */
			tmp = (const unsigned char*) strrchr ((const char*) filename, '\\');
			if (tmp == NULL)
			{	/* last fallback, probably the filename doesn't contain a path, so take it */
				tmp = filename;
				skipSlash = false;
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

static int parseHunk (const unsigned char* cp, vString *hunk, int scope_index)
{
	/*
	   example input: @@ -0,0 +1,134 @@
	   expected output: -0,0 +1,134
	*/

	const char *next_delim;
	const char *start, *end;
	const char *c;
	int i = CORK_NIL;

	cp += 3;
	start = (const char*)cp;

	if (*start != '-')
		return i;

	next_delim = strstr ((const char*)cp, HunkDelim[1]);
	if ((next_delim == NULL)
	    || (! (start < next_delim )))
		return i;
	end = next_delim;
	if (! ( '0' <= *( end - 1 ) && *( end - 1 ) <= '9'))
		return i;
	for (c = start; c < end; c++)
		if (*c == '\t')
			return i;
	vStringNCopyS (hunk, start, end - start);
	i = makeSimpleTag (hunk, K_HUNK);
	tagEntryInfo *e =  getEntryInCorkQueue (i);
	if (e && scope_index > CORK_NIL)
		e->extensionFields.scopeIndex = scope_index;
	return i;
}

static void markTheLastTagAsDeletedFile (int scope_index)
{
	tagEntryInfo *e =  getEntryInCorkQueue (scope_index);

	if (e)
		e->kindIndex = K_DELETED_FILE;
}

static void findDiffTags (void)
{
	vString *filename = vStringNew ();
	vString *hunk = vStringNew ();
	const unsigned char *line, *tmp;
	int delim = DIFF_DELIM_MINUS;
	diffKind kind;
	int scope_index = CORK_NIL;

	while ((line = readLineFromInputFile ()) != NULL)
	{
		const unsigned char* cp = line;

		if (strncmp ((const char*) cp, DiffDelims[delim], 4u) == 0)
		{
			scope_index = CORK_NIL;
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

				if (delim == DIFF_DELIM_PLUS)
					kind = K_NEW_FILE;
				else
					kind = K_MODIFIED_FILE;
				scope_index = makeSimpleTag (filename, kind);
				vStringClear (filename);
			}

			/* restore default delim */
			delim = DIFF_DELIM_MINUS;
		}
		else if ((scope_index > CORK_NIL)
			 && (strncmp ((const char*) cp, DiffDelims[1], 4u) == 0))
		{
			cp += 4;
			if (isspace ((int) *cp)) continue;
			/* when modified filename is /dev/null, the original name is deleted. */
			if (strncmp ((const char*) cp, "/dev/null", 9u) == 0 &&
			    (cp[9] == 0 || isspace (cp[9])))
				markTheLastTagAsDeletedFile (scope_index);
		}
		else if (strncmp ((const char*) cp, HunkDelim[0], 3u) == 0)
		{
			if (parseHunk (cp, hunk, scope_index) != CORK_NIL)
				vStringClear (hunk);
		}
	}
	vStringDelete (hunk);
	vStringDelete (filename);
}

extern parserDefinition* DiffParser (void)
{
	static const char *const extensions [] = { "diff", "patch", NULL };
	parserDefinition* const def = parserNew ("Diff");
	def->kindTable      = DiffKinds;
	def->kindCount  = ARRAY_SIZE (DiffKinds);
	def->extensions = extensions;
	def->parser     = findDiffTags;
	def->useCork    = CORK_QUEUE;
	return def;
}
