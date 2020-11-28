/*
 *
 *  Copyright (c) 2020, Red Hat, Inc.
 *  Copyright (c) 2020, Masatake YAMATO
 *
 *  Author: Masatake YAMATO <yamato@redhat.com>
 *
 *   This source code is released for free distribution under the terms of the
 *   GNU General Public License version 2 or (at your option) any later version.
 *
 */

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include "options.h"
#include "hint.h"
#include "hint_p.h"
#include "routines.h"
#include "field_p.h"

#define TAG_NO_COMPAT_SORT_TYPE
#include "readtags.h"

#include <string.h>


/*
* DATA DEFINITIONS
*/
typedef tagFile hintFile;
typedef tagFileInfo hintFileInfo;

static hintFile *hFile;
static hintFileInfo hFileInfo;


/*
*   FUNCTION DEFINITIONS
*/

/* Copied from readtags-cmd.c.
 * TODO: the code should be unified. */
static const char* tagsStrerror (int err)
{
	if (err > 0)
		return strerror (err);
	else if (err < 0)
	{
		switch (err)
		{
		case TagErrnoUnexpectedSortedMethod:
			return "Unexpected sorted method";
		case TagErrnoUnexpectedFormat:
			return "Unexpected format number";
		case TagErrnoUnexpectedLineno:
			return "Unexpected value for line: field";
		case TagErrnoInvalidArgument:
			return "Unexpected argument passed to the API function";
		default:
			return "Unknown error";
		}
	}
	else
		return "no error";
}

extern void processHintFileOption (const char *const option,
								   const char *const parameter)
{
	if (parameter == NULL || parameter[0] == '\0')
		error (FATAL, "no parameter is given for %s", option);

	verbose ("opening a hint file: %s...", parameter);
	hFile = tagsOpen (parameter, &hFileInfo);
	if (hFile == NULL || !hFileInfo.status.opened)
		error (FATAL, "cannot open tag file as specified as hint: %s: %s",
			   tagsStrerror (hFileInfo.status.error_number), parameter);
	verbose ("done\n");
}

extern bool isHintAvailable (void)
{
	return !(hFile == NULL);
}

extern bool foreachHintEntries (const char *name,
								int flags,
								hintForeachFunc func,
								void *data)
{

	if (hFile == NULL)
		return true;

	hintEntry entry;
	int err;

	if (tagsFind (hFile, &entry, name, flags) == TagSuccess)
	{
		do
		{
			if (! (* func) (name, &entry, data))
				return false;
		}
		while (tagsFindNext (hFile, &entry) == TagSuccess);
		if ((err = tagsGetErrno (hFile)))
			error (FATAL, "error in tagsFindNext (\"%s\"): %s",
				   name, tagsStrerror (err));
	}
	else if ((err = tagsGetErrno (hFile)))
		error (FATAL, "error in tagsFind (\"%s\"): %s",
			   name, tagsStrerror (err));

	return true;
}

extern const char* hintFieldForType (const hintEntry *const hint,
									 fieldType ftype)
{
	if (hFile == NULL)
		return NULL;

	const char *fname = getFieldName (ftype);

	if (fname)
		return tagsField (hint, fname);
	return NULL;
}
