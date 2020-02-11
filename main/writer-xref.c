/*
*   Copyright (c) 1998-2002, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   External interface to entry.c
*/

#include "general.h"  /* must always come first */

#include "entry.h"
#include "field_p.h"
#include "fmt_p.h"
#include "mio.h"
#include "options_p.h"
#include "ptag_p.h"
#include "writer_p.h"

#include <string.h>


static int writeXrefEntry  (tagWriter *writer CTAGS_ATTR_UNUSED,
							MIO * mio, const tagEntryInfo *const tag,
							void *clientData CTAGS_ATTR_UNUSED);
static int writeXrefPtagEntry (tagWriter *writer, MIO * mio, const ptagDesc *desc,
							   const char *const fileName,
							   const char *const pattern,
							   const char *const parserName,
							   void *clientData);

tagWriter xrefWriter = {
	.writeEntry = writeXrefEntry,
	.writePtagEntry = writeXrefPtagEntry,
	.printPtagByDefault = false,
	.preWriteEntry = NULL,
	.postWriteEntry = NULL,
	.rescanFailedEntry = NULL,
	.treatFieldAsFixed = NULL,
	.defaultFileName = NULL,
};

static int writeXrefPtagEntry (tagWriter *writer, MIO * mio, const ptagDesc *desc,
							   const char *const fileName,
							   const char *const pattern,
							   const char *const parserName,
							   void *clientData)
{
	tagEntryInfo e;
	vString *name = vStringNewInit(PSEUDO_TAG_PREFIX);

	memset (&e, 0, sizeof(e));

	e.isPseudoTag = 1;

	vStringCatS (name, desc->name);
	if (parserName)
	{
		vStringCatS (name, PSEUDO_TAG_SEPARATOR);
		vStringCatS (name, parserName);
	}
	e.name = vStringValue (name);
	e.inputFileName = fileName;
	e.pattern = pattern;

	int length = writeXrefEntry (writer, mio, &e, clientData);

	vStringDelete (name);

	return length;
}

static int writeXrefEntry (tagWriter *writer CTAGS_ATTR_UNUSED,
						   MIO * mio, const tagEntryInfo *const tag,
						   void *clientData CTAGS_ATTR_UNUSED)
{
	int length;
	static fmtElement *fmt1;
	static fmtElement *fmt2;

	if (Option.customXfmt)
		length = fmtPrint (Option.customXfmt, mio, tag);
	else
	{
		if (tag->isFileEntry)
			return 0;

		if (Option.tagFileFormat == 1)
		{
			if (fmt1 == NULL)
				fmt1 = fmtNew ("%-16N %4n %-16F %C");
			length = fmtPrint (fmt1, mio, tag);
		}
		else
		{
			if (fmt2 == NULL)
				fmt2 = fmtNew ("%-16N %-10K %4n %-16F %C");
			length = fmtPrint (fmt2, mio, tag);
		}
	}

	mio_putc (mio, '\n');
	length++;

	return length;
}
