/*
*   Copyright (c) 1998-2002, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   External interface to entry.c
*/

#include "general.h"  /* must always come first */

#include <string.h>

#include "debug.h"
#include "entry_p.h"
#include "mio.h"
#include "options_p.h"
#include "read.h"
#include "routines.h"
#include "routines_p.h"
#include "vstring.h"
#include "writer_p.h"


#define ETAGS_FILE  "TAGS"


static int writeEtagsEntry  (tagWriter *writer, MIO * mio, const tagEntryInfo *const tag,
							 void *clientData CTAGS_ATTR_UNUSED);
static void *beginEtagsFile (tagWriter *writer, MIO * mio,
							 void *clientData CTAGS_ATTR_UNUSED);
static bool  endEtagsFile   (tagWriter *writer, MIO * mio, const char* filename,
							 void *clientData CTAGS_ATTR_UNUSED);

tagWriter etagsWriter = {
	.writeEntry = writeEtagsEntry,
	.writePtagEntry = NULL,
	.preWriteEntry = beginEtagsFile,
	.postWriteEntry = endEtagsFile,
	.rescanFailedEntry = NULL,
	.treatFieldAsFixed = NULL,
	.defaultFileName = ETAGS_FILE,
};

struct sEtags {
	char *name;
	MIO *mio;
	size_t byteCount;
	vString *vLine;
};



static void *beginEtagsFile (tagWriter *writer CTAGS_ATTR_UNUSED, MIO *mio CTAGS_ATTR_UNUSED,
							 void *clientData CTAGS_ATTR_UNUSED)
{
	static struct sEtags etags = { NULL, NULL, 0, NULL };

	etags.mio = tempFile ("w+b", &etags.name);
	etags.byteCount = 0;
	etags.vLine = vStringNew ();
	return &etags;
}

static bool endEtagsFile (tagWriter *writer,
						  MIO *mainfp, const char *filename,
						  void *clientData CTAGS_ATTR_UNUSED)
{
	const char *line;
	struct sEtags *etags = writer->private;

	mio_printf (mainfp, "\f\n%s,%ld\n", filename, (long) etags->byteCount);
	setNumTagsAdded (numTagsAdded () + 1);
	abort_if_ferror (mainfp);

	if (etags->mio != NULL)
	{
		mio_rewind (etags->mio);

		while ((line = readLineRaw (etags->vLine, etags->mio)) != NULL)
			mio_puts (mainfp, line);

		vStringDelete (etags->vLine);
		mio_unref (etags->mio);
		remove (etags->name);
		eFree (etags->name);
		etags->vLine = NULL;
		etags->mio = NULL;
		etags->name = NULL;
	}
	return false;
}

static int writeEtagsEntry (tagWriter *writer,
							MIO * mio, const tagEntryInfo *const tag,
							void *clientData CTAGS_ATTR_UNUSED)
{
	int length;
	struct sEtags *etags = writer->private;

	mio = etags->mio;

	if (tag->isFileEntry)
		length = mio_printf (mio, "\177%s\001%lu,0\n",
				tag->name, tag->lineNumber);
	else
	{
		size_t len;
		long seekValue;
		char *const line =
				readLineFromBypassForTag (etags->vLine, tag, &seekValue);
		if (line == NULL || line [0] == '\0')
			return 0;

		len = strlen (line);

		if (tag->truncateLineAfterTag)
			truncateTagLineAfterTag (line, tag->name, true);
		else if (line [len - 1] == '\n')
			line [--len] = '\0';

		if (Option.patternLengthLimit > 0 && Option.patternLengthLimit < len)
		{
			unsigned int truncationLength = Option.patternLengthLimit;

			/* don't cut in the middle of a UTF-8 character, but don't allow
			 * for more than one extra character in case it actually wasn't
			 * UTF-8.  See also entry.c:appendInputLine() */
			while (truncationLength < len &&
			       truncationLength < Option.patternLengthLimit + 3 &&
			       (((unsigned char) line[truncationLength]) & 0xc0) == 0x80)
				truncationLength++;

			line [truncationLength] = '\0';
		}

		length = mio_printf (mio, "%s\177%s\001%lu,%ld\n", line,
				tag->name, tag->lineNumber, seekValue);
	}
	etags->byteCount += length;

	return length;
}
