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
#include "entry.h"
#include "mio.h"
#include "options.h"
#include "output.h"
#include "read.h"
#include "vstring.h"

struct sEtags {
	char *name;
	MIO *mio;
	size_t byteCount;
	vString *vLine;
};



extern void *beginEtagsFile (MIO *mio)
{
	static struct sEtags etags = { NULL, NULL, 0, NULL };

	etags.mio = tempFile ("w+b", &etags.name);
	etags.byteCount = 0;
	etags.vLine = vStringNew ();
	return &etags;
}

extern void endEtagsFile (MIO *mainfp, const char *filename, void *data)
{
	const char *line;
	struct sEtags *etags = data;

	mio_printf (mainfp, "\f\n%s,%ld\n", filename, (long) etags->byteCount);
	abort_if_ferror (mainfp);

	if (etags->mio != NULL)
	{
		mio_rewind (etags->mio);

		while ((line = readLineRaw (etags->vLine, etags->mio)) != NULL)
			mio_puts (mainfp, line);

		vStringDelete (etags->vLine);
		mio_free (etags->mio);
		remove (etags->name);
		eFree (etags->name);
		etags->vLine = NULL;
		etags->mio = NULL;
		etags->name = NULL;
	}
}

extern int writeEtagsEntry (MIO * mio, const tagEntryInfo *const tag, void *data)
{
	int length;
	struct sEtags *etags = data;

	mio = etags->mio;

	if (tag->isFileEntry)
		length = mio_printf (mio, "\177%s\001%lu,0\n",
				tag->name, tag->lineNumber);
	else
	{
		long seekValue;
		char *const line =
				readLineFromBypassAnyway (etags->vLine, tag, &seekValue);
		if (line == NULL)
			return 0;

		if (tag->truncateLine)
			truncateTagLine (line, tag->name, TRUE);
		else
			line [strlen (line) - 1] = '\0';

		length = mio_printf (mio, "%s\177%s\001%lu,%ld\n", line,
				tag->name, tag->lineNumber, seekValue);
	}
	etags->byteCount += length;

	return length;
}
