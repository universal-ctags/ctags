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
#include "parse.h"
#include "parse_p.h"
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

static const char* ada_suffix (const tagEntryInfo *const tag, const char *const line)
{
	kindDefinition *kdef = getLanguageKind(tag->langType, tag->kindIndex);

	Assert (kdef);

	/* Mapping from ctags' kind letter to etags's suffix string.
	 * See https://www.gnu.org/software/emacs/manual/html_node/emacs/Tag-Syntax.html */
	switch (kdef->letter)
	{
	case 'p':
	case 'k':
		return "/b";
	case 'K':
		return "/k";
	case 'P':
		return "/s";
	case 't':
		return "/t";
	case 'R':
	case 'r':
	{
		/* Unlike etags, ctags uses the procedure kind for both
		 * procedures and functions. So in the level, emitting a tag,
		 * we cannot distinguish whether a tag is for a procedureor a
		 * function.
		 *
		 * If the typeref field of the tag is filled, we can say the tag
		 * is for a function. However, Ada parser doesn't implement the
		 * typeref field yet, and implementing it is not so easy.
		 *
		 * So we have to take an unclean way here: scanning the input
		 * line again.
		 * FIXME: remove the scanning code and implement the typeref field
		 * in Ada.
		 */
		const char *r = strstr (line, "return");
		const char *f = strstr (line, "function");
		const char *p = strstr (line, "procedure");
		if (r && f)
			return "/f";
		else if (p && !r)
			return "/p";
		return "";				/* Unknown */
	}
	default:
		return "";
	}
}

static int writeEtagsEntry (tagWriter *writer,
							MIO * mio, const tagEntryInfo *const tag,
							void *clientData CTAGS_ATTR_UNUSED)
{
	langType adaLangType = getNamedLanguage ("Ada", 0);
	Assert (adaLangType != LANG_IGNORE);

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

		length = mio_printf (mio, "%s\177%s%s\001%lu,%ld\n", line,
							 tag->name,
							 (tag->langType == adaLangType)
							 ? ada_suffix (tag, line)
							 : "",
							 tag->lineNumber, seekValue);
	}
	etags->byteCount += length;

	return length;
}
