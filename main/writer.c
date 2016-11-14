/*
*   Copyright (c) 2016, Red Hat, Inc.
*   Copyright (c) 2016, Masatake YAMATO
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*/

#include "general.h"
#include "writer.h"

extern tagWriter ctagsWriter;
extern tagWriter etagsWriter;
extern tagWriter xrefWriter;
extern tagWriter jsonWriter;

static tagWriter *writerTable [WRITER_COUNT] = {
	[WRITER_CTAGS] = &ctagsWriter,
	[WRITER_ETAGS] = &etagsWriter,
	[WRITER_XREF]  = &xrefWriter,
	[WRITER_JSON]  = &jsonWriter,
};

static void *writerData;
static tagWriter *writer;

extern void setTagWriter (writerType wtype)
{
	writer = writerTable [wtype];
	writer->type = wtype;
}

extern bool outputFormatUsedStdoutByDefault (void)
{
	return writer->useStdoutByDefault;
}

extern void writerSetup (MIO *mio)
{
	if (writer->preWriteEntry)
		writerData = writer->preWriteEntry (mio);
	else
		writerData = NULL;
}

extern void writerTeardown (MIO *mio, const char *filename)
{
	if (writer->postWriteEntry)
	{
		writer->postWriteEntry (mio, filename, writerData);
		writerData = NULL;
	}
}

extern int writerWriteTag (MIO * mio, const tagEntryInfo *const tag)
{
	return writer->writeEntry (mio, tag, writerData);
}

extern int writerWritePtag (MIO * mio,
					 const ptagDesc *desc,
					 const char *const fileName,
					 const char *const pattern,
					 const char *const parserName)
{
	if (writer->writePtagEntry == NULL)
		return -1;

	return writer->writePtagEntry (mio, desc, fileName,
								   pattern, parserName, writerData);

}
