/*
*   Copyright (c) 2016, Red Hat, Inc.
*   Copyright (c) 2016, Masatake YAMATO
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*/
#ifndef CTAGS_MAIN_WRITER_H
#define CTAGS_MAIN_WRITER_H

#include "general.h"  /* must always come first */
#include "mio.h"
#include "types.h"

/* Other than writeEntry can be NULL.
   The value returned from preWriteEntry is passed to writeEntry,
   and postWriteEntry. If a resource is allocated in
   preWriteEntry, postWriteEntry should free it. */

typedef enum eWriterType {
	WRITER_DEFAULT,
	WRITER_CTAGS = WRITER_DEFAULT,
	WRITER_ETAGS,
	WRITER_XREF,
	WRITER_JSON,
	WRITER_COUNT,
} writerType;

struct sTagWriter;
typedef struct sTagWriter tagWriter;
struct sTagWriter {
	int (* writeEntry) (MIO * mio, const tagEntryInfo *const tag, void *data);
	int (* writePtagEntry) (MIO * mio, const ptagDesc *desc,
							const char *const fileName,
							const char *const pattern,
							const char *const parserName, void *data);
	void * (* preWriteEntry) (MIO * mio);
	void (* postWriteEntry)  (MIO * mio, const char* filename, void *data);
	bool useStdoutByDefault;

	writerType type;
};

extern void setTagWriter (writerType otype);
extern void writerSetup  (MIO *mio);
extern void writerTeardown (MIO *mio, const char *filename);

int writerWriteTag (MIO * mio, const tagEntryInfo *const tag);
int writerWritePtag (MIO * mio,
					 const ptagDesc *desc,
					 const char *const fileName,
					 const char *const pattern,
					 const char *const parserName);

extern bool outputFormatUsedStdoutByDefault (void);

extern int makePatternStringCommon (const tagEntryInfo *const tag,
				    int putc_func (char , void *),
				    int puts_func (const char* , void *),
				    void *output);
extern void truncateTagLine (char *const line, const char *const token,
			     const bool discardNewline);
extern void abort_if_ferror(MIO *const fp);

extern bool ptagMakeJsonOutputVersion (ptagDesc *desc, void *data CTAGS_ATTR_UNUSED);

#endif
