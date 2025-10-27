/*
*   Copyright (c) 2016, Red Hat, Inc.
*   Copyright (c) 2016, Masatake YAMATO
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*/
#ifndef CTAGS_MAIN_WRITER_PRIVATE_H
#define CTAGS_MAIN_WRITER_PRIVATE_H

#include "general.h"  /* must always come first */
#include "mio.h"
#include "options_p.h"
#include "types.h"

/* Other than writeEntry can be NULL.
   The value returned from preWriteEntry is passed to writeEntry,
   and postWriteEntry. If a resource is allocated in
   preWriteEntry, postWriteEntry should free it. */

typedef enum eWriterType {
	WRITER_UNAVAILABLE = -2,	/* Defined but no implementation. */
	WRITER_UNKNOWN = -1,
	WRITER_DEFAULT,
	WRITER_U_CTAGS = WRITER_DEFAULT,
	WRITER_E_CTAGS,
	WRITER_ETAGS,
	WRITER_XREF,
#ifdef HAVE_JANSSON
	WRITER_JSON,
#endif
	WRITER_CUSTOM,
	WRITER_COUNT,
} writerType;

struct sTagWriter;
typedef struct sTagWriter tagWriter;
struct sTagWriter {
	const char *oformat;		/* name used in CLI: --output-format=
								 * NULL is acceptable.*/
	int (* writeEntry) (tagWriter *writer, MIO * mio, const tagEntryInfo *const tag,
						void *clientData);
	int (* writePtagEntry) (tagWriter *writer, MIO * mio, const ptagDesc *desc,
							const char *const fileName,
							const char *const pattern,
							const char *const parserName,
							void *clientData);
	bool printPtagByDefault;
	void * (* preWriteEntry) (tagWriter *writer, MIO * mio,
							  void *clientData);

	/* Returning TRUE means the output file may be shrunk.
	   In such case the callee may do truncate output file. */
	bool (* postWriteEntry)  (tagWriter *writer, MIO * mio, const char* filename,
							  void *clientData);
	void (* rescanFailedEntry) (tagWriter *writer, unsigned long validTagNum,
								void *clientData);
	bool (* treatFieldAsFixed) (int fieldType);

	void (* checkOptions) (tagWriter *writer, bool fieldsWereReset);

	bool canPrintNullTag;

#ifdef _WIN32
	enum filenameSepOp (* overrideFilenameSeparator) (enum filenameSepOp currentSetting);
#endif	/* _WIN32 */

	const char *defaultFileName;

	/* The value returned from preWriteEntry is stored `private' field.
	   The value must be released in postWriteEntry. */
	void *private;
	writerType type;
	/* The value passed as the second argument for writerSetup iss
	 * stored here. Unlink `private' field, ctags does nothing more. */
	void *clientData;
};

/* customWriter is used only if otype is WRITER_CUSTOM */
extern void setTagWriter (writerType otype, tagWriter *customWriter);
extern void writerSetup  (MIO *mio, void *clientData);
extern bool writerTeardown (MIO *mio, const char *filename);

int writerWriteTag (MIO * mio, const tagEntryInfo *const tag);
int writerWritePtag (MIO * mio,
					 const ptagDesc *desc,
					 const char *const fileName,
					 const char *const pattern,
					 const char *const parserName);

void writerRescanFailed (unsigned long validTagNum);

extern const char *outputDefaultFileName (void);

extern size_t truncateTagLineAfterTag (char *const line, const char *const token,
			     const bool discardNewline);
extern void abort_if_ferror(MIO *const fp);

extern bool ptagMakeJsonOutputVersion (ptagDesc *desc, langType language CTAGS_ATTR_UNUSED, const void *data CTAGS_ATTR_UNUSED);
extern bool ptagMakeCtagsOutputMode (ptagDesc *desc, langType language CTAGS_ATTR_UNUSED, const void *data CTAGS_ATTR_UNUSED);
extern bool ptagMakeCtagsOutputFilesep (ptagDesc *desc, langType language CTAGS_ATTR_UNUSED, const void *data);
extern bool ptagMakeCtagsOutputExcmd (ptagDesc *desc, langType language CTAGS_ATTR_UNUSED, const void *data);

extern bool writerCanPrintPtag (void);
extern bool writerCanPrintNullTag (void);
extern bool writerDoesTreatFieldAsFixed (int fieldType);

extern void writerCheckOptions (bool fieldsWereReset);
extern bool writerPrintPtagByDefault (void);

extern writerType getWrierForOutputFormat (const char *oformat);
extern void printOutputFormats (bool withListHeader, bool machinable, FILE *fp);

#ifdef _WIN32
extern enum filenameSepOp getFilenameSeparator (enum filenameSepOp currentSetting);
#endif	/* _WIN32 */
#endif	/* CTAGS_MAIN_WRITER_PRIVATE_H */
