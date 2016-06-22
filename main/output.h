/*
*   Copyright (c) 2016, Red Hat, Inc.
*   Copyright (c) 2016, Masatake YAMATO
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*/
#ifndef CTAGS_MAIN_OUTPUT_H
#define CTAGS_MAIN_OUTPUT_H

#include "general.h"  /* must always come first */

/* preFuc and Postfunc can be NULL.
   The value returned from preWriteEntryFunc is passed to writeEntryFunc,
   and postWriteEntryFunc. If resource is a resource is allocated in
   preWriteEntryFunc, it should be freed in postWriteEntryFunc. */

typedef int (* writeEntryFunc) (MIO * mio, const tagEntryInfo *const tag, void *data);
typedef void * (* preWriteEntryFunc) (MIO * mio);
typedef void (* postWriteEntryFunc)  (MIO * mio, const char* filename, void *data);

extern void setTagWriter (writeEntryFunc func,
			  preWriteEntryFunc preFunc,
			  postWriteEntryFunc postFunc);


extern int writeEtagsEntry (MIO * mio, const tagEntryInfo *const tag, void *data);
extern void *beginEtagsFile (MIO * mio);
extern void  endEtagsFile   (MIO * mio, const char* filename, void *data);

extern int writeCtagsEntry (MIO * mio, const tagEntryInfo *const tag, void *data __unused__);
extern int writeXrefEntry  (MIO * mio, const tagEntryInfo *const tag, void *data __unused__);

extern int makePatternStringCommon (const tagEntryInfo *const tag,
				    int putc_func (char , void *),
				    int puts_func (const char* , void *),
				    void *output);
extern void truncateTagLine (char *const line, const char *const token,
			     const boolean discardNewline);
extern void abort_if_ferror(MIO *const fp);

#endif 
