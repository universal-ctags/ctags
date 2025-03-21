/*
*   Copyright (c) 1998-2002, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   Main part private interface to read.c
*/
#ifndef CTAGS_MAIN_READ_PRIVATE_H
#define CTAGS_MAIN_READ_PRIVATE_H

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include "mio.h"
#include "types.h"
#include "vstring.h"

/*
*   DATA DECLARATIONS
*/

enum areaBoundaryFlag {
	AREA_BOUNDARY_START = 1UL << 0,
	AREA_BOUNDARY_END   = 1UL << 1,
};

/*
*   FUNCTION PROTOTYPES
*/

extern const char *getInputLanguageName (void);
extern const char *getInputFileTagPath (void);

extern long getInputFileOffsetForLine (unsigned int line);

extern unsigned int countInputLanguageKinds (void);
extern unsigned int countInputLanguageRoles (int kindIndex);

extern bool doesInputLanguageRequestAutomaticFQTag (const tagEntryInfo *e);
extern bool doesSubparserRun (void);
extern langType getLanguageForBaseParser (void);

extern bool isParserMarkedNoEmission (void);
extern void freeInputFileResources (void);

/* Stream opened by getMio can be passed to openInputFile as the 3rd
   argument. If the 3rd argument is NULL, openInputFile calls getMio
   internally. The 3rd argument is introduced for reusing mio object
   created in parser guessing stage. */
extern bool openInputFile (const char *const fileName, const langType language, MIO *mio, time_t mtime);
extern MIO *getMio (const char *const fileName, const char *const openMode,
				    bool memStreamRequired);
extern void resetInputFile (const langType language, bool resetLineFposMap_);
extern void closeInputFile (void);
extern void *getInputFileUserData(void);

extern unsigned int getAreaBoundaryInfo (unsigned long lineNumber);

extern const char *getSourceFileTagPath (void);
extern langType getSourceLanguage (void);

extern time_t getInputFileMtime (void);

/* Bypass: reading from fp in inputFile WITHOUT updating fields in input fields */
extern char *readLineFromBypass (vString *const vLine, MIOPos location, long *const pSeekValue);
extern void   pushArea (
				       bool useMemoryStreamInput,
				       unsigned long startLine, long startCharOffset,
				       unsigned long endLine, long endCharOffset,
				       unsigned long sourceLineOffset,
				       int promise);
extern void   popArea  (void);

extern bool isAreaStacked (void);

#define THIN_AREA_SPEC 0, 0, 0, 0, 0
extern bool isThinAreaSpec (unsigned long startLine, long startCharOffset,
							unsigned long endLine, long endCharOffset,
							unsigned long sourceLineOffset);

#endif  /* CTAGS_MAIN_READ_PRIVATE_H */
