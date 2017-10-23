/*
*   Copyright (c) 1998-2002, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   External interface to read.c
*/
#ifndef CTAGS_MAIN_READ_H
#define CTAGS_MAIN_READ_H

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include <stdio.h>
#include <ctype.h>

#include "parse.h"
#include "vstring.h"
#include "mio.h"

/*
*   MACROS
*/

/*
*   DATA DECLARATIONS
*/

enum eCharacters {
	/* white space characters */
	SPACE         = ' ',
	NEWLINE       = '\n',
	CRETURN       = '\r',
	FORMFEED      = '\f',
	TAB           = '\t',
	VTAB          = '\v',

	/* some hard to read characters */
	DOUBLE_QUOTE  = '"',
	SINGLE_QUOTE  = '\'',
	BACKSLASH     = '\\',

	/* symbolic representations, above 0xFF not to conflict with any byte */
	STRING_SYMBOL = ('S' + 0xff),
	CHAR_SYMBOL   = ('C' + 0xff)
};


/*
*   FUNCTION PROTOTYPES
*/

/* InputFile: reading from fp in inputFile with updating fields in input fields */
extern unsigned long getInputLineNumber (void);
extern int getInputLineOffset (void);
extern const char *getInputFileName (void);
extern MIOPos getInputFilePosition (void);
extern MIOPos getInputFilePositionForLine (unsigned int line);
extern langType getInputLanguage (void);
extern const char *getInputLanguageName (void);
extern const char *getInputFileTagPath (void);
extern bool isInputLanguage (langType lang);
extern bool isInputHeaderFile (void);
extern bool isInputLanguageKindEnabled (int kindIndex);

extern bool isInputLanguageRoleEnabled (int kindIndex, int roleIndex);
extern unsigned int countInputLanguageKinds (void);
extern unsigned int countInputLanguageRoles (int kindIndex);

extern bool doesInputLanguageAllowNullTag (void);
extern bool doesInputLanguageRequestAutomaticFQTag (void);
extern bool doesParserRunAsGuest (void);
extern bool doesSubparserRun (void);
extern bool isParserMarkedNoEmission (void);

extern void freeInputFileResources (void);
extern const unsigned char *getInputFileData (size_t *size);

/* Stream opened by getMio can be passed to openInputFile as the 3rd
   argument. If the 3rd argument is NULL, openInputFile calls getMio
   internally. The 3rd argument is introduced for reusing mio object
   created in parser guessing stage. */
extern bool openInputFile (const char *const fileName, const langType language, MIO *mio);
extern MIO *getMio (const char *const fileName, const char *const openMode,
				    bool memStreamRequired);
extern void resetInputFile (const langType language);

extern void closeInputFile (void);
extern void *getInputFileUserData(void);
extern int getcFromInputFile (void);
extern int getNthPrevCFromInputFile (unsigned int nth, int def);
extern int skipToCharacterInInputFile (int c);
extern void ungetcToInputFile (int c);
extern const unsigned char *readLineFromInputFile (void);

enum nestedInputBoundaryFlag {
	BOUNDARY_START = 1UL << 0,
	BOUNDARY_END   = 1UL << 1,
};
extern unsigned int getNestedInputBoundaryInfo (unsigned long lineNumber);

extern const char *getSourceFileTagPath (void);
extern langType getSourceLanguage (void);
extern unsigned long getSourceLineNumber (void);

/* Raw: reading from given a parameter, mio */
extern char *readLineRaw (vString *const vLine, MIO *const mio);

/* Bypass: reading from fp in inputFile WITHOUT updating fields in input fields */
extern char *readLineFromBypass (vString *const vLine, MIOPos location, long *const pSeekValue);
extern char *readLineFromBypassSlow (vString *const vLine, unsigned long lineNumber,
				     const char *pattern, long *const pSeekValue);

extern void   pushNarrowedInputStream (
				       unsigned long startLine, long startCharOffset,
				       unsigned long endLine, long endCharOffset,
				       unsigned long sourceLineOffset);
extern void   popNarrowedInputStream  (void);

extern void     pushLanguage(const langType language);
extern langType popLanguage (void);

extern unsigned long getInputLineNumberForFileOffset(long offset);

#define THIN_STREAM_SPEC 0, 0, 0, 0, 0
extern bool isThinStreamSpec(unsigned long startLine, long startCharOffset,
							 unsigned long endLine, long endCharOffset,
							 unsigned long sourceLineOffset);

#endif  /* CTAGS_MAIN_READ_H */
