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

#include "types.h"
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
extern unsigned long getInputLineNumberForFileOffset(long offset);
extern int getInputLineOffset (void);
extern const char *getInputFileName (void);
extern MIOPos getInputFilePosition (void);
extern MIOPos getInputFilePositionForLine (unsigned int line);
extern langType getInputLanguage (void);
extern bool isInputLanguage (langType lang);
extern bool isInputHeaderFile (void);
extern bool isInputLanguageKindEnabled (int kindIndex);
extern bool isInputLanguageRoleEnabled (int kindIndex, int roleIndex);

extern const unsigned char *getInputFileData (size_t *size);

extern int getcFromInputFile (void);
extern int getNthPrevCFromInputFile (unsigned int nth, int def);
extern int skipToCharacterInInputFile (int c);
extern int skipToCharacterInInputFile2 (int c0, int c1);
extern void ungetcToInputFile (int c);
extern const unsigned char *readLineFromInputFile (void);

extern unsigned long getSourceLineNumber (void);

/* Raw: reading from given a parameter, mio */
extern char *readLineRaw (vString *const vLine, MIO *const mio);

extern void     pushLanguage(const langType language);
extern langType popLanguage (void);

#endif  /* CTAGS_MAIN_READ_H */
