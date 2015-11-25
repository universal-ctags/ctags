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

#if defined(FILE_WRITE)
# define CONST_FILE
#else
# define CONST_FILE /* const */
/* TEMPORARY FIX 
   `File' global variable should be file local. */
#endif

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include <stdio.h>
#include <ctype.h>

#include "parse.h"
#include "vstring.h"

/*
*   MACROS
*/
#define getInputLineNumber()     File.input.lineNumber
#define getInputFileName()       vStringValue (File.input.name)
#define getInputFilePosition()   File.filePosition
#define getInputLanguage()       File.input.language
#define getInputLanguageName()   getLanguageName (File.input.language)
#define getInputFileTagPath()    File.input.tagPath
#define isInputLanguage(lang)         (boolean)((lang) == File.input.language)
#define isInputHeaderFile()           File.input.isHeader
#define isInputLanguageKindEnabled(c)  isLanguageKindEnabled (File.input.language, c)
#define getSourceFileName()      vStringValue (File.source.name)
#define getSourceFileTagPath()   File.source.tagPath
#define getSourceLanguage()      File.source.language
#define getSourceLanguageName()  getLanguageName (File.source.language)
#define getSourceLanguageFileKind()  getLanguageFileKind (File.source.language)
#define getSourceLanguageAllowNullTag() getLanguageAllowNullTag (File.source.language)
#define getSourceLineNumber()    File.source.lineNumber

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

/*  Maintains the state of the current source file.
 */
typedef struct sInputFileInfo {
	vString *name;           /* name to report for source file */
	char    *tagPath;        /* path of source file relative to tag file */
	unsigned long lineNumber;/* line number in the source file */
	boolean  isHeader;       /* is source file a header file? */
	langType language;       /* language of source file */
} inputFileInfo;

typedef struct sInputFile {
	vString    *path;          /* path of input file (if any) */
	vString    *line;          /* last line read from file */
	const unsigned char* currentLine;  /* current line being worked on */
	FILE       *fp;            /* stream used for reading the file */
	fpos_t      filePosition;  /* file position of current line */
	unsigned int ungetchIdx;
	int         ungetchBuf[3]; /* characters that were ungotten */
	boolean     eof;           /* have we reached the end of file? */
	boolean     newLine;       /* will the next character begin a new line? */

	/*  Contains data pertaining to the original `source' file in which the tag
	 *  was defined. This may be different from the `input' file when #line
	 *  directives are processed (i.e. the input file is preprocessor output).
	 */
	inputFileInfo input; /* name, lineNumber */
	inputFileInfo source;
} inputFile;

/*
*   GLOBAL VARIABLES
*/
extern CONST_FILE inputFile File;

/*
*   FUNCTION PROTOTYPES
*/
extern void freeInputFileResources (void);
extern boolean openInputFile (const char *const fileName, const langType language);
extern void closeInputFile (void);
extern int getcFromInputFile (void);
extern int skipToCharacterInInputFile (int c);

extern void fileUngetc (int c);
extern const unsigned char *fileReadLine (void);

extern char *readLine (vString *const vLine, FILE *const fp);
extern char *readSourceLine (vString *const vLine, fpos_t location, long *const pSeekValue);
extern char *readSourceLineSlow (vString *const vLine, unsigned long lineNumber, const char *pattern, long *const pSeekValue);
extern char* readLineWithNoSeek (vString *const vline, FILE *const pp);

#endif  /* CTAGS_MAIN_READ_H */

/* vi:set tabstop=4 shiftwidth=4: */
