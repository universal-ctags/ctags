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
#include "mio.h"

/*
*   MACROS
*/
#define getInputLineNumber()     File.input.lineNumber
#define getInputFileName()       vStringValue (File.input.name)
#define getInputFilePosition()   File.filePosition
#define getInputFilePositionForLine(line) \
	File.lineFposMap.pos[(((File.lineFposMap.count > (line - 1)) \
			       && (line > 0))? (line - 1): 0)]
#define getInputLanguage()       File.input.language
#define getInputLanguageName()   getLanguageName (File.input.language)
#define getInputFileTagPath()    vStringValue (File.input.tagPath)
#define isInputLanguage(lang)         (boolean)((lang) == File.input.language)
#define isInputHeaderFile()           File.input.isHeader
#define isInputLanguageKindEnabled(c)  isLanguageKindEnabled (File.input.language, c)
#define doesInputLanguageAllowNullTag() doesLanguageAllowNullTag (File.input.language)
#define getInputLanguageFileKind()  getLanguageFileKind (File.input.language)

#define getSourceFileTagPath()   vStringValue (File.source.tagPath)
#define getSourceLanguageName()  getLanguageName (File.source.language)
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

/*  Maintains the state of the current input file.
 */
typedef struct sInputFileInfo {
	vString *name;           /* name to report for input file */
	vString *tagPath;        /* path of input file relative to tag file */
	unsigned long lineNumber;/* line number in the input file */
	boolean  isHeader;       /* is input file a header file? */
	langType language;       /* language of input file */
} inputFileInfo;

typedef struct sInputLineFposMap {
	MIOPos *pos;
	unsigned int count;
	unsigned int size;
} inputLineFposMap;

typedef struct sInputFile {
	vString    *path;          /* path of input file (if any) */
	vString    *line;          /* last line read from file */
	const unsigned char* currentLine;  /* current line being worked on */
	MIO        *fp;            /* stream used for reading the file */
	MIOPos      filePosition;  /* file position of current line */
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

	/* sourceTagPathHolder is a kind of trash box.
	   The buffer pointed by tagPath field of source field can
	   be referred from tagsEntryInfo instances. sourceTagPathHolder
	   is used keeping the buffer till all processing about the current
	   input file is done. After all processing is done, the buffers
	   in sourceTagPathHolder are destroied. */
	stringList  * sourceTagPathHolder;
	inputLineFposMap lineFposMap;
} inputFile;

/*
*   GLOBAL VARIABLES
*/
extern CONST_FILE inputFile File;

/*
*   FUNCTION PROTOTYPES
*/

/* InputFile: reading from fp in inputFile with updating fields in input fields */
extern void                 freeInputFileResources (void);
extern boolean              openInputFile (const char *const fileName, const langType language);
extern void                 closeInputFile (void);
extern int                  getcFromInputFile (void);
extern int                  getNthPrevCFromInputFile (unsigned int nth, int def);
extern int                  skipToCharacterInInputFile (int c);
extern void                 ungetcToInputFile (int c);
extern const unsigned char *readLineFromInputFile (void);

/* Raw: reading from given a parameter, fp */
extern char *readLineRaw           (vString *const vLine, MIO *const fp);
extern char* readLineRawWithNoSeek (vString *const vline, FILE *const pp);

/* Bypass: reading from fp in inputFile WITHOUT updating fields in input fields */
extern char *readLineFromBypass (vString *const vLine, MIOPos location, long *const pSeekValue);
extern char *readLineFromBypassSlow (vString *const vLine, unsigned long lineNumber,
				     const char *pattern, long *const pSeekValue);


#endif  /* CTAGS_MAIN_READ_H */

/* vi:set tabstop=4 shiftwidth=4: */
