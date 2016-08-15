/*
*   Copyright (c) 1996-2002, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains low level input and tag file read functions (newline
*   conversion for input files are performed at this level).
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include <string.h>
#include <ctype.h>

#define FILE_WRITE
#include "read.h"
#include "debug.h"
#include "entry.h"
#include "main.h"
#include "routines.h"
#include "options.h"
#ifdef HAVE_ICONV
# include "mbcs.h"
#endif

#include <ctype.h>
#include <stddef.h>
#ifdef HAVE_SYS_TYPES_H
# include <sys/types.h>  /* declare off_t (not known to regex.h on FreeBSD) */
#endif
#include <regex.h>

/*
*   DATA DECLARATIONS
*/

typedef struct sLangStack {
	langType *languages;
	unsigned int count;
	unsigned int size;
} langStack;

/*  Maintains the state of the current input file.
 */
typedef union sInputLangInfo {
	langStack stack;
	langType  type;
} inputLangInfo;

typedef struct sInputFileInfo {
	vString *name;           /* name to report for input file */
	vString *tagPath;        /* path of input file relative to tag file */
	unsigned long lineNumber;/* line number in the input file */
	unsigned long lineNumberOrigin; /* The value set to `lineNumber'
					   when `resetInputFile' is called
					   on the input stream.
					   This is needed for nested stream. */
	boolean  isHeader;       /* is input file a header file? */

	/* language of input file */
	inputLangInfo langInfo;
} inputFileInfo;

typedef struct sInputLineFposMap {
	MIOPos *pos;
	unsigned int count;
	unsigned int size;
} inputLineFposMap;

typedef struct sNestedInputStreamInfo {
	unsigned long startLine;
	int startCharOffset;
	unsigned long endLine;
	int endCharOffset;
} nestedInputStreamInfo;

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

	nestedInputStreamInfo nestedInputStreamInfo;

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
*   FUNCTION DECLARATIONS
*/
static void     langStackInit (langStack *langStack);
static langType langStackTop  (langStack *langStack);
static void     langStackPush (langStack *langStack, langType type);
static langType langStackPop  (langStack *langStack);
static void     langStackClear(langStack *langStack);


/*
*   DATA DEFINITIONS
*/
static inputFile File;  /* static read through functions */
static inputFile BackupFile;	/* File is copied here when a nested parser is pushed */
static MIOPos StartOfLine;  /* holds deferred position of start of line */

/*
*   FUNCTION DEFINITIONS
*/

extern unsigned long getInputLineNumber (void)
{
	return File.input.lineNumber;
}

extern const char *getInputFileName (void)
{
	return vStringValue (File.input.name);
}

extern MIOPos getInputFilePosition (void)
{
	return File.filePosition;
}

extern MIOPos getInputFilePositionForLine (int line)
{
	return File.lineFposMap.pos[(((File.lineFposMap.count > (line - 1)) \
				      && (line > 0))? (line - 1): 0)];
}

extern langType getInputLanguage (void)
{
	return langStackTop (&File.input.langInfo.stack);
}

extern const char *getInputLanguageName (void)
{
	return getLanguageName (getInputLanguage());
}

extern const char *getInputFileTagPath (void)
{
	return vStringValue (File.input.tagPath);
}

extern boolean isInputLanguage (langType lang)
{
	return (boolean)((lang) == getInputLanguage ());
}

extern boolean isInputHeaderFile (void)
{
	return File.input.isHeader;
}

extern boolean isInputLanguageKindEnabled (char c)
{
	return isLanguageKindEnabled (getInputLanguage (), c);
}

extern boolean doesInputLanguageAllowNullTag (void)
{
	return doesLanguageAllowNullTag (getInputLanguage ());
}

extern kindOption *getInputLanguageFileKind (void)
{
	return getLanguageFileKind (getInputLanguage ());
}

extern boolean doesInputLanguageRequestAutomaticFQTag (void)
{
	return doesLanguageRequestAutomaticFQTag (getInputLanguage ());
}

extern const char *getSourceFileTagPath (void)
{
	return vStringValue (File.source.tagPath);
}

extern const char *getSourceLanguageName (void)
{
	return getLanguageName (File.source.langInfo.type);
}

extern unsigned long getSourceLineNumber (void)
{
	return File.source.lineNumber;
}

static void freeInputFileInfo (inputFileInfo *finfo)
{
	if (finfo->name)
	{
		vStringDelete (finfo->name);
		finfo->name = NULL;
	}
	if (finfo->tagPath)
	{
		vStringDelete (finfo->tagPath);
		finfo->tagPath = NULL;
	}
}

extern void freeInputFileResources (void)
{
	if (File.path != NULL)
		vStringDelete (File.path);
	if (File.line != NULL)
		vStringDelete (File.line);
	freeInputFileInfo (&File.input);
	freeInputFileInfo (&File.source);
}

extern const unsigned char *getInputFileData (size_t *size)
{
	return mio_memory_get_data (File.fp, size);
}

/*
 * inputLineFposMap related functions
 */
static void freeLineFposMap (inputLineFposMap *lineFposMap)
{
	if (lineFposMap->pos)
	{
		free (lineFposMap->pos);
		lineFposMap->pos = NULL;
		lineFposMap->count = 0;
		lineFposMap->size = 0;
	}
}

static void allocLineFposMap (inputLineFposMap *lineFposMap)
{
#define INITIAL_lineFposMap_LEN 256
	lineFposMap->pos = xCalloc (INITIAL_lineFposMap_LEN, MIOPos);
	lineFposMap->size = INITIAL_lineFposMap_LEN;
	lineFposMap->count = 0;
}

static void appendLineFposMap (inputLineFposMap *lineFposMap, MIOPos pos)
{
	if (lineFposMap->size == lineFposMap->count)
	{
		lineFposMap->size *= 2;
		lineFposMap->pos = xRealloc (lineFposMap->pos,
					     lineFposMap->size,
					     MIOPos);
	}
	lineFposMap->pos [lineFposMap->count] = pos;
	lineFposMap->count++;
}

/*
 *   Input file access functions
 */

static void setOwnerDirectoryOfInputFile (const char *const fileName)
{
	const char *const head = fileName;
	const char *const tail = baseFilename (head);

	if (File.path != NULL)
		vStringDelete (File.path);
	if (tail == head)
		File.path = NULL;
	else
	{
		const size_t length = tail - head - 1;
		File.path = vStringNew ();
		vStringNCopyS (File.path, fileName, length);
	}
}

static void setInputFileParametersCommon (inputFileInfo *finfo, vString *const fileName,
					  const langType language,
					  void (* setLang) (inputLangInfo *, langType),
					  stringList *holder)
{
	if (finfo->name != NULL)
		vStringDelete (finfo->name);
	finfo->name = fileName;

	if (finfo->tagPath != NULL)
	{
		if (holder)
			stringListAdd (holder, finfo->tagPath);
		else
			vStringDelete (finfo->tagPath);
	}
	if (! Option.tagRelative || isAbsolutePath (vStringValue (fileName)))
		finfo->tagPath = vStringNewCopy (fileName);
	else
		finfo->tagPath =
				vStringNewOwn (relativeFilename (vStringValue (fileName),
								 getTagFileDirectory ()));

	finfo->isHeader = isIncludeFile (vStringValue (fileName));

	setLang (& (finfo->langInfo), language);
}

static void resetLangOnStack (inputLangInfo *langInfo, langType lang)
{
	Assert (langInfo->stack.count > 0);
	langStackClear  (& (langInfo->stack));
	langStackPush (& (langInfo->stack), lang);
}

static void pushLangOnStack (inputLangInfo *langInfo, langType lang)
{
	langStackPush (& langInfo->stack, lang);
}

static langType popLangOnStack (inputLangInfo *langInfo)
{
	return langStackPop (& langInfo->stack);
}

static void clearLangOnStack (inputLangInfo *langInfo)
{
	return langStackClear (& langInfo->stack);
}

static void setLangToType  (inputLangInfo *langInfo, langType lang)
{
	langInfo->type = lang;
}

static void setInputFileParameters (vString *const fileName, const langType language)
{
	setInputFileParametersCommon (&File.input, fileName,
				      language, pushLangOnStack,
				      NULL);
}

static void setSourceFileParameters (vString *const fileName, const langType language)
{
	setInputFileParametersCommon (&File.source, fileName,
				      language, setLangToType,
				      File.sourceTagPathHolder);
}

static boolean setSourceFileName (vString *const fileName)
{
	const langType language = getFileLanguage (vStringValue (fileName));
	boolean result = FALSE;
	if (language != LANG_IGNORE)
	{
		vString *pathName;
		if (isAbsolutePath (vStringValue (fileName)) || File.path == NULL)
			pathName = vStringNewCopy (fileName);
		else
		{
			char *tmp = combinePathAndFile (
				vStringValue (File.path), vStringValue (fileName));
			pathName = vStringNewOwn (tmp);
		}
		setSourceFileParameters (pathName, language);
		result = TRUE;
	}
	return result;
}

/*
 *   Line directive parsing
 */

static int skipWhite (void)
{
	int c;
	do
		c = mio_getc (File.fp);
	while (c == ' '  ||  c == '\t');
	return c;
}

static unsigned long readLineNumber (void)
{
	unsigned long lNum = 0;
	int c = skipWhite ();
	while (c != EOF  &&  isdigit (c))
	{
		lNum = (lNum * 10) + (c - '0');
		c = mio_getc (File.fp);
	}
	mio_ungetc (File.fp, c);
	if (c != ' '  &&  c != '\t')
		lNum = 0;

	return lNum;
}

/* While ANSI only permits lines of the form:
 *   # line n "filename"
 * Earlier compilers generated lines of the form
 *   # n filename
 * GNU C will output lines of the form:
 *   # n "filename"
 * So we need to be fairly flexible in what we accept.
 */
static vString *readFileName (void)
{
	vString *const fileName = vStringNew ();
	boolean quoteDelimited = FALSE;
	int c = skipWhite ();

	if (c == '"')
	{
		c = mio_getc (File.fp);  /* skip double-quote */
		quoteDelimited = TRUE;
	}
	while (c != EOF  &&  c != '\n'  &&
			(quoteDelimited ? (c != '"') : (c != ' '  &&  c != '\t')))
	{
		vStringPut (fileName, c);
		c = mio_getc (File.fp);
	}
	if (c == '\n')
		mio_ungetc (File.fp, c);
	vStringPut (fileName, '\0');

	return fileName;
}

static boolean parseLineDirective (void)
{
	boolean result = FALSE;
	int c = skipWhite ();
	DebugStatement ( const char* lineStr = ""; )

	if (isdigit (c))
	{
		mio_ungetc (File.fp, c);
		result = TRUE;
	}
	else if (c == 'l'  &&  mio_getc (File.fp) == 'i'  &&
			 mio_getc (File.fp) == 'n'  &&  mio_getc (File.fp) == 'e')
	{
		c = mio_getc (File.fp);
		if (c == ' '  ||  c == '\t')
		{
			DebugStatement ( lineStr = "line"; )
			result = TRUE;
		}
	}
	if (result)
	{
		const unsigned long lNum = readLineNumber ();
		if (lNum == 0)
			result = FALSE;
		else
		{
			vString *const fileName = readFileName ();
			if (vStringLength (fileName) == 0)
			{
				File.source.lineNumber = lNum - 1;  /* applies to NEXT line */
				DebugStatement ( debugPrintf (DEBUG_RAW, "#%s %ld", lineStr, lNum); )
			}
			else if (setSourceFileName (fileName))
			{
				File.source.lineNumber = lNum - 1;  /* applies to NEXT line */
				DebugStatement ( debugPrintf (DEBUG_RAW, "#%s %ld \"%s\"",
								lineStr, lNum, vStringValue (fileName)); )
			}

			if (vStringLength (fileName) > 0 &&
				lNum == 1)
				makeFileTag (vStringValue (fileName));
			vStringDelete (fileName);
			result = TRUE;
		}
	}
	return result;
}

/*
 *   Input file I/O operations
 */

#define MAX_IN_MEMORY_FILE_SIZE (1024*1024)

extern MIO *getMio (const char *const fileName, const char *const openMode,
		    boolean memStreamRequired)
{
	FILE *src;
	fileStatus *st;
	unsigned long size;
	unsigned char *data;

	st = eStat (fileName);
	size = st->size;
	eStatFree (st);
	if ((!memStreamRequired)
	    && (size > MAX_IN_MEMORY_FILE_SIZE || size == 0))
		return mio_new_file (fileName, openMode);

	src = fopen (fileName, openMode);
	if (!src)
		return NULL;

	data = eMalloc (size);
	if (fread (data, 1, size, src) != size)
	{
		eFree (data);
		fclose (src);
		if (memStreamRequired)
			return NULL;
		else
			return mio_new_file (fileName, openMode);
	}
	fclose (src);
	return mio_new_memory (data, size, eRealloc, eFree);
}

/*  This function opens an input file, and resets the line counter.  If it
 *  fails, it will display an error message and leave the File.fp set to NULL.
 */
extern boolean openInputFile (const char *const fileName, const langType language,
			      MIO *mio)
{
	const char *const openMode = "rb";
	boolean opened = FALSE;
	boolean memStreamRequired;

	/*	If another file was already open, then close it.
	 */
	if (File.fp != NULL)
	{
		mio_free (File.fp);  /* close any open input file */
		File.fp = NULL;
	}

	/* File position is used as key for checking the availability of
	   pattern cache in entry.h. If an input file is changed, the
	   key is meaningless. So notifying the changing here. */
	invalidatePatternCache();

	if (File.sourceTagPathHolder == NULL)
		File.sourceTagPathHolder = stringListNew ();
	stringListClear (File.sourceTagPathHolder);

	memStreamRequired = doesParserRequireMemoryStream (language);

	if (mio)
	{
		size_t tmp;
		if (memStreamRequired && (!mio_memory_get_data (mio, &tmp)))
			mio = NULL;
		else
			mio_rewind (mio);
	}

	File.fp = mio? mio_ref (mio): getMio (fileName, openMode, memStreamRequired);

	if (File.fp == NULL)
		error (WARNING | PERROR, "cannot open \"%s\"", fileName);
	else
	{
		opened = TRUE;

		setOwnerDirectoryOfInputFile (fileName);
		mio_getpos (File.fp, &StartOfLine);
		mio_getpos (File.fp, &File.filePosition);
		File.currentLine  = NULL;
		File.eof          = FALSE;
		File.newLine      = TRUE;

		if (File.line != NULL)
			vStringClear (File.line);

		setInputFileParameters  (vStringNewInit (fileName), language);
		File.input.lineNumberOrigin = 0L;
		File.input.lineNumber = File.input.lineNumberOrigin;
		setSourceFileParameters (vStringNewInit (fileName), language);
		File.source.lineNumberOrigin = 0L;
		File.source.lineNumber = File.source.lineNumberOrigin;
		allocLineFposMap (&File.lineFposMap);

		verbose ("OPENING %s as %s language %sfile\n", fileName,
				getLanguageName (language),
				File.input.isHeader ? "include " : "");
	}
	return opened;
}

extern void resetInputFile (const langType language)
{
	Assert (File.fp);

	mio_rewind (File.fp);
	mio_getpos (File.fp, &StartOfLine);
	mio_getpos (File.fp, &File.filePosition);
	File.currentLine  = NULL;
	File.eof          = FALSE;
	File.newLine      = TRUE;

	if (File.line != NULL)
		vStringClear (File.line);

	resetLangOnStack (& (File.input.langInfo), language);
	File.input.lineNumber = File.input.lineNumberOrigin;
	setLangToType (& (File.source.langInfo), language);
	File.source.lineNumber = File.source.lineNumberOrigin;
}

extern void closeInputFile (void)
{
	if (File.fp != NULL)
	{
		clearLangOnStack (& (File.input.langInfo));

		/*  The line count of the file is 1 too big, since it is one-based
		 *  and is incremented upon each newline.
		 */
		if (Option.printTotals)
		{
			fileStatus *status = eStat (vStringValue (File.input.name));
			addTotals (0, File.input.lineNumber - 1L, status->size);
		}
		mio_free (File.fp);
		File.fp = NULL;
		freeLineFposMap (&File.lineFposMap);
	}
}

extern void *getInputFileUserData(void)
{
	return mio_get_user_data (File.fp);
}

/*  Action to take for each encountered input newline.
 */
static void fileNewline (void)
{
	File.filePosition = StartOfLine;

	if (BackupFile.fp == NULL)
		appendLineFposMap (&File.lineFposMap, File.filePosition);

	File.newLine = FALSE;
	File.input.lineNumber++;
	File.source.lineNumber++;
	DebugStatement ( if (Option.breakLine == File.input.lineNumber) lineBreak (); )
	DebugStatement ( debugPrintf (DEBUG_RAW, "%6ld: ", File.input.lineNumber); )
}

/*  This function reads a single character from the stream, performing newline
 *  canonicalization.
 */
static int iFileGetc (void)
{
	int	c;
readnext:
	c = mio_getc (File.fp);

	/*	If previous character was a newline, then we're starting a line.
	 */
	if (File.newLine  &&  c != EOF)
	{
		fileNewline ();
		if (c == '#'  &&  Option.lineDirectives)
		{
			if (parseLineDirective ())
				goto readnext;
			else
			{
				mio_setpos (File.fp, &StartOfLine);
				c = mio_getc (File.fp);
			}
		}
	}

	if (c == EOF)
		File.eof = TRUE;
	else if (c == NEWLINE)
	{
		File.newLine = TRUE;
		mio_getpos (File.fp, &StartOfLine);
	}
	else if (c == CRETURN)
	{
		/* Turn line breaks into a canonical form. The three commonly
		 * used forms if line breaks: LF (UNIX/Mac OS X), CR (Mac OS 9),
		 * and CR-LF (MS-DOS) are converted into a generic newline.
		 */
#ifndef macintosh
		const int next = mio_getc (File.fp);  /* is CR followed by LF? */
		if (next != NEWLINE)
			mio_ungetc (File.fp, next);
		else
#endif
		{
			c = NEWLINE;  /* convert CR into newline */
			File.newLine = TRUE;
			mio_getpos (File.fp, &StartOfLine);
		}
	}
	DebugStatement ( debugPutc (DEBUG_RAW, c); )
	return c;
}

extern void ungetcToInputFile (int c)
{
	const size_t len = ARRAY_SIZE (File.ungetchBuf);

	Assert (File.ungetchIdx < len);
	/* we cannot rely on the assertion that might be disabled in non-debug mode */
	if (File.ungetchIdx < len)
		File.ungetchBuf[File.ungetchIdx++] = c;
}

static vString *iFileGetLine (void)
{
	vString *result = NULL;
	int c;
	File.line = vStringNewOrClear (File.line);
	do
	{
		c = iFileGetc ();
		if (c != EOF)
			vStringPut (File.line, c);
		if (c == '\n'  ||  (c == EOF  &&  vStringLength (File.line) > 0))
		{
			vStringTerminate (File.line);

			if (vStringLength (File.line) > 0)
				matchRegex (File.line, getInputLanguage ());

			result = File.line;
			break;
		}
	} while (c != EOF);
	Assert (result != NULL  ||  File.eof);
	return result;
}

/*  Do not mix use of fileReadLine () and getcFromInputFile () for the same file.
 */
extern int getcFromInputFile (void)
{
	int c;

	/*  If there is an ungotten character, then return it.  Don't do any
	 *  other processing on it, though, because we already did that the
	 *  first time it was read through getcFromInputFile ().
	 */
	if (File.ungetchIdx > 0)
	{
		c = File.ungetchBuf[--File.ungetchIdx];
		return c;  /* return here to avoid re-calling debugPutc () */
	}
	do
	{
		if (File.currentLine != NULL)
		{
			c = *File.currentLine++;
			if (c == '\0')
				File.currentLine = NULL;
		}
		else
		{
			vString* const line = iFileGetLine ();
			if (line != NULL)
				File.currentLine = (unsigned char*) vStringValue (line);
			if (File.currentLine == NULL)
				c = EOF;
			else
				c = '\0';
		}
	} while (c == '\0');
	DebugStatement ( debugPutc (DEBUG_READ, c); )
	return c;
}

/* returns the nth previous character (0 meaning current), or def if nth cannot
 * be accessed.  Note that this can't access previous line data. */
extern int getNthPrevCFromInputFile (unsigned int nth, int def)
{
	const unsigned char *base = (unsigned char *) vStringValue (File.line);
	const unsigned int offset = File.ungetchIdx + 1 + nth;

	if (File.currentLine != NULL && File.currentLine >= base + offset)
		return (int) *(File.currentLine - offset);
	else
		return def;
}

extern int skipToCharacterInInputFile (int c)
{
	int d;
	do
	{
		d = getcFromInputFile ();
	} while (d != EOF && d != c);
	return d;
}

/*  An alternative interface to getcFromInputFile (). Do not mix use of fileReadLine()
 *  and getcFromInputFile() for the same file. The returned string does not contain
 *  the terminating newline. A NULL return value means that all lines in the
 *  file have been read and we are at the end of file.
 */
extern const unsigned char *readLineFromInputFile (void)
{
	vString* const line = iFileGetLine ();
	const unsigned char* result = NULL;
	if (line != NULL)
	{
		result = (const unsigned char*) vStringValue (line);
		vStringStripNewline (line);
		DebugStatement ( debugPrintf (DEBUG_READ, "%s\n", result); )
	}
	return result;
}

/*
 *   Raw file line reading with automatic buffer sizing
 */
extern char *readLineRaw (vString *const vLine, MIO *const fp)
{
	char *result = NULL;

	vStringClear (vLine);
	if (fp == NULL)  /* to free memory allocated to buffer */
		error (FATAL, "NULL file pointer");
	else
	{
		boolean reReadLine;

		/*  If reading the line places any character other than a null or a
		 *  newline at the last character position in the buffer (one less
		 *  than the buffer size), then we must resize the buffer and
		 *  reattempt to read the line.
		 */
		do
		{
			char *const pLastChar = vStringValue (vLine) + vStringSize (vLine) -2;
			long startOfLine;

			startOfLine = mio_tell(fp);
			reReadLine = FALSE;
			*pLastChar = '\0';
			result = mio_gets (fp, vStringValue (vLine), (int) vStringSize (vLine));
			if (result == NULL)
			{
				if (! mio_eof (fp))
					error (FATAL | PERROR, "Failure on attempt to read file");
			}
			else if (*pLastChar != '\0'  &&
					 *pLastChar != '\n'  &&  *pLastChar != '\r')
			{
				/*  buffer overflow */
				reReadLine = vStringAutoResize (vLine);
				if (reReadLine)
					mio_seek (fp, startOfLine, SEEK_SET);
				else
					error (FATAL | PERROR, "input line too big; out of memory");
			}
			else
			{
				char* eol;
				vStringLength(vLine) = mio_tell(fp) - startOfLine;
				/* canonicalize new line */
				eol = vStringValue (vLine) + vStringLength (vLine) - 1;
				if (*eol == '\r')
					*eol = '\n';
				else if (vStringLength (vLine) != 1 && *(eol - 1) == '\r'  &&  *eol == '\n')
				{
					*(eol - 1) = '\n';
					*eol = '\0';
					--vLine->length;
				}
			}
		} while (reReadLine);

#ifdef HAVE_ICONV
		if (isConverting ())
			convertString (vLine);
#endif
	}
	return result;
}

/*  Places into the line buffer the contents of the line referenced by
 *  "location".
 */
extern char *readLineFromBypass (
		vString *const vLine, MIOPos location, long *const pSeekValue)
{
	MIOPos orignalPosition;
	char *result;

	mio_getpos (File.fp, &orignalPosition);
	mio_setpos (File.fp, &location);
	if (pSeekValue != NULL)
		*pSeekValue = mio_tell (File.fp);
	result = readLineRaw (vLine, File.fp);
	mio_setpos (File.fp, &orignalPosition);
	/* If the file is empty, we can't get the line
	   for location 0. readLineFromBypass doesn't know
	   what itself should do; just report it to the caller. */
	return result;
}

/* If a xcmd parser is used, ctags cannot know the location for a tag.
 * In the other hand, etags output and cross reference output require the
 * line after the location.
 *
 * readLineFromBypassSlow retrieves the line for (lineNumber and pattern of a tag).
 */

extern char *readLineFromBypassSlow (vString *const vLine,
				 unsigned long lineNumber,
				 const char *pattern,
				 long *const pSeekValue)
{
	char *result = NULL;


	MIOPos originalPosition;
	char *line;
	size_t len;
	long pos;

	regex_t patbuf;
	char lastc;


	/*
	 * Compile the pattern
	 */
	{
		char *pat;
		int errcode;
		char errmsg[256];

		pat = eStrdup (pattern);
		pat[strlen(pat) - 1] = '\0';
		errcode = regcomp (&patbuf, pat + 1, 0);
		eFree (pat);

		if (errcode != 0)
		{
			regerror (errcode, &patbuf, errmsg, 256);
			error (WARNING, "regcomp %s in readLineFromBypassSlow: %s", pattern, errmsg);
			regfree (&patbuf);
			return NULL;
		}
	}

	/*
	 * Get the line for lineNumber
	 */
	{
		unsigned long n;

		mio_getpos (File.fp, &originalPosition);
		mio_rewind (File.fp);
		line = NULL;
		pos = 0;
		for (n = 0; n < lineNumber; n++)
		{
			pos = mio_tell (File.fp);
			line = readLineRaw (vLine, File.fp);
			if (line == NULL)
				break;
		}
		if (line == NULL)
			goto out;
		else
			len = strlen(line);

		if (len == 0)
			goto out;

		lastc = line[len - 1];
		if (lastc == '\n')
			line[len - 1] = '\0';
	}

	/*
	 * Match
	 */
	{
		regmatch_t pmatch;
		int after_newline = 0;
		if (regexec (&patbuf, line, 1, &pmatch, 0) == 0)
		{
			line[len - 1] = lastc;
			result = line + pmatch.rm_so;
			if (pSeekValue)
			{
				after_newline = ((lineNumber == 1)? 0: 1);
				*pSeekValue = pos + after_newline + pmatch.rm_so;
			}
		}
	}

out:
	regfree (&patbuf);
	mio_setpos (File.fp, &originalPosition);
	return result;
}

/*
 *   Similar to readLineRaw but this doesn't use fgetpos/fsetpos.
 *   Useful for reading from pipe.
 */
char* readLineRawWithNoSeek (vString* const vline, FILE *const pp)
{
	int c;
	boolean nlcr;
	char *result = NULL;

	vStringClear (vline);
	nlcr = FALSE;
	
	while (1)
	{
		c = fgetc (pp);
		
		if (c == EOF)
		{
			if (! feof (pp))
				error (FATAL | PERROR, "Failure on attempt to read file");
			else
				break;
		}

		result = vStringValue (vline);
		
		if (c == '\n' || c == '\r')
			nlcr = TRUE;
		else if (nlcr)
		{
			ungetc (c, pp);
			break;
		}
		else
			vStringPut (vline, c);
	}

	return result;
}

extern void   pushNarrowedInputStream (const langType language,
				       unsigned long startLine, int startCharOffset,
				       unsigned long endLine, int endCharOffset,
				       unsigned long sourceLineOffset)
{
	long p, q;
	MIOPos original;
	MIOPos tmp;
	MIO *subio;

	original = getInputFilePosition ();

	tmp = getInputFilePositionForLine (startLine);
	mio_setpos (File.fp, &tmp);
	mio_seek (File.fp, startCharOffset, SEEK_CUR);
	p = mio_tell (File.fp);

	tmp = getInputFilePositionForLine (endLine);
	mio_setpos (File.fp, &tmp);
	mio_seek (File.fp, endCharOffset, SEEK_CUR);
	q = mio_tell (File.fp);

	mio_setpos (File.fp, &original);

	subio = mio_new_mio (File.fp, p, q - p);


	BackupFile = File;

	File.fp = subio;
	File.nestedInputStreamInfo.startLine = startLine;
	File.nestedInputStreamInfo.startCharOffset = startCharOffset;
	File.nestedInputStreamInfo.endLine = endLine;
	File.nestedInputStreamInfo.endCharOffset = endCharOffset;

	File.input.lineNumberOrigin = ((startLine == 0)? 0: startLine - 1);
	File.source.lineNumberOrigin = ((sourceLineOffset == 0)? 0: sourceLineOffset - 1);
}

extern unsigned int getNestedInputBoundaryInfo (unsigned long lineNumber)
{
	unsigned int info;

	if (File.nestedInputStreamInfo.startLine == 0
	    && File.nestedInputStreamInfo.startCharOffset == 0
	    && File.nestedInputStreamInfo.endLine == 0
	    && File.nestedInputStreamInfo.endCharOffset == 0)
		/* Not in a nested input stream  */
		return 0;

	info = 0;
	if (File.nestedInputStreamInfo.startLine == lineNumber
	    && File.nestedInputStreamInfo.startCharOffset != 0)
		info |= BOUNDARY_START;
	if (File.nestedInputStreamInfo.endLine == lineNumber
	    && File.nestedInputStreamInfo.endCharOffset != 0)
		info |= BOUNDARY_END;

	return info;
}
extern void   popNarrowedInputStream  (void)
{
	mio_free (File.fp);
	File = BackupFile;
	memset (&BackupFile, 0, sizeof (BackupFile));
}

extern void pushLanguage (const langType language)
{
	pushLangOnStack (&File.input.langInfo, language);
}

extern langType popLanguage (void)
{
	return popLangOnStack (&File.input.langInfo);
}

static void langStackInit (langStack *langStack)
{
	langStack->count = 0;
	langStack->size  = 1;
	langStack->languages = xCalloc (langStack->size, langType);
}

static langType langStackTop (langStack *langStack)
{
	Assert (langStack->count > 0);
	return langStack->languages [langStack->count - 1];
}

static void     langStackClear (langStack *langStack)
{
	while (langStack->count > 0)
		langStackPop (langStack);
}

static void     langStackPush (langStack *langStack, langType type)
{
	if (langStack->size == 0)
		langStackInit (langStack);
	else if (langStack->count == langStack->size)
		langStack->languages = xRealloc (langStack->languages,
						 ++ langStack->size, langType);
	langStack->languages [ langStack->count ++ ] = type;
}

static langType langStackPop  (langStack *langStack)
{
	return langStack->languages [ -- langStack->count ];
}
