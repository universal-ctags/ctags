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
#include <stdlib.h>

#define FILE_WRITE
#include "read.h"
#include "read_p.h"
#include "debug.h"
#include "entry_p.h"
#include "routines.h"
#include "routines_p.h"
#include "options_p.h"
#include "parse_p.h"
#include "promise.h"
#include "promise_p.h"
#include "stats_p.h"
#include "trace.h"
#include "trashbox.h"
#ifdef HAVE_ICONV
# include "mbcs.h"
# include "mbcs_p.h"
#endif

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
					   on the area.
					   This is needed for stacked area. */
	bool isHeader;           /* is input file a header file? */
} inputFileInfo;

typedef struct sComputPos {
	MIOPos  pos;
	long    offset;
	bool open;
	int crAdjustment;
	size_t posInAllLines;
} compoundPos;

typedef struct sInputLineFposMap {
	compoundPos *pos;
	unsigned int count;
	unsigned int size;
} inputLineFposMap;

typedef struct sAreaInfo {
	long startOffset;
	unsigned long startLine;
	long startColumn;
	unsigned long endLine;
	long endColumn;
} areaInfo;

typedef struct sInputFile {
	vString    *path;          /* path of input file (if any) */
	vString    *line;          /* last line read from file */
	const unsigned char* currentLine;  /* current line being worked on */
	MIO        *mio;           /* MIO stream used for reading the file */
	compoundPos    filePosition;  /* file position of current line */
	unsigned int ungetchIdx;
	int         ungetchBuf[8]; /* characters that were ungotten */

	bool bomFound;
	/*  Contains data pertaining to the original `source' file in which the tag
	 *  was defined. This may be different from the `input' file when #line
	 *  directives are processed (i.e. the input file is preprocessor output).
	 */
	inputFileInfo input; /* name, lineNumber */
	inputFileInfo source;

	areaInfo areaInfo;

	/* sourceTagPathHolder is a kind of trash box.
	   The buffer pointed by tagPath field of source field can
	   be referred from tagsEntryInfo instances. sourceTagPathHolder
	   is used keeping the buffer till all processing about the current
	   input file is done. After all processing is done, the buffers
	   in sourceTagPathHolder are destroyed. */
	stringList  * sourceTagPathHolder;
	inputLineFposMap lineFposMap;
	vString *allLines;
	int thinDepth;
	time_t mtime;
} inputFile;

enum areaCoord {
	AREA_COORD_ABS,
	AREA_COORD_CURRENT,
};

static inputLangInfo inputLang;
static langType sourceLang;

/*
*   FUNCTION DECLARATIONS
*/
static void     langStackInit (langStack *langStack);
static langType langStackTop  (langStack *langStack);
static langType langStackBotom(langStack *langStack);
static void     langStackPush (langStack *langStack, langType type);
static langType langStackPop  (langStack *langStack);
static void     langStackClear(langStack *langStack);

static MIOPos getInputFilePositionForLineFull (unsigned int line, enum areaCoord areaCoord);
static MIOPos getInputFilePositionForFileOffset (long offset);
static unsigned long getAreaStartLineNumber (void);

/*
*   DATA DEFINITIONS
*/
static inputFile File;  /* static read through functions */
static inputFile BackupFile;	/* File is copied here when a guest parser is pushed */
static compoundPos StartOfLine;  /* holds deferred position of start of line */

/*
*   FUNCTION DEFINITIONS
*/

extern unsigned long getInputLineNumber (void)
{
	if (!File.currentLine && File.input.lineNumber == 1 && isAreaStacked ())
		return getAreaStartLineNumber ();
	return File.input.lineNumber;
}

CTAGS_INLINE
void callWithSavingPosition (MIO *mio,
							 void (* fn) (MIO *, void *),
							 void *data)
{
	MIOPos origin;

	mio_getpos (mio, &origin);
	fn (mio, data);
	mio_setpos (mio, &origin);
}

/* args (startLine): [absolute]
   args (startColumn): [buggy]
   args (endLine): [absolute]
   args (endColumn): [absolute] */
extern void getAreaInfo (unsigned long *startLine,
						 long *startColumn,
						 unsigned long *endLine,
						 long *endColumn)
{
	if (startLine)
		*startLine = File.areaInfo.startLine;
	if (startColumn)
		*startColumn = File.areaInfo.startColumn;
	if (endLine)
		*endLine = File.areaInfo.endLine;
	if (endColumn)
		*endColumn = File.areaInfo.endColumn;
}

/* return: [absolute]
 * If the area is not stacked, return 0.
 * If the area is stacked, the line number of the start of the current
 * area in the absolute coordinate system.
 */
static unsigned long getAreaStartLineNumber (void)
{
	unsigned long startLine = 0;

	if (isAreaStacked())
		getAreaInfo (&startLine, NULL, NULL, NULL);
	return startLine;
}

extern unsigned long translateLineNumber (unsigned long line)
{
	unsigned long area_start_line = getAreaStartLineNumber ();
	return (area_start_line? area_start_line - 1: 0) + line;
}

/* return: [absolute] */
CTAGS_INLINE
long getAreaStartOffset (void)
{
	return File.areaInfo.startOffset;
}

extern long translateFileOffset (unsigned long offset)
{
	return getAreaStartOffset () + offset;
}

extern int getInputColumnNumber (void)
{
	int ret;

	/* input file ---> +-----------------+
	 *                 |                 |
	 *                 |   [.....X.......|
	 *                 |.................|
	 *                 |.....Y]          |
	 *                 |                 |
	 *                 +-----------------+
	 */

	if (File.currentLine)
	{
		unsigned char *base = (unsigned char *) vStringValue (File.line);
		long column_in_current_coord = File.currentLine - base - File.ungetchIdx;

		/* input file ---> +-----------------+
		 *                 |                 |
		 *                 |   [.....X.......|
		 *                 <---dx--->
		 *                 |.................|
		 *                 |.....Y..]        |
		 *                 <-dy->
		 *                 |                 |
		 *                 +-----------------+
		 *
		 * dy => column_in_current_coord.
		 *
		 * dx = x1 + x2:
		 *
		 *                 <x1><-x2->
		 *                 |   [.....X.......|
		 *                 <---dx--->
		 *
		 * x1 => guest_area_column.
		 * x2 => column_in_current_coord.
		 */

		long guest_area_column = 0;
		if (isAreaStacked ())
		{
			/* X or Y */
			unsigned long startLine;
			long startColumn;
			getAreaInfo (&startLine, &startColumn, NULL, NULL);
			if (startLine == File.input.lineNumber)
			{
				/* We are at the start line of the guest area (X). */
				guest_area_column = startColumn;
			}
		}

		ret = guest_area_column + column_in_current_coord;
	}
	else if (File.input.lineNumber)
	{
		/* currentLine is set to NULL but File.input.lineNumber is not zero.
		 * This implies the parser saw EOF.
		 * The way to calculate the offset at the end of file is tricky.
		 *
		 * input file ---> +-----------------+
		 *                 |                 |
		 *                 |   [.............|
		 *                 |.................|
		 *                 |.....Z]          |
		 *                 <-dz->
		 *                 |                 |
		 *                 +-----------------+
		 *
		 * We must calculate dz but we cannot use File.currentLine.
		 *
		 * input file ---> +-----------------+
		 *                 |<========== O ===|
		 *                 |==>[<============|
		 *                 |======= P =======|
		 *                 |====>Z]          |
		 *                 <-dz->
		 *                 |                 |
		 *                 +-----------------+
		 *
		 * O => getAreaStartOffset ()
		 * P => mio_tell (File.mio) - (File.bomFound? 3: 0)
		 *
		 * input file ---> +-----------------+
		 *                 |<================|
		 *                 |===[====Q========|
		 *                 |=================|
		 *                 |====>Z]          |
		 *                 <-dz->
		 *                 |                 |
		 *                 +-----------------+
		 *
		 * Q => getInputFileOffsetForLine(File.input.lineNumber)
		 *
		 * dz = O + P - Q
		 */
		unsigned long ln = getInputLineNumber ();
		ret = getAreaStartOffset ()
			+ mio_tell (File.mio) - (File.bomFound? 3: 0)
			- getInputFileOffsetForLine(ln)
			- File.ungetchIdx;
	}
	else
	{
		/* Read nothing yet. */
		long guest_area_column = 0;
		if (isAreaStacked ())
		{
			unsigned long startLine CTAGS_ATTR_UNUSED;
			getAreaInfo (&startLine, &guest_area_column, NULL, NULL);
			Assert (startLine == 1);
		}
		ret = guest_area_column;
	}

	return ret >= 0 ? ret : 0;
}

extern const char *getInputFileName (void)
{
	if (!File.input.name)
		return NULL;
	return vStringValue (File.input.name);
}

extern MIOPos getInputFilePosition (void)
{
	return File.filePosition.pos;
}

static compoundPos* getInputFileCompoundPosForLine (unsigned int line)
{
	int index;
	if (line > 0)
	{
		if (File.lineFposMap.count > (line - 1))
			index = line - 1;
		else if (File.lineFposMap.count != 0)
			index = File.lineFposMap.count - 1;
		else
			index = 0;
	}
	else
		index = 0;

	return File.lineFposMap.pos + index;
}


/* case areaCoord == AREA_COORD_CURRENT:
 *    return: [current],
 *    args (line): [absolute]
 *
 * case areaCoord == AREA_COORD_ABS:
 *    return: [absolute],
 *    args (line): [absolute]
 */
static MIOPos getInputFilePositionForLineFull (unsigned int line, enum areaCoord posAreaCoord)
{
	if (line == 1 && File.lineFposMap.count == 0)
	{
		/* Any line is not read yet. */
		MIOPos pos;

		if (isAreaStacked() && (posAreaCoord == AREA_COORD_ABS))
			mio_getpos (BackupFile.mio, &pos);
		else
			mio_getpos (File.mio, &pos);
		return pos;
	}

	if (isAreaStacked() && (posAreaCoord == AREA_COORD_CURRENT))
	{
		unsigned long area_start_ln = getAreaStartLineNumber ();
		long abs_offset = getInputFileOffsetForLine (line);
		long area_start_offset = getInputFileOffsetForLine (area_start_ln);
		long rela_offset = abs_offset - area_start_offset;
		MIOPos rela_pos = getInputFilePositionForFileOffset (rela_offset);
		return rela_pos;
	}

	compoundPos *cpos = getInputFileCompoundPosForLine (line);
	return cpos->pos;
}

extern MIOPos getInputFilePositionForLine (unsigned int line)
{
	return getInputFilePositionForLineFull (line, AREA_COORD_CURRENT);
}


extern long getInputFileOffsetForLine (unsigned int line)
{
	compoundPos *cpos = getInputFileCompoundPosForLine (line);
	long r = cpos->offset - (File.bomFound? 3: 0) - cpos->crAdjustment;
	Assert (r >= 0);
	return r;
}

struct mioGetposCallbackData {
	MIOPos *pos;
	long   offset;
};

static void tellAbsolutePosition (MIO *mio, void *data)
{
	struct mioGetposCallbackData *cb_data = data;

	mio_seek (mio, cb_data->offset, SEEK_SET);
	mio_getpos (mio, cb_data->pos);
}

/* return: [current],
 * args (offset): [current] */
static MIOPos getInputFilePositionForFileOffset (long offset)
{
	MIOPos pos;
	struct mioGetposCallbackData data = {
		.pos = &pos,
		.offset = offset,
	};

	callWithSavingPosition (File.mio,
							tellAbsolutePosition,
							&data);

	return pos;
}

extern langType getInputLanguage (void)
{
	return langStackTop (&inputLang.stack);
}

extern const char *getInputLanguageName (void)
{
	return getLanguageName (getInputLanguage());
}

extern const char *getInputFileTagPath (void)
{
	return vStringValue (File.input.tagPath);
}

extern bool isInputLanguage (langType lang)
{
	return (bool)((lang) == getInputLanguage ());
}

extern bool isInputHeaderFile (void)
{
	return File.input.isHeader;
}

extern bool isInputLanguageKindEnabled (int kindIndex)
{
	return isLanguageKindEnabled (getInputLanguage (), kindIndex);
}

extern bool isInputLanguageRoleEnabled (int kindIndex, int roleIndex)
{
	return isLanguageRoleEnabled (getInputLanguage (),
								  kindIndex, roleIndex);
}

extern unsigned int countInputLanguageKinds (void)
{
	return countLanguageKinds (getInputLanguage ());
}

extern unsigned int countInputLanguageRoles (int kindIndex)
{
	return countLanguageRoles (getInputLanguage (), kindIndex);
}

extern bool doesInputLanguageRequestAutomaticFQTag (const tagEntryInfo *e)
{
	return doesLanguageRequestAutomaticFQTag (e->langType);
}

extern const char *getSourceFileTagPath (void)
{
	return vStringValue (File.source.tagPath);
}

extern langType getSourceLanguage (void)
{
	return sourceLang;
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
	return mio_memory_get_data (File.mio, size);
}

/*
 * inputLineFposMap related functions
 */
static void freeLineFposMap (inputLineFposMap *lineFposMap)
{
	if (lineFposMap->pos)
	{
		eFree (lineFposMap->pos);
		lineFposMap->pos = NULL;
		lineFposMap->count = 0;
		lineFposMap->size = 0;
	}
}

static void allocLineFposMap (inputLineFposMap *lineFposMap)
{
#define INITIAL_lineFposMap_LEN 256
	lineFposMap->pos = xCalloc (INITIAL_lineFposMap_LEN, compoundPos);
	lineFposMap->size = INITIAL_lineFposMap_LEN;
	lineFposMap->count = 0;
}

static void resetLineFposMap (inputLineFposMap *lineFposMap)
{
	memset(lineFposMap->pos, 0, sizeof(compoundPos) * lineFposMap->size);
	lineFposMap->count = 0;
}

static void appendLineFposMap (inputLineFposMap *lineFposMap, compoundPos *pos,
							   bool crAdjustment, size_t posInAllLines)
{
	int lastCrAdjustment = 0;

	if (lineFposMap->size == lineFposMap->count)
	{
		lineFposMap->size *= 2;
		lineFposMap->pos = xRealloc (lineFposMap->pos,
					     lineFposMap->size,
					     compoundPos);
	}

	if (lineFposMap->count != 0)
	{
		lineFposMap->pos [lineFposMap->count - 1].open = false;
		lastCrAdjustment = lineFposMap->pos [lineFposMap->count - 1].crAdjustment;
	}

	lineFposMap->pos [lineFposMap->count] = *pos;
	lineFposMap->pos [lineFposMap->count].open = true;
	lineFposMap->pos [lineFposMap->count].crAdjustment
		= lastCrAdjustment + ((crAdjustment)? 1: 0);
	lineFposMap->pos [lineFposMap->count].posInAllLines = posInAllLines;
	lineFposMap->count++;
}

static int compoundPosForOffset (const void* oft, const void *p)
{
	long offset = *(long *)oft;
	const compoundPos *pos = p;
	const compoundPos *next = (compoundPos *)(((char *)pos) + sizeof (compoundPos));

	if (offset < (pos->offset - pos->crAdjustment))
		return -1;
	else if (((pos->offset - pos->crAdjustment) <= offset)
		 && (pos->open
		     || (offset < (next->offset - next->crAdjustment))))
		return 0;
	else
		return 1;
}

extern unsigned long getInputLineNumberForFileOffset(long offset)
{
	compoundPos *p;

	if (File.bomFound)
		offset += 3;

	p = bsearch (&offset, File.lineFposMap.pos, File.lineFposMap.count, sizeof (compoundPos),
		     compoundPosForOffset);
	if (p == NULL)
		return 1;	/* TODO: 0? */
	else
		return 1 + (p - File.lineFposMap.pos);
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

	if (0)
		;
	else if (  Option.tagRelative == TREL_ALWAYS )
		finfo->tagPath =
			vStringNewOwn (relativeFilename (vStringValue (fileName),
							 getTagFileDirectory ()));
	else if ( Option.tagRelative == TREL_NEVER )
		finfo->tagPath =
			vStringNewOwn (absoluteFilename (vStringValue (fileName)));
	else if ( Option.tagRelative == TREL_NO || isAbsolutePath (vStringValue (fileName)) )
		finfo->tagPath = vStringNewCopy (fileName);
	else
		finfo->tagPath =
			vStringNewOwn (relativeFilename (vStringValue (fileName),
							 getTagFileDirectory ()));

	finfo->isHeader = isIncludeFile (vStringValue (fileName));
}

static void resetLangOnStack (inputLangInfo *langInfo, langType lang)
{
	Assert (langInfo->stack.count > 0);
	langStackClear  (& (langInfo->stack));
	langStackPush (& (langInfo->stack), lang);
}

static langType baseLangOnStack (inputLangInfo *langInfo)
{
	Assert (langInfo->stack.count > 0);
	return langStackBotom (& (langInfo->stack));
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
	langStackClear (& langInfo->stack);
}

static void setInputFileParameters (vString *const fileName, const langType language)
{
	setInputFileParametersCommon (&File.input, fileName,
				      language, NULL);
	pushLangOnStack(&inputLang, language);
}

static void setSourceFileParameters (vString *const fileName, const langType language)
{
	setInputFileParametersCommon (&File.source, fileName,
				      language, File.sourceTagPathHolder);
	sourceLang = language;
}

static bool setSourceFileName (vString *const fileName)
{
	const langType language = getLanguageForFilenameAndContents (vStringValue (fileName));
	bool result = false;
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
		result = true;
	}
	return result;
}

/*
 *   Line directive parsing
 */

static void skipWhite (char **str)
{
	while (**str == ' '  ||  **str == '\t')
		(*str)++;
}

static unsigned long readLineNumber (char **str)
{
	char *s;
	unsigned long lNum = 0;

	skipWhite (str);
	s = *str;
	while (*s != '\0' && isdigit ((unsigned char) *s))
	{
		lNum = (lNum * 10) + (*s - '0');
		s++;
	}
	if (*s != ' ' && *s != '\t')
		lNum = 0;
	*str = s;

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
static vString *readFileName (char *s)
{
	vString *const fileName = vStringNew ();
	bool quoteDelimited = false;

	skipWhite (&s);
	if (*s == '"')
	{
		s++;  /* skip double-quote */
		quoteDelimited = true;
	}
	while (*s != '\0'  &&  *s != '\n'  &&
			(quoteDelimited ? (*s != '"') : (*s != ' '  &&  *s != '\t')))
	{
		vStringPut (fileName, *s);
		s++;
	}
	vStringPut (fileName, '\0');

	return fileName;
}

static bool parseLineDirective (char *s)
{
	bool result = false;

	skipWhite (&s);
	DebugStatement ( const char* lineStr = ""; )

	if (isdigit ((unsigned char) *s))
		result = true;
	else if (strncmp (s, "line", 4) == 0)
	{
		s += 4;
		if (*s == ' '  ||  *s == '\t')
		{
			DebugStatement ( lineStr = "line"; )
			result = true;
		}
	}
	if (result)
	{
		const unsigned long lNum = readLineNumber (&s);
		if (lNum == 0)
			result = false;
		else
		{
			vString *const fileName = readFileName (s);
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
			result = true;
		}
	}
	return result;
}

/*
 *   Input file I/O operations
 */
#ifdef DEBUG
#define MAX_IN_MEMORY_FILE_SIZE 0
#else
#define MAX_IN_MEMORY_FILE_SIZE (1024*1024)
#endif

static MIO *getMioFull (const char *const fileName, const char *const openMode,
		    bool memStreamRequired, time_t *mtime)
{
	FILE *src;
	fileStatus *st;
	unsigned long size;
	unsigned char *data;

	st = eStat (fileName);
	size = st->size;
	if (mtime)
		*mtime = st->mtime;
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
	return mio_new_memory (data, size, eRealloc, eFreeNoNullCheck);
}

extern MIO *getMio (const char *const fileName, const char *const openMode,
		    bool memStreamRequired)
{
	return getMioFull (fileName, openMode, memStreamRequired, NULL);
}

/* Return true if utf8 BOM is found */
static bool checkUTF8BOM (MIO *mio, bool skipIfFound)
{
	bool r = false;
	if ((0xEF == mio_getc (mio))
		&& (0xBB == mio_getc (mio))
		&& (0xBF == mio_getc (mio)))
		r = true;

	if (! (r && skipIfFound))
		mio_rewind (mio);
	return r;
}

static void rewindInputFile (inputFile *f)
{
	mio_rewind (f->mio);
	if (f->bomFound)
	{
		int c CTAGS_ATTR_UNUSED;

		c = mio_getc (f->mio);
		Assert (c == 0xEF);
		c = mio_getc (f->mio);
		Assert (c == 0xBB);
		c = mio_getc (f->mio);
		Assert (c == 0xBF);
	}
}

/*  This function opens an input file, and resets the line counter.  If it
 *  fails, it will display an error message and leave the File.mio set to NULL.
 */
extern bool openInputFile (const char *const fileName, const langType language,
			      MIO *mio, time_t mtime)
{
	const char *const openMode = "rb";
	bool opened = false;
	bool memStreamRequired;

	/*	If another file was already open, then close it.
	 */
	if (File.mio != NULL)
	{
		mio_unref (File.mio);  /* close any open input file */
		File.mio = NULL;
	}

	/* File position is used as key for checking the availability of
	   pattern cache in entry.h. If an input file is changed, the
	   key is meaningless. So notifying the changing here. */
	invalidatePatternCache();

	if (File.sourceTagPathHolder == NULL)
	{
		File.sourceTagPathHolder = stringListNew ();
		DEFAULT_TRASH_BOX(File.sourceTagPathHolder, stringListDelete);
	}
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

	File.mio = mio? mio_ref (mio): getMioFull (fileName, openMode, memStreamRequired, &File.mtime);

	if (File.mio == NULL)
		error (WARNING | PERROR, "cannot open \"%s\"", fileName);
	else
	{
		opened = true;

		if (File.mio == mio)
			File.mtime = mtime;

		File.bomFound = checkUTF8BOM (File.mio, true);

		setOwnerDirectoryOfInputFile (fileName);
		mio_getpos (File.mio, &StartOfLine.pos);
		mio_getpos (File.mio, &File.filePosition.pos);
		File.filePosition.offset = StartOfLine.offset = mio_tell (File.mio);
		File.currentLine  = NULL;

		File.line = vStringNewOrClear (File.line);
		File.ungetchIdx = 0;

		setInputFileParameters  (vStringNewInit (fileName), language);
		File.input.lineNumberOrigin = 0L;
		File.input.lineNumber = File.input.lineNumberOrigin;
		setSourceFileParameters (vStringNewInit (fileName), language);
		File.source.lineNumberOrigin = 0L;
		File.source.lineNumber = File.source.lineNumberOrigin;
		allocLineFposMap (&File.lineFposMap);

		File.thinDepth = 0;
		verbose ("OPENING%s %s as %s language %sfile [%s%s]\n",
				 (File.bomFound? "(skipping utf-8 bom)": ""),
				 fileName,
				 getLanguageName (language),
				 File.input.isHeader ? "include " : "",
				 mio? "reused": "new",
				 memStreamRequired? ",required": "");
	}
	return opened;
}

extern time_t getInputFileMtime (void)
{
	return File.mtime;
}

extern void resetInputFile (const langType language, bool resetLineFposMap_)
{
	Assert (File.mio);

	rewindInputFile  (&File);
	mio_getpos (File.mio, &StartOfLine.pos);
	mio_getpos (File.mio, &File.filePosition.pos);
	File.filePosition.offset = StartOfLine.offset = mio_tell (File.mio);
	File.currentLine  = NULL;

	Assert (File.line);
	vStringClear (File.line);
	File.ungetchIdx = 0;

	if (hasLanguageMultilineRegexPatterns (language)
		|| hasLanguagePostRunRegexPatterns (language))
		File.allLines = vStringNew ();

	if (resetLineFposMap_)
		resetLineFposMap(&File.lineFposMap);

	resetLangOnStack (& inputLang, language);
	File.input.lineNumber = File.input.lineNumberOrigin;
	sourceLang = language;
	File.source.lineNumber = File.source.lineNumberOrigin;
}

extern void closeInputFile (void)
{
	if (File.mio != NULL)
	{
		clearLangOnStack (& inputLang);

		/*  The line count of the file is 1 too big, since it is one-based
		 *  and is incremented upon each newline.
		 */
		if (Option.printTotals)
		{
			fileStatus *status = eStat (vStringValue (File.input.name));
			addTotals (0, getInputLineNumber () - 1L, status->size);
		}
		mio_unref (File.mio);
		File.mio = NULL;
		freeLineFposMap (&File.lineFposMap);
	}
}

extern void *getInputFileUserData(void)
{
	return mio_get_user_data (File.mio);
}

/*  Action to take for each encountered input newline.
 */
static void fileNewline (bool crAdjustment, size_t posInAllLines)
{
	File.filePosition = StartOfLine;

	if (BackupFile.mio == NULL)
		appendLineFposMap (&File.lineFposMap, &File.filePosition,
						   crAdjustment, posInAllLines);

	File.input.lineNumber++;
	File.source.lineNumber++;
	DebugStatement ( if (Option.breakLine == getInputLineNumber ()) lineBreak (); )
	DebugStatement ( debugPrintf (DEBUG_RAW, "%6ld: ", getInputLineNumber ()); )
}

extern void ungetcToInputFile (int c)
{
	const size_t len = ARRAY_SIZE (File.ungetchBuf);

	Assert (File.ungetchIdx < len);
	/* we cannot rely on the assertion that might be disabled in non-debug mode */
	if (File.ungetchIdx < len)
		File.ungetchBuf[File.ungetchIdx++] = c;
}

typedef enum eEolType {
	eol_eof = 0,
	eol_nl,
	eol_cr_nl,
} eolType;

static eolType readLine (vString *const vLine, MIO *const mio)
{
	char *str;
	size_t size;
	eolType r = eol_nl;

	vStringClear (vLine);

	str = vStringValue (vLine);
	size = vStringSize (vLine);

	for (;;)
	{
		bool newLine;
		bool eof;

		if (mio_gets (mio, str, size) == NULL)
		{
			if (!mio_eof (mio))
				error (FATAL | PERROR, "Failure on attempt to read file");
		}
		vStringSetLength (vLine);
		newLine = vStringLength (vLine) > 0 && vStringLast (vLine) == '\n';
		eof = mio_eof (mio);
		if (eof)
			r = eol_eof;

		/* Turn line breaks into a canonical form. The three commonly
		 * used forms of line breaks are: LF (UNIX/Mac OS X), CR-LF (MS-DOS) and
		 * CR (Mac OS 9). As CR-only EOL isn't handled by gets() and Mac OS 9
		 * is dead, we only handle CR-LF EOLs and convert them into LF. */
		if (newLine && vStringLength (vLine) > 1 &&
			vStringChar (vLine, vStringLength (vLine) - 2) == '\r')
		{
			vStringChar (vLine, vStringLength (vLine) - 2) = '\n';
			vStringChop (vLine);
			r = eol_cr_nl;
		}

		if (newLine || eof)
			break;

		vStringResize (vLine, vStringLength (vLine) * 2);
		str = vStringValue (vLine) + vStringLength (vLine);
		size = vStringSize (vLine) - vStringLength (vLine);
	}
	return r;
}

static vString *iFileGetLine (bool chop_newline)
{
	eolType eol;
	langType lang = getInputLanguage();

	Assert (File.line);
	eol = readLine (File.line, File.mio);

	if (vStringLength (File.line) > 0)
	{
		/* Use StartOfLine from previous iFileGetLine() call */
		fileNewline (eol == eol_cr_nl, File.allLines? vStringLength(File.allLines): 0);
		/* Store StartOfLine for the next iFileGetLine() call */
		mio_getpos (File.mio, &StartOfLine.pos);
		StartOfLine.offset = mio_tell (File.mio);

		if (Option.lineDirectives && vStringChar (File.line, 0) == '#')
			parseLineDirective (vStringValue (File.line) + 1);

		if (File.allLines)
			vStringCat (File.allLines, File.line);

		bool chopped = vStringStripNewline (File.line);

		matchLanguageRegex (lang, File.line, false);

		if (chopped && !chop_newline)
			vStringPutNewlinAgainUnsafe (File.line);

		return File.line;
	}
	else
	{
		if (File.allLines)
		{
			matchLanguageMultilineRegex (lang, File.allLines);
			matchLanguageMultitableRegex (lang, File.allLines);

			if (hasLanguagePostRunRegexPatterns (lang))
			{

				unsigned input_ln = File.input.lineNumber;
				unsigned source_ln = File.source.lineNumber;
				MIOPos pos = File.filePosition.pos;

				vString *line = vStringNew();
				for (size_t i = 0; i < File.lineFposMap.count; i++)
				{
					File.input.lineNumber = i + 1;
					File.source.lineNumber = File.input.lineNumber;
					File.filePosition.pos = File.lineFposMap.pos[i].pos;

					vStringNCopySUnsafe(line,
										vStringValue(File.allLines) +
										File.lineFposMap.pos[i].posInAllLines,
										(((i + 1) < File.lineFposMap.count)
										 ? File.lineFposMap.pos[i+1].posInAllLines
										 : vStringLength (File.allLines))
										- File.lineFposMap.pos[i].posInAllLines);
					matchLanguageRegex (lang, line, true);
				}
				vStringDelete(line);

				File.filePosition.pos = pos;
				File.input.lineNumber = input_ln;
				File.source.lineNumber = source_ln;
			}

			/* To limit the execution of multiline/multitable parser(s) only
			   ONCE, clear File.allLines field. */
			vStringDelete (File.allLines);
			File.allLines = NULL;
		}
		return NULL;
	}
}

/*  Do not mix use of readLineFromInputFile () and getcFromInputFile () for the same file.
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
			vString* const line = iFileGetLine (false);
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

extern int skipToCharacterInInputFile2 (int c0, int c1)
{
	int d;
	do
	{
		skipToCharacterInInputFile(c0);
		do
			d = getcFromInputFile ();
		while (d == c0 && d != c1);
	} while (d != EOF && d != c1);
	return d;
}

/*  An alternative interface to getcFromInputFile (). Do not mix use of readLineFromInputFile()
 *  and getcFromInputFile() for the same file. The returned string does not contain
 *  the terminating newline. A NULL return value means that all lines in the
 *  file have been read and we are at the end of file.
 */
extern const unsigned char *readLineFromInputFileWithLength (size_t *length)
{
	vString* const line = iFileGetLine (true);
	const unsigned char* result = NULL;
	if (line != NULL)
	{
		result = (const unsigned char*) vStringValue (line);
		*length = vStringLength (line);
		DebugStatement ( debugPrintf (DEBUG_READ, "%s\n", result); )
	}
	return result;
}

extern const unsigned char *readLineFromInputFile (void)
{
	size_t dummy;
	return readLineFromInputFileWithLength(&dummy);
}

/*
 *   Raw file line reading with automatic buffer sizing
 */
extern char *readLineRaw (vString *const vLine, MIO *const mio)
{
	if (mio == NULL)  /* to free memory allocated to buffer */
		error (FATAL, "NULL file pointer");
	else
	{
		readLine (vLine, mio);

#ifdef HAVE_ICONV
		if (isConverting ())
			convertString (vLine);
#endif
	}
	return vStringLength (vLine) > 0 ? vStringValue (vLine) : NULL;
}

/*  Places into the line buffer the contents of the line referenced by
 *  "pos".
 */
struct readLineRawCbData {
	MIOPos pos;
	long *offset;
	vString *vLine;
	char *result;
};

static void readLineRawCb (MIO *mio, void *data)
{
	struct readLineRawCbData *cb_data = data;

	mio_setpos (mio, &cb_data->pos);
	mio_clearerr (mio);
	if (cb_data->offset)
		*cb_data->offset = mio_tell (mio);

	/* If the file is empty, we can't get the line
	   for position 0. readLineFromBypass doesn't know
	   what itself should do; just report it to the caller. */
	cb_data->result = readLineRaw (cb_data->vLine, mio);
}

extern char *readLineFromBypass (
		vString *const vLine, MIOPos pos, long *const offset)
{
	struct readLineRawCbData data = {
		.pos = pos,
		.offset = offset,
		.vLine = vLine,
		.result = NULL,
	};

	callWithSavingPosition (File.mio, readLineRawCb, &data);

	return data.result;
}

extern bool   pushArea (
				       bool useMemoryStreamInput,
				       unsigned long startLine, long startColumn,
				       unsigned long endLine, long endColumn,
				       unsigned long sourceLineOffset,
				       int promise)
{
	long p, q;
	MIOPos original;
	MIOPos tmp;
	MIO *subio;

	if (isThinAreaSpec (startLine, startColumn,
						endLine, endColumn,
						sourceLineOffset))
	{
		if ((!useMemoryStreamInput
			 || mio_memory_get_data (File.mio, NULL)))
		{
			File.thinDepth++;
			verbose ("push thin area (%d)\n", File.thinDepth);
			return true;
		}
		error(WARNING, "INTERNAL ERROR: though pushing MEMORY based thin area, "
			  "underlying area is a FILE base: %s@%s",
			  vStringValue (File.input.name), vStringValue (File.input.tagPath));
		AssertNotReached ();
	}
	Assert (File.thinDepth == 0);

	original = getInputFilePosition ();

	tmp = getInputFilePositionForLine (startLine);
	mio_setpos (File.mio, &tmp);
	mio_seek (File.mio, startColumn, SEEK_CUR);
	p = mio_tell (File.mio);

	tmp = getInputFilePositionForLine (endLine);
	mio_setpos (File.mio, &tmp);
	if (endColumn == EOL_COLUMN)
	{
		long line_start = mio_tell (File.mio);
		vString *tmpstr = vStringNew ();
		readLine (tmpstr, File.mio);
		endColumn = mio_tell (File.mio) - line_start;
		vStringDelete (tmpstr);
		Assert (endColumn >= 0);
	}
	else
		mio_seek (File.mio, endColumn, SEEK_CUR);
	q = mio_tell (File.mio);

	mio_setpos (File.mio, &original);

	if (q <= p)
		return false;

	invalidatePatternCache();

	size_t size = q - p;
	subio = mio_new_mio (File.mio, p, size);
	if (subio == NULL)
		error (FATAL, "memory for mio may be exhausted");

	runModifiers (promise,
				  startLine, startColumn,
				  endLine, endColumn,
				  mio_memory_get_data (subio, NULL),
				  size);

	BackupFile = File;

	File.mio = subio;
	File.bomFound = false;
	File.areaInfo.startOffset = p;
	File.areaInfo.startLine = startLine;
	File.areaInfo.startColumn = startColumn;
	File.areaInfo.endLine = endLine;
	File.areaInfo.endColumn = endColumn;

	File.input.lineNumberOrigin = ((startLine == 0)? 0: startLine - 1);
	File.source.lineNumberOrigin = ((sourceLineOffset == 0)? 0: sourceLineOffset - 1);

	return true;
}

extern bool isAreaStacked (void)
{
	return !(File.areaInfo.startLine == 0
			 && File.areaInfo.startColumn == 0
			 && File.areaInfo.endLine == 0
			 && File.areaInfo.endColumn == 0);
}

extern unsigned int getAreaBoundaryInfo (unsigned long lineNumber)
{
	unsigned int info;

	if (!isAreaStacked())
		return 0;

	info = 0;
	if (File.areaInfo.startLine == lineNumber
	    && File.areaInfo.startColumn != 0)
		info |= AREA_BOUNDARY_START;
	if (File.areaInfo.endLine == lineNumber
	    && File.areaInfo.endColumn != 0)
		info |= AREA_BOUNDARY_END;

	return info;
}
extern void   popArea  (void)
{
	if (File.thinDepth)
	{
		File.thinDepth--;
		verbose ("CLEARING thin flag(%d)\n", File.thinDepth);
		return;
	}
	mio_unref (File.mio);
	File = BackupFile;
	memset (&BackupFile, 0, sizeof (BackupFile));
}

extern void pushLanguage (const langType language)
{
	pushLangOnStack (& inputLang, language);
}

extern langType popLanguage (void)
{
	return popLangOnStack (& inputLang);
}

extern langType getLanguageForBaseParser (void)
{
	return baseLangOnStack (& inputLang);
}

static void langStackInit (langStack *langStack)
{
	langStack->count = 0;
	langStack->size  = 1;
	langStack->languages = xCalloc (langStack->size, langType);
	DEFAULT_TRASH_BOX(&(langStack->languages), eFreeIndirect);
}

static langType langStackTop (langStack *langStack)
{
	Assert (langStack->count > 0);
	return langStack->languages [langStack->count - 1];
}

static langType langStackBotom(langStack *langStack)
{
	Assert (langStack->count > 0);
	return langStack->languages [0];
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

extern bool isThinAreaSpec (unsigned long startLine, long startColumn,
							unsigned long endLine, long endColumn,
							unsigned long sourceLineOffset)
{
	return (startLine == 0 &&
			startColumn == 0 &&
			endLine == 0 &&
			endColumn == 0 &&
			sourceLineOffset == 0);
}

#ifdef DO_TRACING
extern bool isTraced (void)
{
	if (File.mio == NULL)
		/* A parser is not given. In that case, just check whether --_trace option
		   is given or not. */
		return isMainTraced ();
	else
		/* A parser is given. In that case, check whether the current parser is
		   specified in --_trace=<LANG>,... option */
		return isLanguageTraced (getInputLanguage ());
}
#endif	/* DO_TRACING */
