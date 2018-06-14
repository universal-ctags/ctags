/*
*   Copyright (c) 1996-2002, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for creating tag entries.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include <string.h>
#include <ctype.h>        /* to define isspace () */
#include <errno.h>

#if defined (HAVE_SYS_TYPES_H)
# include <sys/types.h>	  /* to declare off_t on some hosts */
#endif
#if defined (HAVE_TYPES_H)
# include <types.h>       /* to declare off_t on some hosts */
#endif
#if defined (HAVE_UNISTD_H)
# include <unistd.h>      /* to declare close (), ftruncate (), truncate () */
#endif

/*  These header files provide for the functions necessary to do file
 *  truncation.
 */
#ifdef HAVE_FCNTL_H
# include <fcntl.h>
#endif
#ifdef HAVE_IO_H
# include <io.h>
#endif

#include <stdint.h>

#include "debug.h"
#include "entry.h"
#include "field.h"
#include "fmt.h"
#include "kind.h"
#include "main.h"
#include "options.h"
#include "ptag.h"
#include "read.h"
#include "routines.h"
#include "ptrarray.h"
#include "sort.h"
#include "strlist.h"
#include "subparser.h"
#include "trashbox.h"
#include "writer.h"
#include "xtag.h"

/*
*   MACROS
*/

/*
 *  Portability defines
 */
#if !defined(HAVE_TRUNCATE) && !defined(HAVE_FTRUNCATE) && !defined(HAVE_CHSIZE)
# define USE_REPLACEMENT_TRUNCATE
#endif

/*  Hack for ridiculous practice of Microsoft Visual C++.
 */
#if defined (WIN32) && defined (_MSC_VER)
# define chsize         _chsize
# define open           _open
# define close          _close
# define O_RDWR         _O_RDWR
#endif


/*  Maintains the state of the tag file.
 */
typedef struct eTagFile {
	char *name;
	char *directory;
	MIO *mio;
	struct sNumTags { unsigned long added, prev; } numTags;
	struct sMax { size_t line, tag; } max;
	vString *vLine;

	unsigned int cork;
	struct sCorkQueue {
		struct sTagEntryInfo* queue;
		unsigned int length;
		unsigned int count;
	} corkQueue;

	bool patternCacheValid;
} tagFile;

/*
*   DATA DEFINITIONS
*/

static tagFile TagFile = {
    NULL,               /* tag file name */
    NULL,               /* tag file directory (absolute) */
    NULL,               /* file pointer */
    { 0, 0 },           /* numTags */
    { 0, 0 },        /* max */
    NULL,                /* vLine */
    .cork = false,
    .corkQueue = {
	    .queue = NULL,
	    .length = 0,
	    .count  = 0
    },
    .patternCacheValid = false,
};

static bool TagsToStdout = false;

/*
*   FUNCTION PROTOTYPES
*/
#ifdef NEED_PROTO_TRUNCATE
extern int truncate (const char *path, off_t length);
#endif

#ifdef NEED_PROTO_FTRUNCATE
extern int ftruncate (int fd, off_t length);
#endif

/*
*   FUNCTION DEFINITIONS
*/

extern void freeTagFileResources (void)
{
	if (TagFile.directory != NULL)
		eFree (TagFile.directory);
	vStringDelete (TagFile.vLine);
}

extern const char *tagFileName (void)
{
	return TagFile.name;
}

/*
*   Pseudo tag support
*/

extern void abort_if_ferror(MIO *const mio)
{
	if (mio_error (mio))
		error (FATAL | PERROR, "cannot write tag file");
}

static void rememberMaxLengths (const size_t nameLength, const size_t lineLength)
{
	if (nameLength > TagFile.max.tag)
		TagFile.max.tag = nameLength;

	if (lineLength > TagFile.max.line)
		TagFile.max.line = lineLength;
}

static void addCommonPseudoTags (void)
{
	for (int i = 0; i < PTAG_COUNT; i++)
	{
		if (isPtagCommonInParsers (i))
			makePtagIfEnabled (i, NULL);
	}
}

extern void makeFileTag (const char *const fileName)
{
	tagEntryInfo tag;

	if (!isXtagEnabled(XTAG_FILE_NAMES))
		return;

	initTagEntry (&tag, baseFilename (fileName), KIND_FILE_INDEX);

	tag.isFileEntry     = true;
	tag.lineNumberEntry = true;
	markTagExtraBit (&tag, XTAG_FILE_NAMES);

	tag.lineNumber = 1;
	if (isFieldEnabled (FIELD_END_LINE))
	{
		/* isFieldEnabled is called again in the rendering
		   stage. However, it is called here for avoiding
		   unnecessary read line loop. */
		while (readLineFromInputFile () != NULL)
			; /* Do nothing */
		tag.extensionFields.endLine = getInputLineNumber ();
	}

	makeTagEntry (&tag);
}

static void updateSortedFlag (
		const char *const line, MIO *const mio, MIOPos startOfLine)
{
	const char *const tab = strchr (line, '\t');

	if (tab != NULL)
	{
		const long boolOffset = tab - line + 1;  /* where it should be */

		if (line [boolOffset] == '0'  ||  line [boolOffset] == '1')
		{
			MIOPos nextLine;

			if (mio_getpos (mio, &nextLine) == -1 || mio_setpos (mio, &startOfLine) == -1)
				error (WARNING, "Failed to update 'sorted' pseudo-tag");
			else
			{
				MIOPos flagLocation;
				int c, d;

				do
					c = mio_getc (mio);
				while (c != '\t'  &&  c != '\n');
				mio_getpos (mio, &flagLocation);
				d = mio_getc (mio);
				if (c == '\t'  &&  (d == '0'  ||  d == '1')  &&
					d != (int) Option.sorted)
				{
					mio_setpos (mio, &flagLocation);
					mio_putc (mio, Option.sorted == SO_FOLDSORTED ? '2' :
						(Option.sorted == SO_SORTED ? '1' : '0'));
				}
				mio_setpos (mio, &nextLine);
			}
		}
	}
}

/*  Look through all line beginning with "!_TAG_FILE", and update those which
 *  require it.
 */
static long unsigned int updatePseudoTags (MIO *const mio)
{
	enum { maxEntryLength = 20 };
	char entry [maxEntryLength + 1];
	unsigned long linesRead = 0;
	MIOPos startOfLine;
	size_t entryLength;
	const char *line;

	sprintf (entry, "%sTAG_FILE", PSEUDO_TAG_PREFIX);
	entryLength = strlen (entry);
	Assert (entryLength < maxEntryLength);

	mio_getpos (mio, &startOfLine);
	line = readLineRaw (TagFile.vLine, mio);
	while (line != NULL  &&  line [0] == entry [0])
	{
		++linesRead;
		if (strncmp (line, entry, entryLength) == 0)
		{
			char tab, classType [16];

			if (sscanf (line + entryLength, "%15s%c", classType, &tab) == 2  &&
				tab == '\t')
			{
				if (strcmp (classType, "_SORTED") == 0)
					updateSortedFlag (line, mio, startOfLine);
			}
			mio_getpos (mio, &startOfLine);
		}
		line = readLineRaw (TagFile.vLine, mio);
	}
	while (line != NULL)  /* skip to end of file */
	{
		++linesRead;
		line = readLineRaw (TagFile.vLine, mio);
	}
	return linesRead;
}

/*
 *  Tag file management
 */

static bool isValidTagAddress (const char *const excmd)
{
	bool isValid = false;

	if (strchr ("/?", excmd [0]) != NULL)
		isValid = true;
	else
	{
		char *address = xMalloc (strlen (excmd) + 1, char);
		if (sscanf (excmd, "%[^;\n]", address) == 1  &&
			strspn (address,"0123456789") == strlen (address))
				isValid = true;
		eFree (address);
	}
	return isValid;
}

static bool isCtagsLine (const char *const line)
{
	enum fieldList { TAG, TAB1, SRC_FILE, TAB2, EXCMD, NUM_FIELDS };
	bool ok = false;  /* we assume not unless confirmed */
	const size_t fieldLength = strlen (line) + 1;
	char *const fields = xMalloc (NUM_FIELDS * fieldLength, char);

	if (fields == NULL)
		error (FATAL, "Cannot analyze tag file");
	else
	{
#define field(x)		(fields + ((size_t) (x) * fieldLength))

		const int numFields = sscanf (
			line, "%[^\t]%[\t]%[^\t]%[\t]%[^\r\n]",
			field (TAG), field (TAB1), field (SRC_FILE),
			field (TAB2), field (EXCMD));

		/*  There must be exactly five fields: two tab fields containing
		 *  exactly one tab each, the tag must not begin with "#", and the
		 *  file name should not end with ";", and the excmd must be
		 *  acceptable.
		 *
		 *  These conditions will reject tag-looking lines like:
		 *      int a;        <C-comment>
		 *      #define LABEL <C-comment>
		 */
		if (numFields == NUM_FIELDS   &&
			strlen (field (TAB1)) == 1  &&
			strlen (field (TAB2)) == 1  &&
			field (TAG) [0] != '#'      &&
			field (SRC_FILE) [strlen (field (SRC_FILE)) - 1] != ';'  &&
			isValidTagAddress (field (EXCMD)))
				ok = true;

		eFree (fields);
	}
	return ok;
}

static bool isEtagsLine (const char *const line)
{
	bool result = false;
	if (line [0] == '\f')
		result = (bool) (line [1] == '\n'  ||  line [1] == '\r');
	return result;
}

static bool isTagFile (const char *const filename)
{
	bool ok = false;  /* we assume not unless confirmed */
	MIO *const mio = mio_new_file (filename, "rb");

	if (mio == NULL  &&  errno == ENOENT)
		ok = true;
	else if (mio != NULL)
	{
		const char *line = readLineRaw (TagFile.vLine, mio);

		if (line == NULL)
			ok = true;
		else
			ok = (bool) (isCtagsLine (line) || isEtagsLine (line));
		mio_free (mio);
	}
	return ok;
}

extern void openTagFile (void)
{
	setDefaultTagFileName ();
	TagsToStdout = isDestinationStdout ();

	if (TagFile.vLine == NULL)
		TagFile.vLine = vStringNew ();

	/*  Open the tags file.
	 */
	if (TagsToStdout)
	{
		if (Option.sorted == SO_UNSORTED)
		{
			/* Passing NULL for keeping stdout open.
			   stdout can be used for debugging purpose.*/
			TagFile.mio = mio_new_fp(stdout, NULL);
			TagFile.name = eStrdup ("/dev/stdout");
		}
		else
		{
			/* Open a tempfile with read and write mode. Read mode is used when
			 * write the result to stdout. */
			TagFile.mio = tempFile ("w+", &TagFile.name);
		}

		if (isXtagEnabled (XTAG_PSEUDO_TAGS))
			addCommonPseudoTags ();
	}
	else
	{
		bool fileExists;

		TagFile.name = eStrdup (Option.tagFileName);
		fileExists = doesFileExist (TagFile.name);
		if (fileExists  &&  ! isTagFile (TagFile.name))
			error (FATAL,
			  "\"%s\" doesn't look like a tag file; I refuse to overwrite it.",
				  TagFile.name);

		if (Option.etags)
		{
			if (Option.append  &&  fileExists)
				TagFile.mio = mio_new_file (TagFile.name, "a+b");
			else
				TagFile.mio = mio_new_file (TagFile.name, "w+b");
		}
		else
		{
			if (Option.append  &&  fileExists)
			{
				TagFile.mio = mio_new_file (TagFile.name, "r+");
				if (TagFile.mio != NULL)
				{
					TagFile.numTags.prev = updatePseudoTags (TagFile.mio);
					mio_free (TagFile.mio);
					TagFile.mio = mio_new_file (TagFile.name, "a+");
				}
			}
			else
			{
				TagFile.mio = mio_new_file (TagFile.name, "w");
				if (TagFile.mio != NULL && isXtagEnabled (XTAG_PSEUDO_TAGS))
					addCommonPseudoTags ();
			}
		}
		if (TagFile.mio == NULL)
			error (FATAL | PERROR, "cannot open tag file");
	}

	if (TagFile.directory == NULL)
	{
		if (TagsToStdout)
			TagFile.directory = eStrdup (CurrentDirectory);
		else
			TagFile.directory = absoluteDirname (TagFile.name);
	}
}

#ifdef USE_REPLACEMENT_TRUNCATE

static void copyBytes (MIO* const fromMio, MIO* const toMio, const long size)
{
	enum { BufferSize = 1000 };
	long toRead, numRead;
	char* buffer = xMalloc (BufferSize, char);
	long remaining = size;
	do
	{
		toRead = (0 < remaining && remaining < BufferSize) ?
					remaining : (long) BufferSize;
		numRead = mio_read (fromMio, buffer, (size_t) 1, (size_t) toRead);
		if (mio_write (toMio, buffer, (size_t)1, (size_t)numRead) < (size_t)numRead)
			error (FATAL | PERROR, "cannot complete write");
		if (remaining > 0)
			remaining -= numRead;
	} while (numRead == toRead  &&  remaining != 0);
	eFree (buffer);
}

static void copyFile (const char *const from, const char *const to, const long size)
{
	MIO* const fromMio = mio_new_file (from, "rb");
	if (fromMio == NULL)
		error (FATAL | PERROR, "cannot open file to copy");
	else
	{
		MIO* const toMio = mio_new_file (to, "wb");
		if (toMio == NULL)
			error (FATAL | PERROR, "cannot open copy destination");
		else
		{
			copyBytes (fromMio, toMio, size);
			mio_free (toMio);
		}
		mio_free (fromMio);
	}
}

/*  Replacement for missing library function.
 */
static int replacementTruncate (const char *const name, const long size)
{
	char *tempName = NULL;
	MIO *mio = tempFile ("w", &tempName);
	mio_free (mio);
	copyFile (name, tempName, size);
	copyFile (tempName, name, WHOLE_FILE);
	remove (tempName);
	eFree (tempName);

	return 0;
}

#endif

#ifndef EXTERNAL_SORT
static void internalSortTagFile (void)
{
	MIO *mio;

	/*  Open/Prepare the tag file and place its lines into allocated buffers.
	 */
	if (TagsToStdout)
	{
		mio = TagFile.mio;
		mio_seek (mio, 0, SEEK_SET);
	}
	else
	{
		mio = mio_new_file (tagFileName (), "r");
		if (mio == NULL)
			failedSort (mio, NULL);
	}

	internalSortTags (TagsToStdout,
			  mio,
			  TagFile.numTags.added + TagFile.numTags.prev);

	if (! TagsToStdout)
		mio_free (mio);
}
#endif

static void sortTagFile (void)
{
	if (TagFile.numTags.added > 0L)
	{
		if (Option.sorted != SO_UNSORTED)
		{
			verbose ("sorting tag file\n");
#ifdef EXTERNAL_SORT
			externalSortTags (TagsToStdout, TagFile.mio);
#else
			internalSortTagFile ();
#endif
		}
		else if (TagsToStdout)
			catFile (TagFile.mio);
	}
}

static void resizeTagFile (const long newSize)
{
	int result;

#ifdef USE_REPLACEMENT_TRUNCATE
	result = replacementTruncate (TagFile.name, newSize);
#else
# ifdef HAVE_TRUNCATE
	result = truncate (TagFile.name, (off_t) newSize);
# else
	const int fd = open (TagFile.name, O_RDWR);

	if (fd == -1)
		result = -1;
	else
	{
#  ifdef HAVE_FTRUNCATE
		result = ftruncate (fd, (off_t) newSize);
#  else
#   ifdef HAVE_CHSIZE
		result = chsize (fd, newSize);
#   endif
#  endif
		close (fd);
	}
# endif
#endif
	if (result == -1)
		fprintf (stderr, "Cannot shorten tag file: errno = %d\n", errno);
}

static void writeEtagsIncludes (MIO *const mio)
{
	if (Option.etagsInclude)
	{
		unsigned int i;
		for (i = 0  ;  i < stringListCount (Option.etagsInclude)  ;  ++i)
		{
			vString *item = stringListItem (Option.etagsInclude, i);
			mio_printf (mio, "\f\n%s,include\n", vStringValue (item));
		}
	}
}

extern void closeTagFile (const bool resize)
{
	long desiredSize, size;

	if (Option.etags)
		writeEtagsIncludes (TagFile.mio);
	mio_flush (TagFile.mio);

	if ((TagsToStdout && (Option.sorted == SO_UNSORTED)))
	{
		if (mio_free (TagFile.mio) != 0)
			error (FATAL | PERROR, "cannot close tag file");
		goto out;
	}

	abort_if_ferror (TagFile.mio);
	desiredSize = mio_tell (TagFile.mio);
	mio_seek (TagFile.mio, 0L, SEEK_END);
	size = mio_tell (TagFile.mio);
	if (! TagsToStdout)
		/* The tag file should be closed before resizing. */
		if (mio_free (TagFile.mio) != 0)
			error (FATAL | PERROR, "cannot close tag file");

	if (resize  &&  desiredSize < size)
	{
		DebugStatement (
			debugPrintf (DEBUG_STATUS, "shrinking %s from %ld to %ld bytes\n",
				TagFile.name, size, desiredSize); )
		resizeTagFile (desiredSize);
	}
	sortTagFile ();
	if (TagsToStdout)
	{
		if (mio_free (TagFile.mio) != 0)
			error (FATAL | PERROR, "cannot close tag file");
		remove (tagFileName ());  /* remove temporary file */
	}

 out:
	eFree (TagFile.name);
	TagFile.name = NULL;
}

/*
 *  Tag entry management
 */

/*  This function copies the current line out to a specified file. It has no
 *  effect on the fileGetc () function.  During copying, any '\' characters
 *  are doubled and a leading '^' or trailing '$' is also quoted. End of line
 *  characters (line feed or carriage return) are dropped.
 */
static size_t appendInputLine (int putc_func (char , void *), const char *const line, void * data, bool *omitted)
{
	size_t length = 0;
	const char *p;

	/*  Write everything up to, but not including, a line end character.
	 */
	*omitted = false;
	for (p = line  ;  *p != '\0'  ;  ++p)
	{
		const int next = *(p + 1);
		const int c = *p;

		if (c == CRETURN  ||  c == NEWLINE)
			break;

		if (Option.patternLengthLimit != 0 && length >= Option.patternLengthLimit)
		{
			*omitted = true;
			break;
		}
		/*  If character is '\', or a terminal '$', then quote it.
		 */
		if (c == BACKSLASH  ||  c == (Option.backward ? '?' : '/')  ||
			(c == '$'  &&  (next == NEWLINE  ||  next == CRETURN)))
		{
			putc_func (BACKSLASH, data);
			++length;
		}
		putc_func (c, data);
		++length;
	}

	return length;
}

static int vstring_putc (char c, void *data)
{
	vString *str = data;
	vStringPut (str, c);
	return 1;
}

static int vstring_puts (const char* s, void *data)
{
	vString *str = data;
	int len = vStringLength (str);
	vStringCatS (str, s);
	return vStringLength (str) - len;
}

static bool isPosSet(MIOPos pos)
{
	char * p = (char *)&pos;
	bool r = false;
	unsigned int i;

	for (i = 0; i < sizeof(pos); i++)
		r |= p[i];
	return r;
}

extern char *readLineFromBypassAnyway (vString *const vLine, const tagEntryInfo *const tag,
				   long *const pSeekValue)
{
	char * line;

	if (isPosSet (tag->filePosition) || (tag->pattern == NULL))
		line = 	readLineFromBypass (vLine, tag->filePosition, pSeekValue);
	else
		line = readLineFromBypassSlow (vLine, tag->lineNumber, tag->pattern, pSeekValue);

	return line;
}

/*  Truncates the text line containing the tag at the character following the
 *  tag, providing a character which designates the end of the tag.
 */
extern void truncateTagLineAfterTag (
		char *const line, const char *const token, const bool discardNewline)
{
	char *p = strstr (line, token);

	if (p != NULL)
	{
		p += strlen (token);
		if (*p != '\0'  &&  ! (*p == '\n'  &&  discardNewline))
			++p;    /* skip past character terminating character */
		*p = '\0';
	}
}

static char* getFullQualifiedScopeNameFromCorkQueue (const tagEntryInfo * inner_scope)
{

	int kindIndex = KIND_GHOST_INDEX;
	langType lang;
	const tagEntryInfo *scope = inner_scope;
	stringList *queue = stringListNew ();
	vString *v;
	vString *n;
	unsigned int c;
	const char *sep;

	while (scope)
	{
		if (!scope->placeholder)
		{
			if (kindIndex != KIND_GHOST_INDEX)
			{
				sep = scopeSeparatorFor (lang, kindIndex, scope->kindIndex);
				v = vStringNewInit (sep);
				stringListAdd (queue, v);
			}
			/* TODO: scope field of SCOPE can be used for optimization. */
			v = vStringNewInit (scope->name);
			stringListAdd (queue, v);
			kindIndex = scope->kindIndex;
			lang = scope->langType;
		}
		scope =  getEntryInCorkQueue (scope->extensionFields.scopeIndex);
	}

	n = vStringNew ();
	while ((c = stringListCount (queue)) > 0)
	{
		v = stringListLast (queue);
		vStringCat (n, v);
		vStringDelete (v);
		stringListRemoveLast (queue);
	}
	stringListDelete (queue);

	return vStringDeleteUnwrap (n);
}

extern void getTagScopeInformation (tagEntryInfo *const tag,
				    const char **kind, const char **name)
{
	if (kind)
		*kind = NULL;
	if (name)
		*name = NULL;

	if (tag->extensionFields.scopeKindIndex == KIND_GHOST_INDEX
	    && tag->extensionFields.scopeName == NULL
	    && tag->extensionFields.scopeIndex != CORK_NIL
	    && TagFile.corkQueue.count > 0)
	{
		const tagEntryInfo * scope;
		char *full_qualified_scope_name;

		scope = getEntryInCorkQueue (tag->extensionFields.scopeIndex);
		full_qualified_scope_name = getFullQualifiedScopeNameFromCorkQueue(scope);
		Assert (full_qualified_scope_name);

		/* Make the information reusable to generate full qualified entry, and xformat output*/
		tag->extensionFields.scopeLangType = scope->langType;
		tag->extensionFields.scopeKindIndex = scope->kindIndex;
		tag->extensionFields.scopeName = full_qualified_scope_name;
	}

	if (tag->extensionFields.scopeKindIndex != KIND_GHOST_INDEX  &&
	    tag->extensionFields.scopeName != NULL)
	{
		if (kind)
		{
			langType lang = (tag->extensionFields.scopeLangType == LANG_AUTO)
				? tag->langType
				: tag->extensionFields.scopeLangType;
			kindDefinition *kdef = getLanguageKind (lang,
													tag->extensionFields.scopeKindIndex);
			*kind = kdef->name;
		}
		if (name)
			*name = tag->extensionFields.scopeName;
	}
}


static int   makePatternStringCommon (const tagEntryInfo *const tag,
				      int (* putc_func) (char , void *),
				      int (* puts_func) (const char* , void *),
				      void *output)
{
	int length = 0;

	char *line;
	int searchChar;
	const char *terminator;
	bool  omitted;
	size_t line_len;

	bool making_cache = false;
	int (* puts_o_func)(const char* , void *);
	void * o_output;

	static vString *cached_pattern;
	static MIOPos   cached_location;
	if (TagFile.patternCacheValid
	    && (! tag->truncateLineAfterTag)
	    && (memcmp (&tag->filePosition, &cached_location, sizeof(MIOPos)) == 0))
		return puts_func (vStringValue (cached_pattern), output);

	line = readLineFromBypass (TagFile.vLine, tag->filePosition, NULL);
	if (line == NULL)
	{
		/* This can be occurs if the size of input file is zero, and
		   an empty regex pattern (//) matches to the input. */
		line = "";
	}

	if (tag->truncateLineAfterTag)
		truncateTagLineAfterTag (line, tag->name, false);

	line_len = strlen (line);
	searchChar = Option.backward ? '?' : '/';
	terminator = (line_len > 0 && (line [line_len - 1] == '\n')) ? "$": "";

	if (!tag->truncateLineAfterTag)
	{
		making_cache = true;
		cached_pattern = vStringNewOrClearWithAutoRelease (cached_pattern);

		puts_o_func = puts_func;
		o_output    = output;
		putc_func   = vstring_putc;
		puts_func   = vstring_puts;
		output      = cached_pattern;
	}

	length += putc_func(searchChar, output);
	if ((tag->boundaryInfo & BOUNDARY_START) == 0)
		length += putc_func('^', output);
	length += appendInputLine (putc_func, line, output, &omitted);
	length += puts_func (omitted? "": terminator, output);
	length += putc_func (searchChar, output);

	if (making_cache)
	{
		puts_o_func (vStringValue (cached_pattern), o_output);
		cached_location = tag->filePosition;
		TagFile.patternCacheValid = true;
	}

	return length;
}

extern char* makePatternString (const tagEntryInfo *const tag)
{
	vString* pattern = vStringNew ();
	makePatternStringCommon (tag, vstring_putc, vstring_puts, pattern);
	return vStringDeleteUnwrap (pattern);
}

static tagField * tagFieldNew(fieldType ftype, const char *value, bool valueOwner)
{
	tagField *f = xMalloc (1, tagField);

	f->ftype = ftype;
	f->value = value;
	f->valueOwner = valueOwner;
	return f;
}

static void tagFieldDelete (tagField * f)
{
	if (f->valueOwner)
		eFree((void *)f->value);
	eFree (f);
}

static void attachParserFieldGeneric (tagEntryInfo *const tag, fieldType ftype, const char * value,
									  bool valueOwner)
{
	if (tag->usedParserFields < PRE_ALLOCATED_PARSER_FIELDS)
	{
		tag->parserFields [tag->usedParserFields].ftype = ftype;
		tag->parserFields [tag->usedParserFields].value = value;
		tag->parserFields [tag->usedParserFields].valueOwner = valueOwner;
		tag->usedParserFields++;
	}
	else if (tag->parserFieldsDynamic == NULL)
	{
		tag->parserFieldsDynamic = ptrArrayNew((ptrArrayDeleteFunc)tagFieldDelete);
		PARSER_TRASH_BOX(tag->parserFieldsDynamic, ptrArrayDelete);
		attachParserFieldGeneric (tag, ftype, value, valueOwner);
	}
	else
	{
		tagField *f = tagFieldNew (ftype, value, valueOwner);
		ptrArrayAdd(tag->parserFieldsDynamic, f);
		tag->usedParserFields++;
	}
}

extern void attachParserField (tagEntryInfo *const tag, fieldType ftype, const char * value)
{
	attachParserFieldGeneric (tag, ftype, value, false);
}

extern void attachParserFieldToCorkEntry (int index,
					 fieldType ftype,
					 const char *value)
{
	tagEntryInfo * tag;
	const char * v;

	if (index == CORK_NIL)
		return;

	tag = getEntryInCorkQueue(index);
	Assert (tag != NULL);

	v = eStrdup (value);
	attachParserFieldGeneric (tag, ftype, v, true);
}

extern const tagField* getParserField (const tagEntryInfo * tag, int index)
{
	if (index < 0
		|| tag->usedParserFields <= ((unsigned int)index) )
		return NULL;
	else if (index < PRE_ALLOCATED_PARSER_FIELDS)
		return tag->parserFields + index;
	else
	{
		unsigned int n = index - PRE_ALLOCATED_PARSER_FIELDS;
		return ptrArrayItem(tag->parserFieldsDynamic, n);
	}
}

static void copyParserFields (const tagEntryInfo *const tag, tagEntryInfo* slot)
{
	unsigned int i;
	const char* value;

	for (i = 0; i < tag->usedParserFields; i++)
	{
		const tagField *f = getParserField (tag, i);
		Assert(f);

		value = f->value;
		if (value)
			value = eStrdup (value);

		attachParserFieldGeneric (slot,
								  f->ftype,
								  value,
								  true);
	}

}

static void recordTagEntryInQueue (const tagEntryInfo *const tag, tagEntryInfo* slot)
{
	*slot = *tag;

	if (slot->pattern)
		slot->pattern = eStrdup (slot->pattern);

	slot->inputFileName = eStrdup (slot->inputFileName);
	slot->name = eStrdup (slot->name);
	if (slot->extensionFields.access)
		slot->extensionFields.access = eStrdup (slot->extensionFields.access);
	if (slot->extensionFields.fileScope)
		slot->extensionFields.fileScope = eStrdup (slot->extensionFields.fileScope);
	if (slot->extensionFields.implementation)
		slot->extensionFields.implementation = eStrdup (slot->extensionFields.implementation);
	if (slot->extensionFields.inheritance)
		slot->extensionFields.inheritance = eStrdup (slot->extensionFields.inheritance);
	if (slot->extensionFields.scopeName)
		slot->extensionFields.scopeName = eStrdup (slot->extensionFields.scopeName);
	if (slot->extensionFields.signature)
		slot->extensionFields.signature = eStrdup (slot->extensionFields.signature);
	if (slot->extensionFields.typeRef[0])
		slot->extensionFields.typeRef[0] = eStrdup (slot->extensionFields.typeRef[0]);
	if (slot->extensionFields.typeRef[1])
		slot->extensionFields.typeRef[1] = eStrdup (slot->extensionFields.typeRef[1]);
#ifdef HAVE_LIBXML
	if (slot->extensionFields.xpath)
		slot->extensionFields.xpath = eStrdup (slot->extensionFields.xpath);
#endif

	if (slot->extraDynamic)
	{
		int n = countXtags () - XTAG_COUNT;
		slot->extraDynamic = xCalloc ((n / 8) + 1, uint8_t);
		memcpy (slot->extraDynamic, tag->extraDynamic, (n / 8) + 1);
	}

	if (slot->sourceFileName)
		slot->sourceFileName = eStrdup (slot->sourceFileName);


	slot->usedParserFields = 0;
	slot->parserFieldsDynamic = NULL;
	copyParserFields (tag, slot);
	if (slot->parserFieldsDynamic)
		PARSER_TRASH_BOX_TAKE_BACK(slot->parserFieldsDynamic);
}

static void clearParserFields (tagEntryInfo *const tag)
{
	unsigned int i, n;
	const char* value;

	if ( tag->usedParserFields < PRE_ALLOCATED_PARSER_FIELDS )
		n = tag->usedParserFields;
	else
		n = PRE_ALLOCATED_PARSER_FIELDS;

	for (i = 0; i < n; i++)
	{
		value = tag->parserFields[i].value;
		if (value && tag->parserFields[i].valueOwner)
			eFree ((char *)value);
		tag->parserFields[i].value = NULL;
		tag->parserFields[i].ftype = FIELD_UNKNOWN;
	}
	if (tag->parserFieldsDynamic)
	{
		ptrArrayDelete (tag->parserFieldsDynamic);
		tag->parserFieldsDynamic = NULL;
	}
}

static void clearTagEntryInQueue (tagEntryInfo* slot)
{
	if (slot->pattern)
		eFree ((char *)slot->pattern);
	eFree ((char *)slot->inputFileName);
	eFree ((char *)slot->name);

	if (slot->extensionFields.access)
		eFree ((char *)slot->extensionFields.access);
	if (slot->extensionFields.fileScope)
		eFree ((char *)slot->extensionFields.fileScope);
	if (slot->extensionFields.implementation)
		eFree ((char *)slot->extensionFields.implementation);
	if (slot->extensionFields.inheritance)
		eFree ((char *)slot->extensionFields.inheritance);
	if (slot->extensionFields.scopeName)
		eFree ((char *)slot->extensionFields.scopeName);
	if (slot->extensionFields.signature)
		eFree ((char *)slot->extensionFields.signature);
	if (slot->extensionFields.typeRef[0])
		eFree ((char *)slot->extensionFields.typeRef[0]);
	if (slot->extensionFields.typeRef[1])
		eFree ((char *)slot->extensionFields.typeRef[1]);
#ifdef HAVE_LIBXML
	if (slot->extensionFields.xpath)
		eFree ((char *)slot->extensionFields.xpath);
#endif

	if (slot->extraDynamic)
		eFree (slot->extraDynamic);

	if (slot->sourceFileName)
		eFree ((char *)slot->sourceFileName);

	clearParserFields (slot);
}

static unsigned int queueTagEntry(const tagEntryInfo *const tag)
{
	unsigned int i;
	void *tmp;
	tagEntryInfo * slot;

	if (! (TagFile.corkQueue.count < TagFile.corkQueue.length))
	{
		if (!TagFile.corkQueue.length)
			TagFile.corkQueue.length = 1;

		tmp = eRealloc (TagFile.corkQueue.queue,
				sizeof (*TagFile.corkQueue.queue) * TagFile.corkQueue.length * 2);

		TagFile.corkQueue.length *= 2;
		TagFile.corkQueue.queue = tmp;
	}

	i = TagFile.corkQueue.count;
	TagFile.corkQueue.count++;


	slot = TagFile.corkQueue.queue + i;
	recordTagEntryInQueue (tag, slot);

	return i;
}


extern void setupWriter (void)
{
	writerSetup (TagFile.mio);
}

extern bool teardownWriter (const char *filename)
{
	return writerTeardown (TagFile.mio, filename);
}

static bool isTagWritable(const tagEntryInfo *const tag)
{
	if (tag->placeholder)
		return false;

	if (! isLanguageKindEnabled(tag->langType, tag->kindIndex))
		return false;

	if (tag->extensionFields.roleBits)
	{
		size_t available_roles;

		if (!isXtagEnabled (XTAG_REFERENCE_TAGS))
			return false;

		available_roles = countLanguageRoles(tag->langType,
											 tag->kindIndex);
		if (tag->extensionFields.roleBits >=
			(makeRoleBit(available_roles)))
			return false;

		/* TODO: optimization
		   A Bitmasks represeting all enabled roles can be calculated at the
		   end of initializing the parser. Calculating each time when checking
		   a tag entry is not needed. */
		for (unsigned int roleIndex = 0; roleIndex < available_roles; roleIndex++)
		{
			if (isRoleAssigned(tag, roleIndex))
			{
				if (isLanguageRoleEnabled (tag->langType, tag->kindIndex,
											 roleIndex))
					return true;
			}

		}
		return false;
	}
	else if (isLanguageKindRefOnly(tag->langType, tag->kindIndex))
	{
		error (WARNING, "definition tag for refonly kind(%s) is made: %s",
			   getLanguageKind(tag->langType, tag->kindIndex)->name,
			   tag->name);
		/* This one is not so critical. */
	}

	if (!isXtagEnabled(XTAG_ANONYMOUS)
		&& isTagExtraBitMarked(tag, XTAG_ANONYMOUS))
		return false;

	return true;
}

static void writeTagEntry (const tagEntryInfo *const tag, bool checkingNeeded)
{
	int length = 0;

	Assert (tag->kindIndex != KIND_GHOST_INDEX);

	if (checkingNeeded && !isTagWritable(tag))
		return;

	DebugStatement ( debugEntry (tag); )

	if (includeExtensionFlags ()
	    && isXtagEnabled (XTAG_QUALIFIED_TAGS)
	    && doesInputLanguageRequestAutomaticFQTag ())
	{
		/* const is discarded to update the cache field of TAG. */
		writerBuildFqTagCache ( (tagEntryInfo *const)tag);
	}

	length = writerWriteTag (TagFile.mio, tag);

	++TagFile.numTags.added;
	rememberMaxLengths (strlen (tag->name), (size_t) length);
	DebugStatement ( mio_flush (TagFile.mio); )

	abort_if_ferror (TagFile.mio);
}

extern bool writePseudoTag (const ptagDesc *desc,
			       const char *const fileName,
			       const char *const pattern,
			       const char *const parserName)
{
	int length;

	length = writerWritePtag (TagFile.mio, desc, fileName,
							  pattern, parserName);
	if (length < 0)
		return false;

	abort_if_ferror (TagFile.mio);

	++TagFile.numTags.added;
	rememberMaxLengths (strlen (desc->name), (size_t) length);

	return true;
}

extern void corkTagFile(void)
{
	TagFile.cork++;
	if (TagFile.cork == 1)
	{
		  TagFile.corkQueue.length = 1;
		  TagFile.corkQueue.count = 1;
		  TagFile.corkQueue.queue = eMalloc (sizeof (*TagFile.corkQueue.queue));
		  memset (TagFile.corkQueue.queue, 0, sizeof (*TagFile.corkQueue.queue));
	}
}

extern void uncorkTagFile(void)
{
	unsigned int i;

	TagFile.cork--;

	if (TagFile.cork > 0)
		return ;

	for (i = 1; i < TagFile.corkQueue.count; i++)
	{
		tagEntryInfo *tag = TagFile.corkQueue.queue + i;
		writeTagEntry (tag, true);
		if (doesInputLanguageRequestAutomaticFQTag ()
		    && isXtagEnabled (XTAG_QUALIFIED_TAGS)
		    && (tag->extensionFields.scopeKindIndex != KIND_GHOST_INDEX)
		    && tag->extensionFields.scopeName
		    && tag->extensionFields.scopeIndex)
			makeQualifiedTagEntry (tag);
	}
	for (i = 1; i < TagFile.corkQueue.count; i++)
		clearTagEntryInQueue (TagFile.corkQueue.queue + i);

	memset (TagFile.corkQueue.queue, 0,
		sizeof (*TagFile.corkQueue.queue) * TagFile.corkQueue.count);
	TagFile.corkQueue.count = 0;
	eFree (TagFile.corkQueue.queue);
	TagFile.corkQueue.queue = NULL;
	TagFile.corkQueue.length = 0;
}

extern tagEntryInfo *getEntryInCorkQueue   (unsigned int n)
{
	if ((CORK_NIL < n) && (n < TagFile.corkQueue.count))
		return TagFile.corkQueue.queue + n;
	else
		return NULL;
}

extern tagEntryInfo *getEntryOfNestingLevel (const NestingLevel *nl)
{
	if (nl == NULL)
		return NULL;
	return getEntryInCorkQueue (nl->corkIndex);
}

extern size_t        countEntryInCorkQueue (void)
{
	return TagFile.corkQueue.count;
}

static void makeTagEntriesForSubwords (tagEntryInfo *const subtag)
{
	stringList *list;

	subtag->extensionFields.scopeIndex = CORK_NIL;
	markTagExtraBit (subtag, XTAG_SUBWORD);

	list = stringListNewBySplittingWordIntoSubwords(subtag->name);
	for (unsigned int i = 0; i < stringListCount(list); i++)
	{
		vString *subword = stringListItem (list, i);

		subtag->name = vStringValue(subword);

		if (TagFile.cork)
			queueTagEntry (subtag);
		else
			writeTagEntry (subtag, false);
	}
	stringListDelete (list);
}

extern int makeTagEntry (const tagEntryInfo *const tag)
{
	int r = CORK_NIL;
	Assert (tag->name != NULL);

	if (!TagFile.cork)
		if (!isTagWritable (tag))
			goto out;

	if (tag->name [0] == '\0' && (!tag->placeholder))
	{
		if (!doesInputLanguageAllowNullTag())
			error (WARNING, "ignoring null tag in %s(line: %lu)",
			       getInputFileName (), tag->lineNumber);
		goto out;
	}

	if (TagFile.cork)
		r = queueTagEntry (tag);
	else
		writeTagEntry (tag, false);

	notifyMakeTagEntry (tag, r);

	if (isXtagEnabled (XTAG_SUBWORD))
	{
		tagEntryInfo subtag = *tag;
		makeTagEntriesForSubwords (&subtag);
	}
out:
	return r;
}

extern int makeQualifiedTagEntry (const tagEntryInfo *const e)
{
	int r = CORK_NIL;
	tagEntryInfo x;
	int xk;
	const char *sep;
	static vString *fqn;

	if (isXtagEnabled (XTAG_QUALIFIED_TAGS))
	{
		x = *e;
		markTagExtraBit (&x, XTAG_QUALIFIED_TAGS);

		fqn = vStringNewOrClearWithAutoRelease (fqn);

		if (e->extensionFields.scopeName)
		{
			vStringCatS (fqn, e->extensionFields.scopeName);
			xk = e->extensionFields.scopeKindIndex;
			sep = scopeSeparatorFor (e->langType, e->kindIndex, xk);
			vStringCatS (fqn, sep);
		}
		else
		{
			/* This is an top level item. prepend a root separator
			   if the kind of the item has. */
			sep = scopeSeparatorFor (e->langType, e->kindIndex, KIND_GHOST_INDEX);
			if (sep == NULL)
			{
				/* No root separator. The name of the
				   optional tag and that of full qualified tag
				   are the same; recording the full qualified tag
				   is meaningless. */
				return r;
			}
			else
				vStringCatS (fqn, sep);
		}
		vStringCatS (fqn, e->name);

		x.name = vStringValue (fqn);
		/* makeExtraTagEntry of c.c doesn't clear scope
		   related fields. */
#if 0
		x.extensionFields.scopeKind = NULL;
		x.extensionFields.scopeName = NULL;
		x.extensionFields.scopeIndex = CORK_NIL;
#endif

		bool in_subparser
			= isTagExtraBitMarked (&x,
								   XTAG_TAGS_GENERATED_BY_SUBPARSER);

		if (in_subparser)
			pushLanguage(x.langType);

		r = makeTagEntry (&x);

		if (in_subparser)
			popLanguage();
	}
	return r;
}

extern void initTagEntry (tagEntryInfo *const e, const char *const name,
						  int kindIndex)
{
	initTagEntryFull(e, name,
			 getInputLineNumber (),
			 getInputLanguage (),
			 getInputFilePosition (),
			 getInputFileTagPath (),
			 kindIndex,
			 0,
			 getSourceFileTagPath(),
			 getSourceLanguage(),
			 getSourceLineNumber() - getInputLineNumber ());
}

extern void initRefTagEntry (tagEntryInfo *const e, const char *const name,
			     int kindIndex, int roleIndex)
{
	initTagEntryFull(e, name,
			 getInputLineNumber (),
			 getInputLanguage (),
			 getInputFilePosition (),
			 getInputFileTagPath (),
			 kindIndex,
			 makeRoleBit(roleIndex),
			 getSourceFileTagPath(),
			 getSourceLanguage(),
			 getSourceLineNumber() - getInputLineNumber ());
}

extern void initTagEntryFull (tagEntryInfo *const e, const char *const name,
			      unsigned long lineNumber,
			      langType langType_,
			      MIOPos      filePosition,
			      const char *inputFileName,
			      int kindIndex,
			      roleBitsType roleBits,
			      const char *sourceFileName,
			      langType sourceLangType,
			      long sourceLineNumberDifference)
{
	int i;

	Assert (getInputFileName() != NULL);

	memset (e, 0, sizeof (tagEntryInfo));
	e->lineNumberEntry = (bool) (Option.locate == EX_LINENUM);
	e->lineNumber      = lineNumber;
	e->boundaryInfo    = getNestedInputBoundaryInfo (lineNumber);
	e->langType        = langType_;
	e->filePosition    = filePosition;
	e->inputFileName   = inputFileName;
	e->name            = name;
	e->extensionFields.scopeLangType = LANG_AUTO;
	e->extensionFields.scopeKindIndex = KIND_GHOST_INDEX;
	e->extensionFields.scopeIndex     = CORK_NIL;

	Assert (kindIndex < 0 || kindIndex < countLanguageKinds(langType_));
	e->kindIndex = kindIndex;

	Assert (roleBits == 0
			|| (roleBits < (makeRoleBit(countLanguageRoles(langType_, kindIndex)))));
	e->extensionFields.roleBits = roleBits;
	if (roleBits)
		markTagExtraBit (e, XTAG_REFERENCE_TAGS);

	if (doesParserRunAsGuest ())
		markTagExtraBit (e, XTAG_TAGS_GENERATED_BY_GUEST_PARSERS);
	if (doesSubparserRun ())
		markTagExtraBit (e, XTAG_TAGS_GENERATED_BY_SUBPARSER);

	e->sourceLangType = sourceLangType;
	e->sourceFileName = sourceFileName;
	e->sourceLineNumberDifference = sourceLineNumberDifference;

	e->usedParserFields = 0;

	for ( i = 0; i < PRE_ALLOCATED_PARSER_FIELDS; i++ )
		e->parserFields[i].ftype = FIELD_UNKNOWN;

	if (isParserMarkedNoEmission ())
		e->placeholder = 1;
}

static void    markTagExtraBitFull     (tagEntryInfo *const tag, xtagType extra, bool mark)
{
	unsigned int index;
	unsigned int offset;
	uint8_t *slot;

	Assert (extra != XTAG_UNKNOWN);

	if (extra < XTAG_COUNT)
	{
		index = (extra / 8);
		offset = (extra % 8);
		slot = tag->extra;
	}
	else if (tag->extraDynamic)
	{
		Assert (extra < countXtags ());

		index = ((extra - XTAG_COUNT) / 8);
		offset = ((extra - XTAG_COUNT) % 8);
		slot = tag->extraDynamic;
	}
	else
	{
		Assert (extra < countXtags ());

		int n = countXtags () - XTAG_COUNT;
		tag->extraDynamic = xCalloc ((n / 8) + 1, uint8_t);
		PARSER_TRASH_BOX(tag->extraDynamic, eFree);
		markTagExtraBit (tag, extra);
		return;
	}

	if (mark)
		slot [ index ] |= (1 << offset);
	else
		slot [ index ] &= ~(1 << offset);
}

extern void    markTagExtraBit     (tagEntryInfo *const tag, xtagType extra)
{
	markTagExtraBitFull (tag, extra, true);
}

extern bool isTagExtraBitMarked (const tagEntryInfo *const tag, xtagType extra)
{
	unsigned int index;
	unsigned int offset;
	const uint8_t *slot;

	Assert (extra != XTAG_UNKNOWN);

	if (extra < XTAG_COUNT)
	{
		index = (extra / 8);
		offset = (extra % 8);
		slot = tag->extra;

	}
	else if (!tag->extraDynamic)
		return false;
	else
	{
		Assert (extra < countXtags ());
		index = ((extra - XTAG_COUNT) / 8);
		offset = ((extra - XTAG_COUNT) % 8);
		slot = tag->extraDynamic;

	}
	return !! ((slot [ index ]) & (1 << offset));
}

static void assignRoleFull(tagEntryInfo *const e, int roleIndex, bool assign)
{
	if (roleIndex == ROLE_INDEX_DEFINITION)
	{
		if (assign)
		{
			e->extensionFields.roleBits = 0;
			markTagExtraBitFull (e, XTAG_REFERENCE_TAGS, false);
		}
	}
	else if (roleIndex > ROLE_INDEX_DEFINITION)
	{
		Assert (roleIndex < countLanguageRoles(e->langType, e->kindIndex));

		if (assign)
			e->extensionFields.roleBits |= (makeRoleBit(roleIndex));
		else
			e->extensionFields.roleBits &= ~(makeRoleBit(roleIndex));
		markTagExtraBitFull (e, XTAG_REFERENCE_TAGS, e->extensionFields.roleBits);
	}
	else
		AssertNotReached();
}

extern void assignRole(tagEntryInfo *const e, int roleIndex)
{
	assignRoleFull(e, roleIndex, true);
}

extern bool isRoleAssigned(const tagEntryInfo *const e, int roleIndex)
{
	if (roleIndex == ROLE_INDEX_DEFINITION)
		return (!e->extensionFields.roleBits);
	else
		return (e->extensionFields.roleBits & makeRoleBit(roleIndex));
}

extern unsigned long numTagsAdded(void)
{
	return TagFile.numTags.added;
}

extern void setNumTagsAdded (unsigned long nadded)
{
	TagFile.numTags.added = nadded;
}

extern unsigned long numTagsTotal(void)
{
	return TagFile.numTags.added + TagFile.numTags.prev;
}

extern unsigned long maxTagsLine (void)
{
	return (unsigned long)TagFile.max.line;
}

extern void invalidatePatternCache(void)
{
	TagFile.patternCacheValid = false;
}

extern void tagFilePosition (MIOPos *p)
{
	mio_getpos (TagFile.mio, p);
}

extern void setTagFilePosition (MIOPos *p)
{
	mio_setpos (TagFile.mio, p);
}

extern const char* getTagFileDirectory (void)
{
	return TagFile.directory;
}
