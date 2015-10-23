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

#include "debug.h"
#include "ctags.h"
#include "entry.h"
#include "field.h"
#include "main.h"
#include "options.h"
#include "read.h"
#include "routines.h"
#include "sort.h"
#include "strlist.h"

/*
*   MACROS
*/
#define PSEUDO_TAG_PREFIX       "!_"
#define PSEUDO_TAG_SEPARATOR    "!"

#define includeExtensionFlags()         (Option.tagFileFormat > 1)

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

/*
*   DATA DEFINITIONS
*/

tagFile TagFile = {
    NULL,               /* tag file name */
    NULL,               /* tag file directory (absolute) */
    NULL,               /* file pointer */
    { 0, 0 },           /* numTags */
    { 0, 0, 0 },        /* max */
    { NULL, NULL, 0 },  /* etags */
    NULL,                /* vLine */
    .cork = FALSE,
    .corkQueue = {
	    .queue = NULL,
	    .length = 0,
	    .count  = 0
    }
};

static boolean TagsToStdout = FALSE;

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

static void abort_if_ferror(FILE *const fp)
{
	if (ferror (fp))
		error (FATAL | PERROR, "cannot write tag file");
}

static void rememberMaxLengths (const size_t nameLength, const size_t lineLength)
{
	if (nameLength > TagFile.max.tag)
		TagFile.max.tag = nameLength;

	if (lineLength > TagFile.max.line)
		TagFile.max.line = lineLength;
}

extern void writePseudoTag (
		const char *const tagName,
		const char *const fileName,
		const char *const pattern,
		const char *const language)
{
	const int length = language
	  ? fprintf (TagFile.fp, "%s%s%s%s\t%s\t%s\n",
		     PSEUDO_TAG_PREFIX, tagName,
		     PSEUDO_TAG_SEPARATOR, language, fileName, pattern)
	  : fprintf (TagFile.fp, "%s%s\t%s\t/%s/\n",
		     PSEUDO_TAG_PREFIX, tagName, fileName, pattern);

	abort_if_ferror (TagFile.fp);

	++TagFile.numTags.added;
	rememberMaxLengths (strlen (tagName), (size_t) length);
}

static void addPseudoTags (void)
{
	if (! Option.xref)
	{
		char format [11];
		const char *formatComment = "unknown format";
		char commit_id [8] = "";

		sprintf (format, "%u", Option.tagFileFormat);

		if (Option.tagFileFormat == 1)
			formatComment = "original ctags format";
		else if (Option.tagFileFormat == 2)
			formatComment =
				"extended format; --format=1 will not append ;\" to lines";

		writePseudoTag ("TAG_FILE_FORMAT", format, formatComment, NULL);
		writePseudoTag ("TAG_FILE_SORTED",
			Option.sorted == SO_FOLDSORTED ? "2" :
			(Option.sorted == SO_SORTED ? "1" : "0"),
			"0=unsorted, 1=sorted, 2=foldcase",
			NULL);
		writePseudoTag ("TAG_PROGRAM_AUTHOR",  AUTHOR_NAME,  "", NULL);
		writePseudoTag ("TAG_PROGRAM_NAME",    PROGRAM_NAME, "Derived from Exuberant Ctags", NULL);
		writePseudoTag ("TAG_PROGRAM_URL",     PROGRAM_URL,  "official site", NULL);

#if defined(CTAGS_COMMIT_ID) && CTAGS_COMMIT_ID != 0
			snprintf (commit_id, sizeof (commit_id), "%.7x", CTAGS_COMMIT_ID);
#endif
		writePseudoTag ("TAG_PROGRAM_VERSION", PROGRAM_VERSION, commit_id, NULL);

#ifdef HAVE_ICONV
		if (Option.outputEncoding)
			writePseudoTag ("TAG_FILE_ENCODING", Option.outputEncoding, "", NULL);
#endif
	}
}

static void updateSortedFlag (
		const char *const line, FILE *const fp, fpos_t startOfLine)
{
	const char *const tab = strchr (line, '\t');

	if (tab != NULL)
	{
		const long boolOffset = tab - line + 1;  /* where it should be */

		if (line [boolOffset] == '0'  ||  line [boolOffset] == '1')
		{
			fpos_t nextLine;

			if (fgetpos (fp, &nextLine) == -1 || fsetpos (fp, &startOfLine) == -1)
				error (WARNING, "Failed to update 'sorted' pseudo-tag");
			else
			{
				fpos_t flagLocation;
				int c, d;

				do
					c = fgetc (fp);
				while (c != '\t'  &&  c != '\n');
				fgetpos (fp, &flagLocation);
				d = fgetc (fp);
				if (c == '\t'  &&  (d == '0'  ||  d == '1')  &&
					d != (int) Option.sorted)
				{
					fsetpos (fp, &flagLocation);
					fputc (Option.sorted == SO_FOLDSORTED ? '2' :
						(Option.sorted == SO_SORTED ? '1' : '0'), fp);
				}
				fsetpos (fp, &nextLine);
			}
		}
	}
}

/*  Look through all line beginning with "!_TAG_FILE", and update those which
 *  require it.
 */
static long unsigned int updatePseudoTags (FILE *const fp)
{
	enum { maxEntryLength = 20 };
	char entry [maxEntryLength + 1];
	unsigned long linesRead = 0;
	fpos_t startOfLine;
	size_t entryLength;
	const char *line;

	sprintf (entry, "%sTAG_FILE", PSEUDO_TAG_PREFIX);
	entryLength = strlen (entry);
	Assert (entryLength < maxEntryLength);

	fgetpos (fp, &startOfLine);
	line = readLine (TagFile.vLine, fp);
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
					updateSortedFlag (line, fp, startOfLine);
			}
			fgetpos (fp, &startOfLine);
		}
		line = readLine (TagFile.vLine, fp);
	}
	while (line != NULL)  /* skip to end of file */
	{
		++linesRead;
		line = readLine (TagFile.vLine, fp);
	}
	return linesRead;
}

/*
 *  Tag file management
 */

static boolean isValidTagAddress (const char *const excmd)
{
	boolean isValid = FALSE;

	if (strchr ("/?", excmd [0]) != NULL)
		isValid = TRUE;
	else
	{
		char *address = xMalloc (strlen (excmd) + 1, char);
		if (sscanf (excmd, "%[^;\n]", address) == 1  &&
			strspn (address,"0123456789") == strlen (address))
				isValid = TRUE;
		eFree (address);
	}
	return isValid;
}

static boolean isCtagsLine (const char *const line)
{
	enum fieldList { TAG, TAB1, SRC_FILE, TAB2, EXCMD, NUM_FIELDS };
	boolean ok = FALSE;  /* we assume not unless confirmed */
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
				ok = TRUE;

		eFree (fields);
	}
	return ok;
}

static boolean isEtagsLine (const char *const line)
{
	boolean result = FALSE;
	if (line [0] == '\f')
		result = (boolean) (line [1] == '\n'  ||  line [1] == '\r');
	return result;
}

static boolean isTagFile (const char *const filename)
{
	boolean ok = FALSE;  /* we assume not unless confirmed */
	FILE *const fp = fopen (filename, "rb");

	if (fp == NULL  &&  errno == ENOENT)
		ok = TRUE;
	else if (fp != NULL)
	{
		const char *line = readLine (TagFile.vLine, fp);

		if (line == NULL)
			ok = TRUE;
		else
			ok = (boolean) (isCtagsLine (line) || isEtagsLine (line));
		fclose (fp);
	}
	return ok;
}

extern void copyBytes (FILE* const fromFp, FILE* const toFp, const long size)
{
	enum { BufferSize = 1000 };
	long toRead, numRead;
	char* buffer = xMalloc (BufferSize, char);
	long remaining = size;
	do
	{
		toRead = (0 < remaining && remaining < BufferSize) ?
					remaining : (long) BufferSize;
		numRead = fread (buffer, (size_t) 1, (size_t) toRead, fromFp);
		if (fwrite (buffer, (size_t)1, (size_t)numRead, toFp) < (size_t)numRead)
			error (FATAL | PERROR, "cannot complete write");
		if (remaining > 0)
			remaining -= numRead;
	} while (numRead == toRead  &&  remaining != 0);
	eFree (buffer);
}

extern void copyFile (const char *const from, const char *const to, const long size)
{
	FILE* const fromFp = fopen (from, "rb");
	if (fromFp == NULL)
		error (FATAL | PERROR, "cannot open file to copy");
	else
	{
		FILE* const toFp = fopen (to, "wb");
		if (toFp == NULL)
			error (FATAL | PERROR, "cannot open copy destination");
		else
		{
			copyBytes (fromFp, toFp, size);
			fclose (toFp);
		}
		fclose (fromFp);
	}
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
		/* Open a tempfile with read and write mode. Read mode is used when
		 * write the result to stdout. */
		TagFile.fp = tempFile ("w+", &TagFile.name);
	else
	{
		boolean fileExists;

		TagFile.name = eStrdup (Option.tagFileName);
		fileExists = doesFileExist (TagFile.name);
		if (fileExists  &&  ! isTagFile (TagFile.name))
			error (FATAL,
			  "\"%s\" doesn't look like a tag file; I refuse to overwrite it.",
				  TagFile.name);

		if (Option.etags)
		{
			if (Option.append  &&  fileExists)
				TagFile.fp = fopen (TagFile.name, "a+b");
			else
				TagFile.fp = fopen (TagFile.name, "w+b");
		}
		else
		{
			if (Option.append  &&  fileExists)
			{
				TagFile.fp = fopen (TagFile.name, "r+");
				if (TagFile.fp != NULL)
				{
					TagFile.numTags.prev = updatePseudoTags (TagFile.fp);
					fclose (TagFile.fp);
					TagFile.fp = fopen (TagFile.name, "a+");
				}
			}
			else
			{
				TagFile.fp = fopen (TagFile.name, "w");
				if (TagFile.fp != NULL)
					addPseudoTags ();
			}
		}
		if (TagFile.fp == NULL)
			error (FATAL | PERROR, "cannot open tag file");
	}
	if (TagsToStdout)
		TagFile.directory = eStrdup (CurrentDirectory);
	else
		TagFile.directory = absoluteDirname (TagFile.name);
}

#ifdef USE_REPLACEMENT_TRUNCATE

/*  Replacement for missing library function.
 */
static int replacementTruncate (const char *const name, const long size)
{
	char *tempName = NULL;
	FILE *fp = tempFile ("w", &tempName);
	fclose (fp);
	copyFile (name, tempName, size);
	copyFile (tempName, name, WHOLE_FILE);
	remove (tempName);
	eFree (tempName);

	return 0;
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
			externalSortTags (TagsToStdout);
#else
			internalSortTags (TagsToStdout);
#endif
		}
		else if (TagsToStdout)
			catFile (TagFile.fp);
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
		fprintf (errout, "Cannot shorten tag file: errno = %d\n", errno);
}

static void writeEtagsIncludes (FILE *const fp)
{
	if (Option.etagsInclude)
	{
		unsigned int i;
		for (i = 0  ;  i < stringListCount (Option.etagsInclude)  ;  ++i)
		{
			vString *item = stringListItem (Option.etagsInclude, i);
			fprintf (fp, "\f\n%s,include\n", vStringValue (item));
		}
	}
}

extern void closeTagFile (const boolean resize)
{
	long desiredSize, size;

	if (Option.etags)
		writeEtagsIncludes (TagFile.fp);
	fflush (TagFile.fp);
	abort_if_ferror (TagFile.fp);
	desiredSize = ftell (TagFile.fp);
	fseek (TagFile.fp, 0L, SEEK_END);
	size = ftell (TagFile.fp);
	if (! TagsToStdout)
		/* The tag file should be closed before resizing. */
		if (fclose (TagFile.fp) != 0)
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
		if (fclose (TagFile.fp) != 0)
			error (FATAL | PERROR, "cannot close tag file");
		remove (tagFileName ());  /* remove temporary file */
	}
	eFree (TagFile.name);
	TagFile.name = NULL;
}

extern void beginEtagsFile (void)
{
	TagFile.etags.fp = tempFile ("w+b", &TagFile.etags.name);
	TagFile.etags.byteCount = 0;
}

extern void endEtagsFile (const char *const name)
{
	const char *line;

	fprintf (TagFile.fp, "\f\n%s,%ld\n", name, (long) TagFile.etags.byteCount);
	abort_if_ferror (TagFile.fp);

	if (TagFile.etags.fp != NULL)
	{
		rewind (TagFile.etags.fp);
		while ((line = readLine (TagFile.vLine, TagFile.etags.fp)) != NULL)
			fputs (line, TagFile.fp);
		fclose (TagFile.etags.fp);
		remove (TagFile.etags.name);
		eFree (TagFile.etags.name);
		TagFile.etags.fp = NULL;
		TagFile.etags.name = NULL;
	}
}

/*
 *  Tag entry management
 */

static size_t appendSourceLine ( void putc_func (char , void *), const char *const line, void * data)
{
	size_t length = 0;
	const char *p;

	/*  Write everything up to, but not including, a line end character.
	 */
	for (p = line  ;  *p != '\0'  ;  ++p)
	{
		const int next = *(p + 1);
		const int c = *p;

		if (c == CRETURN  ||  c == NEWLINE)
			break;

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

static void vstring_putc (char c, void *data)
{
	vString *str = data;
	vStringPut (str, c);
}

static void file_putc (char c, void *data)
{
	FILE *fp = data;
	putc (c, fp);
}

/*  This function copies the current line out to a specified file. It has no
 *  effect on the fileGetc () function.  During copying, any '\' characters
 *  are doubled and a leading '^' or trailing '$' is also quoted. End of line
 *  characters (line feed or carriage return) are dropped.
 */
static size_t writeSourceLine (FILE *const fp, const char *const line)
{
	return appendSourceLine (file_putc, line, fp);
}

/*  Writes "line", stripping leading and duplicate white space.
 */
static size_t writeCompactSourceLine (FILE *const fp, const char *const line)
{
	boolean lineStarted = FALSE;
	size_t  length = 0;
	const char *p;
	int c;

	/*  Write everything up to, but not including, the newline.
	 */
	for (p = line, c = *p  ;  c != NEWLINE  &&  c != '\0'  ;  c = *++p)
	{
		if (lineStarted  || ! isspace (c))  /* ignore leading spaces */
		{
			lineStarted = TRUE;
			if (isspace (c))
			{
				int next;

				/*  Consume repeating white space.
				 */
				while (next = *(p+1) , isspace (next)  &&  next != NEWLINE)
					++p;
				c = ' ';  /* force space character for any white space */
			}
			if (c != CRETURN  ||  *(p + 1) != NEWLINE)
			{
				putc (c, fp);
				++length;
			}
		}
	}
	return length;
}

static boolean isPosSet(fpos_t pos)
{
	char * p = (char *)&pos;
	boolean r = FALSE;
	unsigned int i;

	for (i = 0; i < sizeof(pos); i++)
		r |= p[i];
	return r;
}

static char *readSourceLineAnyway (vString *const vLine, const tagEntryInfo *const tag,
				   long *const pSeekValue)
{
	char * line;

	if (isPosSet (tag->filePosition) || (tag->pattern == NULL))
		line = 	readSourceLine (vLine, tag->filePosition, pSeekValue);
	else
		line = readSourceLineSlow (vLine, tag->lineNumber, tag->pattern, pSeekValue);

	return line;
}

static const char* escapeName (const tagEntryInfo * tag, fieldType ftype)
{
	fieldDesc *fdesc = getFieldDesc (ftype);
	return renderFieldEscaped (fdesc, tag);
}

static int writeXrefEntry (const tagEntryInfo *const tag)
{
	const char *line;
	int length;

	if (tag->isFileEntry)
		return 0;

	line = readSourceLineAnyway (TagFile.vLine, tag, NULL);

	if (Option.tagFileFormat == 1)
		length = fprintf (TagFile.fp, "%-16s %4lu %-16s ",
				  escapeName (tag, FIELD_NAME),
				  tag->lineNumber,
				  escapeName (tag, FIELD_SOURCE_FILE));
	else
		length = fprintf (TagFile.fp, "%-16s %-10s %4lu %-16s ",
				  escapeName (tag, FIELD_NAME),
				  tag->kind->name, tag->lineNumber,
				  escapeName (tag, FIELD_SOURCE_FILE));

	/* If no associated line for tag is found, we cannot prepare
	 * parameter to writeCompactSourceLine(). In this case we
	 * use an empty string as LINE.
	 */
	length += writeCompactSourceLine (TagFile.fp, line? line: "");
	putc (NEWLINE, TagFile.fp);
	++length;

	return length;
}

/*  Truncates the text line containing the tag at the character following the
 *  tag, providing a character which designates the end of the tag.
 */
static void truncateTagLine (
		char *const line, const char *const token, const boolean discardNewline)
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

static int writeEtagsEntry (const tagEntryInfo *const tag)
{
	int length;

	if (tag->isFileEntry)
		length = fprintf (TagFile.etags.fp, "\177%s\001%lu,0\n",
				tag->name, tag->lineNumber);
	else
	{
		long seekValue;
		char *const line =
				readSourceLineAnyway (TagFile.vLine, tag, &seekValue);
		if (line == NULL)
			return 0;

		if (tag->truncateLine)
			truncateTagLine (line, tag->name, TRUE);
		else
			line [strlen (line) - 1] = '\0';

		length = fprintf (TagFile.etags.fp, "%s\177%s\001%lu,%ld\n", line,
				tag->name, tag->lineNumber, seekValue);
	}
	TagFile.etags.byteCount += length;

	return length;
}

static char* getFullQualifiedScopeNameFromCorkQueue (const tagEntryInfo * inner_scope)
{

	const tagEntryInfo *scope = inner_scope;
	stringList *queue = stringListNew ();
	vString *v;
	vString *n;
	unsigned int c;

	while (scope)
	{
		if (!scope->placeholder)
		{
			v = vStringNewInit (escapeName (scope, FIELD_NAME));
			stringListAdd (queue, v);
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
		if (c != 1)
			vStringPut (n, '.');
	}
	stringListDelete (queue);

	return vStringDeleteUnwrap (n);
}

static int addExtensionFields (const tagEntryInfo *const tag)
{
	const char* const kindKey = getFieldDesc (FIELD_KIND_KEY)->enabled
		?getFieldDesc (FIELD_KIND_KEY)->name
		:"";
	const char* const kindFmt = getFieldDesc (FIELD_KIND_KEY)->enabled
		?"%s\t%s:%s"
		:"%s\t%s%s";
	const char* const scopeKey = getFieldDesc (FIELD_SCOPE_KEY)->enabled
		?getFieldDesc (FIELD_SCOPE_KEY)->name
		:"";
	const char* const scopeFmt = getFieldDesc (FIELD_SCOPE_KEY)->enabled
		?"%s\t%s:%s:%s"
		:"%s\t%s%s:%s";

	boolean first = TRUE;
	const char* separator = ";\"";
	const char* const empty = "";
	int length = 0;
/* "sep" returns a value only the first time it is evaluated */
#define sep (first ? (first = FALSE, separator) : empty)

	if (tag->kind->name != NULL && (getFieldDesc (FIELD_KIND_LONG)->enabled  ||
		 (getFieldDesc (FIELD_KIND)->enabled  && tag->kind == '\0')))
		length += fprintf (TagFile.fp, kindFmt, sep, kindKey, tag->kind->name);
	else if (tag->kind != '\0'  && (getFieldDesc (FIELD_KIND)->enabled ||
			(getFieldDesc (FIELD_KIND_LONG)->enabled &&  tag->kind->name == NULL)))
	{
		char str[2] = {tag->kind->letter, '\0'};
		length += fprintf (TagFile.fp, kindFmt, sep, kindKey, str);
	}

	if (getFieldDesc (FIELD_LINE_NUMBER)->enabled)
		length += fprintf (TagFile.fp, "%s\t%s:%ld", sep,
				   getFieldDesc (FIELD_LINE_NUMBER)->name,
				   tag->lineNumber);

	if (getFieldDesc (FIELD_LANGUAGE)->enabled  &&  tag->language != NULL)
		length += fprintf (TagFile.fp, "%s\t%s:%s", sep,
				   getFieldDesc (FIELD_LANGUAGE)->name,
				   tag->language);

	if (getFieldDesc (FIELD_SCOPE)->enabled)
	{
		if (tag->extensionFields.scopeKind != NULL  &&
		    tag->extensionFields.scopeName != NULL)
			length += fprintf (TagFile.fp, scopeFmt, sep,
					   scopeKey,
					   tag->extensionFields.scopeKind->name,
					   escapeName (tag, FIELD_SCOPE));
		else if (tag->extensionFields.scopeIndex != SCOPE_NIL
			 && TagFile.corkQueue.count > 0)
		{
			const tagEntryInfo * scope;
			char *full_qualified_scope_name;

			scope = getEntryInCorkQueue (tag->extensionFields.scopeIndex);
			full_qualified_scope_name = getFullQualifiedScopeNameFromCorkQueue(scope);
			Assert (full_qualified_scope_name);
			length += fprintf (TagFile.fp, scopeFmt, sep,
					   scopeKey,
					   scope->kind->name, full_qualified_scope_name);

			/* TODO: Make the value pointed by full_qualified_scope_name reusable. */
			eFree (full_qualified_scope_name);
		}
	}

	if (getFieldDesc (FIELD_TYPE_REF)->enabled &&
			tag->extensionFields.typeRef [0] != NULL  &&
			tag->extensionFields.typeRef [1] != NULL)
		length += fprintf (TagFile.fp, "%s\t%s:%s:%s", sep,
				   getFieldDesc (FIELD_TYPE_REF)->name,
				   tag->extensionFields.typeRef [0],
				   escapeName (tag, FIELD_TYPE_REF));

	if (getFieldDesc (FIELD_FILE_SCOPE)->enabled &&  tag->isFileScope)
		length += fprintf (TagFile.fp, "%s\t%s:", sep,
				   getFieldDesc (FIELD_FILE_SCOPE)->name);

	if (getFieldDesc (FIELD_INHERITANCE)->enabled &&
			tag->extensionFields.inheritance != NULL)
		length += fprintf (TagFile.fp, "%s\t%s:%s", sep,
				   getFieldDesc (FIELD_INHERITANCE)->name,
				   escapeName (tag, FIELD_INHERITANCE));

	if (getFieldDesc (FIELD_ACCESS)->enabled &&  tag->extensionFields.access != NULL)
		length += fprintf (TagFile.fp, "%s\t%s:%s", sep,
				   getFieldDesc (FIELD_ACCESS)->name,
				   tag->extensionFields.access);

	if (getFieldDesc (FIELD_IMPLEMENTATION)->enabled &&
			tag->extensionFields.implementation != NULL)
		length += fprintf (TagFile.fp, "%s\t%s:%s", sep,
				   getFieldDesc (FIELD_IMPLEMENTATION)->name,
				   tag->extensionFields.implementation);

	if (getFieldDesc (FIELD_SIGNATURE)->enabled &&
			tag->extensionFields.signature != NULL)
		length += fprintf (TagFile.fp, "%s\t%s:%s", sep,
				   getFieldDesc (FIELD_SIGNATURE)->name,
				   escapeName (tag, FIELD_SIGNATURE));

	return length;
#undef sep
}

static char* makePatternString (const tagEntryInfo *const tag)
{
	char *const line = readSourceLine (TagFile.vLine, tag->filePosition, NULL);
	const int searchChar = Option.backward ? '?' : '/';
	boolean newlineTerminated;
	vString* pattern;

	pattern = vStringNew ();
	if (line == NULL)
		error (FATAL, "bad tag in %s", vStringValue (File.name));
	if (tag->truncateLine)
		truncateTagLine (line, tag->name, FALSE);
	newlineTerminated = (boolean) (line [strlen (line) - 1] == '\n');

	vStringPut (pattern, searchChar);
	vStringPut (pattern, '^');
	appendSourceLine (vstring_putc, line, pattern);
	vStringCatS (pattern, newlineTerminated ? "$":"");
	vStringPut (pattern, searchChar);

	return vStringDeleteUnwrap (pattern);
}

static int writePatternEntry (const tagEntryInfo *const tag)
{
	char *const line = readSourceLine (TagFile.vLine, tag->filePosition, NULL);
	const int searchChar = Option.backward ? '?' : '/';
	boolean newlineTerminated;
	int length = 0;

	if (line == NULL)
		error (FATAL, "bad tag in %s", vStringValue (File.name));
	if (tag->truncateLine)
		truncateTagLine (line, tag->name, FALSE);
	newlineTerminated = (boolean) (line [strlen (line) - 1] == '\n');

	length += fprintf (TagFile.fp, "%c^", searchChar);
	length += writeSourceLine (TagFile.fp, line);
	length += fprintf (TagFile.fp, "%s%c", newlineTerminated ? "$":"", searchChar);

	return length;
}

static int writeLineNumberEntry (const tagEntryInfo *const tag)
{
	return fprintf (TagFile.fp, "%lu", tag->lineNumber);
}

static int writeCtagsEntry (const tagEntryInfo *const tag)
{
	int length = fprintf (TagFile.fp, "%s\t%s\t",
			      escapeName (tag, FIELD_NAME),
			      escapeName (tag, FIELD_SOURCE_FILE));

	if (tag->lineNumberEntry)
		length += writeLineNumberEntry (tag);
	else if (tag->pattern)
		length += fprintf(TagFile.fp, "%s", tag->pattern);
	else
		length += writePatternEntry (tag);

	if (includeExtensionFlags ())
		length += addExtensionFields (tag);

	length += fprintf (TagFile.fp, "\n");

	return length;
}

static void recordTagEntryInQueue (const tagEntryInfo *const tag, tagEntryInfo* slot)
{
	*slot = *tag;

	if (slot->pattern)
		slot->pattern = eStrdup (slot->pattern);
	else if (!slot->lineNumberEntry)
		slot->pattern = makePatternString (slot);

	slot->sourceFileName = eStrdup (slot->sourceFileName);
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
}

static void clearTagEntryInQueue (tagEntryInfo* slot)
{
	if (slot->pattern)
		eFree ((char *)slot->pattern);
	eFree ((char *)slot->sourceFileName);
	eFree ((char *)slot->name);

	if (slot->extensionFields.access)
		eFree ((char *)slot->extensionFields.access);
	if (slot->extensionFields.fileScope)
		eFree ((char *)slot->extensionFields.fileScope);
	if (slot->extensionFields.implementation)
		eFree ((char *)slot->extensionFields.implementation);
	if (slot->extensionFields.inheritance)
		eFree ((char *)slot->extensionFields.inheritance);
	if (slot->extensionFields.scopeKind)
		slot->extensionFields.scopeKind = NULL;
	if (slot->extensionFields.scopeName)
		eFree ((char *)slot->extensionFields.scopeName);
	if (slot->extensionFields.signature)
		eFree ((char *)slot->extensionFields.signature);
	if (slot->extensionFields.typeRef[0])
		eFree ((char *)slot->extensionFields.typeRef[0]);
	if (slot->extensionFields.typeRef[1])
		eFree ((char *)slot->extensionFields.typeRef[1]);
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

static void writeTagEntry (const tagEntryInfo *const tag)
{
	int length = 0;

	if (tag->placeholder)
		return;

	DebugStatement ( debugEntry (tag); )
	if (Option.xref)
		length = writeXrefEntry (tag);
	else if (Option.etags)
		length = writeEtagsEntry (tag);
	else
		length = writeCtagsEntry (tag);

	++TagFile.numTags.added;
	rememberMaxLengths (strlen (tag->name), (size_t) length);
	DebugStatement ( fflush (TagFile.fp); )

	abort_if_ferror (TagFile.fp);
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
		writeTagEntry (TagFile.corkQueue.queue + i);
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
	if ((SCOPE_NIL < n) && (n < TagFile.corkQueue.count))
		return TagFile.corkQueue.queue + n;
	else
		return NULL;
}

extern size_t        countEntryInCorkQueue (void)
{
	return TagFile.corkQueue.count;
}

extern int makeTagEntry (const tagEntryInfo *const tag)
{
	int r = SCOPE_NIL;
	Assert (tag->name != NULL);
	Assert (getSourceLanguageFileKind() == tag->kind || isSourceLanguageKindEnabled (tag->kind->letter));

	if (tag->name [0] == '\0' && (!tag->placeholder))
	{
		if (!getSourceLanguageAllowNullTag())
			error (WARNING, "ignoring null tag in %s(line: %lu)",
			       vStringValue (File.name), tag->lineNumber);
		goto out;
	}

	if (TagFile.cork)
		r = queueTagEntry (tag);
	else
		writeTagEntry (tag);
out:
	return r;
}

extern void initTagEntry (tagEntryInfo *const e, const char *const name,
			  const kindOption *kind)
{
	initTagEntryFull(e, name,
			 getSourceLineNumber (),
			 getSourceLanguageName (),
			 getInputFilePosition (),
			 getSourceFileTagPath (),
			 kind);
}

extern void initTagEntryFull (tagEntryInfo *const e, const char *const name,
			      unsigned long lineNumber,
			      const char* language,
			      fpos_t      filePosition,
			      const char *sourceFileName,
			      const kindOption *kind)
{
	Assert (File.source.name != NULL);

	memset (e, 0, sizeof (tagEntryInfo));
	e->lineNumberEntry = (boolean) (Option.locate == EX_LINENUM);
	e->lineNumber      = lineNumber;
	e->language        = language;
	e->filePosition    = filePosition;
	e->sourceFileName  = sourceFileName;
	e->name            = name;
	e->extensionFields.scopeIndex     = SCOPE_NIL;
	e->kind = kind;
}

/* vi:set tabstop=4 shiftwidth=4: */
