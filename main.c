/*
*   $Id$
*
*   Copyright (c) 1996-2001, Darren Hiebert
*
*   Author: Darren Hiebert <dhiebert@users.sourceforge.net>
*           http://ctags.sourceforge.net
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License. It is provided on an as-is basis and no
*   responsibility is accepted for its failure to perform as expected.
*
*   This is a reimplementation of the ctags (1) program. It is an attempt to
*   provide a fully featured ctags program which is free of the limitations
*   which most (all?) others are subject to.
*
*   This module contains top level start-up and portability functions.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"	/* must always come first */

#ifdef HAVE_STDLIB_H
# include <stdlib.h>		/* to declare malloc (), realloc () */
#endif
#include <string.h>
#include <errno.h>
#include <ctype.h>

#include <stdio.h>		/* to declare SEEK_SET (hopefully) */
#ifdef HAVE_FCNTL_H
# include <fcntl.h>		/* to declar O_RDWR, O_CREAT, O_EXCL */
#endif
#ifdef HAVE_UNISTD_H
# include <unistd.h>		/* to declare mkstemp () */
#endif

#ifdef AMIGA
# include <dos/dosasl.h>	/* for struct AnchorPath */
# include <clib/dos_protos.h>	/* function prototypes */
# define ANCHOR_BUF_SIZE 512
# define ANCHOR_SIZE (sizeof (struct AnchorPath) + ANCHOR_BUF_SIZE)
# ifdef __SASC
   extern struct DosLibrary *DOSBase;
#  include <pragmas/dos_pragmas.h>
# endif
#endif

#include <stdarg.h>

/*  To declare "struct stat" and stat ().
 */
#if defined (HAVE_SYS_TYPES_H)
# include <sys/types.h>
#else
# if defined (HAVE_TYPES_H)
#  include <types.h>
# endif
#endif
#ifdef HAVE_SYS_STAT_H
# include <sys/stat.h>
#else
# ifdef HAVE_STAT_H
#  include <stat.h>
# endif
#endif

/*  To provide directory searching for recursion feature.
 */
#ifdef HAVE_DIRENT_H
# ifdef __BORLANDC__
#  define boolean BORLAND_boolean
# endif
# include <dirent.h>
# undef boolean
#endif
#ifdef HAVE_DIRECT_H
# include <direct.h>	/* to _getcwd */
#endif
#ifdef HAVE_DOS_H
# include <dos.h>	/* to declare FA_DIREC */
#endif
#ifdef HAVE_DIR_H
# include <dir.h>	/* to declare findfirst () and findnext () */
#endif
#ifdef HAVE_IO_H
# include <io.h>	/* to declare _finddata_t in MSVC++ 4.x */
#endif

/*  To provide timings features if available.
 */
#ifdef HAVE_CLOCK
# ifdef HAVE_TIME_H
#  include <time.h>
# endif
#else
# ifdef HAVE_TIMES
#  ifdef HAVE_SYS_TIMES_H
#   include <sys/times.h>
#  endif
# endif
#endif

#include "debug.h"
#include "entry.h"
#include "keyword.h"
#include "main.h"
#include "options.h"
#include "parse.h"
#include "read.h"
#include "vstring.h"

#ifdef TRAP_MEMORY_CALLS
# include "safe_malloc.h"
#endif

/*
*   MACROS
*/

/*
 *  Miscellaneous macros
 */
#define selected(var,feature)	(((int)(var) & (int)(feature)) == (int)feature)
#define plural(value)		(((unsigned long)(value) == 1L) ? "" : "s")

/*
 *  Portability macros
 */
#ifndef PATH_SEPARATOR
# if defined (MSDOS_STYLE_PATH)
#  define PATH_SEPARATOR '\\'
# elif defined (QDOS)
#  define PATH_SEPARATOR '_'
# else
#  define PATH_SEPARATOR '/'
# endif
#endif

#if defined (MSDOS_STYLE_PATH) && defined (UNIX_PATH_SEPARATOR)
# define OUTPUT_PATH_SEPARATOR	'/'
#else
# define OUTPUT_PATH_SEPARATOR	PATH_SEPARATOR
#endif

/*  File type tests.
 */
#ifndef S_ISREG
# if defined (S_IFREG) && ! defined (AMIGA)
#  define S_ISREG(mode)	    ((mode) & S_IFREG)
# else
#  define S_ISREG(mode)	    TRUE	/* assume regular file */
# endif
#endif

#ifndef S_ISLNK
# ifdef S_IFLNK
#  define S_ISLNK(mode)	    (((mode) & S_IFMT) == S_IFLNK)
# else
#  define S_ISLNK(mode)	    FALSE	/* assume no soft links */
# endif
#endif

#ifndef S_ISDIR
# ifdef S_IFDIR
#  define S_ISDIR(mode)	    (((mode) & S_IFMT) == S_IFDIR)
# else
#  define S_ISDIR(mode)	    FALSE	/* assume no soft links */
# endif
#endif

#ifndef S_IXUSR
# define S_IXUSR 0
#endif
#ifndef S_IXGRP
# define S_IXGRP 0
#endif
#ifndef S_IXOTH
# define S_IXOTH 0
#endif

#ifndef S_IRUSR
# define S_IRUSR 0400
#endif
#ifndef S_IWUSR
# define S_IWUSR 0200
#endif

/*  Hack for rediculous practice of Microsoft Visual C++.
 */
#if defined (WIN32)
# if defined (_MSC_VER)
#  define stat    _stat
#  define getcwd  _getcwd
#  define PATH_MAX  _MAX_PATH
# elif defined (__BORLANDC__)
#  define PATH_MAX  MAXPATH
# endif
#endif

#ifndef PATH_MAX
# define PATH_MAX 256
#endif

/*
*   DATA DEFINITIONS
*/
#if defined (MSDOS_STYLE_PATH)
static const char PathDelimiters [] = ":/\\";
#elif defined (VMS)
static const char PathDelimiters [] = ":]>";
#endif

#ifndef TMPDIR
# define TMPDIR "/tmp"
#endif

char *CurrentDirectory = NULL;
static const char *ExecutableProgram = NULL;
static const char *ExecutableName = NULL;
static stringList* Excluded = NULL;

static struct { long files, lines, bytes; } Totals = { 0, 0, 0 };

#ifdef AMIGA
# include "ctags.h"
  static const char *VERsion = "$VER: "PROGRAM_NAME" "PROGRAM_VERSION" "
# ifdef __SASC
  __AMIGADATE__
# else
  __DATE__
# endif
  " "AUTHOR_NAME" $";
#endif

/*
*   FUNCTION PROTOTYPES
*/
#ifdef NEED_PROTO_STAT
extern int stat (const char *, struct stat *);
#endif
#ifdef NEED_PROTO_LSTAT
extern int lstat (const char *, struct stat *);
#endif

static boolean createTagsForEntry (const char *const entryName);

/*
*   FUNCTION DEFINITIONS
*/

extern const char *getExecutableName (void)
{
    return ExecutableName;
}

static void setCurrentDirectory (void)
{
#ifdef AMIGA
    char* const cwd = eStrdup (".");
#else
    char* const cwd = getcwd (NULL, PATH_MAX);
#endif
    CurrentDirectory = xMalloc (strlen (cwd) + 2, char);
    if (cwd [strlen (cwd) - (size_t) 1] == PATH_SEPARATOR)
	strcpy (CurrentDirectory, cwd);
    else
	sprintf (CurrentDirectory, "%s%c", cwd, OUTPUT_PATH_SEPARATOR);
    free (cwd);
}

extern void error (const errorSelection selection,
		   const char *const format, ...)
{
    va_list ap;

    va_start (ap, format);
    fprintf (errout, "%s: %s", getExecutableName (),
	    selected (selection, WARNING) ? "Warning: " : "");
    vfprintf (errout, format, ap);
    if (selected (selection, PERROR))
#ifdef HAVE_STRERROR
	fprintf (errout, " : %s", strerror (errno));
#else
	perror (" ");
#endif
    fputs ("\n", errout);
    va_end (ap);
    if (selected (selection, FATAL))
	exit (1);
}

#ifndef HAVE_STRICMP
extern int stricmp (const char *s1, const char *s2)
{
    int result;
    do
    {
	result = toupper ((int) *s1) - toupper ((int) *s2);
    } while (result == 0  &&  *s1++ != '\0'  &&  *s2++ != '\0');
    return result;
}
#endif

#ifndef HAVE_STRNICMP
extern int strnicmp (const char *s1, const char *s2, size_t n)
{
    int result;
    do
    {
	result = toupper ((int) *s1) - toupper ((int) *s2);
    } while (result == 0  &&  --n > 0  &&  *s1++ != '\0'  &&  *s2++ != '\0');
    return result;
}
#endif

#ifndef HAVE_STRSTR
extern char* strstr (const char *str, const char *substr)
{
    const size_t length = strlen (substr);
    const char *match = NULL;
    const char *p;

    for (p = str  ;  *p != '\0'  &&  match == NULL  ;  ++p)
	if (strncmp (p, substr, length) == 0)
	    match = p;
    return (char*) match;
}
#endif

extern char* eStrdup (const char* str)
{
    char* result = xMalloc (strlen (str) + 1, char);
    strcpy (result, str);
    return result;
}

extern void *eMalloc (const size_t size)
{
    void *buffer = malloc (size);

    if (buffer == NULL)
	error (FATAL, "out of memory");

    return buffer;
}

extern void *eCalloc (const size_t count, const size_t size)
{
    void *buffer = calloc (count, size);

    if (buffer == NULL)
	error (FATAL, "out of memory");

    return buffer;
}

extern void *eRealloc (void *const ptr, const size_t size)
{
    void *buffer;
    if (ptr == NULL)
	buffer = eMalloc (size);
    else
    {
	buffer = realloc (ptr, size);
	if (buffer == NULL)
	    error (FATAL, "out of memory");
    }
    return buffer;
}

extern void eFree (void *const ptr)
{
    Assert (ptr != NULL);
    free (ptr);
}

extern void toLowerString (char* str)
{
    while (*str != '\0')
    {
	*str = tolower ((int) *str);
	++str;
    }
}

extern void toUpperString (char* str)
{
    while (*str != '\0')
    {
	*str = toupper ((int) *str);
	++str;
    }
}

/*  Newly allocated string containing lower case conversion of a string.
 */
extern char* newLowerString (const char* str)
{
    char* const result = xMalloc (strlen (str) + 1, char);
    int i = 0;
    do
	result [i] = tolower ((int) str [i]);
    while (str [i++] != '\0');
    return result;
}

/*  Newly allocated string containing upper case conversion of a string.
 */
extern char* newUpperString (const char* str)
{
    char* const result = xMalloc (strlen (str) + 1, char);
    int i = 0;
    do
	result [i] = toupper ((int) str [i]);
    while (str [i++] != '\0');
    return result;
}

extern long unsigned int getFileSize (const char *const name)
{
    struct stat fileStatus;
    unsigned long size = 0;

    if (stat (name, &fileStatus) == 0)
	size = fileStatus.st_size;

    return size;
}

static boolean isSymbolicLink (const char *const name)
{
#if defined (MSDOS) || defined (WIN32) || defined (VMS) || defined (__EMX__) || defined (AMIGA)
    return FALSE;
#else
    struct stat fileStatus;
    boolean result = FALSE;

    if (lstat (name, &fileStatus) == 0)
	result = (boolean) (S_ISLNK (fileStatus.st_mode));

    return result;
#endif
}

static boolean isNormalFile (const char *const name)
{
    struct stat fileStatus;
    boolean result = FALSE;

    if (stat (name, &fileStatus) == 0)
	result = (boolean) (S_ISREG (fileStatus.st_mode));

    return result;
}

extern boolean isExecutable (const char *const name)
{
    struct stat fileStatus;
    boolean result = FALSE;

    if (stat (name, &fileStatus) == 0)
	result = (boolean) ((fileStatus.st_mode & (S_IXUSR|S_IXGRP|S_IXOTH)) != 0);

    return result;
}

extern boolean isSameFile (const char *const name1, const char *const name2)
{
    boolean result = FALSE;
#if defined (HAVE_STAT_ST_INO)
    struct stat stat1, stat2;

    if (stat (name1, &stat1) == 0  &&  stat (name2, &stat2) == 0)
	result = (boolean) (stat1.st_ino == stat2.st_ino);
#elif defined (CASE_INSENSITIVE_FILENAMES)
    result = (boolean) (stricmp (name1, name2) == 0);
#else
    result = (boolean) (strcmp (name1, name2) == 0);
#endif
    return result;
}

#ifdef HAVE_MKSTEMP

static boolean isSetUID (const char *const name)
{
#if defined (VMS) || defined (MSDOS) || defined (WIN32) || defined (__EMX__) || defined (AMIGA)
    return FALSE;
#else
    struct stat fileStatus;
    boolean result = FALSE;

    if (stat (name, &fileStatus) == 0)
	result = (boolean) ((fileStatus.st_mode & S_ISUID) != 0);

    return result;
#endif
}

#endif

static boolean isDirectory (const char *const name)
{
    boolean result = FALSE;
#ifdef AMIGA
    struct FileInfoBlock *const fib = xMalloc (1, struct FileInfoBlock);

    if (fib != NULL)
    {
	const BPTR flock = Lock ((UBYTE *) name, (long) ACCESS_READ);

	if (flock != (BPTR) NULL)
	{
	    if (Examine (flock, fib))
		result = ((fib->fib_DirEntryType >= 0) ? TRUE : FALSE);
	    UnLock (flock);
	}
	eFree (fib);
    }
#else
    struct stat fileStatus;

    if (stat (name, &fileStatus) == 0)
	result = (boolean) S_ISDIR (fileStatus.st_mode);
#endif
    return result;
}

extern boolean doesFileExist (const char *const fileName)
{
    struct stat fileStatus;

    return (boolean) (stat (fileName, &fileStatus) == 0);
}

#ifndef HAVE_FGETPOS

extern int fgetpos (FILE *stream, fpos_t *pos)
{
    int result = 0;

    *pos = ftell (stream);
    if (*pos == -1L)
	result = -1;

    return result;
}

extern int fsetpos (FILE *stream, fpos_t const *pos)
{
    return fseek (stream, *pos, SEEK_SET);
}

#endif

extern void addTotals (const unsigned int files,
		       const long unsigned int lines,
		       const long unsigned int bytes)
{
    Totals.files += files;
    Totals.lines += lines;
    Totals.bytes += bytes;
}

extern boolean isDestinationStdout (void)
{
    boolean toStdout = FALSE;

    if (Option.xref  ||  Option.filter  ||
	(Option.tagFileName != NULL  &&  (strcmp (Option.tagFileName, "-") == 0
#if defined (VMS)
    || strcmp (Option.tagFileName, "sys$output") == 0
#else
    || strcmp (Option.tagFileName, "/dev/stdout") == 0
#endif
	)))
	toStdout = TRUE;
    return toStdout;
}

extern FILE *tempFile (const char *const mode, char **const pName)
{
    char *name;
    FILE *fp;
    int fd;
#ifdef HAVE_MKSTEMP
    const char *const pattern = "tags.XXXXXX";
    const char *tmpdir = NULL;
    if (! isSetUID (ExecutableProgram))
	tmpdir = getenv ("TMPDIR");
    if (tmpdir == NULL)
	tmpdir = TMPDIR;
    name = xMalloc (strlen (tmpdir) + 1 + strlen (pattern) + 1, char);
    sprintf (name, "%s%c%s", tmpdir, OUTPUT_PATH_SEPARATOR, pattern);
    fd = mkstemp (name);
#else
    name = xMalloc (L_tmpnam, char);
    if (tmpnam (name) != name)
	error (FATAL | PERROR, "cannot assign temporary file name");
    fd = open (name, O_RDWR | O_CREAT | O_EXCL, S_IRUSR | S_IWUSR);
#endif
    if (fd == -1)
	error (FATAL | PERROR, "cannot open temporary file");
    fp = fdopen (fd, mode);
    if (fp == NULL)
	error (FATAL | PERROR, "cannot open temporary file");
    DebugStatement (
	debugPrintf (DEBUG_STATUS, "opened temporary file %s\n", name); )
    Assert (*pName == NULL);
    *pName = name;
    return fp;
}

/*
 *  Pathname manipulation (O/S dependent!!!)
 */

extern const char *baseFilename (const char *const filePath)
{
#if defined (MSDOS_STYLE_PATH) || defined (VMS)
    const char *tail = NULL;
    unsigned int i;

    /*  Find whichever of the path delimiters is last.
     */
    for (i = 0  ;  i < strlen (PathDelimiters)  ;  ++i)
    {
	const char *sep = strrchr (filePath, PathDelimiters [i]);

	if (sep > tail)
	    tail = sep;
    }
#else
    const char *tail = strrchr (filePath, PATH_SEPARATOR);
#endif
    if (tail == NULL)
	tail = filePath;
    else
	++tail;			/* step past last delimiter */
#ifdef VAXC
    {
	/* remove version number from filename */
	char *p = strrchr ((char *) tail, ';');
	if (p != NULL)
	    *p = '\0';
    }
#endif

    return tail;
}

extern boolean isAbsolutePath (const char *const path)
{
    boolean result = FALSE;
#if defined (MSDOS_STYLE_PATH)
    if (strchr (PathDelimiters, path [0]) != NULL)
	result = TRUE;
    else if (isalpha (path [0])  &&  path [1] == ':')
    {
	if (strchr (PathDelimiters, path [2]) != NULL)
	    result = TRUE;
	else
	    /*  We don't support non-absolute file names with a drive
	     *  letter, like `d:NAME' (it's too much hassle).
	     */
	    error (FATAL,
		"%s: relative file names with drive letters not supported",
		path);
    }
#elif defined (VMS)
    result = (boolean) (strchr (path, ':') != NULL);
#else
    result = (boolean) (path [0] == PATH_SEPARATOR);
#endif
    return result;
}

/* Return a newly-allocated string whose contents concatenate those of
 * s1, s2, s3.
 * Routine adapted from Gnu etags.
 */
static char* concat (const char *s1, const char *s2, const char *s3)
{
  int len1 = strlen (s1), len2 = strlen (s2), len3 = strlen (s3);
  char *result = xMalloc (len1 + len2 + len3 + 1, char);

  strcpy (result, s1);
  strcpy (result + len1, s2);
  strcpy (result + len1 + len2, s3);
  result [len1 + len2 + len3] = '\0';

  return result;
}

/* Return a newly allocated string containing the absolute file name of FILE
 * given CWD (which should end with a slash).
 * Routine adapted from Gnu etags.
 */
static char* absoluteFilename (const char *file)
{
    char *slashp, *cp;
    char *res = NULL;

    if (isAbsolutePath (file))
	res = eStrdup (file);
    else
	res = concat (CurrentDirectory, file, "");

    /* Delete the "/dirname/.." and "/." substrings. */
    slashp = strchr (res, '/');
    while (slashp != NULL  &&  slashp [0] != '\0')
    {
	if (slashp[1] == '.')
	{
	    if (slashp [2] == '.' && (slashp [3] == '/' || slashp [3] == '\0'))
	    {
		cp = slashp;
		do
		    cp--;
		while (cp >= res  &&  ! isAbsolutePath (cp));
		if (cp < res)
		    cp = slashp;/* the absolute name begins with "/.." */
#ifdef MSDOS_STYLE_PATH
		/* Under MSDOS and NT we get `d:/NAME' as absolute file name,
		 * so the luser could say `d:/../NAME'. We silently treat this
		 * as `d:/NAME'.
		 */
		else if (cp [0] != '/')
		    cp = slashp;
#endif
		strcpy (cp, slashp + 3);
		slashp = cp;
		continue;
	    }
	    else if (slashp [2] == '/'  ||  slashp [2] == '\0')
	    {
		strcpy (slashp, slashp + 2);
		continue;
	    }
	}
	slashp = strchr (slashp + 1, '/');
    }

    if (res [0] == '\0')
	return eStrdup ("/");
    else
    {
#ifdef MSDOS_STYLE_PATH
	/* Canonicalize drive letter case.  */
	if (res [1] == ':'  &&  islower (res [0]))
	    res [0] = toupper (res [0]);
#endif

	return res;
    }
}

/* Return a newly allocated string containing the absolute file name of dir
 * where FILE resides given CWD (which should end with a slash).
 * Routine adapted from Gnu etags.
 */
extern char* absoluteDirname (char *file)
{
    char *slashp, *res;
    char save;
#ifdef MSDOS_STYLE_PATH
    char *p;
    for (p = file  ;  *p != '\0'  ;  p++)
	if (*p == '\\')
	    *p = '/';
#endif
    slashp = strrchr (file, '/');
    if (slashp == NULL)
	res = eStrdup (CurrentDirectory);
    else
    {
	save = slashp [1];
	slashp [1] = '\0';
	res = absoluteFilename (file);
	slashp [1] = save;
    }
    return res;
}

/* Return a newly allocated string containing the file name of FILE relative
 * to the absolute directory DIR (which should end with a slash).
 * Routine adapted from Gnu etags.
 */
extern char* relativeFilename (const char *file, const char *dir)
{
    const char *fp, *dp;
    char *absdir, *res;
    int i;

    /* Find the common root of file and dir (with a trailing slash). */
    absdir = absoluteFilename (file);
    fp = absdir;
    dp = dir;
    while (*fp++ == *dp++)
	continue;
    fp--;
    dp--;			/* back to the first differing char */
    do
    {				/* look at the equal chars until '/' */
	if (fp == absdir)
	    return absdir;	/* first char differs, give up */
	fp--;
	dp--;
    } while (*fp != '/');

    /* Build a sequence of "../" strings for the resulting relative file name.
     */
    i = 0;
    while ((dp = strchr (dp + 1, '/')) != NULL)
	i += 1;
    res = xMalloc (3 * i + strlen (fp + 1) + 1, char);
    res [0] = '\0';
    while (i-- > 0)
	strcat (res, "../");

    /* Add the file name relative to the common root of file and dir. */
    strcat (res, fp + 1);
    free (absdir);

    return res;
}

extern vString *combinePathAndFile (const char *const path,
				    const char *const file)
{
    vString *const filePath = vStringNew ();
#ifdef VMS
    const char *const directoryId = strstr (file, ".DIR;1");

    if (directoryId == NULL)
    {
	const char *const versionId = strchr (file, ';');

	vStringCopyS (filePath, path);
	if (versionId == NULL)
	    vStringCatS (filePath, file);
	else
	    vStringNCatS (filePath, file, versionId - file);
	vStringCopyToLower (filePath, filePath);
    }
    else
    {
	/*  File really is a directory; append it to the path.
	 *  Gotcha: doesn't work with logical names.
	 */
	vStringNCopyS (filePath, path, strlen (path) - 1);
	vStringPut (filePath, '.');
	vStringNCatS (filePath, file, directoryId - file);
	if (strchr (path, '[') != NULL)
	    vStringPut (filePath, ']');
	else
	    vStringPut (filePath, '>');
	vStringTerminate (filePath);
    }
#else
    const int lastChar = path [strlen (path) - 1];
# ifdef MSDOS_STYLE_PATH
    boolean terminated = (boolean) (strchr (PathDelimiters, lastChar) != NULL);
# else
    boolean terminated = (boolean) (lastChar == PATH_SEPARATOR);
# endif

    vStringCopyS (filePath, path);
    if (! terminated)
    {
	vStringPut (filePath, OUTPUT_PATH_SEPARATOR);
	vStringTerminate (filePath);
    }
    vStringCatS (filePath, file);
#endif

    return filePath;
}

/*
 *	Create tags
 */

extern void processExcludeOption (const char *const __unused__ option,
				  const char *const parameter)
{
    if (parameter [0] == '\0')
	freeList (&Excluded);
    else if (parameter [0] == '@')
    {
	stringList* const sl = stringListNewFromFile (parameter + 1);
	if (Excluded == NULL)
	    Excluded = sl;
	else
	    stringListCombine (Excluded, sl);
	verbose ("    adding exclude patterns from %s\n", parameter + 1);
    }
    else
    {
	vString *const item = vStringNewInit (parameter);
	if (Excluded == NULL)
	    Excluded = stringListNew ();
	stringListAdd (Excluded, item);
	verbose ("    adding exclude pattern: %s\n", parameter);
    }
}

static boolean excludedFile (const char* const name)
{
    const char* base = baseFilename (name);
    boolean result = FALSE;
    if (Excluded != NULL)
    {
	result = stringListFileMatched (Excluded, base);
	if (! result  &&  name != base)
	    result = stringListFileMatched (Excluded, name);
    }
#ifdef AMIGA
    /* not a good solution, but the only one which works often */
    if (! result)
	result = (boolean) (strcmp (name, TagFile.name) == 0);
#endif
    return result;
}

# if defined (MSDOS) || defined (WIN32)

static boolean createTagsForMatchingEntries (char *const pattern)
{
    boolean resize = FALSE;
    const size_t dirLength = baseFilename (pattern) - (char *) pattern;
    vString *const filePath = vStringNew ();
#if defined (HAVE_FINDFIRST)
    struct ffblk fileInfo;
    int result = findfirst (pattern, &fileInfo, FA_DIREC);

    while (result == 0)
    {
	const char *const entryName = fileInfo.ff_name;

	/*  We must not recurse into the directories "." or "..".
	 */
	if (strcmp (entryName, ".") != 0  &&  strcmp (entryName, "..") != 0)
	{
	    vStringNCopyS (filePath, pattern, dirLength);
	    vStringCatS (filePath, entryName);
	    resize |= createTagsForEntry (vStringValue (filePath));
	}
	result = findnext (&fileInfo);
    }
#elif defined (HAVE__FINDFIRST)
    struct _finddata_t fileInfo;
    long hFile = _findfirst (pattern, &fileInfo);

    if (hFile != -1L)
    {
	do
	{
	    const char *const entryName = fileInfo.name;

	    /*  We must not recurse into the directories "." or "..".
	     */
	    if (strcmp (entryName, ".") != 0  &&  strcmp (entryName, "..") != 0)
	    {
		vStringNCopyS (filePath, pattern, dirLength);
		vStringCatS (filePath, entryName);
		resize |= createTagsForEntry (vStringValue (filePath));
	    }
	} while (_findnext (hFile, &fileInfo) == 0);
	_findclose (hFile);
    }
#endif

    vStringDelete (filePath);
    return resize;
}

#elif defined (AMIGA)

static boolean createTagsForMatchingEntries (char *const pattern)
{
    boolean resize = FALSE;
    struct AnchorPath *const anchor =
			(struct AnchorPath *) eMalloc ((size_t) ANCHOR_SIZE);

    if (anchor != NULL)
    {
	LONG result;

	memset (anchor, 0, (size_t) ANCHOR_SIZE);
	anchor->ap_Strlen = ANCHOR_BUF_SIZE; /* ap_Length no longer supported */

	/*  Allow '.' for current directory.
	 */
#ifdef APF_DODOT
	anchor->ap_Flags = APF_DODOT | APF_DOWILD;
#else
	anchor->ap_Flags = APF_DoDot | APF_DoWild;
#endif

	result = MatchFirst ((UBYTE *) pattern, anchor);
	while (result == 0)
	{
	    resize |= createTagsForEntry ((char *) anchor->ap_Buf);
	    result = MatchNext (anchor);
	}
	MatchEnd (anchor);
	eFree (anchor);
    }
    return resize;
}

#endif

static boolean isRecursiveLink (const char* const dirName)
{
    boolean result = FALSE;
    char* const path = absoluteFilename (dirName);
    while (path [strlen (path) - 1] == PATH_SEPARATOR)
	path [strlen (path) - 1] = '\0';
    while (! result  &&  strlen (path) > (size_t) 1)
    {
	char *const separator = strrchr (path, PATH_SEPARATOR);
	if (separator == NULL)
	    break;
	else if (separator == path)	/* backed up to root directory */
	    *(separator + 1) = '\0';
	else
	    *separator = '\0';
	result = isSameFile (path, dirName);
    }
    eFree (path);
    return result;
}

static boolean recurseIntoDirectory (const char *const dirName)
{
    boolean resize = FALSE;
    if (isRecursiveLink (dirName))
	verbose ("ignoring \"%s\" (recursive link)\n", dirName);
    else if (! Option.recurse)
	verbose ("ignoring \"%s\" (directory)\n", dirName);
    else
    {
#if defined (HAVE_OPENDIR)
	DIR *const dir = opendir (dirName);
	if (dir == NULL)
	    error (WARNING | PERROR, "cannot recurse into directory \"%s\"",
		   dirName);
	else
	{
	    struct dirent *entry;
	    verbose ("RECURSING into directory \"%s\"\n", dirName);
	    while ((entry = readdir (dir)) != NULL)
	    {
		if (strcmp (entry->d_name, ".") != 0  &&
		    strcmp (entry->d_name, "..") != 0)
		{
		    vString *filePath;
		    if (strcmp (dirName, ".") == 0)
			filePath = vStringNewInit (entry->d_name);
		    else
			filePath = combinePathAndFile (dirName, entry->d_name);
		    resize |= createTagsForEntry (vStringValue (filePath));
		    vStringDelete (filePath);
		}
	    }
	    closedir (dir);
	}
#elif defined (AMIGA) || defined (MSDOS) || defined (WIN32)
	vString *const pattern = vStringNew ();
	verbose ("RECURSING into directory \"%s\"\n", dirName);
# ifdef AMIGA
	if (*dirName != '\0'  &&  strcmp (dirName, ".") != 0)
	{
	    vStringCopyS (pattern, dirName);
	    if (dirName [strlen (dirName) - 1] != '/')
		vStringPut (pattern, '/');
	}
	vStringCatS (pattern, "#?");
# else
	vStringCopyS (pattern, dirName);
	vStringPut (pattern, OUTPUT_PATH_SEPARATOR);
	vStringCatS (pattern, "*.*");
# endif
	resize = createTagsForMatchingEntries (vStringValue (pattern));
	vStringDelete (pattern);
#endif	/* HAVE_OPENDIR */
    }
    return resize;
}

static boolean createTagsForEntry (const char *const entryName)
{
    boolean resize = FALSE;

    Assert (entryName != NULL);
    if (excludedFile (entryName))
	verbose ("excluding \"%s\"\n", entryName);
    else if (isSymbolicLink (entryName)  &&  ! Option.followLinks)
	verbose ("ignoring \"%s\" (symbolic link)\n", entryName);
    else if (! doesFileExist (entryName))
	error (WARNING | PERROR, "cannot open source file \"%s\"", entryName);
    else if (isDirectory (entryName))
	resize = recurseIntoDirectory (entryName);
    else if (! isNormalFile (entryName))
	verbose ("ignoring \"%s\" (special file)\n", entryName);
    else
	resize = parseFile (entryName);

    return resize;
}

static boolean createTagsForArgs (cookedArgs* const args)
{
    boolean resize = FALSE;

    /*  Generate tags for each argument on the command line.
     */
    while (! cArgOff (args))
    {
	const char *arg = cArgItem (args);

#if defined (MSDOS) || defined (WIN32)
	vString *const pattern = vStringNewInit (arg);
	char *patternS = vStringValue (pattern);

	/*  We must transform the "." and ".." forms into something that can
	 *  be expanded by the MSDOS/Windows functions.
	 */
	if (Option.recurse  &&
	    (strcmp (patternS, ".") == 0  ||  strcmp (patternS, "..") == 0))
	{
	    vStringPut (pattern, OUTPUT_PATH_SEPARATOR);
	    vStringCatS (pattern, "*.*");
	}
	resize |= createTagsForMatchingEntries (patternS);
	vStringDelete (pattern);
#else
	resize |= createTagsForEntry (arg);
#endif
	cArgForth (args);
	parseOptions (args);
    }
    return resize;
}

/*  Read from an opened file a list of file names for which to generate tags.
 */
static boolean createTagsFromFileInput (FILE* const fp, const boolean filter)
{
    boolean resize = FALSE;
    if (fp != NULL)
    {
	cookedArgs* args = cArgNewFromLineFile (fp);
	parseOptions (args);
	while (! cArgOff (args))
	{
	    resize |= createTagsForEntry (cArgItem (args));
	    if (filter)
	    {
		if (Option.filterTerminator != NULL)
		    fputs (Option.filterTerminator, stdout);
		fflush (stdout);
	    }
	    cArgForth (args);
	    parseOptions (args);
	}
	cArgDelete (args);
    }
    return resize;
}

/*  Read from a named file a list of file names for which to generate tags.
 */
static boolean createTagsFromListFile (const char* const fileName)
{
    boolean resize;
    Assert (fileName != NULL);
    if (strcmp (fileName, "-") == 0)
	resize = createTagsFromFileInput (stdin, FALSE);
    else
    {
	FILE* const fp = fopen (fileName, "r");
	if (fp == NULL)
	    error (FATAL | PERROR, "cannot open list file \"%s\"", fileName);
	resize = createTagsFromFileInput (fp, FALSE);
	fclose (fp);
    }
    return resize;
}

#if defined (HAVE_CLOCK)
# define CLOCK_AVAILABLE
# ifndef CLOCKS_PER_SEC
#  define CLOCKS_PER_SEC	1000000
# endif
#elif defined (HAVE_TIMES)
# define CLOCK_AVAILABLE
# define CLOCKS_PER_SEC	60
static clock_t clock (void)
{
    struct tms buf;

    times (&buf);
    return (buf.tms_utime + buf.tms_stime);
}
#else
# define clock()  (clock_t)0
#endif

static void printTotals (const clock_t *const timeStamps)
{
    const unsigned long totalTags = TagFile.numTags.added +
				    TagFile.numTags.prev;

    fprintf (errout, "%ld file%s, %ld line%s (%ld kB) scanned",
	    Totals.files, plural (Totals.files),
	    Totals.lines, plural (Totals.lines),
	    Totals.bytes/1024L);
#ifdef CLOCK_AVAILABLE
    {
	const double interval = ((double) (timeStamps [1] - timeStamps [0])) /
				CLOCKS_PER_SEC;

	fprintf (errout, " in %.01f seconds", interval);
	if (interval != (double) 0.0)
	    fprintf (errout, " (%lu kB/s)",
		    (unsigned long) (Totals.bytes / interval) / 1024L);
    }
#endif
    fputc ('\n', errout);

    fprintf (errout, "%lu tag%s added to tag file",
	    TagFile.numTags.added, plural (TagFile.numTags.added));
    if (Option.append)
	fprintf (errout, " (now %lu tags)", totalTags);
    fputc ('\n', errout);

    if (totalTags > 0  &&  Option.sorted)
    {
	fprintf (errout, "%lu tag%s sorted", totalTags, plural (totalTags));
#ifdef CLOCK_AVAILABLE
	fprintf (errout, " in %.02f seconds",
		((double) (timeStamps [2] - timeStamps [1])) / CLOCKS_PER_SEC);
#endif
	fputc ('\n', errout);
    }

#ifdef DEBUG
    fprintf (errout, "longest tag line = %lu\n",
	    (unsigned long) TagFile.max.line);
#endif
}

static void makeTags (cookedArgs* args)
{
    clock_t timeStamps [3];
    boolean resize = FALSE;
    boolean files = (boolean)(! cArgOff (args) || Option.fileList != NULL
			      || Option.filter);

    if (! files  &&  ! Option.recurse)
	error (FATAL, "No files specified. Try \"%s --help\".",
	       getExecutableName ());

#define timeStamp(n) timeStamps[(n)]=(Option.printTotals ? clock():(clock_t)0)
    if (! Option.filter)
	openTagFile ();

    timeStamp (0);

    if (! cArgOff (args))
    {
	verbose ("Reading command line arguments\n");
	resize = createTagsForArgs (args);
    }
    if (Option.fileList != NULL)
    {
	verbose ("Reading list file\n");
	resize = (boolean) (createTagsFromListFile (Option.fileList) || resize);
    }
    if (Option.filter)
    {
	verbose ("Reading filter input\n");
	resize = (boolean) (createTagsFromFileInput (stdin, TRUE) || resize);
    }
    if (! files  &&  Option.recurse)
	resize = recurseIntoDirectory (".");

    timeStamp (1);

    if (! Option.filter)
	closeTagFile (resize);

    timeStamp (2);

    if (Option.printTotals)
	printTotals (timeStamps);
#undef timeStamp
}

/*
 *	Start up code
 */

static void setExecutableName (const char *const path)
{
    ExecutableProgram = path;
    ExecutableName = baseFilename (path);
#ifdef VAXC
{
    /* remove filetype from executable name */
    char *p = strrchr (ExecutableName, '.');
    if (p != NULL)
	*p = '\0';
}
#endif
}

extern int main (int __unused__ argc, char **argv)
{
    cookedArgs *args;
#ifdef VMS
    extern int getredirection (int *ac, char ***av);

    /* do wildcard expansion and I/O redirection */
    getredirection (&argc, &argv);
#endif

#ifdef AMIGA
    /* This program doesn't work when started from the Workbench */
    if (argc == 0)
	exit (1);
#endif

#ifdef __EMX__
    _wildcard (&argc, &argv);	/* expand wildcards in argument list */
#endif

#if defined (macintosh) && BUILD_MPW_TOOL == 0
    argc = ccommand (&argv);
#endif

    setCurrentDirectory ();
    setExecutableName (*argv++);
    checkRegex ();

    args = cArgNewFromArgv (argv);
    previewFirstOption (args);
    testEtagsInvocation ();
    initializeParsing ();
    initOptions ();
    readOptionConfiguration ();
    verbose ("Reading initial options from command line\n");
    parseOptions (args);
    checkOptions ();
    makeTags (args);

    /*  Clean up.
     */
    eFree (CurrentDirectory);
    freeList (&Excluded);
    cArgDelete (args);
    freeKeywordTable ();
    freeSourceFileResources ();
    freeTagFileResources ();
    freeOptionResources ();
    freeParserResources ();
    freeRegexResources ();

    exit (0);
    return 0;
}

/* vi:set tabstop=8 shiftwidth=4: */
