/*
*   Copyright (c) 2002-2003, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains a lose assortment of shared functions.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#ifdef HAVE_STDLIB_H
# include <stdlib.h>  /* to declare malloc (), realloc () */
#endif
#include <ctype.h>
#include <string.h>
#include <stdio.h>  /* to declare tempnam(), and SEEK_SET (hopefully) */

#ifdef HAVE_FCNTL_H
# include <fcntl.h>  /* to declare O_RDWR, O_CREAT, O_EXCL */
#endif
#ifdef HAVE_UNISTD_H
# include <unistd.h>  /* to declare mkstemp () */
#endif

#ifdef HAVE_LIMITS_H
# include <limits.h>  /* to declare MB_LEN_MAX */
#endif
#ifndef MB_LEN_MAX
# define MB_LEN_MAX 6
#endif

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

#ifdef HAVE_DIRECT_H
# include <direct.h>  /* to _getcwd */
#endif
#ifdef HAVE_DIR_H
# include <dir.h>  /* to declare findfirst() and findnext() */
#endif
#ifdef HAVE_IO_H
# include <io.h>  /* to declare open() */
#endif
#include "debug.h"
#include "routines.h"
#ifdef HAVE_ICONV
# include "mbcs.h"
#endif
#ifdef HAVE_ERRNO_H
# include <errno.h>
#endif

#include "options.h"

/*
*   MACROS
*/
#ifndef TMPDIR
# define TMPDIR "/tmp"
#endif

/*  File type tests.
 */
#ifndef S_ISREG
# if defined (S_IFREG)
#  define S_ISREG(mode)		((mode) & S_IFREG)
# endif
#endif

#ifndef S_ISLNK
# ifdef S_IFLNK
#  define S_ISLNK(mode)		(((mode) & S_IFMT) == S_IFLNK)
# else
#  define S_ISLNK(mode)		FALSE  /* assume no soft links */
# endif
#endif

#ifndef S_ISDIR
# ifdef S_IFDIR
#  define S_ISDIR(mode)		(((mode) & S_IFMT) == S_IFDIR)
# else
#  define S_ISDIR(mode)		FALSE  /* assume no soft links */
# endif
#endif

#ifndef S_IFMT
# define S_IFMT 0
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

#ifndef S_ISUID
# define S_ISUID 0
#endif

#ifndef S_ISGID
# define S_ISGID 0
#endif

/*  Hack for ridiculous practice of Microsoft Visual C++.
 */
#if defined (WIN32)
# if defined (_MSC_VER)
#  define stat    _stat
#  define getcwd  _getcwd
#  define currentdrive() (_getdrive() + 'A' - 1)
#  define PATH_MAX  _MAX_PATH
# else
#  define currentdrive() 'C'
# endif
#endif

#ifndef PATH_MAX
# define PATH_MAX 256
#endif

/*
 *  Miscellaneous macros
 */


/*
*   DATA DEFINITIONS
*/
#if defined (MSDOS_STYLE_PATH)
const char *const PathDelimiters = ":/\\";
#endif

char *CurrentDirectory;

static const char *ExecutableProgram;
static const char *ExecutableName;

/*
*   FUNCTION PROTOTYPES
*/
#ifdef NEED_PROTO_STAT
extern int stat (const char *, struct stat *);
#endif
#ifdef NEED_PROTO_LSTAT
extern int lstat (const char *, struct stat *);
#endif
#if defined (WIN32)
# define lstat(fn,buf) stat(fn,buf)
#endif

/*
*   FUNCTION DEFINITIONS
*/

extern void freeRoutineResources (void)
{
	if (CurrentDirectory != NULL)
		eFree (CurrentDirectory);
}

extern void setExecutableName (const char *const path)
{
	ExecutableProgram = path;
	ExecutableName = baseFilename (path);
}

extern const char *getExecutableName (void)
{
	return ExecutableName;
}

extern const char *getExecutablePath (void)
{
	return ExecutableProgram;
}

/*
 *  Memory allocation functions
 */

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

/*
 *  String manipulation functions
 */

/*
 * Compare two strings, ignoring case.
 * Return 0 for match, < 0 for smaller, > 0 for bigger
 * Make sure case is folded to uppercase in comparison (like for 'sort -f')
 * This makes a difference when one of the chars lies between upper and lower
 * ie. one of the chars [ \ ] ^ _ ` for ascii. (The '_' in particular !)
 */
extern int struppercmp (const char *s1, const char *s2)
{
	int result;
	do
	{
		result = toupper ((int) *s1) - toupper ((int) *s2);
	} while (result == 0  &&  *s1++ != '\0'  &&  *s2++ != '\0');
	return result;
}

extern int strnuppercmp (const char *s1, const char *s2, size_t n)
{
	int result;
	do
	{
		result = toupper ((int) *s1) - toupper ((int) *s2);
	} while (result == 0  &&  --n > 0  &&  *s1++ != '\0'  &&  *s2++ != '\0');
	return result;
}

#ifndef HAVE_STRSTR
extern char* strstr (const char *str, const char *substr)
{
	const size_t length = strlen (substr);
	const char *p;

	for (p = str  ;  *p != '\0'  ;  ++p)
		if (strncmp (p, substr, length) == 0)
			return (char*) p;
	return NULL;
}
#endif

extern char* strrstr (const char *str, const char *substr)
{
	const size_t length = strlen (substr);
	const char *p;

	for (p = str + strlen(str) - length  ;  p >= str  ;  --p)
		if (strncmp (p, substr, length) == 0)
			return (char*) p;
	return NULL;
}

extern char* eStrdup (const char* str)
{
	char* result = xMalloc (strlen (str) + 1, char);
	strcpy (result, str);
	return result;
}

extern char* eStrndup (const char* str, size_t len)
{
	char* result = xMalloc (len + 1, char);
	memset(result, 0, len + 1);
	strncpy (result, str, len);
	return result;
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

/* Safe wrapper for strtoul
 *
 * The conversion result is placed in value and must only be used if the
 * function returned true.
 */
extern boolean strToULong(const char *const str, int base, unsigned long *value)
{
	char *endptr;

	errno = 0;
	*value = strtoul (str, &endptr, base);
	return *endptr == '\0' && str != endptr && errno == 0;
}

/* Safe wrapper for strtol/atol
 *
 * The conversion result is placed in value and must only be used if the
 * function returned true.
 */
extern boolean strToLong(const char *const str, int base, long *value)
{
	char *endptr;

	errno = 0;
	*value = strtol (str, &endptr, base);
	return *endptr == '\0' && str != endptr && errno == 0;
}

extern boolean strToUInt(const char *const str, int base, unsigned int *value)
{
	unsigned long ulong_value;

	if(!strToULong(str, base, &ulong_value) || ulong_value > UINT_MAX)
		return false;

	*value = (unsigned int) ulong_value;
	return true;
}

extern boolean strToInt(const char *const str, int base, int *value)
{
	long long_value;

	if(!strToLong(str, base, &long_value) || long_value > INT_MAX || long_value < INT_MIN)
		return false;

	*value = (int) long_value;
	return true;
}

/*
 * File system functions
 */

extern void setCurrentDirectory (void)
{
	char* buf;
	if (CurrentDirectory == NULL)
		CurrentDirectory = xMalloc ((size_t) (PATH_MAX + 1), char);
	buf = getcwd (CurrentDirectory, PATH_MAX);
	if (buf == NULL)
		perror ("");
	if (CurrentDirectory [strlen (CurrentDirectory) - (size_t) 1] !=
			PATH_SEPARATOR)
	{
		sprintf (CurrentDirectory + strlen (CurrentDirectory), "%c",
				OUTPUT_PATH_SEPARATOR);
	}
}

/* For caching of stat() calls */
extern fileStatus *eStat (const char *const fileName)
{
	struct stat status;
	static fileStatus file;
	if (file.name == NULL  ||  strcmp (fileName, file.name) != 0)
	{
		eStatFree (&file);
		file.name = eStrdup (fileName);
		if (lstat (file.name, &status) != 0)
			file.exists = FALSE;
		else
		{
			file.isSymbolicLink = (boolean) S_ISLNK (status.st_mode);
			if (file.isSymbolicLink  &&  stat (file.name, &status) != 0)
				file.exists = FALSE;
			else
			{
				file.exists = TRUE;
				file.isDirectory = (boolean) S_ISDIR (status.st_mode);
				file.isNormalFile = (boolean) (S_ISREG (status.st_mode));
				file.isExecutable = (boolean) ((status.st_mode &
					(S_IXUSR | S_IXGRP | S_IXOTH)) != 0);
				file.isSetuid = (boolean) ((status.st_mode & S_ISUID) != 0);
				file.isSetgid = (boolean) ((status.st_mode & S_ISGID) != 0);
				file.size = status.st_size;
			}
		}
	}
	return &file;
}

extern void eStatFree (fileStatus *status)
{
	if (status->name != NULL)
	{
		eFree (status->name);
		status->name = NULL;
	}
}

extern boolean doesFileExist (const char *const fileName)
{
	fileStatus *status = eStat (fileName);
	return status->exists;
}

extern boolean doesExecutableExist (const char *const fileName)
{
	fileStatus *status = eStat (fileName);
	return status->exists && status->isExecutable;
}

extern boolean isRecursiveLink (const char* const dirName)
{
	boolean result = FALSE;
	fileStatus *status = eStat (dirName);
	if (status->isSymbolicLink)
	{
		char* const path = absoluteFilename (dirName);
		while (path [strlen (path) - 1] == PATH_SEPARATOR)
			path [strlen (path) - 1] = '\0';
		while (! result  &&  strlen (path) > (size_t) 1)
		{
			char *const separator = strrchr (path, PATH_SEPARATOR);
			if (separator == NULL)
				break;
			else if (separator == path)  /* backed up to root directory */
				*(separator + 1) = '\0';
			else
				*separator = '\0';
			result = isSameFile (path, dirName);
		}
		eFree (path);
	}
	return result;
}

/*
 *  Pathname manipulation (O/S dependent!!!)
 */

static boolean isPathSeparator (const int c)
{
	boolean result;
#if defined (MSDOS_STYLE_PATH)
	result = (boolean) (strchr (PathDelimiters, c) != NULL);
#else
	result = (boolean) (c == PATH_SEPARATOR);
#endif
	return result;
}

#if ! defined (HAVE_STAT_ST_INO)

static void canonicalizePath (char *const path __unused__)
{
# if defined (MSDOS_STYLE_PATH)
	char *p;
	for (p = path  ;  *p != '\0'  ;  ++p)
		if (isPathSeparator (*p)  &&  *p != ':')
			*p = PATH_SEPARATOR;
# endif
}

#endif

extern boolean isSameFile (const char *const name1, const char *const name2)
{
	boolean result = FALSE;
#if defined (HAVE_STAT_ST_INO)
	struct stat stat1, stat2;

	if (stat (name1, &stat1) == 0  &&  stat (name2, &stat2) == 0)
		result = (boolean) (stat1.st_ino == stat2.st_ino);
#else
	{
		char *const n1 = absoluteFilename (name1);
		char *const n2 = absoluteFilename (name2);
		canonicalizePath (n1);
		canonicalizePath (n2);
# if defined (CASE_INSENSITIVE_FILENAMES)
		result = (boolean) (strcasecmp (n1, n2) == 0);
# else
		result = (boolean) (strcmp (n1, n2) == 0);
# endif
		free (n1);
		free (n2);
	}
#endif
	return result;
}

extern const char *baseFilename (const char *const filePath)
{
#if defined (MSDOS_STYLE_PATH)
	const char *tail = NULL;
	unsigned int i;

	/*  Find whichever of the path delimiters is last.
	 */
	for (i = 0  ;  i < strlen (PathDelimiters)  ;  ++i)
	{
# ifdef HAVE_MBLEN
		const char *p;
		int ml;

		/* Some DBCS has letter contains 0x5C in trailing byte.
		 * So skip to the trailing byte. */
		for (p = filePath  ;  *p != '\0'  ;  ++p)
		{
			ml = mblen(p, MB_LEN_MAX);
			if (ml > 1)
				p += ml - 1;
			else if (*p == PathDelimiters [i] && p > tail)
				tail = p;
		}
# else
		const char *sep = strrchr (filePath, PathDelimiters [i]);

		if (sep > tail)
			tail = sep;
# endif
	}
#else
	const char *tail = strrchr (filePath, PATH_SEPARATOR);
#endif
	if (tail == NULL)
		tail = filePath;
	else
		++tail;  /* step past last delimiter */

	return tail;
}

extern const char *fileExtension (const char *const fileName)
{
	const char *extension;
	const char *pDelimiter = NULL;
	const char *const base = baseFilename (fileName);
	if (pDelimiter == NULL)
	    pDelimiter = strrchr (base, '.');

	if (pDelimiter == NULL)
		extension = "";
	else
		extension = pDelimiter + 1;  /* skip to first char of extension */

	return extension;
}

extern char* baseFilenameSansExtensionNew (const char *const fileName,
					   const char *const templateExt)
{
	const char *pDelimiter = NULL;
	const char *const base = baseFilename (fileName);
	char* shorten_base;

	pDelimiter = strrchr (base, templateExt[0]);

	if (pDelimiter && (strcmp (pDelimiter, templateExt) == 0))
	{
		shorten_base = eStrndup (base, pDelimiter - base);
		return shorten_base;
	}
	else
		return NULL;
}

extern boolean isAbsolutePath (const char *const path)
{
	boolean result = FALSE;
#if defined (MSDOS_STYLE_PATH)
	if (isPathSeparator (path [0]))
		result = TRUE;
	else if (isalpha (path [0])  &&  path [1] == ':')
	{
		if (isPathSeparator (path [2]))
			result = TRUE;
		else
			/*  We don't support non-absolute file names with a drive
			 *  letter, like `d:NAME' (it's too much hassle).
			 */
			error (FATAL,
				"%s: relative file names with drive letters not supported",
				path);
	}
#else
	result = isPathSeparator (path [0]);
#endif
	return result;
}

extern char *combinePathAndFile (
	const char *const path, const char *const file)
{
	vString *const filePath = vStringNew ();
	const int lastChar = path [strlen (path) - 1];
	boolean terminated = isPathSeparator (lastChar);

	vStringCopyS (filePath, path);
	if (! terminated)
	{
		vStringPut (filePath, OUTPUT_PATH_SEPARATOR);
		vStringTerminate (filePath);
	}
	vStringCatS (filePath, file);

	return vStringDeleteUnwrap (filePath);
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
extern char* absoluteFilename (const char *file)
{
	char *slashp, *cp;
	char *res = NULL;
	if (isAbsolutePath (file))
	{
#ifdef MSDOS_STYLE_PATH
		if (file [1] == ':')
			res = eStrdup (file);
		else
		{
			char drive [3];
			sprintf (drive, "%c:", currentdrive ());
			res = concat (drive, file, "");
		}
#else
		res = eStrdup (file);
#endif
	}
	else
		res = concat (CurrentDirectory, file, "");

	/* Delete the "/dirname/.." and "/." substrings. */
	slashp = strchr (res, PATH_SEPARATOR);
	while (slashp != NULL  &&  slashp [0] != '\0')
	{
		if (slashp[1] == '.')
		{
			if (slashp [2] == '.' &&
				(slashp [3] == PATH_SEPARATOR || slashp [3] == '\0'))
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
				else if (cp [0] != PATH_SEPARATOR)
					cp = slashp;
#endif
				memmove (cp, slashp + 3, strlen (slashp + 3) + 1);
				slashp = cp;
				continue;
			}
			else if (slashp [2] == PATH_SEPARATOR  ||  slashp [2] == '\0')
			{
				memmove (slashp, slashp + 2, strlen (slashp + 2) + 1);
				continue;
			}
		}
		slashp = strchr (slashp + 1, PATH_SEPARATOR);
	}

	if (res [0] == '\0')
		return eStrdup ("/");
	else
	{
#ifdef MSDOS_STYLE_PATH
		/* Canonicalize drive letter case. */
		if (res [1] == ':'  &&  islower (res [0]))
			res [0] = toupper (res [0]);
#endif

		return res;
	}
}

/* Return a newly allocated string containing the absolute file name of dir
 * where `file' resides given `CurrentDirectory'.
 * Routine adapted from Gnu etags.
 */
extern char* absoluteDirname (char *file)
{
	char *slashp, *res;
	char save;
	slashp = strrchr (file, PATH_SEPARATOR);
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
	dp--;  /* back to the first differing char */
	do
	{  /* look at the equal chars until path sep */
		if (fp == absdir)
			return absdir;  /* first char differs, give up */
		fp--;
		dp--;
	} while (*fp != PATH_SEPARATOR);

	/* Build a sequence of "../" strings for the resulting relative file name.
	 */
	i = 0;
	while ((dp = strchr (dp + 1, PATH_SEPARATOR)) != NULL)
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

extern MIO *tempFile (const char *const mode, char **const pName)
{
	char *name;
	FILE *fp;
	MIO *mio;
	int fd;
#if defined(HAVE_MKSTEMP)
	const char *const pattern = "tags.XXXXXX";
	const char *tmpdir = NULL;
	fileStatus *file = eStat (ExecutableProgram);
# ifdef WIN32
	tmpdir = getenv ("TMP");
# else
	if (! file->isSetuid)
		tmpdir = getenv ("TMPDIR");
# endif
	if (tmpdir == NULL)
		tmpdir = TMPDIR;
	name = xMalloc (strlen (tmpdir) + 1 + strlen (pattern) + 1, char);
	sprintf (name, "%s%c%s", tmpdir, OUTPUT_PATH_SEPARATOR, pattern);
	fd = mkstemp (name);
	eStatFree (file);
#elif defined(HAVE_TEMPNAM)
	const char *tmpdir = NULL;
# ifdef WIN32
	tmpdir = getenv ("TMP");
# endif
	if (tmpdir == NULL)
		tmpdir = TMPDIR;
	name = tempnam (tmpdir, "tags");
	if (name == NULL)
		error (FATAL | PERROR, "cannot allocate temporary file name");
	fd = open (name, O_RDWR | O_CREAT | O_EXCL, S_IRUSR | S_IWUSR);
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
	mio = mio_new_fp (fp, fclose);
	DebugStatement (
		debugPrintf (DEBUG_STATUS, "opened temporary file %s\n", name); )
	Assert (*pName == NULL);
	*pName = name;
	return mio;
}
