/*
*   Copyright (c) 1996-2003, Darren Hiebert
*
*   Author: Darren Hiebert <dhiebert@users.sourceforge.net>
*           http://ctags.sourceforge.net
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*   It is provided on an as-is basis and no responsibility is accepted for its
*   failure to perform as expected.
*
*   This is a reimplementation of the ctags (1) program. It is an attempt to
*   provide a fully featured ctags program which is free of the limitations
*   which most (all?) others are subject to.
*
*   This module contains the start-up code and routines to determine the list
*   of files to parsed for tags.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#if HAVE_DECL___ENVIRON
#include <unistd.h>
#elif HAVE_DECL__NSGETENVIRON
#include <crt_externs.h>
#endif

#include <string.h>

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

/*  To provide directory searching for recursion feature.
 */

#ifdef HAVE_DIRENT_H
# ifdef HAVE_SYS_TYPES_H
#  include <sys/types.h>  /* required by dirent.h */
# endif
# include <dirent.h>  /* to declare opendir() */
# undef boolean
#endif
#ifdef HAVE_DIRECT_H
# include <direct.h>  /* to _getcwd() */
#endif
#ifdef HAVE_DIR_H
# include <dir.h>  /* to declare findfirst() and findnext */
#endif
#ifdef HAVE_IO_H
# include <io.h>  /* to declare _findfirst() */
#endif


#include "debug.h"
#include "entry.h"
#include "error.h"
#include "field.h"
#include "keyword.h"
#include "main.h"
#include "options.h"
#include "output.h"
#include "read.h"
#include "routines.h"

/*
*   MACROS
*/
#define plural(value)  (((unsigned long)(value) == 1L) ? "" : "s")

/*
*   DATA DEFINITIONS
*/
static struct { long files, lines, bytes; } Totals = { 0, 0, 0 };
static mainLoopFunc mainLoop;
static void *mainData;

/*
*   FUNCTION PROTOTYPES
*/
static boolean createTagsForEntry (const char *const entryName);

/*
*   FUNCTION DEFINITIONS
*/

extern void addTotals (
		const unsigned int files, const long unsigned int lines,
		const long unsigned int bytes)
{
	Totals.files += files;
	Totals.lines += lines;
	Totals.bytes += bytes;
}

extern boolean isDestinationStdout (void)
{
	boolean toStdout = FALSE;

	if (outpuFormatUsedStdoutByDefault() ||  Option.filter  ||
		(Option.tagFileName != NULL  &&  (strcmp (Option.tagFileName, "-") == 0
						  || strcmp (Option.tagFileName, "/dev/stdout") == 0
		)))
		toStdout = TRUE;
	return toStdout;
}

#if defined (HAVE_OPENDIR)
static boolean recurseUsingOpendir (const char *const dirName)
{
	boolean resize = FALSE;
	DIR *const dir = opendir (dirName);
	if (dir == NULL)
		error (WARNING | PERROR, "cannot recurse into directory \"%s\"", dirName);
	else
	{
		struct dirent *entry;
		while ((entry = readdir (dir)) != NULL)
		{
			if (strcmp (entry->d_name, ".") != 0  &&
				strcmp (entry->d_name, "..") != 0)
			{
				char *filePath;
				boolean free_p = FALSE;
				if (strcmp (dirName, ".") == 0)
					filePath = entry->d_name;
				else
				  {
					filePath = combinePathAndFile (dirName, entry->d_name);
					free_p = TRUE;
				  }
				resize |= createTagsForEntry (filePath);
				if (free_p)
					eFree (filePath);
			}
		}
		closedir (dir);
	}
	return resize;
}

#elif defined (HAVE_FINDFIRST) || defined (HAVE__FINDFIRST)

static boolean createTagsForWildcardEntry (
		const char *const pattern, const size_t dirLength,
		const char *const entryName)
{
	boolean resize = FALSE;
	/* we must not recurse into the directories "." or ".." */
	if (strcmp (entryName, ".") != 0  &&  strcmp (entryName, "..") != 0)
	{
		vString *const filePath = vStringNew ();
		vStringNCopyS (filePath, pattern, dirLength);
		vStringCatS (filePath, entryName);
		resize = createTagsForEntry (vStringValue (filePath));
		vStringDelete (filePath);
	}
	return resize;
}

static boolean createTagsForWildcardUsingFindfirst (const char *const pattern)
{
	boolean resize = FALSE;
	const size_t dirLength = baseFilename (pattern) - pattern;
#if defined (HAVE_FINDFIRST)
	struct ffblk fileInfo;
	int result = findfirst (pattern, &fileInfo, FA_DIREC);
	while (result == 0)
	{
		const char *const entry = (const char *) fileInfo.ff_name;
		resize |= createTagsForWildcardEntry (pattern, dirLength, entry);
		result = findnext (&fileInfo);
	}
#elif defined (HAVE__FINDFIRST)
	struct _finddata_t fileInfo;
	findfirst_t hFile = _findfirst (pattern, &fileInfo);
	if (hFile != -1L)
	{
		do
		{
			const char *const entry = (const char *) fileInfo.name;
			resize |= createTagsForWildcardEntry (pattern, dirLength, entry);
		} while (_findnext (hFile, &fileInfo) == 0);
		_findclose (hFile);
	}
#endif
	return resize;
}

#endif


static boolean recurseIntoDirectory (const char *const dirName)
{
	static unsigned int recursionDepth = 0;

	recursionDepth++;

	boolean resize = FALSE;
	if (isRecursiveLink (dirName))
		verbose ("ignoring \"%s\" (recursive link)\n", dirName);
	else if (! Option.recurse)
		verbose ("ignoring \"%s\" (directory)\n", dirName);
	else if(recursionDepth > Option.maxRecursionDepth)
		verbose ("not descending in directory \"%s\" (depth %u > %u)\n",
				dirName, recursionDepth, Option.maxRecursionDepth);
	else
	{
		verbose ("RECURSING into directory \"%s\"\n", dirName);
#if defined (HAVE_OPENDIR)
		resize = recurseUsingOpendir (dirName);
#elif defined (HAVE_FINDFIRST) || defined (HAVE__FINDFIRST)
		{
			vString *const pattern = vStringNew ();
			vStringCopyS (pattern, dirName);
			vStringPut (pattern, OUTPUT_PATH_SEPARATOR);
			vStringCatS (pattern, "*.*");
			resize = createTagsForWildcardUsingFindfirst (vStringValue (pattern));
			vStringDelete (pattern);
		}
#endif
	}

	recursionDepth--;

	return resize;
}

static boolean createTagsForEntry (const char *const entryName)
{
	boolean resize = FALSE;
	fileStatus *status = eStat (entryName);

	Assert (entryName != NULL);
	if (isExcludedFile (entryName))
		verbose ("excluding \"%s\"\n", entryName);
	else if (status->isSymbolicLink  &&  ! Option.followLinks)
		verbose ("ignoring \"%s\" (symbolic link)\n", entryName);
	else if (! status->exists)
		error (WARNING | PERROR, "cannot open input file \"%s\"", entryName);
	else if (status->isDirectory)
		resize = recurseIntoDirectory (entryName);
	else if (! status->isNormalFile)
		verbose ("ignoring \"%s\" (special file)\n", entryName);
	else
		resize = parseFile (entryName);

	eStatFree (status);
	return resize;
}

#ifdef MANUAL_GLOBBING

static boolean createTagsForWildcardArg (const char *const arg)
{
	boolean resize = FALSE;
	vString *const pattern = vStringNewInit (arg);
	char *patternS = vStringValue (pattern);

#if defined (HAVE_FINDFIRST) || defined (HAVE__FINDFIRST)
	/*  We must transform the "." and ".." forms into something that can
	 *  be expanded by the findfirst/_findfirst functions.
	 */
	if (Option.recurse  &&
		(strcmp (patternS, ".") == 0  ||  strcmp (patternS, "..") == 0))
	{
		vStringPut (pattern, OUTPUT_PATH_SEPARATOR);
		vStringCatS (pattern, "*.*");
	}
	resize |= createTagsForWildcardUsingFindfirst (patternS);
#endif
	vStringDelete (pattern);
	return resize;
}

#endif

static boolean createTagsForArgs (cookedArgs *const args)
{
	boolean resize = FALSE;

	/*  Generate tags for each argument on the command line.
	 */
	while (! cArgOff (args))
	{
		const char *const arg = cArgItem (args);

#ifdef MANUAL_GLOBBING
		resize |= createTagsForWildcardArg (arg);
#else
		resize |= createTagsForEntry (arg);
#endif
		cArgForth (args);
		parseCmdlineOptions (args);
	}
	return resize;
}

/*  Read from an opened file a list of file names for which to generate tags.
 */
static boolean createTagsFromFileInput (FILE *const fp, const boolean filter)
{
	boolean resize = FALSE;
	if (fp != NULL)
	{
		cookedArgs *args = cArgNewFromLineFile (fp);
		parseCmdlineOptions (args);
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
			parseCmdlineOptions (args);
		}
		cArgDelete (args);
	}
	return resize;
}

/*  Read from a named file a list of file names for which to generate tags.
 */
static boolean createTagsFromListFile (const char *const fileName)
{
	boolean resize;
	Assert (fileName != NULL);
	if (strcmp (fileName, "-") == 0)
		resize = createTagsFromFileInput (stdin, FALSE);
	else
	{
		FILE *const fp = fopen (fileName, "r");
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
#  define CLOCKS_PER_SEC		1000000
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
	const unsigned long totalTags = numTagsTotal();
	const unsigned long addedTags = numTagsAdded();

	fprintf (stderr, "%ld file%s, %ld line%s (%ld kB) scanned",
			Totals.files, plural (Totals.files),
			Totals.lines, plural (Totals.lines),
			Totals.bytes/1024L);
#ifdef CLOCK_AVAILABLE
	{
		const double interval = ((double) (timeStamps [1] - timeStamps [0])) /
								CLOCKS_PER_SEC;

		fprintf (stderr, " in %.01f seconds", interval);
		if (interval != (double) 0.0)
			fprintf (stderr, " (%lu kB/s)",
					(unsigned long) (Totals.bytes / interval) / 1024L);
	}
#endif
	fputc ('\n', stderr);

	fprintf (stderr, "%lu tag%s added to tag file",
			addedTags, plural(addedTags));
	if (Option.append)
		fprintf (stderr, " (now %lu tags)", totalTags);
	fputc ('\n', stderr);

	if (totalTags > 0  &&  Option.sorted != SO_UNSORTED)
	{
		fprintf (stderr, "%lu tag%s sorted", totalTags, plural (totalTags));
#ifdef CLOCK_AVAILABLE
		fprintf (stderr, " in %.02f seconds",
				((double) (timeStamps [2] - timeStamps [1])) / CLOCKS_PER_SEC);
#endif
		fputc ('\n', stderr);
	}

#ifdef DEBUG
	fprintf (stderr, "longest tag line = %lu\n",
		 (unsigned long) maxTagsLine ());
#endif
}

static boolean etagsInclude (void)
{
	return (boolean)(Option.etags && Option.etagsInclude != NULL);
}

extern void setMainLoop (mainLoopFunc func, void *data)
{
	mainLoop = func;
	mainData = data;
}

static void runMainLoop (cookedArgs *args)
{
	(* mainLoop) (args, mainData);
}

static void batchMakeTags (cookedArgs *args, void *user CTAGS_ATTR_UNUSED)
{
	clock_t timeStamps [3];
	boolean resize = FALSE;
	boolean files = (boolean)(! cArgOff (args) || Option.fileList != NULL
							  || Option.filter);

	if (! files)
	{
		if (filesRequired ())
			error (FATAL, "No files specified. Try \"%s --help\".",
				getExecutableName ());
		else if (! Option.recurse && ! etagsInclude ())
			return;
	}

#define timeStamp(n) timeStamps[(n)]=(Option.printTotals ? clock():(clock_t)0)
	if ((! Option.filter) && (! Option.printLanguage))
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

	if ((! Option.filter) && (!Option.printLanguage))
		closeTagFile (resize);

	timeStamp (2);

	if (Option.printTotals)
		printTotals (timeStamps);
#undef timeStamp
}

static boolean isSafeVar (const char* var)
{
	const char *safe_vars[] = {
		"BASH_FUNC_module()=",
		"BASH_FUNC_scl()=",
		NULL
	};
	const char *sv;

	for (sv = safe_vars[0]; sv != NULL; sv++)
		if (strncmp(var, sv, strlen (sv)) == 0)
			return TRUE;

	return FALSE;
}

static void sanitizeEnviron (void)
{
	char **e = NULL;
	int i;

#if HAVE_DECL___ENVIRON
	e = __environ;
#elif HAVE_DECL__NSGETENVIRON
{
	char ***ep = _NSGetEnviron();
	if (ep)
		e = *ep;
}
#endif

	if (!e)
		return;

	for (i = 0; e [i]; i++)
	{
		char *value;

		value = strchr (e [i], '=');
		if (!value)
			continue;

		value++;
		if (!strncmp (value, "() {", 4))
		{
			if (isSafeVar (e [i]))
				continue;
			error (WARNING, "reset environment: %s", e [i]);
			value [0] = '\0';
		}
	}
}

/*
 *		Start up code
 */

extern int main (int argc CTAGS_ATTR_UNUSED, char **argv)
{
	cookedArgs *args;

	setErrorPrinter (stderrDefaultErrorPrinter, NULL);
	setMainLoop (batchMakeTags, NULL);
	setTagWriter (&ctagsWriter);

	setCurrentDirectory ();
	setExecutableName (*argv++);
	sanitizeEnviron ();
	checkRegex ();
	initFieldDescs ();

	args = cArgNewFromArgv (argv);
	previewFirstOption (args);
	testEtagsInvocation ();
	initializeParsing ();
	initOptions ();
	readOptionConfiguration ();
	verbose ("Reading initial options from command line\n");
	parseCmdlineOptions (args);
	checkOptions ();

	runMainLoop (args);

	/*  Clean up.
	 */
	cArgDelete (args);
	freeKeywordTable ();
	freeRoutineResources ();
	freeInputFileResources ();
	freeTagFileResources ();
	freeOptionResources ();
	freeParserResources ();
	freeRegexResources ();
	freeXcmdResources ();
#ifdef HAVE_ICONV
	freeEncodingResources ();
#endif

	if (Option.printLanguage)
		return (Option.printLanguage == TRUE)? 0: 1;

	exit (0);
	return 0;
}
