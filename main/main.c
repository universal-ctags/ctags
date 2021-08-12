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

#include <stdlib.h>
#include <string.h>
#include <time.h>

/*  To provide directory searching for recursion feature.
 */

#ifdef HAVE_DIRENT_H
# ifdef HAVE_SYS_TYPES_H
#  include <sys/types.h>  /* required by dirent.h */
# endif
# include <dirent.h>  /* to declare opendir() */
#endif
#ifdef HAVE_DIRECT_H
# include <direct.h>  /* to _getcwd() */
#endif
#ifdef HAVE_IO_H
# include <io.h>  /* to declare _findfirst() */
#endif


#include "ctags.h"
#include "debug.h"
#include "entry_p.h"
#include "error_p.h"
#include "field_p.h"
#include "keyword_p.h"
#include "main_p.h"
#include "options_p.h"
#include "optscript.h"
#include "parse_p.h"
#include "read_p.h"
#include "routines_p.h"
#include "stats_p.h"
#include "trace.h"
#include "trashbox_p.h"
#include "writer_p.h"
#include "xtag_p.h"

#ifdef HAVE_JANSSON
#include "interactive_p.h"
#include <jansson.h>
#include <errno.h>
#endif

/*
*   DATA DEFINITIONS
*/
static mainLoopFunc mainLoop;
static void *mainData;

/*
*   FUNCTION PROTOTYPES
*/
static bool createTagsForEntry (const char *const entryName);

/*
*   FUNCTION DEFINITIONS
*/

#if defined (HAVE_OPENDIR) && (defined (HAVE_DIRENT_H) || defined (_MSC_VER))
static bool recurseUsingOpendir (const char *const dirName)
{
	bool resize = false;
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
				bool free_p = false;
				if (strcmp (dirName, ".") == 0)
					filePath = entry->d_name;
				else
				{
					filePath = combinePathAndFile (dirName, entry->d_name);
					free_p = true;
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
#endif

#ifdef HAVE__FINDFIRST

static bool createTagsForWildcardEntry (
		const char *const pattern, const size_t dirLength,
		const char *const entryName)
{
	bool resize = false;
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

static bool createTagsForWildcardUsingFindfirst (const char *const pattern)
{
	bool resize = false;
	const size_t dirLength = baseFilename (pattern) - pattern;
	struct _finddata_t fileInfo;
	intptr_t hFile = _findfirst (pattern, &fileInfo);
	if (hFile != -1L)
	{
		do
		{
			const char *const entry = (const char *) fileInfo.name;
			resize |= createTagsForWildcardEntry (pattern, dirLength, entry);
		} while (_findnext (hFile, &fileInfo) == 0);
		_findclose (hFile);
	}
	return resize;
}

#endif


static bool recurseIntoDirectory (const char *const dirName)
{
	static unsigned int recursionDepth = 0;

	recursionDepth++;

	bool resize = false;
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
#if defined (HAVE_OPENDIR) && (defined (HAVE_DIRENT_H) || defined (_MSC_VER))
		resize = recurseUsingOpendir (dirName);
#elif defined (HAVE__FINDFIRST)
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

static bool createTagsForEntry (const char *const entryName)
{
	bool resize = false;
	fileStatus *status = eStat (entryName);

	Assert (entryName != NULL);
	if (isExcludedFile (entryName, true))
		verbose ("excluding \"%s\" (the early stage)\n", entryName);
	else if (status->isSymbolicLink  &&  ! Option.followLinks)
		verbose ("ignoring \"%s\" (symbolic link)\n", entryName);
	else if (! status->exists)
		error (WARNING | PERROR, "cannot open input file \"%s\"", entryName);
	else if (status->isDirectory)
		resize = recurseIntoDirectory (entryName);
	else if (! status->isNormalFile)
		verbose ("ignoring \"%s\" (special file)\n", entryName);
	else if (isExcludedFile (entryName, false))
		verbose ("excluding \"%s\"\n", entryName);
	else
		resize = parseFile (entryName);

	eStatFree (status);
	return resize;
}

#ifdef MANUAL_GLOBBING

static bool createTagsForWildcardArg (const char *const arg)
{
	bool resize = false;
	vString *const pattern = vStringNewInit (arg);
	char *patternS = vStringValue (pattern);

#if defined (HAVE__FINDFIRST)
	/*  We must transform the "." and ".." forms into something that can
	 *  be expanded by the _findfirst function.
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

static bool createTagsForArgs (cookedArgs *const args)
{
	bool resize = false;

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
static bool createTagsFromFileInput (FILE *const fp, const bool filter)
{
	bool resize = false;
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
static bool createTagsFromListFile (const char *const fileName)
{
	bool resize;
	Assert (fileName != NULL);
	if (strcmp (fileName, "-") == 0)
		resize = createTagsFromFileInput (stdin, false);
	else
	{
		FILE *const fp = fopen (fileName, "r");
		if (fp == NULL)
			error (FATAL | PERROR, "cannot open list file \"%s\"", fileName);
		resize = createTagsFromFileInput (fp, false);
		fclose (fp);
	}
	return resize;
}

static bool etagsInclude (void)
{
	return (bool)(Option.etags && Option.etagsInclude != NULL);
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
	bool resize = false;
	bool files = (bool)(! cArgOff (args) || Option.fileList != NULL
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
		resize = (bool) (createTagsFromListFile (Option.fileList) || resize);
	}
	if (Option.filter)
	{
		verbose ("Reading filter input\n");
		resize = (bool) (createTagsFromFileInput (stdin, true) || resize);
	}
	if (! files  &&  Option.recurse)
		resize = recurseIntoDirectory (".");

	timeStamp (1);

	if ((! Option.filter) && (!Option.printLanguage))
		closeTagFile (resize);

	timeStamp (2);

	if (Option.printTotals)
	{
		printTotals (timeStamps, Option.append, Option.sorted);
		if (Option.printTotals > 1)
			for (unsigned int i = 0; i < countParsers(); i++)
				printParserStatisticsIfUsed (i);
	}

#undef timeStamp
}

#ifdef HAVE_JANSSON
void interactiveLoop (cookedArgs *args CTAGS_ATTR_UNUSED, void *user)
{
	struct interactiveModeArgs *iargs = user;

	if (iargs->sandbox) {
		/* As of jansson 2.6, the object hashing is seeded off
		   of /dev/urandom, so trigger the hash seeding
		   before installing the syscall filter.
		*/
		json_t * tmp = json_object ();
		json_decref (tmp);

		if (installSyscallFilter ()) {
			error (FATAL, "install_syscall_filter failed");
			/* The explicit exit call is needed because
			   "error (FATAL,..." just prints a message in
			   interactive mode. */
			exit (1);
		}
	}

	char buffer[1024];
	json_t *request;

	fputs ("{\"_type\": \"program\", \"name\": \"" PROGRAM_NAME "\", \"version\": \"" PROGRAM_VERSION "\"}\n", stdout);
	fflush (stdout);

	while (fgets (buffer, sizeof(buffer), stdin))
	{
		if (buffer[0] == '\n')
			continue;

		request = json_loads (buffer, JSON_DISABLE_EOF_CHECK, NULL);
		if (! request)
		{
			error (FATAL, "invalid json");
			goto next;
		}

		json_t *command = json_object_get (request, "command");
		if (! command)
		{
			error (FATAL, "command name not found");
			goto next;
		}

		if (!strcmp ("generate-tags", json_string_value (command)))
		{
			json_int_t size = -1;
			const char *filename;

			if (json_unpack (request, "{ss}", "filename", &filename) == -1)
			{
				error (FATAL, "invalid generate-tags request");
				goto next;
			}

			json_unpack (request, "{sI}", "size", &size);

			openTagFile ();
			if (size == -1)
			{					/* read from disk */
				if (iargs->sandbox) {
					error (FATAL,
						   "invalid request in sandbox submode: reading file contents from a file is limited");
					closeTagFile (false);
					goto next;
				}

				createTagsForEntry (filename);
			}
			else
			{					/* read nbytes from stream */
				unsigned char *data = eMalloc (size);
				size = fread (data, 1, size, stdin);
				MIO *mio = mio_new_memory (data, size, eRealloc, eFreeNoNullCheck);
				parseFileWithMio (filename, mio, NULL);
				mio_unref (mio);
			}

			closeTagFile (false);
			fputs ("{\"_type\": \"completed\", \"command\": \"generate-tags\"}\n", stdout);
			fflush(stdout);
		}
		else
		{
			error (FATAL, "unknown command name");
			goto next;
		}

	next:
		json_decref (request);
	}
}
#endif

static bool isSafeVar (const char* var)
{
	const char *safe_vars[] = {
		"BASH_FUNC_module()=",
		"BASH_FUNC_scl()=",
		NULL
	};
	const char *sv;

	for (sv = safe_vars[0]; sv != NULL; sv++)
		if (strncmp(var, sv, strlen (sv)) == 0)
			return true;

	return false;
}

static void sanitizeEnviron (void)
{
	char **e;
	int i;

#if HAVE_DECL___ENVIRON
	e = __environ;
#elif HAVE_DECL__NSGETENVIRON
	{
		char ***ep = _NSGetEnviron();
		if (ep)
			e = *ep;
		else
			e = NULL;
	}
#else
	e = NULL;
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

extern int ctags_cli_main (int argc CTAGS_ATTR_UNUSED, char **argv)
{
	cookedArgs *args;

#if defined(WIN32) && defined(HAVE_MKSTEMP)
	/* MinGW-w64's mkstemp() uses rand() for generating temporary files. */
	srand ((unsigned int) clock ());
#endif

	initDefaultTrashBox ();

	DEBUG_INIT();

	setErrorPrinter (stderrDefaultErrorPrinter, NULL);
	setMainLoop (batchMakeTags, NULL);
	setTagWriter (WRITER_U_CTAGS, NULL);

	setCurrentDirectory ();
	setExecutableName (*argv++);
	sanitizeEnviron ();
	checkRegex ();
	initFieldObjects ();
	initXtagObjects ();

	args = cArgNewFromArgv (argv);
	previewFirstOption (args);
	initializeParsing ();
	testEtagsInvocation ();
	initOptions ();
	initRegexOptscript ();
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
#ifdef HAVE_ICONV
	freeEncodingResources ();
#endif

	finiDefaultTrashBox();

	if (Option.printLanguage)
		return (Option.printLanguage == true)? 0: 1;

	exit (0);
	return 0;
}
