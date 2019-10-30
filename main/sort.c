/*
*   Copyright (c) 1996-2002, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions to sort the tag entries.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#if defined (HAVE_IO_H)
# include <io.h>
#endif
#include <stdlib.h>  /* to declare malloc () */
#if defined (HAVE_UNISTD_H)
# include <unistd.h>
#endif
#include <string.h>
#include <stdio.h>

#include "debug.h"
#include "entry_p.h"
#include "options_p.h"
#include "read.h"
#include "routines.h"
#include "sort_p.h"

/*
*   FUNCTION DEFINITIONS
*/

extern void catFile (MIO *mio)
{
	if (mio != NULL)
	{
		int c;
		mio_seek (mio, 0, SEEK_SET);
		while ((c = mio_getc (mio)) != EOF)
			putchar (c);
		fflush (stdout);
	}
}

#ifdef EXTERNAL_SORT

#ifdef NON_CONST_PUTENV_PROTOTYPE
# define PE_CONST
#else
# define PE_CONST const
#endif

/*
   Output file name should not be evaluated in system(3) function.
   The name must be used as is. Quotations are required to block the
   evaluation.

   Normal single-quotes are used to quote a cstring:
   a => 'a'
   " => '"'

   If a single-quote is included in the cstring, use double quotes for quoting it.
   ' => ''"'"''
*/
static void appendCstringWithQuotes (vString *dest, const char* cstr)
{
#ifdef WIN32
	vStringCatS (dest, cstr);
#else
	vStringPut (dest, '\'');
	for (const char *o = cstr; *o; o++)
	{
		if (*o == '\'')
			vStringCatS (dest, "'\"'\"'");
		else
			vStringPut (dest, *o);
	}
	vStringPut (dest, '\'');
#endif
}

extern void externalSortTags (const bool toStdout, MIO *tagFile)
{
	const char *const sortNormalCommand = "sort -u";
	const char *const sortFoldedCommand = "sort -u -f";
	const char *sortCommand =
		Option.sorted == SO_FOLDSORTED ? sortFoldedCommand : sortNormalCommand;
# ifndef HAVE_SETENV
	PE_CONST char *const sortOrder1 = "LC_COLLATE=C";
	PE_CONST char *const sortOrder2 = "LC_ALL=C";
# endif
	vString *cmd = vStringNew ();
	int ret = -1;

	if (cmd != NULL)
	{
		/*  Ensure ASCII value sort order.
		 */
#if defined (HAVE_SETENV) || defined (HAVE_PUTENV)
# ifdef HAVE_SETENV
		setenv ("LC_COLLATE", "C", 1);
		setenv ("LC_ALL", "C", 1);
# else
		putenv (sortOrder1);
		putenv (sortOrder2);
# endif
		vStringCatS (cmd, sortCommand);
		if (! toStdout)
		{
			vStringCatS (cmd, " -o ");
			appendCstringWithQuotes (cmd, tagFileName ());
			vStringPut (cmd, ' ');
			appendCstringWithQuotes (cmd, tagFileName ());
		}
#else
		vStringCatS (cmd, sortOrder1);
		vStringPut (cmd, ' ');
		vStringCatS (cmd, sortOrder2);
		vStringPut (cmd, ' ');
		vStringCatS (cmd, sortCommand);
		if (! toStdout)
		{
			vStringCats (cmd, " -o ");
			appendCstringWithQuotes (cmd, tagFileName ());
			vStringPut (cmd, ' ');
			appendCstringWithQuotes (cmd, tagFileName ());
		}
#endif
		verbose ("system (\"%s\")\n", vStringValue (cmd));
		if (toStdout)
		{
			const int fdstdin = 0;
			int fdsave;

			fdsave = dup (fdstdin);
			if (fdsave < 0)
				error (FATAL | PERROR, "cannot save stdin fd");
			if (dup2 (fileno (mio_file_get_fp (tagFile)), fdstdin) < 0)
				error (FATAL | PERROR, "cannot redirect stdin");
			if (lseek (fdstdin, 0, SEEK_SET) != 0)
				error (FATAL | PERROR, "cannot rewind tag file");
			ret = system (vStringValue (cmd));
			if (dup2 (fdsave, fdstdin) < 0)
				error (FATAL | PERROR, "cannot restore stdin fd");
			close (fdsave);
		}
		else
			ret = system (vStringValue (cmd));
		vStringDelete (cmd);
	}
	if (ret != 0)
		error (FATAL | PERROR, "cannot sort tag file");
}

#else

/*
 *  These functions provide a basic internal sort. No great memory
 *  optimization is performed (e.g. recursive subdivided sorts),
 *  so have lots of memory if you have large tag files.
 */

extern void failedSort (MIO *const mio, const char* msg)
{
	const char* const cannotSort = "cannot sort tag file";
	if (mio != NULL)
		mio_unref (mio);
	if (msg == NULL)
		error (FATAL | PERROR, "%s", cannotSort);
	else
		error (FATAL, "%s: %s", msg, cannotSort);
}

static int compareTagsFolded(const void *const one, const void *const two)
{
	const char *const line1 = *(const char* const*) one;
	const char *const line2 = *(const char* const*) two;

	return struppercmp (line1, line2);
}

static int compareTags (const void *const one, const void *const two)
{
	const char *const line1 = *(const char* const*) one;
	const char *const line2 = *(const char* const*) two;

	return strcmp (line1, line2);
}

static void writeSortedTags (
		char **const table, const size_t numTags, const bool toStdout, bool newlineReplaced)
{
	MIO *mio;
	size_t i;

	/*  Write the sorted lines back into the tag file.
	 */
	if (toStdout)
		mio = mio_new_fp (stdout, NULL);
	else
	{
		mio = mio_new_file (tagFileName (), "w");
		if (mio == NULL)
			failedSort (mio, NULL);
	}
	for (i = 0 ; i < numTags ; ++i)
	{
		/*  Here we filter out identical tag *lines* (including search
		 *  pattern) if this is not an xref file.
		 */
		if (i == 0  ||  Option.xref  ||  strcmp (table [i], table [i-1]) != 0)
		{
			if (mio_puts (mio, table [i]) == EOF)
				failedSort (mio, NULL);
			else if (newlineReplaced)
				mio_putc (mio, '\n');
		}
	}
	if (toStdout)
		mio_flush (mio);
	mio_unref (mio);
}

extern void internalSortTags (const bool toStdout, MIO* mio, size_t numTags)
{
	vString *vLine = vStringNew ();
	const char *line;
	size_t i;
	int (*cmpFunc)(const void *, const void *);
	bool newlineReplaced = false;

	/*  Allocate a table of line pointers to be sorted.
	 */
	const size_t tableSize = numTags * sizeof (char *);
	char **table = (char **) malloc (tableSize);  /* line pointers */
	DebugStatement ( size_t mallocSize = tableSize; )  /* cumulative total */


	cmpFunc = Option.sorted == SO_FOLDSORTED ? compareTagsFolded : compareTags;
	if (table == NULL)
		failedSort (mio, "out of memory");

	for (i = 0  ;  i < numTags  &&  ! mio_eof (mio)  ;  )
	{
		line = readLineRaw (vLine, mio);
		if (line == NULL)
		{
			if (! mio_eof (mio))
				failedSort (mio, NULL);
			break;
		}
		else if (*line == '\0'  ||  strcmp (line, "\n") == 0)
			;  /* ignore blank lines */
		else
		{
			const size_t stringSize = strlen (line) + 1;

			table [i] = (char *) malloc (stringSize);
			if (table [i] == NULL)
				failedSort (mio, "out of memory");
			DebugStatement ( mallocSize += stringSize; )
			strcpy (table [i], line);
			if (table[i][stringSize - 2] == '\n')
			{
				table[i][stringSize - 2] = '\0';
				newlineReplaced = true;
			}
			++i;
		}
	}
	numTags = i;
	vStringDelete (vLine);

	/*  Sort the lines.
	 */
	qsort (table, numTags, sizeof (*table), cmpFunc);

	writeSortedTags (table, numTags, toStdout, newlineReplaced);

	PrintStatus (("sort memory: %ld bytes\n", (long) mallocSize));
	for (i = 0 ; i < numTags ; ++i)
		free (table [i]);
	free (table);
}

#endif
