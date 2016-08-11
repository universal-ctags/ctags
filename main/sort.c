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
#if defined (HAVE_STDLIB_H)
# include <stdlib.h>  /* to declare malloc () */
#endif
#if defined (HAVE_UNISTD_H)
# include <unistd.h>
#endif
#include <string.h>
#include <stdio.h>

#include "debug.h"
#include "entry.h"
#include "options.h"
#include "read.h"
#include "routines.h"
#include "sort.h"

/*
*   FUNCTION DEFINITIONS
*/

extern void catFile (MIO *fp)
{
	if (fp != NULL)
	{
		int c;
		mio_seek (fp, 0, SEEK_SET);
		while ((c = mio_getc (fp)) != EOF)
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

extern void externalSortTags (const boolean toStdout, MIO *tagFile)
{
	const char *const sortNormalCommand = "sort -u";
	const char *const sortFoldedCommand = "sort -u -f";
	const char *sortCommand =
		Option.sorted == SO_FOLDSORTED ? sortFoldedCommand : sortNormalCommand;
	PE_CONST char *const sortOrder1 = "LC_COLLATE=C";
	PE_CONST char *const sortOrder2 = "LC_ALL=C";
	const size_t length = 4 + strlen (sortOrder1) + strlen (sortOrder2) +
			strlen (sortCommand) + (2 * strlen (tagFileName ()));
	char *const cmd = (char *) malloc (length + 1);
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
		if (toStdout)
			sprintf (cmd, "%s", sortCommand);
		else
			sprintf (cmd, "%s -o %s %s", sortCommand,
					tagFileName (), tagFileName ());
#else
		if (toStdout)
			sprintf (cmd, "%s %s %s", sortOrder1, sortOrder2, sortCommand);
		else
			sprintf (cmd, "%s %s %s -o %s %s", sortOrder1, sortOrder2,
					sortCommand, tagFileName (), tagFileName ());
#endif
		verbose ("system (\"%s\")\n", cmd);
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
			ret = system (cmd);
			if (dup2 (fdsave, fdstdin) < 0)
				error (FATAL | PERROR, "cannot restore stdin fd");
			close (fdsave);
		}
		else
			ret = system (cmd);
		free (cmd);

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

extern void failedSort (MIO *const fp, const char* msg)
{
	const char* const cannotSort = "cannot sort tag file";
	if (fp != NULL)
		mio_free (fp);
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
		char **const table, const size_t numTags, const boolean toStdout)
{
	MIO *fp;
	size_t i;

	/*  Write the sorted lines back into the tag file.
	 */
	if (toStdout)
		fp = mio_new_fp (stdout, NULL);
	else
	{
		fp = mio_new_file (tagFileName (), "w");
		if (fp == NULL)
			failedSort (fp, NULL);
	}
	for (i = 0 ; i < numTags ; ++i)
	{
		/*  Here we filter out identical tag *lines* (including search
		 *  pattern) if this is not an xref file.
		 */
		if (i == 0  ||  Option.xref  ||  strcmp (table [i], table [i-1]) != 0)
			if (mio_puts (fp, table [i]) == EOF)
				failedSort (fp, NULL);
	}
	if (toStdout)
		mio_flush (fp);
	mio_free (fp);
}

extern void internalSortTags (const boolean toStdout, MIO* fp, size_t numTags)
{
	vString *vLine = vStringNew ();
	const char *line;
	size_t i;
	int (*cmpFunc)(const void *, const void *);

	/*  Allocate a table of line pointers to be sorted.
	 */
	const size_t tableSize = numTags * sizeof (char *);
	char **const table = (char **) malloc (tableSize);  /* line pointers */
	DebugStatement ( size_t mallocSize = tableSize; )  /* cumulative total */


	cmpFunc = Option.sorted == SO_FOLDSORTED ? compareTagsFolded : compareTags;
	if (table == NULL)
		failedSort (fp, "out of memory");

	for (i = 0  ;  i < numTags  &&  ! mio_eof (fp)  ;  )
	{
		line = readLineRaw (vLine, fp);
		if (line == NULL)
		{
			if (! mio_eof (fp))
				failedSort (fp, NULL);
			break;
		}
		else if (*line == '\0'  ||  strcmp (line, "\n") == 0)
			;  /* ignore blank lines */
		else
		{
			const size_t stringSize = strlen (line) + 1;

			table [i] = (char *) malloc (stringSize);
			if (table [i] == NULL)
				failedSort (fp, "out of memory");
			DebugStatement ( mallocSize += stringSize; )
			strcpy (table [i], line);
			++i;
		}
	}
	numTags = i;
	vStringDelete (vLine);

	/*  Sort the lines.
	 */
	qsort (table, numTags, sizeof (*table), cmpFunc);

	writeSortedTags (table, numTags, toStdout);

	PrintStatus (("sort memory: %ld bytes\n", (long) mallocSize));
	for (i = 0 ; i < numTags ; ++i)
		free (table [i]);
	free (table);
}

#endif
