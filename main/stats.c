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
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include <stdio.h>

#include "entry_p.h"
#include "options_p.h"
#include "stats_p.h"

/*
*   MACROS
*/
#define plural(value)  (((unsigned long)(value) == 1L) ? "" : "s")

/*
*   DATA DEFINITIONS
*/
static struct { long files, lines, bytes; } Totals = { 0, 0, 0 };


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

extern void printTotals (const clock_t *const timeStamps, bool append, sortType sorted)
{
	const unsigned long totalTags = numTagsTotal();
	const unsigned long addedTags = numTagsAdded();

	fprintf (stderr, "%ld file%s, %ld line%s (%ld kB) scanned",
			Totals.files, plural (Totals.files),
			Totals.lines, plural (Totals.lines),
			Totals.bytes/1024L);

	const double interval = ((double) (timeStamps [1] - timeStamps [0])) /
							CLOCKS_PER_SEC;

	fprintf (stderr, " in %.01f seconds", interval);
	if (interval != (double) 0.0)
		fprintf (stderr, " (%lu kB/s)",
				(unsigned long) (Totals.bytes / interval) / 1024L);

	fputc ('\n', stderr);

	fprintf (stderr, "%lu tag%s added to tag file",
			addedTags, plural(addedTags));
	if (append)
		fprintf (stderr, " (now %lu tags)", totalTags);
	fputc ('\n', stderr);

	if (totalTags > 0  &&  sorted != SO_UNSORTED)
	{
		fprintf (stderr, "%lu tag%s sorted", totalTags, plural (totalTags));
		fprintf (stderr, " in %.02f seconds",
				((double) (timeStamps [2] - timeStamps [1])) / CLOCKS_PER_SEC);
		fputc ('\n', stderr);
	}

#ifdef DEBUG
	fprintf (stderr, "longest tag line = %lu\n",
		 (unsigned long) maxTagsLine ());
#endif
}
