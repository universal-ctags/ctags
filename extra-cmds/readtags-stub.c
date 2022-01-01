/*
*   Copyright (c) 1998-2003, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This code is derived from the main part of ctags.
*   This source code is NOT released for the public domain.
*/

/*
*   INCLUDE FILES
*/

#include "general.h"
#include "readtags-stub.h"
#include "routines.h"
#include "routines_p.h"

#include <errno.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef DEBUG

extern void debugAssert (const char *assertion, const char *file, unsigned int line, const char *function)
{
	fprintf(stderr, "readtags: %s:%u: %s%sAssertion `%s' failed.\n",
	        file, line,
	        function ? function : "", function ? ": " : "",
	        assertion);
	fflush(stderr);
	abort();
}

#endif

#define selected(var,feature)	(((int)(var) & (int)(feature)) == (int)feature)

extern bool stderrDefaultErrorPrinter (const errorSelection selection,
					  const char *const format,
					  va_list ap, void *data CTAGS_ATTR_UNUSED)
{
	fprintf (stderr, "%s: %s", getExecutableName (),
		 selected (selection, WARNING) ? "Warning: " :
		 selected (selection, NOTICE) ? "Notice: " : "");
	vfprintf (stderr, format, ap);
	if (selected (selection, PERROR))
	{
#ifdef HAVE_STRERROR
		fprintf (stderr, " : %s", strerror (errno));
#else
		perror (" ");
#endif
	}
	fputs ("\n", stderr);

	return (selected (selection, FATAL))? true: false;
}

extern void error (const errorSelection selection,
		   const char *const format, ...)
{
	va_list ap;
	bool shouldExit;

	if (selected (selection, NOTICE))
		return;

	va_start (ap, format);
	shouldExit = stderrDefaultErrorPrinter (selection, format, ap, NULL);
	va_end (ap);

	if (shouldExit)
		exit (1);
}
