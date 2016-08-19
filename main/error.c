/*
*   Copyright (c) 2002-2003, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains a lose assortment of shared functions.
*/

#include "general.h"  /* must always come first */
#include <string.h>
#include <errno.h>

#include "error.h"
#include "options.h"

#define selected(var,feature)	(((int)(var) & (int)(feature)) == (int)feature)

static errorPrintFunc errorPrinter;
static void *errorPrinterData;

extern void setErrorPrinter (errorPrintFunc printer, void *data)
{
	errorPrinter = printer;
	errorPrinterData = data;
}

extern boolean stderrDefaultErrorPrinter (const errorSelection selection,
					  const char *const format,
					  va_list ap, void *data CTAGS_ATTR_UNUSED)
{
	fprintf (stderr, "%s: %s", getExecutableName (),
		 selected (selection, WARNING) ? "Warning: " : "");
	vfprintf (stderr, format, ap);
	if (selected (selection, PERROR))
#ifdef HAVE_STRERROR
		fprintf (stderr, " : %s", strerror (errno));
#else
	perror (" ");
#endif
	fputs ("\n", stderr);

	return (selected (selection, FATAL) || Option.fatalWarnings)? TRUE: FALSE;
}

extern void error (const errorSelection selection,
		   const char *const format, ...)
{
	va_list ap;
	boolean shouldExit;

	va_start (ap, format);
	shouldExit = (* errorPrinter) (selection, format, ap, errorPrinterData);
	va_end (ap);

	if (shouldExit)
		exit (1);
}

