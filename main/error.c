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

#if HAVE_JANSSON
#include <jansson.h>
#endif

#define selected(var,feature)	(((int)(var) & (int)(feature)) == (int)feature)

static errorPrintFunc errorPrinter;
static void *errorPrinterData;

extern void setErrorPrinter (errorPrintFunc printer, void *data)
{
	errorPrinter = printer;
	errorPrinterData = data;
}

extern bool stderrDefaultErrorPrinter (const errorSelection selection,
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

	return (selected (selection, FATAL) || Option.fatalWarnings)? true: false;
}

extern void error (const errorSelection selection,
		   const char *const format, ...)
{
	va_list ap;
	bool shouldExit;

	va_start (ap, format);
	shouldExit = (* errorPrinter) (selection, format, ap, errorPrinterData);
	va_end (ap);

	if (shouldExit)
		exit (1);
}

#if HAVE_JANSSON
bool jsonErrorPrinter (const errorSelection selection, const char *const format, va_list ap, void *data)
{
  char *reason;
  vasprintf (&reason, format, ap);

  json_t *response = json_object ();
  json_object_set_new (response, "error", json_string (reason));
  if (selected (selection, WARNING))
    json_object_set_new (response, "warning", json_true ());
  if (selected (selection, FATAL))
    json_object_set_new (response, "fatal", json_true ());
  if (selected (selection, PERROR)) {
    json_object_set_new (response, "errno", json_integer (errno));
    json_object_set_new (response, "perror", json_string (strerror (errno)));
  }
  json_dumpf (response, stdout, 0);
  fprintf (stdout, "\n");

  json_decref (response);
  free (reason);

	return false;
}
#endif
