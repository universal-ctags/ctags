/*
*   Copyright (c) 1996-2002, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains debugging functions.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include <ctype.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>

#include "debug.h"
#include "options.h"
#include "read.h"

/*
*   FUNCTION DEFINITIONS
*/

#ifdef DEBUG

extern void lineBreak (void) {}  /* provides a line-specified break point */

extern void debugPrintf (
		const enum eDebugLevels level, const char *const format, ... )
{
	va_list ap;

	va_start (ap, format);
	if (debug (level))
		vprintf (format, ap);
	fflush (stdout);
	va_end (ap);
}

extern void debugPutc (const int level, const int c)
{
	if (debug (level)  &&  c != EOF)
	{
		     if (c == STRING_SYMBOL)  printf ("\"string\"");
		else if (c == CHAR_SYMBOL)    printf ("'c'");
		else                          putchar (c);

		fflush (stdout);
	}
}

extern void debugParseNest (const boolean increase, const unsigned int level)
{
	debugPrintf (DEBUG_PARSE, "<*%snesting:%d*>", increase ? "++" : "--", level);
}

extern void debugCppNest (const boolean begin, const unsigned int level)
{
	debugPrintf (DEBUG_CPP, "<*cpp:%s level %d*>", begin ? "begin":"end", level);
}

extern void debugCppIgnore (const boolean ignore)
{
	debugPrintf (DEBUG_CPP, "<*cpp:%s ignore*>", ignore ? "begin":"end");
}

extern void debugEntry (const tagEntryInfo *const tag)
{
	const char *const scope = tag->isFileScope ? "{fs}" : "";

	if (debug (DEBUG_PARSE))
	{
		printf ("<#%s%s:%s", scope, tag->kind->name, tag->name);

		if (tag->extensionFields.scopeKind != NULL  &&
				tag->extensionFields.scopeName != NULL)
			printf (" [%s:%s]", tag->extensionFields.scopeKind->name,
					tag->extensionFields.scopeName);

		if (isFieldEnabled (FIELD_INHERITANCE) &&
				tag->extensionFields.inheritance != NULL)
			printf (" [inherits:%s]", tag->extensionFields.inheritance);

		if (isFieldEnabled (FIELD_FILE_SCOPE) &&
				tag->isFileScope && ! isInputHeaderFile ())
			printf (" [file:]");

		if (isFieldEnabled (FIELD_ACCESS) &&
				tag->extensionFields.access != NULL)
			printf (" [access:%s]", tag->extensionFields.access);

		if (isFieldEnabled (FIELD_IMPLEMENTATION) &&
				tag->extensionFields.implementation != NULL)
			printf (" [imp:%s]", tag->extensionFields.implementation);

		if (isFieldEnabled (FIELD_TYPE_REF) &&
				tag->extensionFields.typeRef [0] != NULL  &&
				tag->extensionFields.typeRef [1] != NULL)
			printf (" [%s:%s]", tag->extensionFields.typeRef [0],
					tag->extensionFields.typeRef [1]);

		printf ("#>");
		fflush (stdout);
	}
}

extern void debugAssert (const char *assertion, const char *file, unsigned int line, const char *function)
{
	fprintf(stderr, "ctags: %s:%u: %s%sAssertion `%s' failed.\n",
	        file, line,
	        function ? function : "", function ? ": " : "",
	        assertion);
	if (getInputFileName())
	{
		fprintf(stderr, "ctags: %s:%u: parsing %s:%lu as %s\n",
		        file, line,
		        getInputFileName(), getInputLineNumber(),
		        getInputLanguageName());
	}
	fflush(stderr);
	abort();
}

#endif
