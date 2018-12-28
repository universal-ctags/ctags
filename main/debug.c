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
#include <string.h>

#include "debug.h"
#include "entry_p.h"
#include "options.h"
#include "parse_p.h"
#include "read.h"
#include "read_p.h"

/*
*   FUNCTION DEFINITIONS
*/

#ifdef DEBUG
#include "htable.h"


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

extern void debugParseNest (const bool increase, const unsigned int level)
{
	debugPrintf (DEBUG_PARSE, "<*%snesting:%d*>", increase ? "++" : "--", level);
}

extern void debugCppNest (const bool begin, const unsigned int level)
{
	debugPrintf (DEBUG_CPP, "<*cpp:%s level %d*>", begin ? "begin":"end", level);
}

extern void debugCppIgnore (const bool ignore)
{
	debugPrintf (DEBUG_CPP, "<*cpp:%s ignore*>", ignore ? "begin":"end");
}

extern void debugEntry (const tagEntryInfo *const tag)
{
	const char *const scope = tag->isFileScope ? "{fs}" : "";

	if (debug (DEBUG_PARSE))
	{
		langType lang = (tag->extensionFields.scopeLangType == LANG_AUTO)
			? tag->langType
			: tag->extensionFields.scopeLangType;
		kindDefinition *scopeKindDef = getLanguageKind(lang,
													   tag->extensionFields.scopeKindIndex);
		printf ("<#%s%s:%s", scope, getTagKindName(tag), tag->name);

		if (tag->extensionFields.scopeKindIndex != KIND_GHOST_INDEX  &&
				tag->extensionFields.scopeName != NULL)
			printf (" [%s:%s]", scopeKindDef->name,
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

static int debugScopeDepth;
#define DEBUG_INDENT_UNIT 4

static char debugPrefix[DEBUG_INDENT_UNIT + 1];

extern void debugInit (void)
{
	memset(debugPrefix, ' ', DEBUG_INDENT_UNIT);
	debugPrefix[DEBUG_INDENT_UNIT] = '\0';
}

extern void debugIndent(void)
{
	for(int i=0;i< debugScopeDepth;i++)
		fputs(debugPrefix, stderr);
}

extern void debugInc(void)
{
	debugScopeDepth++;
}

extern void debugDec(void)
{
	debugScopeDepth--;
	if(debugScopeDepth < 0)
		debugScopeDepth = 0;
}



struct circularRefChecker {
	hashTable *visitTable;
	int counter;
};

extern void circularRefCheckerDestroy (struct circularRefChecker * checker)
{
	hashTableDelete (checker->visitTable);
	checker->visitTable = NULL;
	eFree (checker);
}

extern struct circularRefChecker * circularRefCheckerNew (void)
{
	Assert (sizeof(void *) >= sizeof(int));

	struct circularRefChecker *c = xMalloc (1, struct circularRefChecker);

	c->visitTable = hashTableNew (17, hashPtrhash, hashPtreq, NULL, NULL);
	c->counter = 0;

	return c;
}

extern int circularRefCheckerCheck (struct circularRefChecker *c, void *ptr)
{
	union conv {
		int i;
		void *ptr;
	} v;

	v.ptr = hashTableGetItem(c->visitTable, ptr);
	if (v.ptr)
		return v.i;
	else
	{
		v.i = ++c->counter;
		hashTablePutItem (c->visitTable, ptr, v.ptr);
		return 0;
	}
}

extern int circularRefCheckerGetCurrent (struct circularRefChecker *c)
{
	return c->counter;
}

extern void circularRefCheckClear (struct circularRefChecker *c)
{
	hashTableClear (c->visitTable);
	c->counter = 0;
}

#endif
