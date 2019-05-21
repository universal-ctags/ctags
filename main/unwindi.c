/*
 *
 *  Copyright (c) 2019, Red Hat, Inc.
 *  Copyright (c) 2019, Masatake YAMATO
 *
 *  Author: Masatake YAMATO <yamato@redhat.com>
 *
 *   This source code is released for free distribution under the terms of the
 *   GNU General Public License version 2 or (at your option) any later version.
 *
 *   Unwindable input stream / Unlimited ungetc
 *
 */

/*
*   INCLUDE FILES
*/

#include "general.h"

#include "debug.h"
#include "gcc-attr.h"
#include "inline.h"
#include "mio.h"
#include "objpool.h"
#include "ptrarray.h"
#include "read.h"
#include "routines.h"
#include "trashbox.h"
#include "unwindi.h"

#include <string.h>

typedef struct sUugcChar {
	int c;
	/* lineNumber before reading the char (frontLineNumber).
	 * The lineNumber after reading the char (rearLineNumber) can be calculated:
	 * If the char is \n, rearLineNumber is frontLineNumber + 1.
	 * If the char is not, rearLineNumber is the same as frontLineNumber. */
	unsigned long lineNumber;
} uugcChar;


static ptrArray *uugcInputFile;
static uugcChar *uugcCurrentChar;
static objPool  *uugcCharPool;

static struct sUwiStats uwiStats;

static void deleteChar (void *c)
{
	eFree (c);
}

static void* newChar (void *data CTAGS_ATTR_UNUSED)
{
	return xMalloc (1, uugcChar);
}

CTAGS_INLINE void uugcDeleteC (uugcChar *c)
{
	if (c == uugcCurrentChar)
		uugcCurrentChar = NULL;

	objPoolPut (uugcCharPool, c);
}

static void uugcActivate (void)
{
	Assert (!uugcInputFile);
	Assert (!uugcCurrentChar);

	if (uugcCharPool == NULL)
	{
		uugcCharPool = objPoolNew(256,
								  newChar,
								  deleteChar,
								  NULL,
								  NULL);
		DEFAULT_TRASH_BOX(uugcCharPool, objPoolDelete);
	}

	uugcInputFile = ptrArrayNew ((ptrArrayDeleteFunc)uugcDeleteC);
}

static void uugcDeactive(void)
{
	Assert (uugcInputFile);
	ptrArrayDelete (uugcInputFile);
	uugcInputFile = NULL;
	uugcCurrentChar = NULL;
}

CTAGS_INLINE uugcChar *uugcNewC (int chr, unsigned long ln)
{
	Assert (uugcCharPool);

	uugcChar *c = objPoolGet (uugcCharPool);
	c->c = chr;
	c->lineNumber = ln;
	return c;
}

CTAGS_INLINE uugcChar *uugciGetC (void)
{
	uugcChar *c;

	Assert (uugcInputFile);

	if (ptrArrayCount (uugcInputFile) > 0)
	{
		c = ptrArrayLast (uugcInputFile);
		ptrArrayRemoveLast (uugcInputFile);
	}
	else
	{
		unsigned long lineNumber = getInputLineNumber ();
		int chr = getcFromInputFile();
		c = uugcNewC (chr, lineNumber);
	}

	uugcCurrentChar = c;
	return uugcCurrentChar;
}

CTAGS_INLINE void uugcUngetC (uugcChar *c)
{
	uugcCurrentChar = NULL;

	if (c->c == EOF)
	{
		ptrArrayClear (uugcInputFile);
		uugcDeleteC (c);
		return;
	}

	ptrArrayAdd (uugcInputFile, c);
}

CTAGS_INLINE void uugcInjectC (int chr)
{
	if (chr == EOF)
		return;

	uugcChar *lastc = NULL;
	if (ptrArrayCount (uugcInputFile) > 0)
		lastc = ptrArrayLast (uugcInputFile);

	unsigned long lineNumber;
	if (lastc)
	{
		if (chr == '\n' && lastc->lineNumber > 0)
			lineNumber = lastc->lineNumber - 1;
		else
			lineNumber = lastc->lineNumber;
	}
	else
	{
		lineNumber = getInputLineNumber ();
		if (chr == '\n')
			lineNumber--;
	}

	uugcChar *c = uugcNewC(chr, lineNumber);
	uugcUngetC (c);
}

CTAGS_INLINE long uugcGetLineNumber ()
{
	Assert (uugcInputFile);

	if (uugcCurrentChar)
	{
		unsigned long ln;
		if (uugcCurrentChar->c == '\n')
			ln = uugcCurrentChar->lineNumber + 1;
		else
			ln = uugcCurrentChar->lineNumber;
		return ln;
	}
	else if (ptrArrayCount (uugcInputFile) > 0)
	{
		uugcChar *c = ptrArrayLast (uugcInputFile);
		return c->lineNumber;
	}
	else
		return getInputLineNumber ();
}

CTAGS_INLINE MIOPos uugcGetFilePosition (void)
{
	if (uugcCurrentChar)
	{
		unsigned long ln;
		if (uugcCurrentChar->c == '\n')
			ln = uugcCurrentChar->lineNumber + 1;
		else
			ln = uugcCurrentChar->lineNumber;
		return getInputFilePositionForLine (ln);
	}
	else if (ptrArrayCount (uugcInputFile) > 0)
	{
		uugcChar *c = ptrArrayLast (uugcInputFile);
		return getInputFilePositionForLine (c->lineNumber);
	}
	else
		return getInputFilePosition ();
}

static ptrArray *uwiBuffer;
static unsigned int *uwiMarkerStack;
static unsigned int uwiMarkerStackLength;
static unsigned int *uwiCurrentMarker;

extern void uwiActivate (unsigned int stackLength)
{
	Assert (stackLength > 0);

	uugcActivate ();
	uwiBuffer = ptrArrayNew ((ptrArrayDeleteFunc)uugcDeleteC);
	uwiMarkerStackLength = stackLength;
	uwiMarkerStack = xMalloc (stackLength, unsigned int);
	uwiCurrentMarker = NULL;

	uwiStatsInit (&uwiStats);
}

extern void uwiDeactivate (struct sUwiStats *statsToBeUpdated)
{
	Assert (uwiBuffer);
	Assert (uwiMarkerStack);

	if (statsToBeUpdated)
	{
		if (statsToBeUpdated->maxLength < uwiStats.maxLength)
			statsToBeUpdated->maxLength = uwiStats.maxLength;
		if (!statsToBeUpdated->overflow)
			statsToBeUpdated->overflow = uwiStats.overflow;
		if (!statsToBeUpdated->underflow)
			statsToBeUpdated->underflow = uwiStats.underflow;
	}

	ptrArrayDelete (uwiBuffer);
	eFree (uwiMarkerStack);
	uwiBuffer = NULL;
	uwiMarkerStack = NULL;
	uwiMarkerStackLength = 0;
	uugcDeactive();
}

extern int uwiGetC ()
{
	int c;
	uugcChar *chr = uugciGetC ();

	c = chr->c;

	if (uwiCurrentMarker)
	{
		*uwiCurrentMarker += 1;
		ptrArrayAdd (uwiBuffer, chr);
	}
	else
	{
		uugcCurrentChar = NULL;
		uugcDeleteC (chr);
	}

	return c;
}

extern void uwiUngetC (int c)
{
	Assert (!uwiCurrentMarker);
	uugcInjectC (c);
}

extern unsigned long uwiGetLineNumber (void)
{
	return uugcGetLineNumber ();
}

extern MIOPos uwiGetFilePosition (void)
{
	return uugcGetFilePosition ();
}

extern void uwiPushMarker (void)
{

	if (uwiStats.maxLength < (uwiCurrentMarker - uwiMarkerStack) + 1)
		uwiStats.maxLength = (uwiCurrentMarker - uwiMarkerStack) + 1;

	if (uwiCurrentMarker - uwiMarkerStack >= ( uwiMarkerStackLength - 1 )) {
		error (WARNING,
			"trying to add too many markers during parsing: %s "
			"(this is a bug, please consider filing an issue)", getInputFileName());
		uwiCurrentMarker = NULL;
		uwiStats.overflow = true;
	}

	if (uwiCurrentMarker) uwiCurrentMarker++;
	else uwiCurrentMarker = uwiMarkerStack;

	*uwiCurrentMarker = 0;
}

extern void uwiPopMarker (const int upto, const bool revertChars)
{
	if (uwiCurrentMarker - uwiMarkerStack < 0) {
		error (WARNING,
				"trying to drop too many markers during parsing: %s "
				"(this is a bug, please consider filing an issue)", getInputFileName());

		uwiCurrentMarker = NULL;
		uwiStats.underflow = true;
		return;
	}

	uwiClearMarker (upto, revertChars);

	if (uwiCurrentMarker == uwiMarkerStack) uwiCurrentMarker = NULL;
	else uwiCurrentMarker--;
}

extern void uwiClearMarker (const int upto, const bool revertChars)
{
	Assert (uwiCurrentMarker);
	int count = (upto <= 0)? *uwiCurrentMarker : upto;
	void (*charHandler)(uugcChar *) = revertChars ? uugcUngetC : uugcDeleteC;

	while (count-- > 0)
	{
		charHandler (ptrArrayLast (uwiBuffer));
		ptrArrayRemoveLast (uwiBuffer);
		*uwiCurrentMarker -= 1;
	}
}

extern void uwiDropMaker ()
{
	uwiPopMarker (0, false);
}

extern void uwiStatsInit (struct sUwiStats *stats)
{
	memset (stats, 0, sizeof (*stats));
}

extern void uwiStatsPrint (struct sUwiStats *stats)
{
	fprintf(stderr, "Unwinding the longest input stream stack usage: %d\n",
			stats->maxLength);
	fprintf(stderr, "Unwinding input stream stack overflow incidence: %s\n",
			stats->overflow? "yes": "no");
	fprintf(stderr, "Unwinding input stream stack underflow incidence: %s\n",
			stats->underflow? "yes": "no");
}
