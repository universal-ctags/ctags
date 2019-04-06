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

static ptrArray *uwiMarkerStack;
static ptrArray *uwiCurrentMarker;
extern void uwiActivate   (void)
{
	Assert (!uwiMarkerStack);

	uugcActivate ();
	uwiMarkerStack = ptrArrayNew ((ptrArrayDeleteFunc)ptrArrayDelete);
}

extern void uwiDeactivate (void)
{
	Assert (uwiMarkerStack);
	ptrArrayDelete (uwiMarkerStack);
	uugcDeactive();
}

extern int uwiGetC ()
{
	int c;
	uugcChar *chr = uugciGetC ();

	c = chr->c;

	if (uwiCurrentMarker)
		ptrArrayAdd (uwiCurrentMarker, chr);
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
	Assert (uwiMarkerStack);

	if (uwiCurrentMarker)
		ptrArrayAdd (uwiMarkerStack, uwiCurrentMarker);

	uwiCurrentMarker = ptrArrayNew ((ptrArrayDeleteFunc)uugcDeleteC);
}

extern void uwiPopMarker (int upto)
{
	Assert (uwiCurrentMarker);

	int count = (upto < 0)? ptrArrayCount (uwiCurrentMarker): upto;
	while (count > 0)
	{
		uugcUngetC (ptrArrayLast (uwiCurrentMarker));
		ptrArrayRemoveLast (uwiCurrentMarker);
		count--;
	}

	ptrArrayDelete (uwiCurrentMarker);

	uwiCurrentMarker = NULL;
	if (ptrArrayCount (uwiMarkerStack) > 0)
	{
		uwiCurrentMarker = ptrArrayLast (uwiMarkerStack);
		ptrArrayRemoveLast (uwiMarkerStack);
	}
}

extern void	 uwiDropMaker ()
{
	uwiPopMarker  (0);
}
