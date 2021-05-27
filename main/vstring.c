/*
*   Copyright (c) 1998-2002, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions supporting resizeable strings.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include <limits.h>  /* to define INT_MAX */
#include <string.h>
#include <ctype.h>

#include "debug.h"
#include "routines.h"
#include "vstring.h"
#include "trashbox.h"

/*
*   DATA DEFINITIONS
*/
static const size_t vStringInitialSize = 32;

/*
*   FUNCTION DEFINITIONS
*/

/*
*   External interface
*/

extern void vStringResize (vString *const string, const size_t newSize)
{
	size_t size = vStringInitialSize;

	while (size < newSize)
		size *= 2;

	if (size > string->size)
	{
		string->size = size;
		string->buffer = xRealloc (string->buffer, size, char);
	}
}

extern void vStringTruncate (vString *const string, const size_t length)
{
	Assert (length <= string->length);
	string->length = length;
	string->buffer[string->length] = '\0';
	DebugStatement ( memset (string->buffer + string->length, 0,
	                         string->size - string->length); )
}

extern void vStringDelete (vString *const string)
{
	if (string != NULL)
	{
		if (string->buffer != NULL)
			eFree (string->buffer);
		eFree (string);
	}
}

extern vString *vStringNew (void)
{
	vString *const string = xMalloc (1, vString);

	string->length = 0;
	string->size   = vStringInitialSize;
	string->buffer = xMalloc (string->size, char);

	vStringClear (string);

	return string;
}

extern vString *vStringNewCopy (const vString *const string)
{
	vString *vs = vStringNew ();
	vStringCatS (vs, string->buffer);
	return vs;
}

extern vString *vStringNewInit (const char *const s)
{
	vString *vs = vStringNew ();
	vStringCatS (vs, s);
	return vs;
}

extern vString *vStringNewNInit (const char *const s, const size_t length)
{
	vString *vs = vStringNew ();
	vStringNCatS (vs, s, length);
	return vs;
}

static void stringCat (
		vString *const string, const char *const s, const size_t length)
{
	if (string->length + length + 1 > string->size)
		vStringResize (string, string->length + length + 1);

	memcpy (string->buffer + string->length, s, length);
	string->length += length;
	vStringPut (string, '\0');
}

extern void vStringNCat (
		vString *const string, const vString *const s, const size_t length)
{
	size_t len = vStringLength (s);

	len = len < length ? len: length;
	stringCat (string, s->buffer, len);
}

extern void vStringNCatS (
		vString *const string, const char *const s, const size_t length)
{
	size_t len = strlen (s);

	len = len < length ? len : length;
	stringCat (string, s, len);
}

extern void vStringNCatSUnsafe (
		vString *const string, const char *const s, const size_t length)
{
	stringCat (string, s, length);
}

extern void vStringCat (vString *const string, const vString *const s)
{
	size_t len = vStringLength (s);

	stringCat (string, s->buffer, len);
}

extern void vStringCatS (vString *const string, const char *const s)
{
	size_t len = strlen (s);

	stringCat (string, s, len);
}

/*  Strip trailing newline from string.
 */
extern bool vStringStripNewline (vString *const string)
{
	const size_t final = string->length - 1;

	if (string->length == 0)
		return false;

	if (string->buffer [final] == '\n')
	{
		string->buffer [final] = '\0';
		string->length--;
		return true;
	}

	return false;
}

/*  Strip leading white space from string.
 */
extern void vStringStripLeading (vString *const string)
{
	size_t n = 0;

	while (n < string->length && isspace ((int) string->buffer [n]))
		n++;
	if (n > 0)
	{
		memmove (string->buffer, string->buffer + n, string->length - n);
		vStringTruncate (string, string->length - n);
	}
}

/*  Strip trailing white space from string.
 */
extern void vStringStripTrailing (vString *const string)
{
	while (string->length > 0 &&
		   isspace ((int) string->buffer [string->length - 1]))
	{
		string->length--;
		string->buffer [string->length] = '\0';
	}
}

/*  Chop last character from string.
 */
extern void vStringChop (vString *const string)
{
	if (string->length > 0)
	{
		--string->length;
		string->buffer [string->length] = '\0';
	}
}

extern void vStringCopy (vString *const string, const vString *const s)
{
	vStringClear (string);
	vStringCat (string, s);
}

extern void vStringCopyS (vString *const string, const char *const s)
{
	vStringClear (string);
	vStringCatS (string, s);
}

extern void vStringNCopy (
		vString *const string, const vString *const s, const size_t length)
{
	vStringClear (string);
	vStringNCat (string, s, length);
}

extern void vStringNCopyS (
		vString *const string, const char *const s, const size_t length)
{
	vStringClear (string);
	vStringNCatS (string, s, length);
}

extern void vStringCopyToLower (vString *const dest, const vString *const src)
{
	const size_t length = src->length;
	const char *s = src->buffer;
	char *d;
	size_t i;

	if (dest->size < src->size)
		vStringResize (dest, src->size);
	d = dest->buffer;
	for (i = 0  ;  i < length  ;  ++i)
	{
		int c = s [i];

		d [i] = tolower (c);
	}
	d [i] = '\0';
}

extern void vStringSetLength (vString *const string)
{
	string->length = strlen (string->buffer);
}

extern vString *vStringNewOwn (char *s)
{
	vString *r;

	r = vStringNewInit (s);
	eFree (s);

	return r;
}

extern char    *vStringDeleteUnwrap       (vString *const string)
{
	char *buffer = NULL;


	if (string != NULL)
	{
		buffer = string->buffer;
		string->buffer = NULL;

		string->size = 0;
		string->length = 0;

		eFree (string);
	}

	return buffer;
}

extern char    *vStringStrdup (const vString *const string)
{
	char *str = xMalloc (vStringLength(string) + 1, char);
	str[vStringLength(string)] = '\0';
	memcpy (str, string->buffer, vStringLength(string));
	return str;
}

static char valueToXDigit (int v)
{
	Assert (v >= 0 && v <= 0xF);

	if (v >= 0xA)
		return 'A' + (v - 0xA);
	else
		return '0' + v;
}

extern void vStringCatSWithEscaping (vString* b, const char *s)
{
	for(; *s; s++)
	{
		int c = *s;

		/* escape control characters (incl. \t) */
		if ((c > 0x00 && c <= 0x1F) || c == 0x7F || c == '\\')
		{
			vStringPut (b, '\\');

			switch (c)
			{
				/* use a short form for known escapes */
			case '\a':
				c = 'a'; break;
			case '\b':
				c = 'b'; break;
			case '\t':
				c = 't'; break;
			case '\n':
				c = 'n'; break;
			case '\v':
				c = 'v'; break;
			case '\f':
				c = 'f'; break;
			case '\r':
				c = 'r'; break;
			case '\\':
				c = '\\'; break;
			default:
				vStringPut (b, 'x');
				vStringPut (b, valueToXDigit ((c & 0xF0) >> 4));
				vStringPut (b, valueToXDigit (c & 0x0F));
				continue;
			}
		}
		vStringPut (b, c);
	}
}

extern void vStringCatSWithEscapingAsPattern (vString *output, const char* input)
{
	while (*input)
	{
		switch (*input)
		{
		case '\\':
			vStringPut(output, '\\');
			vStringPut(output, '\\');
			break;
		case '/':
			vStringPut(output, '\\');
			vStringPut(output, '/');
			break;
		default:
			vStringPut(output, *input);
			break;

		}
		input++;
	}
}

extern vString *vStringNewOrClear (vString *const string)
{
	if (string)
	{
		vStringClear (string);
		return string;
	}
	else
		return vStringNew ();
}

extern vString *vStringNewOrClearWithAutoRelease (vString *const string)
{
	vString *r;

	bool autoRelease = false;
	if (!string)
		autoRelease = true;

	r = vStringNewOrClear(string);
	if (autoRelease)
		DEFAULT_TRASH_BOX(r, vStringDelete);

	return r;
}

extern void vStringTranslate(vString *const string, char fromC, char toC)
{
	for (unsigned int i = 0; i < vStringLength(string); i++)
	{
		if (string->buffer[i] == fromC)
			string->buffer[i] = toC;
	}
}
