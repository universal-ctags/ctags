/*
*   Copyright (c) 1998-2002, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   Provides the external interface for resizeable strings.
*/
#ifndef CTAGS_MAIN_VSTRING_H
#define CTAGS_MAIN_VSTRING_H

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include <stdlib.h>  /* to define size_t */

#include <stdio.h>

#include "debug.h"
#include "inline.h"

/*
*   MACROS
*/

#define vStringValue(vs)      ((vs)->buffer)
#define vStringChar(vs,i)     ((vs)->buffer[i])
#define vStringLast(vs)       ((vs)->buffer[(vs)->length - 1])
#define vStringLength(vs)     ((vs)->length)
#define vStringIsEmpty(vs)    ((vs)->length == 0)
#define vStringSize(vs)       ((vs)->size)
#define vStringLower(vs)      toLowerString((vs)->buffer)
#define vStringUpper(vs)      toUpperString((vs)->buffer)
#define vStringClear(string) \
	do { \
		vString *vStringClear_s = (string); \
		vStringClear_s->length = 0; \
		vStringClear_s->buffer[0] = '\0'; \
	} while (false)

/*
*   DATA DECLARATIONS
*/

typedef struct sVString {
	size_t  length;  /* size of buffer used */
	size_t  size;    /* allocated size of buffer */
	char   *buffer;  /* location of buffer */
} vString;

/*
*   FUNCTION PROTOTYPES
*/
extern void vStringResize (vString *const string, const size_t newSize);
extern vString *vStringNew (void);
extern void vStringDelete (vString *const string);
extern bool vStringStripNewline (vString *const string);
extern void vStringStripLeading (vString *const string);
extern void vStringChop (vString *const string);
extern void vStringStripTrailing (vString *const string);
extern void vStringCat (vString *const string, const vString *const s);
extern void vStringCatS (vString *const string, const char *const s);
extern void vStringNCat (vString *const string, const vString *const s, const size_t length);

/* vStringNCatS calls strnlen(S,LENGTH) thought it takes LENGTH because
 * the handle the case that the length of S is smaller than LENGTH.
 *
 * In the case a caller knows the length equals to or is greater than LENGTH,
 * calling strlen is just overhead. vStringNCatSUnsafe doesn't call strlen. */
extern void vStringNCatS (vString *const string, const char *const s, const size_t length);
extern void vStringNCatSUnsafe (vString *const string, const char *const s, const size_t length);

extern vString *vStringNewCopy (const vString *const string);
extern vString *vStringNewInit (const char *const s);
extern vString *vStringNewNInit (const char *const s, const size_t length);
extern void vStringCopy (vString *const string, const vString *const s);
extern void vStringCopyS (vString *const string, const char *const s);
extern void vStringNCopy (vString *const string, const vString *const s, const size_t length);
extern void vStringNCopyS (vString *const string, const char *const s, const size_t length);
extern void vStringNCopySUnsafe (vString *const string, const char *const s, const size_t length);
extern void vStringCopyToLower (vString *const dest, const vString *const src);
extern void vStringSetLength (vString *const string);
extern void vStringTruncate (vString *const string, const size_t length);
#define vStringTruncateTrailing vStringTruncate
extern void vStringTruncateLeading (vString *const string, const size_t length);
extern void vStringTranslate(vString *const string, char fromC, char toC);

extern vString *vStringNewOrClear (vString *const string);
extern vString *vStringNewOrClearWithAutoRelease (vString *const string);

extern vString *vStringNewOwn (char *s);
extern char    *vStringDeleteUnwrap (vString *const string);
extern char    *vStringStrdup (const vString *const string);

extern void vStringCatSWithEscaping (vString* b, const char *s);
extern void vStringCatSWithEscapingAsPattern (vString *output, const char* input);

/*
*   INLINE FUNCTIONS
*/
CTAGS_INLINE void vStringPutNewlinAgainUnsafe (vString *const string)
{
	string->buffer [string->length++] = '\n';
}

CTAGS_INLINE void vStringPutImpl (vString *const string, const int c)
{
	/* verify the given character is an unsigned char value */
	Assert (c >= 0 && c <= 0xff);

	if (string->length + 1 == string->size)  /*  check for buffer overflow */
		vStringResize (string, string->size * 2);

	string->buffer [string->length] = (char) c;
	if (c != '\0')
		string->buffer [++string->length] = '\0';
}

#define vStringPut(s, c) (sizeof(c) == sizeof(char) \
						  ? vStringPutImpl((s), (unsigned char) (c)) \
						  : vStringPutImpl((s), (c)))

CTAGS_INLINE bool vStringPutWithLimitImpl (vString *const string, const int c,
										   unsigned int maxlen)
{
	if (vStringLength (string) < maxlen || maxlen == 0)
	{
		vStringPut (string, c);
		return true;
	}
	return false;
}

#define vStringPutWithLimit(s, c, l) \
	(sizeof(c) == sizeof(char) \
	 ? vStringPutWithLimitImpl((s), (unsigned char) (c), (l)) \
	 : vStringPutWithLimitImpl((s), (c), (l)))

CTAGS_INLINE void vStringAccumulate (vString *accumulator, vString *string)
{
	vStringCat (accumulator, string);
	vStringClear (string);
}

#define vStringPutUnlessEmpty(s, c)				\
	do {										\
		if (!vStringIsEmpty(s))					\
			vStringPut ((s), (c));				\
	} while (0)

#define vStringJoin(string, c, s) do {			\
		vStringPutUnlessEmpty ((string), (c));	\
		vStringCat((string), (s));				\
	} while (0)

#define vStringJoinS(string, c, s) do {			\
		vStringPutUnlessEmpty ((string), (c));	\
		vStringCatS((string), (s));				\
	} while (0)

#endif  /* CTAGS_MAIN_VSTRING_H */
