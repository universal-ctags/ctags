/*
*   Copyright (c) 1998-2002, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   External interface to get.c
*/
#ifndef _GET_H
#define _GET_H

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include "ctags.h"  /* to define langType */

/*
*   MACROS
*/
/*  Is the character valid as a character of a C identifier?
 *  VMS allows '$' in identifiers.
 */
#define isident(c)  (isalnum(c) || (c) == '_' || (c) == '$')

/*  Is the character valid as the first character of a C identifier?
 *  C++ allows '~' in destructors.
 *  VMS allows '$' in identifiers.
 */
#define isident1(c)  ( ((c >= 0) && (c < 0x80) && isalpha(c)) \
		       || (c) == '_' || (c) == '~' || (c) == '$')
/* NOTE about isident1 profitability

   Doing the same as isascii before passing value to isalpha
   ----------------------------------------------------------
   cppGetc() can return the value out of range of char.
   cppGetc calls skipToEndOfString and skipToEndOfString can
   return STRING_SYMBOL(== 338).

   Depending on the platform, isalpha(338) returns different value .
   As far as Fedora22, it returns 0. On Windows 2010, it returns 1.

   man page on Fedora 22 says:

       These functions check whether c, which must have the value of an
       unsigned char or EOF, falls into a certain character class
       according to the specified locale.

   isascii is for suitable to verify the range of input. However, it
   is not portable enough. */

/*
*   FUNCTION PROTOTYPES
*/
extern boolean isBraceFormat (void);
extern unsigned int getDirectiveNestLevel (void);

struct sKindOption;
extern void cppInit (const boolean state,
		     const boolean hasAtLiteralStrings,
		     const boolean hasSingleQuoteLiteralNumbers,
		     const struct sKindOption *defineMacroKind,
		     const struct sKindOption *headerKind);
extern void cppTerminate (void);
extern void cppBeginStatement (void);
extern void cppEndStatement (void);
extern void cppUngetc (const int c);
extern int cppGetc (void);
extern int skipOverCComment (void);

#endif  /* _GET_H */

/* vi:set tabstop=4 shiftwidth=4: */
