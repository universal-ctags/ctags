/*
*   Copyright (c) 1998-2002, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   External interface to get.c
*/
#ifndef CTAGS_MAIN_GET_H
#define CTAGS_MAIN_GET_H

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */
#include "types.h"

/*
*   MACROS
*/
/*  Is the character valid as a character of a C identifier?
 *  VMS allows '$' in identifiers.
 */
#define cppIsident(c)  (isalnum(c) || (c) == '_' || (c) == '$')

/*  Is the character valid as the first character of a C identifier?
 *  C++ allows '~' in destructors.
 *  VMS allows '$' in identifiers.
 */
#define cppIsident1(c)  ( ((c >= 0) && (c < 0x80) && isalpha(c)) \
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

#define RoleTemplateUndef { true, "undef", "undefined" }

#define RoleTemplateSystem { true, "system", "system header" }
#define RoleTemplateLocal  { true, "local", "local header" }

/*
*   FUNCTION PROTOTYPES
*/
extern bool cppIsBraceFormat (void);
extern unsigned int cppGetDirectiveNestLevel (void);

extern void cppInit (const bool state,
		     const bool hasAtLiteralStrings,
		     const bool hasCxxRawLiteralStrings,
		     const bool hasSingleQuoteLiteralNumbers,
		     const kindOption *defineMacroKind,
		     int macroUndefRoleIndex,
		     const kindOption *headerKind,
		     int headerSystemRoleIndex, int headerLocalRoleIndex);
extern void cppTerminate (void);
extern void cppBeginStatement (void);
extern void cppEndStatement (void);
extern void cppUngetc (const int c);
extern int cppGetc (void);
extern int cppSkipOverCComment (void);

typedef struct sCppIgnoredTokenInfo {
	bool ignoreFollowingParenthesis; /* -I SOMETHING+ */
	char * replacement;              /* -I SOMETHING=REPLACEMENT */
} cppIgnoredTokenInfo;
extern const cppIgnoredTokenInfo * cppIsIgnoreToken (const char *const name);

#endif  /* CTAGS_MAIN_GET_H */
