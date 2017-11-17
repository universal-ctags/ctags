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
#include "vstring.h"

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
			 int defineMacroKindIndex,
		     int macroUndefRoleIndex,
		     int headerKindIndex,
		     int headerSystemRoleIndex, int headerLocalRoleIndex);

extern void cppTerminate (void);
extern void cppBeginStatement (void);
extern void cppEndStatement (void);
extern void cppUngetc (const int c);
extern void cppUngetString(const char * string,int len);
extern int cppGetc (void);
extern int cppSkipOverCComment (void);

/* notify the external parser state for the purpose of conditional
   branch choice. The CXX parser stores the block level here. */
extern void cppPushExternalParserBlock(void);
extern void cppPopExternalParserBlock(void);

#define CPP_MACRO_REPLACEMENT_FLAG_VARARGS 1
#define CPP_MACRO_REPLACEMENT_FLAG_STRINGIFY 2

typedef struct sCppMacroReplacementPartInfo {
	int parameterIndex; /* -1 if this part is a constant */
	int flags;
	vString * constant; /* not NULL only if parameterIndex != -1 */
	struct sCppMacroReplacementPartInfo * next;
} cppMacroReplacementPartInfo;

typedef struct sCppMacroInfo {
	bool hasParameterList; /* true if the macro has a trailing () */
	cppMacroReplacementPartInfo * replacements;
} cppMacroInfo;

extern const cppMacroInfo * cppFindMacro (const char *const name);

/*
* Build a replacement string for the specified macro.
* If the macro has parameters, they will be used.
* Parameters not found in the list will be assumed to be empty.
* May return NULL or equivalently an empty replacement string.
*/
extern vString * cppBuildMacroReplacement(
		const cppMacroInfo * macro,
		const char ** parameters, /* may be NULL */
		int parameterCount
	);

#endif  /* CTAGS_MAIN_GET_H */
