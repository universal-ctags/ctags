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

#include "debug.h"
#include "mio.h"
#include "ptrarray.h"
#include "types.h"
#include "vstring.h"

/*
*   MACROS
*/
/* symbolic representations, above 0xFF not to conflict with any byte */
#define CPP_STRING_SYMBOL ('S' + 0xff)
#define CPP_CHAR_SYMBOL ('C' + 0xff)

/*
 * cppIs... macros are for the value returned from cppGetc().  Don't
 * use "char" value. Don't pass a value stored to C-string
 * (char*... or char[]) or vString.
 *
 * cppGetc() can return the value out of range of unsigned char.
 * cppGetc calls skipToEndOfString() and skipToEndOfString() internally.
 * They return STRING_SYMBOL (== 338) and CHAR_SYMBOL (== 322) in a
 * case. (cppGetc() can return EOF (== -1). However, it is not an issue
 * here.)
 *
 * is...() macros/functions defined in ctype.h can handle the value of
 * an unsigned char or EOF; we cannot pass STRING_SYMBOL or CHAR_SYMBOL
 * returned from cppGetc().
 *
 * Depending on the platform, isalpha(338) returns different value.
 * As far as Fedora22, it returns 0. On Windows 2010, it returns 1.
 *
 * So, we need cppIs... macros.
 * cppIs... macros considers STRING_SYMBOL and CHAR_SYMBOL */

#define cppIsascii(c) ((c >= 0) && (c < 0x80))
/* isascii is not portable enough. */

/*  Is the character valid as a character of a C identifier?
 *  VMS allows '$' in identifiers.
 */
#define cppIsalnum(c)  (cppIsascii(c) && isalnum(c))
#define cppIsident(c)  (cppIsalnum(c)					\
						|| (c) == '_' || (c) == '$')

/*  Is the character valid as the first character of a C identifier?
 *  C++ allows '~' in destructors.
 *  VMS allows '$' in identifiers.
 */
#define cppIsalpha(c)   (cppIsascii(c) && isalpha(c))
#define cppIsident1(c)  (cppIsalpha(c)					\
						  || (c) == '_' || (c) == '~' || (c) == '$')

#define cppIsspace(c)   (cppIsascii(c) && isspace(c))
#define cppIsdigit(c)   (cppIsascii(c) && isdigit(c))


#define RoleTemplateUndef { true, "undef", "undefined" }
#define RoleTemplateCondition { false, "condition", "used in part of #if/#ifdef/#elif conditions" }

#define RoleTemplateSystem { true, "system", "system header" }
#define RoleTemplateLocal  { true, "local", "local header" }

typedef enum {
	CPREPRO_MACRO, CPREPRO_HEADER, CPREPRO_PARAM,
} cPreProKind;

/*
*   FUNCTION PROTOTYPES
*/
extern bool cppIsBraceFormat (void);
extern unsigned int cppGetDirectiveNestLevel (void);

/* Don't forget to set useCort true in your parser.
 * The corkQueue is needed to capture macro parameters.
 */
extern void cppInit (const bool state,
		     const bool hasAtLiteralStrings,
		     const bool hasCxxRawLiteralStrings,
		     const bool hasSingleQuoteLiteralNumbers,
		     int defineMacroKindIndex,
		     int macroUndefRoleIndex,
		     int macroConditionRoleIndex,
		     int headerKindIndex,
		     int headerSystemRoleIndex, int headerLocalRoleIndex,
		     int macroParamKindIndex,
		     int macrodefFieldIndex);

extern void cppTerminate (void);
extern void cppBeginStatement (void);
extern void cppEndStatement (void);
extern void cppUngetc (const int c);
extern int cppUngetBufferSize(void);
extern void cppUngetString(const char * string,int len);
extern int cppGetc (void);
extern const vString * cppGetLastCharOrStringContents (void);

/*
 * Replacement for vStringPut that can handle c > 0xff
 */
extern void cppVStringPut (vString * string, const int c);

/* Notify the external parser state for the purpose of conditional
 * branch choice. The CXX parser stores the block level here. */
extern void cppPushExternalParserBlock(void);
extern void cppPopExternalParserBlock(void);

/*
 * Macro expander related declarations
 */

/* We stop applying macro replacements if the unget buffer gets too big
   as it is a sign of recursive macro expansion */
#define CPP_MAXIMUM_UNGET_BUFFER_SIZE_FOR_MACRO_REPLACEMENTS 65536

/* We stop applying macro replacements if a macro is used so many
   times in a recursive macro expansion. */
#define CPP_MAXIMUM_MACRO_USE_COUNT 8

typedef struct sCppMacroReplacementPartInfo cppMacroReplacementPartInfo;

typedef struct sCppMacroInfo {
	char *name;			/* the name of macro. Useful for debugging. */
	bool hasParameterList; /* true if the macro has a trailing () */
	cppMacroReplacementPartInfo * replacements;
	int useCount;
	struct sCppMacroInfo * next;
} cppMacroInfo;

struct sCppMacroTokens;
typedef struct sCppMacroTokens cppMacroTokens;

struct sCppMacroArg;
typedef struct sCppMacroArg cppMacroArg;

/* Expanding macros
 * ================
 *
 * Build a replacement tokens for the specified macro.
 * If the macro has parameters, they will be used.
 * Parameters not found in the list will be assumed to be empty.
 * May return NULL or equivalently an empty replacement tokens.
 *
 * Data types
 * ----------
 * + cppFindMacro:
 *   Macro definition.
 * + cppMacroArg:
 *   Argument passed to a macro.
 * + cppMacroTokens:
 *   The result of macro expansion. You can push back this object
 *   to the input stream.
 *
 * Functions
 * ---------
 * + cppFindMacro()
 * Find a macro definition for a given macro.
 * Macros defined with -D option are stored to a database behind this function.
 *
 * + cppMacroArgNew()
 * Make an object representing a macro argument.
 * STR passed to cppMacroArgNew is freed when deleting it with
 * cppMacroArgDelete() if FREE_STR_WHEN_DELETING is true.
 *
 * + cppExpandMacro()
 * Expand the macro with arguments.
 * MACRO is a cppMacroInfo object returned from cppFindMacro.
 * ARGS is a ptrArray containing cppMacroArg objects.
 * LINENUMBER and FILEPOSITION are the place where the macro is expanded.
 * Pass the returned value to cppUngetMacroTokens.
 * cppExpandMacro allocates the cppMacroTokens. It is deleted in
 *
 * + cppExpandMacroAsNewString()
 * A wrapper of cppExpandMacro. This function returns
 * a vString instead of returning a cppMacroTokens object.
 *
 */
extern cppMacroInfo * cppFindMacro (const char *const name);
extern void cppUngetMacroTokens (cppMacroTokens *tokens);

extern cppMacroArg *cppMacroArgNew (const char *str, bool free_str_when_deleting,
									unsigned long lineNumber, MIOPos filePosition);
extern void cppMacroArgDelete (void *macroArg);
extern cppMacroTokens *cppExpandMacro (cppMacroInfo * macro, const ptrArray *args,
									   unsigned long lineNumber, MIOPos filePosition);
extern vString *cppExpandMacroAsNewString (cppMacroInfo * macro, const ptrArray *args);

extern unsigned long cppGetInputLineNumber (void);
extern MIOPos cppGetInputFilePosition (void);

#ifdef DEBUG
extern vString *cppFlattenMacroTokensToNewString (cppMacroTokens *tokens);
extern void cppDebugPutc (const int level, const int c);
#endif

#endif  /* CTAGS_MAIN_GET_H */
