/*
*   Copyright (c) 1998-2002, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   External interface to debug.c
*/
#ifndef CTAGS_MAIN_DEBUG_H
#define CTAGS_MAIN_DEBUG_H

/*
*   Include files
*/
#include "general.h"  /* must always come first */

#include "gvars.h"
#include "types.h"
#ifdef DEBUG
# include <assert.h>
#endif

/*
*   Macros
*/

#ifdef DEBUG
# define debug(level)      ((ctags_debugLevel & (long)(level)) != 0)
# define DebugStatement(x) x
# define PrintStatus(x)    if (debug(DEBUG_STATUS)) printf x;
# ifdef NDEBUG
#  define Assert(c) do {} while(0)
#  define AssertNotReached() do {} while(0)
# else
   /* We expect cc supports c99 standard. */
#  if defined __STDC_VERSION__ && __STDC_VERSION__ >= 199901L
#   define ASSERT_FUNCTION __func__
#  else
#   define ASSERT_FUNCTION ((const char*)0)
#  endif
#  define Assert(c) ((c) ? ((void)0) : debugAssert(#c, __FILE__, __LINE__, ASSERT_FUNCTION))
#  define AssertNotReached() Assert(!"The control reaches unexpected place")
# endif
#else
# define DebugStatement(x)
# define PrintStatus(x)
# define Assert(c) do {} while(0)
# define AssertNotReached() do {} while(0)
# ifndef NDEBUG
#  define NDEBUG
# endif
#endif

#ifdef DEBUG
/* This makes valgrind report an error earlier. */
#define DISABLE_OBJPOOL
#endif
/*
*   Data declarations
*/

/*  Defines the debugging levels.
 */
enum eDebugLevels {
	DEBUG_READ   = 0x01,  /* echo raw (filtered) characters */
	DEBUG_PARSE  = 0x02,  /* echo parsing results */
	DEBUG_STATUS = 0x04,  /* echo file status information */
	DEBUG_OPTION = 0x08,  /* echo option parsing */
	DEBUG_CPP    = 0x10,  /* echo characters out of pre-processor */
	DEBUG_RAW    = 0x20   /* echo raw (filtered) characters */
};

/*
*   Function prototypes
*/
extern void lineBreak (void);
extern void debugPrintf (const enum eDebugLevels level, const char *const format, ...) CTAGS_ATTR_PRINTF (2, 3);
extern void debugPutc (const int level, const int c);
extern void debugParseNest (const bool increase, const unsigned int level);
extern void debugCppNest (const bool begin, const unsigned int level);
extern void debugCppIgnore (const bool ignore);
extern void debugEntry (const tagEntryInfo *const tag);
extern void debugAssert (const char *assertion, const char *file, unsigned int line, const char *function) attr__noreturn;

#ifdef DEBUG
#define DEBUG_INIT() debugInit()
extern void debugInit (void);
extern void debugIndent(void);
extern void debugInc(void);
extern void debugDec(void);

struct circularRefChecker;
extern struct circularRefChecker * circularRefCheckerNew (void);
extern void circularRefCheckerDestroy (struct circularRefChecker * checker);
extern int circularRefCheckerCheck (struct circularRefChecker *c, void *ptr);
extern int circularRefCheckerGetCurrent (struct circularRefChecker *c);
extern void circularRefCheckClear (struct circularRefChecker *c);

#else
#define DEBUG_INIT() do { } while(0)
#endif	/* DEBUG */
#endif  /* CTAGS_MAIN_DEBUG_H */
