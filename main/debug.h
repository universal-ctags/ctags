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

#ifdef DEBUG
# include <assert.h>
#endif
#include "entry.h"

/*
*   Macros
*/

#ifdef DEBUG
# define debug(level)      ((Option.debugLevel & (long)(level)) != 0)
# define DebugStatement(x) x
# define PrintStatus(x)    if (debug(DEBUG_STATUS)) printf x;
# ifdef NDEBUG
#  define Assert(c)
#  define AssertNotReached()
# else
   /* based on glibc's assert.h __ASSERT_FUNCTION */
#  if defined (__GNUC__) && (__GNUC__ > 2 || (__GNUC__ == 2 && __GNUC_MINOR__ >= 4))
#   define ASSERT_FUNCTION __PRETTY_FUNCTION__
#  elif defined __STDC_VERSION__ && __STDC_VERSION__ >= 199901L
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
# define Assert(c)
# define AssertNotReached()
# ifndef NDEBUG
#  define NDEBUG
# endif
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
extern void debugParseNest (const boolean increase, const unsigned int level);
extern void debugCppNest (const boolean begin, const unsigned int level);
extern void debugCppIgnore (const boolean ignore);
extern void debugEntry (const tagEntryInfo *const tag);
extern void debugAssert (const char *assertion, const char *file, unsigned int line, const char *function) attr__noreturn;

#endif  /* CTAGS_MAIN_DEBUG_H */
