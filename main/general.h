/*
*   Copyright (c) 1998-2003, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   Provides the general (non-ctags-specific) environment assumed by all.
*/
#ifndef CTAGS_MAIN_GENERAL_H
#define CTAGS_MAIN_GENERAL_H

/*
*   INCLUDE FILES
*/
#if defined (HAVE_CONFIG_H)
# include <config.h>
#elif defined (WIN32)
# include "e_msoft.h"
#endif

/*  To provide timings features if available.
 */
#ifdef HAVE_CLOCK
# ifdef HAVE_TIME_H
#  include <time.h>
# endif
#else
# ifdef HAVE_TIMES
#  ifdef HAVE_SYS_TIMES_H
#   include <sys/times.h>
#  endif
# endif
#endif

/*
*   MACROS
*/
#include "gcc-attr.h"

/*
 *  Portability macros
 */
#if !defined(HAVE_STRCASECMP) && !defined(strcasecmp)
# ifdef HAVE_STRICMP
#  define strcasecmp(s1,s2) stricmp(s1,s2)
# else
#  define strcasecmp(s1,s2) struppercmp(s1,s2)
# endif
#endif

#if !defined(HAVE_STRNCASECMP) && !defined(strncasecmp)
# ifdef HAVE_STRNICMP
#  define strncasecmp(s1,s2,n) strnicmp(s1,s2,n)
# else
#  define strncasecmp(s1,s2,n) strnuppercmp(s1,s2,n)
# endif
#endif

/*
*   DATA DECLARATIONS
*/

#ifdef HAVE_STDBOOL_H
# include <stdbool.h>
#endif

/*
*   FUNCTION PROTOTYPES
*/

#if defined (NEED_PROTO_REMOVE) && defined (HAVE_REMOVE)
extern int remove (const char *);
#endif

#if defined (NEED_PROTO_UNLINK) && ! defined (HAVE_REMOVE)
extern void *unlink (const char *);
#endif

#ifdef NEED_PROTO_GETENV
extern char *getenv (const char *);
#endif

/*
*   HACK for #1610.
*/

#ifdef ICONV_USE_LIB_PREFIX
#define iconv libiconv
#define iconv_open libiconv_open
#define iconv_close libiconv_close
#endif

/*
*  Prepare clock() and its related macros
*/
#if defined (HAVE_CLOCK)
# define CLOCK_AVAILABLE
# ifndef CLOCKS_PER_SEC
#  define CLOCKS_PER_SEC		1000000
# endif
#elif defined (HAVE_TIMES)
# define CLOCK_AVAILABLE
# define CLOCKS_PER_SEC	60
static clock_t clock (void)
{
	struct tms buf;

	times (&buf);
	return (buf.tms_utime + buf.tms_stime);
}
#else
# define clock()  (clock_t)0
#endif

#endif  /* CTAGS_MAIN_GENERAL_H */
