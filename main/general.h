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

/*  To provide timings features.
 */
#include <time.h>

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
*   HACK for #1610.
*/

#ifdef ICONV_USE_LIB_PREFIX
#define iconv libiconv
#define iconv_open libiconv_open
#define iconv_close libiconv_close
#endif

#endif  /* CTAGS_MAIN_GENERAL_H */
