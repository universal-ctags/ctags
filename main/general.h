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
#if (defined (HAVE_FORK) && defined (HAVE_WAITPID) && defined (HAVE_EXECV) && defined (HAVE_PIPE))
#define HAVE_COPROC
#endif
#elif defined (WIN32)
# include "e_msoft.h"
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

#ifdef USE_STDBOOL_H
# include <stdbool.h>
#endif

#include "mybool.h"

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

#endif  /* CTAGS_MAIN_GENERAL_H */
