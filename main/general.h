/*
*   Copyright (c) 1998-2003, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2.
*
*   Provides the general (non-ctags-specific) environment assumed by all.
*/
#ifndef _GENERAL_H
#define _GENERAL_H

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

/* Define standard error destination
 */
#ifndef errout
# define errout	stderr
#endif

/* Define regex if supported */
#if (defined (HAVE_REGCOMP) && !defined (REGCOMP_BROKEN))
# define HAVE_REGEX 1
#endif

/*  Prevent warnings about unused variables in GCC. */
#if defined (__GNUC__) && !defined (__GNUG__)
# ifdef __MINGW32__
#  define __unused__
# else
#  define __unused__ __attribute__((unused))
# endif
# define __printf__(s,f)  __attribute__((format (printf, s, f)))
# define attr__noreturn __attribute__((__noreturn__))
#else
# define __unused__
# define __printf__(s,f)
# define attr__noreturn
#endif

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

#undef FALSE
#undef TRUE
#ifdef __cplusplus
typedef bool boolean;
#define FALSE false
#define TRUE true
#else
typedef enum { FALSE, TRUE } boolean;
#endif

#if ! defined (HAVE_FGETPOS) && ! defined (fpos_t)
# define fpos_t long
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

#endif  /* _GENERAL_H */

/* vi:set tabstop=4 shiftwidth=4: */
