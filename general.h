/*
*   $Id$
*
*   Copyright (c) 1998-2002, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   Provides the general (non-ctags-specific) environment assumed by all.
*/
#ifndef _GENERAL_H
#define _GENERAL_H

/*
*   INCLUDE FILES
*/
#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

/* Include correct file for compilation environment */

#ifdef AMIGA
# include "e_amiga.h"
#endif

#ifdef __CYGWIN__
# include "e_cygwin.h"
#endif

#ifdef DJGPP
# include "e_djgpp.h"
#endif

#ifdef macintosh
# include "e_mac.h"
#endif

#if defined (MSDOS) || defined (WIN32)
# include "e_msoft.h"
#endif

#ifdef OS2
# include "e_os2.h"
#endif

#ifdef QDOS
# include "e_qdos.h"
#endif

#ifdef RISCOS
# include "e_riscos.h"
#endif

#if defined (__vms) && ! defined (VMS)
# define VMS 1
#endif
#ifdef VMS
# include "e_vms.h"
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

/* Variant names */
#if !defined(HAVE_STRCASECMP) && defined(HAVE_STRICMP) && !defined(strcasecmp)
# define strcasecmp stricmp
#endif
#if !defined(HAVE_STRNCASECMP) && defined(HAVE_STRNICMP) && !defined(strncasecmp)
# define strncasecmp strnicmp
#endif

/*  This is a helpful internal feature of later versions (> 2.7) of GCC
 *  to prevent warnings about unused variables.
 */
#if (__GNUC__ > 2  ||  (__GNUC__ == 2  &&  __GNUC_MINOR__ >= 7)) && !(defined (__APPLE_CC__) || defined (__GNUG__))
# define __unused__	__attribute__((unused))
# define __printf__(s,f)  __attribute__((format (printf, s, f)))
#else
# define __unused__
# define __printf__(s,f)
#endif

/*
*   DATA DECLARATIONS
*/

#undef FALSE
#undef TRUE
#ifdef VAXC
typedef enum { FALSE, TRUE } booleanType;
typedef int boolean;
#else
# ifdef __cplusplus
typedef bool boolean;
#define FALSE false
#define TRUE true
# else
typedef enum { FALSE, TRUE } boolean;
# endif
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

#endif	/* _GENERAL_H */

/* vi:set tabstop=8 shiftwidth=4: */
