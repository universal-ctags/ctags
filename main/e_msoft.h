/*
*   Copyright (c) 2002-2003, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   Configures ctags for Microsoft environment.
*/
#ifndef E_MSOFT_H
#define E_MSOFT_H

#define CASE_INSENSITIVE_FILENAMES 1
#define MANUAL_GLOBBING 1
#define MSDOS_STYLE_PATH 1
#define HAVE_FCNTL_H 1
#define HAVE_IO_H 1
#define HAVE_SYS_STAT_H 1
#define HAVE_SYS_TYPES_H 1
#define HAVE_CHSIZE 1
#define HAVE_DIRECT_H 1
#define HAVE_STRICMP 1
#define HAVE_STRNICMP 1
#define HAVE_STRSTR 1
#define HAVE_STRERROR 1
#define HAVE__FINDFIRST 1
#define HAVE_FINDNEXT 1
#define findfirst_t intptr_t
#define HAVE_MKSTEMP 1
#define HAVE_FNMATCH 1
#define HAVE_FNMATCH_H 1
#define HAVE_PUTENV 1
#define TMPDIR "\\"

int mkstemp (char *template_name);

#ifdef _MSC_VER

# if _MSC_VER < 1900
#  define snprintf _snprintf
# endif

#if (_MSC_VER >= 1800) // Visual Studio 2013 or newer
#define HAVE_STDBOOL_H 1
#else
typedef enum { false, true } bool;
#endif

# ifndef _CRT_SECURE_NO_DEPRECATE
#  define _CRT_SECURE_NO_DEPRECATE 1
# endif
# pragma warning(disable : 4996)

#elif defined (__MINGW32__)

# include <_mingw.h>
# define HAVE_STDBOOL_H 1
# define HAVE_DIRENT_H 1
# define ffblk _finddata_t
# define FA_DIREC _A_SUBDIR
# define ff_name name

# if defined(__USE_MINGW_ANSI_STDIO) && defined(__MINGW64_VERSION_MAJOR)
#  define HAVE_ASPRINTF 1
# endif

#endif

#endif
