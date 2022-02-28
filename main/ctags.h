/*
*   Copyright (c) 1996-2002, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   Program definitions
*/
#ifndef CTAGS_MAIN_CTAGS_H
#define CTAGS_MAIN_CTAGS_H

#include "general.h"

/*
*   MACROS
*/
#if defined (HAVE_CONFIG_H)
# define PROGRAM_VERSION PACKAGE_VERSION
#else
# define PROGRAM_VERSION "5.9.0"
#endif
#define PROGRAM_NAME      "Universal Ctags"
#define PROGRAM_URL       "https://ctags.io/"
#define PROGRAM_COPYRIGHT "Copyright (C) 2015-2022"
#define AUTHOR_NAME       "Universal Ctags Team"

/*
 * Constant
 */
extern const char* ctags_repoinfo;
#define CTAGS_FIELD_PREFIX "UCTAGS"

/*
 * Reserved words
 */
#define RSV_LANGMAP_DEFAULT "default"
#define RSV_LANG_ALL "all"
#define RSV_LANG_AUTO "auto"
#define RSV_NONE "NONE"
#endif	/* CTAGS_MAIN_CTAGS_H */
