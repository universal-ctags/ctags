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

/* VERSION.REVISION.PATCH:

   For incompatible changes for the command line interface (CLI)
   of ctags or readtags commands, increment VERSION.
   Removing a parameter of a parser is part of incompatible change
   for the CLI.
   When OUTPUT_VERSOIN_AGE set to 0, increment VERSION. Set REVISION
   and PATCH to 0.
   For changing setting versionAge member of a parser, increment
   VERSION. Set REVISION and PATCH to 0.
   When chaging VERSION, set REVISION and PATCH to 0.

   For uppper compatible changes for the CLI,
   increment REVISION.
   Adding a new parameter to a parser is an uppper compatible change.
   When incrementing OUTPUT_VERSOIN_CURRENT but not setting
   OUTPUT_VERSOIN_AGE to 0, increment REVISION.
   For changing increment versionCurrent member of a parser but not
   setting versionAge to 0, increment REVISION.
   When changing REVISION, set PATCH to 0.

   For implementation changes like bug fixes, increment PATCH. */

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
