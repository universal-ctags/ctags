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
   When OUTPUT_VERSION_AGE set to 0, increment VERSION. Set REVISION
   and PATCH to 0.
   For changing setting versionAge member of a parser, increment
   VERSION. Set REVISION and PATCH to 0.
   When chaging VERSION, set REVISION and PATCH to 0.

   For uppper compatible changes for the CLI,
   increment REVISION.
   Adding a new parameter to a parser is an uppper compatible change.
   When incrementing OUTPUT_VERSION_CURRENT but not setting
   OUTPUT_VERSION_AGE to 0, increment REVISION.
   For changing increment versionCurrent member of a parser but not
   setting versionAge to 0, increment REVISION.
   When changing REVISION, set PATCH to 0.

   For implementation changes like bug fixes, increment PATCH. */

#if defined (HAVE_CONFIG_H)
# define PROGRAM_VERSION PACKAGE_VERSION
#else
# define PROGRAM_VERSION "6.0.0"
#endif
#define PROGRAM_NAME      "Universal Ctags"
#define PROGRAM_URL       "https://ctags.io/"
#define PROGRAM_COPYRIGHT "Copyright (C) 2015-2022"
#define AUTHOR_NAME       "Universal Ctags Team"

/* The concept of CURRENT and AGE is taken from libtool.
 * However, we deleted REVISION in libtool when importing
 * the concept of versioning from libtool.
 *
 * If common fields, common extras, pseudo tags have been added,
 * removed or changed since last release, increment CURRENT.
 * If they have been added since last release, increment AGE.
 * If they have been removed since last release, set AGE to 0
 *
 * From the command line of ctags, you can see the version
 * information with --version and --version=NONE.
 *
 * In the tags file, !_TAGS_OUTPUT_VERSION shows the the version.
 *
 * Chaning for the command line interface, and implementation changes
 * like bug fixes don't affect the CURRENT an AGE.
 */
#define OUTPUT_VERSION_CURRENT 0
#define OUTPUT_VERSION_AGE 0

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
