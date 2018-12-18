/*
*   $Id$
*
*   Copyright (c) 2015, vim-jp
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for checking multibyte character set.
*/
#ifndef CTAGS_MAIN_MBCS_PRIVATE_H
#define CTAGS_MAIN_MBCS_PRIVATE_H

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include "vstring.h"

#ifdef HAVE_ICONV

/*
*   FUNCTION PROTOTYPES
*/
extern bool openConverter (const char*, const char*);
extern bool convertString (vString *const);
extern void closeConverter (void);

#endif /* HAVE_ICONV */

#endif /* CTAGS_MAIN_MBCS_PRIVATE_H */
