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

#include "general.h"  /* must always come first */
#include "vstring.h"

#ifdef HAVE_ICONV

extern boolean isConverting (void);
extern boolean openConverter (char*, char*);
extern boolean convertString (vString *const);
extern void closeConverter (void);

#endif /* HAVE_ICONV */
