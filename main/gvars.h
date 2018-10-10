/*
*   Copyright (c) 1998-2003, Darren Hiebert
*   Copyright (c) 2018, Red Hat, Inc.
 *  Copyright (c) 2018, Masatake YAMATO
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   Declare variables visible to parsers
*/
#ifndef CTAGS_GVARS_H
#define CTAGS_GVARS_H

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#ifdef DEBUG

/* -d  debugging output */
extern long ctags_debugLevel;

#endif	/* DEBUG */

/* -V  verbose */
extern bool ctags_verbose;

#endif	/* CTAGS_GVARS_H */
