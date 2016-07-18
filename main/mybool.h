/*
*   Copyright (c) 1998-2003, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*/

/* This is derived from general.h.
   Only readtags related source file should include this.
   ctags related source file should include genera.h instead. */

#ifndef CTAGS_MAIN_MYBOOL_H
#define CTAGS_MAIN_MYBOOL_H

#undef FALSE
#undef TRUE
#ifdef __cplusplus
typedef bool boolean;
#define FALSE false
#define TRUE true
#else
typedef enum { FALSE, TRUE } boolean;
#endif

#endif	/* CTAGS_MAIN_MYBOOL_H */
