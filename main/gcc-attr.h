/*
*   Copyright (c) 1998-2003, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*/

/* This is derived from general.h.
   Only readtags related source file should include this.
   ctags related source file should include genera.h instead. */

#ifndef CTAGS_MAIN_GCC_ATTR_H
#define CTAGS_MAIN_GCC_ATTR_H

/*  Prevent warnings about unused variables in GCC. */
#if defined (__GNUC__) && !defined (__GNUG__)
# ifdef __MINGW32__
#  define CTAGS_ATTR_UNUSED
# else
#  define CTAGS_ATTR_UNUSED __attribute__((unused))
# endif
# define CTAGS_ATTR_PRINTF(s,f)  __attribute__((format (printf, s, f)))
# define attr__noreturn __attribute__((__noreturn__))
#else
# define CTAGS_ATTR_UNUSED
# define CTAGS_ATTR_PRINTF(s,f)
# define attr__noreturn
#endif

#endif	/* CTAGS_MAIN_GCC_ATTR_H */
