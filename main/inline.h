/*
 *
 *  Copyright (c) 2016, 2017 Matthew Brush
 *
 *
 *   This source code is released for free distribution under the terms of the
 *   GNU General Public License version 2 or (at your option) any later version.
 *
 */

#ifndef CTAGS_MAIN_INLINE_H
#define CTAGS_MAIN_INLINE_H

#if defined(__STDC_VERSION__) && __STDC_VERSION__ >= 199901L
# define CTAGS_INLINE static inline
#elif defined(_MSC_VER)
# define CTAGS_INLINE static __inline
#elif defined(__GNUC__) || defined(__clang__)
# define CTAGS_INLINE static __inline__
// #elif ... other compilers/tests here ...
// # define CTAGS_INLINE ...
#else
# define CTAGS_INLINE static
#endif

#endif /* CTAGS_MAIN_INLINE_H */
