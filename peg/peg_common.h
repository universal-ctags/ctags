/*
 *   Copyright (c) 2021 Masatake YAMATO
 *
 *   This source code is released for free distribution under the terms of the
 *   GNU General Public License version 2 or (at your option) any later version.
 *
 *   This module contains macros, data decls and prototypes to generate tags for Kotlin.
 */

#ifndef CTAGS_PEG_COMMON
#define CTAGS_PEG_COMMON

#define PCC_GETCHAR(auxil) getcFromInputFile()
#define PCC_MALLCO(auxil,size) eMalloc(size)
#define PCC_REALLOC(auxil,ptr,size) eRealloc(ptr,size)
#define PCC_FREE(auxil,ptr) eFreeNoNullCheck((void *)ptr)
#define PCC_ERROR(auxil) reportError(auxil)

#endif	/* !CTAGS_PEG_COMMON */
