/*
*   Copyright (c) 1998-2002, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This code is derived from the main part of ctags.
*   This source code is NOT released for the public domain.
*/
#ifndef READTAGS_STUB_H
#define READTAGS_STUB_H

#include "general.h"

#ifdef DEBUG
# include <assert.h>
#endif

enum eDebugLevels {
	DEBUG_READ   = 0x01,  /* echo raw (filtered) characters */
	DEBUG_PARSE  = 0x02,  /* echo parsing results */
	DEBUG_STATUS = 0x04,  /* echo file status information */
	DEBUG_OPTION = 0x08,  /* echo option parsing */
	DEBUG_CPP    = 0x10,  /* echo characters out of pre-processor */
	DEBUG_RAW    = 0x20   /* echo raw (filtered) characters */
};

extern void debugAssert (const char *assertion, const char *file, unsigned int line, const char *function) attr__noreturn;
extern void debugPrintf (const enum eDebugLevels level, const char *const format, ...) CTAGS_ATTR_PRINTF (2, 3);

#endif	/* READTAGS_STUB_H */
