/*
*   Copyright (c) 2000-2003, Darren Hiebert
*   Copyright (c) 2017, Red Hat, Inc.
*   Copyright (c) 2017, Masatake YAMATO
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for applying regular expression matching.
*
*   The code for utilizing the Gnu regex package with regards to processing the
*   regex option and checking for regex matches was adapted from routines in
*   Gnu etags.
*/

#ifndef CTAGS_MAIN_LREGEX_H
#define CTAGS_MAIN_LREGEX_H

/*
*   INCLUDE FILES
*/
#include "general.h"

/*
*   DATA DECLARATIONS
*/
typedef struct sTagRegexTable {
	const char *const regex;
	const char* const name;
	const char* const kinds;
	const char *const flags;
	bool    *disabled;
	bool  mline;
} tagRegexTable;

typedef struct {
	size_t start;   /* character index in line where match starts */
	size_t length;  /* length of match */
} regexMatch;

/* Return value is referred when {exclusive} is also specified.
   The input line is consumed when "{exclusive}" is specified and
   the value returned from the callback function is true. */
typedef bool (*regexCallback) (const char *line, const regexMatch *matches, unsigned int count,
			       void *userData);

#endif	/* CTAGS_MAIN_LREGEX_H */
