/*
 *
 *  Copyright (c) 2016, Red Hat, Inc.
 *  Copyright (c) 2016, Masatake YAMATO
 *
 *  Author: Masatake YAMATO <yamato@redhat.com>
 *
 *   This source code is released for free distribution under the terms of the
 *   GNU General Public License version 2 or (at your option) any later version.
 *
 */

#ifndef CTAGS_MAIN_PTAG_H
#define CTAGS_MAIN_PTAG_H

#include "general.h"

#define PSEUDO_TAG_PREFIX       "!_"
#define PSEUDO_TAG_SEPARATOR    "!"

typedef enum ePtagType { /* pseudo tag content control */
	PTAG_UNKNOWN = -1,
	/* Only --output-format=json use this ptag.
	   Applications of the output may expect this comes first in the output. */
	PTAG_JSON_OUTPUT_VERSION,

	PTAG_FILE_FORMAT,
	PTAG_FILE_SORTED,
	PTAG_PROGRAM_AUTHOR,
	PTAG_PROGRAM_NAME,
	PTAG_PROGRAM_URL,
	PTAG_PROGRAM_VERSION,
#ifdef HAVE_ICONV
	PTAG_FILE_ENCODING,
#endif
	PTAG_KIND_SEPARATOR,
	PTAG_KIND_DESCRIPTION,
	PTAG_FIELD_DESCRIPTION,
	PTAG_EXTRA_DESCRIPTION,
	PTAG_ROLE_DESCRIPTION,
	PTAG_OUTPUT_MODE,
	PTAG_OUTPUT_FILESEP,
	PTAG_PATTERN_TRUNCATION,
	PTAG_PROC_CWD,
	PTAG_OUTPUT_EXCMD,
	PTAG_COUNT
} ptagType;

#endif	/* CTAGS_MAIN_PTAG_H */
