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
#ifndef CTAGS_MAIN_PTAG_PRIVATE_H
#define CTAGS_MAIN_PTAG_PRIVATE_H

#include "general.h"
#include "types.h"

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

typedef enum ePtagFlag {
	/* use isPtagCommonInParsers() for testing. */
	PTAGF_COMMON = 1 << 0,
	/* use isPtagParserSpecific for testing.
	 * PSEUDO_TAG_SEPARATOR is used for printing. */
	PTAGF_PARSER = 1 << 1,
} ptagFlag;

struct sPtagDesc {
	bool enabled;
	const char* name;
	const char* description;  /* displayed in --list-pseudo-tags output */

	/* For making ptags for common in parsers, LANG_IGNOR is for the second
	 * argument and a pointer for optionValues type value for the third argument
	 * are passed.
	 *
	 * For parser specific ptags, the pointer for parserObject
	 * of the parser is passed as the thrid argument.
	 */
	bool (* makeTag) (ptagDesc *, langType, const void *);

	ptagFlag flags;
};

extern bool makePtagIfEnabled (ptagType type, langType language, const void *data);
extern ptagDesc* getPtagDesc (ptagType type);
extern ptagType  getPtagTypeForName (const char *name);
extern void printPtags (bool withListHeader, bool machinable, FILE *fp);
extern bool isPtagEnabled (ptagType type);
extern bool isPtagCommonInParsers (ptagType type);
extern bool isPtagParserSpecific (ptagType type);
extern bool enablePtag (ptagType type, bool state);

#endif	/* CTAGS_MAIN_PTAG_PRIVATE_H */
