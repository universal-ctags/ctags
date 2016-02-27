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
	PTAG_COUNT
} ptagType;

typedef struct sPtagDesc {
	boolean enabled;
	const char* name;
	const char* description;  /* displayed in --list-pseudo-tags output */
	boolean (* makeTag) (struct sPtagDesc *, void *);
	boolean common;
} ptagDesc;

struct ptagXcmdData {
	const char *fileName;
	const char *pattern;
	const char *language;
};

extern boolean makePtagIfEnabled (ptagType type, void *data);
extern ptagDesc* getPtagDesc (ptagType type);
extern ptagType  getPtagTypeForName (const char *name);
extern void printPtag (ptagType type);
extern boolean isPtagEnabled (ptagType type);
extern boolean isPtagCommon  (ptagType type);
extern boolean enablePtag (ptagType type, boolean state);

#endif	/* CTAGS_MAIN_FIELD_H */
