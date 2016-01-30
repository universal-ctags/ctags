/*
 *
 *  Copyright (c) 2015, Red Hat, Inc.
 *  Copyright (c) 2015, Masatake YAMATO
 *
 *  Author: Masatake YAMATO <yamato@redhat.com>
 *
 *   This source code is released for free distribution under the terms of the
 *   GNU General Public License version 2 or (at your option) any later version.
 *
 */
#ifndef CTAGS_MAIN_XTAG_H
#define CTAGS_MAIN_XTAG_H

#include "general.h"

typedef enum eXtagType { /* extra tag content control */
	XTAG_UNKNOWN = -1,

	XTAG_FILE_SCOPE,
	XTAG_FILE_NAMES,
	XTAG_PSEUDO_TAGS,
	XTAG_QUALIFIED_TAGS,
	XTAG_REFERENCE_TAGS,
	XTAG_FILE_NAMES_WITH_TOTAL_LINES,

	XTAG_COUNT
} xtagType;

typedef struct sXtagDesc {
	boolean enabled;
	unsigned char letter;
	const char* description;  /* displayed in --list-extras output */

	/* If the value for "enabled" is given dynamically,
	   use this field.

	   "enabled" field of Pseudo extra tag depends on where
	   the output stream is connected to. If it is connected
	   to standared output, the tag is disabled by default.
	   If it is connected to a regular file, the tag is enabled
	   by default. */
	boolean (* isEnabled) (struct sXtagDesc *desc);
} xtagDesc;

extern xtagDesc* getXtagDesc (xtagType type);
extern xtagType  getXtagTypeForOption (char letter);
extern void printXtag (xtagType type);
extern boolean isXtagEnabled (xtagType type);
extern boolean enableXtag (xtagType type, boolean state);

#endif	/* CTAGS_MAIN_FIELD_H */
