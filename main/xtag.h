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
	XTAG_QUALIFIED_TAGS,
	XTAG_FILE_NAMES_WITH_TOTAL_LINES,

	XTAG_COUNT
} xtagType;

typedef struct sXtagDesc {
	boolean enabled;
	unsigned char letter;
	const char* description;  /* displayed in --list-extras output */

	/* If the default vlalue for  */
	boolean (* isEnabled) (struct sXtagDesc *desc);
} xtagDesc;

extern xtagDesc* getXtagDesc (xtagType type);
extern xtagType  getXtagTypeForOption (char letter);
extern void printXtags (void);
extern boolean isXtagEnabled (xtagType type);
extern boolean enableXtag (xtagType type, boolean state);

#endif	/* CTAGS_MAIN_FIELD_H */
