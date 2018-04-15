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
#include "colprint.h"


typedef enum eXtagType { /* extra tag content control */
	XTAG_UNKNOWN = -1,

	XTAG_FILE_SCOPE,
	XTAG_FILE_NAMES,
	XTAG_PSEUDO_TAGS,
	XTAG_QUALIFIED_TAGS,
	XTAG_REFERENCE_TAGS,
	XTAG_TAGS_GENERATED_BY_GUEST_PARSERS,
	XTAG_TAGS_GENERATED_BY_SUBPARSER,
	XTAG_SUBWORD,
	XTAG_ANONYMOUS,

	XTAG_COUNT
} xtagType;

typedef struct sXtagDefinition {
	bool enabled;
	/* letter, and ftype are initialized in the main part,
	   not in a parser. */
#define NUL_XTAG_LETTER '\0'
	unsigned char letter;
	const char* name;	 /* used in extra: field */
	const char* description;  /* displayed in --list-extra output */

	/* If the value for "enabled" is given dynamically,
	   use this field.

	   "enabled" field of Pseudo extra tag depends on where
	   the output stream is connected to. If it is connected
	   to standard output, the tag is disabled by default.
	   If it is connected to a regular file, the tag is enabled
	   by default. */
	bool (* isEnabled) (struct sXtagDefinition *def);
	bool (* isFixed)   (struct sXtagDefinition *def);
	void (* enable)    (struct sXtagDefinition *def, bool state);

	unsigned int xtype;	/* Given from the main part */
} xtagDefinition;

extern xtagDefinition* getXtagDefinition (xtagType type);
extern xtagType  getXtagTypeForLetter (char letter);
extern xtagType  getXtagTypeForNameAndLanguage (const char *name, langType language);
extern bool isXtagEnabled (xtagType type);
extern bool enableXtag (xtagType type, bool state);
extern bool isXtagFixed (xtagType type);
extern bool isCommonXtag (xtagType type);
extern int  getXtagOwner (xtagType type);

const char* getXtagName (xtagType type);

extern void initXtagObjects (void);
extern int countXtags (void);

extern int defineXtag (xtagDefinition *def, langType language);
extern xtagType nextSiblingXtag (xtagType type);

/* --list-extras implementation. LANGUAGE must be initialized. */
extern struct colprintTable * xtagColprintTableNew (void);
extern void xtagColprintAddCommonLines (struct colprintTable *table);
extern void xtagColprintAddLanguageLines (struct colprintTable *table, langType language);
extern void xtagColprintTablePrint (struct colprintTable *table,
									bool withListHeader, bool machinable, FILE *fp);

#endif	/* CTAGS_MAIN_FIELD_H */
