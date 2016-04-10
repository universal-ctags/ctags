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
#ifndef CTAGS_MAIN_FIELD_H
#define CTAGS_MAIN_FIELD_H

#include "general.h"
#include "entry.h"

typedef enum eFieldType { /* extension field content control */
	FIELD_UNKNOWN = -1,

	/* BASIC FIELDS */
	FIELD_NAME,
	FIELD_INPUT_FILE,
	FIELD_PATTERN,
	FIELD_COMPACT_INPUT_LINE,

	/* EXTENSION FIELDS */
	FIELD_ACCESS,
	FIELD_FILE_SCOPE,
	FIELD_INHERITANCE,
	FIELD_KIND_LONG,
	FIELD_KIND,
	FIELD_LANGUAGE,
	FIELD_IMPLEMENTATION,
	FIELD_LINE_NUMBER,
	FIELD_ROLE,
	FIELD_REF_MARK,
	FIELD_SIGNATURE,
	FIELD_SCOPE,
	FIELD_TYPE_REF,
	FIELD_KIND_KEY,
	FIELD_SCOPE_KEY,
	FIELD_EXTRA,
	FIELD_COUNT
} fieldType ;

struct sFieldDesc;
typedef struct sFieldDesc fieldDesc;


extern fieldType getFieldTypeForOption (char letter);
extern boolean isFieldEnabled (fieldType type);
extern boolean enableField (fieldType type, boolean state);
extern boolean isFieldFixed (fieldType type);
extern const char* getFieldName(fieldType type);
extern void printFields (void);

extern boolean isFieldRenderable (fieldType type);

extern const char* renderFieldEscaped (fieldType type, const tagEntryInfo *tag);

#endif	/* CTAGS_MAIN_FIELD_H */
