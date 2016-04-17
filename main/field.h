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
#include "vstring.h"

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
	FIELD_SIGNATURE,
	FIELD_SCOPE,
	FIELD_TYPE_REF,
	FIELD_KIND_KEY,

	/* EXTENSION FIELDS NEWLY INTRODUCED IN UCTAGS */
	FIELD_ROLE,
	FIELD_REF_MARK,
	FIELD_SCOPE_KEY,
	FIELD_EXTRA,
	FIELD_BUILTIN_LAST = FIELD_EXTRA,
} fieldType ;

struct sFieldDesc;
typedef struct sFieldDesc fieldDesc;

struct sTagEntryInfo;

typedef const char* (* renderEscaped) (const struct sTagEntryInfo *const tag,
				       const char *value,
				       vString * buffer);

#define FIELD_LETTER_NO_USE '\0'
typedef struct sFieldSpec {
	/* lettern, and ftype are initialized in the main part,
	   not in a parser. */
#define NUL_FIELD_LETTER '\0'
	unsigned char letter;
	const char* name;
	const char* description;
	boolean enabled;
	renderEscaped renderEscaped;

	unsigned int ftype;	/* Given from the main part */
} fieldSpec;


extern fieldType getFieldTypeForOption (char letter);
extern fieldType getFieldTypeForName (const char *name);
extern fieldType getFieldTypeForNameAndLanguage (const char *fieldName, int language);
extern boolean isFieldEnabled (fieldType type);
extern boolean enableField (fieldType type, boolean state);
extern boolean isFieldFixed (fieldType type);
extern boolean isFieldOwnedByParser (fieldType type);
extern const char* getFieldName (fieldType type);
extern void printFields (void);

extern boolean isFieldRenderable (fieldType type);

extern const char* renderFieldEscaped (fieldType type, const struct sTagEntryInfo *tag, int index);

extern void initFieldDescs (void);
extern int countFields (void);

/* language should be typed to langType.
   Use int here to avoid circular dependency */
extern int defineField (fieldSpec *spec, int language);
extern fieldType nextFieldSibling (fieldType type);

#endif	/* CTAGS_MAIN_FIELD_H */
