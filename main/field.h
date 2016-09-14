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
#include "types.h"

#include "vstring.h"

typedef enum eFieldType { /* extension field content control */
	FIELD_UNKNOWN = -1,

	/* BASIC FIELDS */
	FIELD_NAME,
	FIELD_INPUT_FILE,
	FIELD_PATTERN,
	FIELD_COMPACT_INPUT_LINE,

	/* EXTENSION FIELDS */
	FIELD_EXTENSION_START,
	FIELD_ACCESS = FIELD_EXTENSION_START,
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
	FIELD_XPATH,
	FIELD_SCOPE_KIND_LONG,
	FIELD_END,
	FIELD_BUILTIN_LAST = FIELD_END,
} fieldType ;

typedef const char* (* renderEscaped) (const tagEntryInfo *const tag,
				       const char *value,
				       vString * buffer);
typedef boolean (* isValueAvailable) (const struct sTagEntryInfo *const tag);

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
	isValueAvailable isValueAvailable;

	unsigned int ftype;	/* Given from the main part */
} fieldSpec;


extern fieldType getFieldTypeForOption (char letter);

/*
   `getFieldTypeForName' is for looking for a field not owned by any parser,

   `getFieldTypeForNameAndLanguage' can be used for getting all fields having
   the same name; specify `LANG_AUTO' as `language' parameter to get the first
   field having the name. With the returned fieldType, `nextSiblingField' gets
   the next field having the same name. `nextSiblingField' returns `FIELD_UNKNOWN'
   at the end of iteration.

   Specifying `LANG_IGNORE' has the same effects as `LANG_AUTO'. However,
   internally, each parser is not initialized. `LANG_IGNORE' is a bit faster. */
extern fieldType getFieldTypeForName (const char *name);
extern fieldType getFieldTypeForNameAndLanguage (const char *fieldName, langType language);
extern boolean isFieldEnabled (fieldType type);
extern boolean enableField (fieldType type, boolean state, boolean warnIfFixedField);
extern boolean isCommonField (fieldType type);
extern int     getFieldOwner (fieldType type);
extern const char* getFieldName (fieldType type);
extern void printFields (int language);

extern boolean isFieldRenderable (fieldType type);

extern boolean doesFieldHaveValue (fieldType type, const tagEntryInfo *tag);
extern const char* renderFieldEscaped (fieldType type, const tagEntryInfo *tag, int index);

extern void initFieldDescs (void);
extern int countFields (void);

/* language should be typed to langType.
   Use int here to avoid circular dependency */
extern int defineField (fieldSpec *spec, langType language);
extern fieldType nextSiblingField (fieldType type);

#endif	/* CTAGS_MAIN_FIELD_H */
