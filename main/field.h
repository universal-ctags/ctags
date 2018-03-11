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
#include "colprint.h"
#include "writer.h"
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
	FIELD_ROLES,
	FIELD_REF_MARK,
	FIELD_SCOPE_KEY,
	FIELD_EXTRAS,
	FIELD_XPATH,
	FIELD_SCOPE_KIND_LONG,
	FIELD_END_LINE,
	FIELD_BUILTIN_LAST = FIELD_END_LINE,
} fieldType ;

typedef const char* (* renderEscaped) (const tagEntryInfo *const tag,
				       const char *value,
				       vString * buffer,
					   bool *rejected);
typedef bool (* isValueAvailable) (const struct sTagEntryInfo *const tag);

#define fieldDataTypeFalgs "sib" /* used in --list-fields */
typedef enum eFieldDataType {
	FIELDTYPE_STRING  = 1 << 0,
	FIELDTYPE_INTEGER = 1 << 1,
	FIELDTYPE_BOOL    = 1 << 2,

	/* used in --list-fields */
	FIELDTYPE_END_MARKER = 1 << 3,
} fieldDataType;

#define FIELD_LETTER_NO_USE '\0'
typedef struct sFieldDefinition {
	/* letter, and ftype are initialized in the main part,
	   not in a parser. */
#define NUL_FIELD_LETTER '\0'
	unsigned char letter;
	const char* name;
	const char* description;
	bool enabled;
	renderEscaped renderEscaped [WRITER_COUNT];
	isValueAvailable isValueAvailable;
	fieldDataType dataType; /* used in json output */

	unsigned int ftype;	/* Given from the main part */
} fieldDefinition;


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
extern bool isFieldEnabled (fieldType type);
extern bool enableField (fieldType type, bool state, bool warnIfFixedField);
extern bool isCommonField (fieldType type);
extern int     getFieldOwner (fieldType type);
extern const char* getFieldName (fieldType type);
extern unsigned int getFieldDataType (fieldType type);
extern void printFields (int language);

/* Whether the field specified with TYPE has a
   method for rendering in the current format. */
extern bool isFieldRenderable (fieldType type);

extern bool doesFieldHaveValue (fieldType type, const tagEntryInfo *tag);
extern const char* renderFieldEscaped (writerType writer, fieldType type, const tagEntryInfo *tag, int index,
									   bool *rejected);

extern void initFieldObjects (void);
extern int countFields (void);

/* language should be typed to langType.
   Use int here to avoid circular dependency */
extern int defineField (fieldDefinition *spec, langType language);
extern fieldType nextSiblingField (fieldType type);

/* --list-fields implementation. LANGUAGE must be initialized. */
extern struct colprintTable * fieldColprintTableNew (void);
extern void fieldColprintAddCommonLines (struct colprintTable *table);
extern void fieldColprintAddLanguageLines (struct colprintTable *table, langType language);
extern void fieldColprintTablePrint (struct colprintTable *table,
									 bool withListHeader, bool machinable, FILE *fp);

#endif	/* CTAGS_MAIN_FIELD_H */
