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

/*
*   INCLUDE FILES
*/

#include "general.h"
#include "types.h"

#include "vstring.h"

/*
*   DATA DECLARATIONS
*/

typedef enum eFieldType { /* extension field content control */
	FIELD_UNKNOWN = -1,

	/* BASIC FIELDS */
	FIELD_NAME,
	FIELD_INPUT_FILE,
	FIELD_PATTERN,

	FIELD_ECTAGS_START,
	FIELD_COMPACT_INPUT_LINE = FIELD_ECTAGS_START,

	/* EXTENSION FIELDS */
	FIELD_JSON_LOOP_START,
	FIELD_FILE_SCOPE = FIELD_JSON_LOOP_START,
	FIELD_KIND_LONG,
	FIELD_KIND,
	FIELD_LANGUAGE,
	FIELD_LINE_NUMBER,
	FIELD_SCOPE,
	FIELD_TYPE_REF,
	FIELD_KIND_KEY,
	FIELD_ECTAGS_LOOP_START,
	FIELD_INHERITANCE = FIELD_ECTAGS_LOOP_START,
	FIELD_ACCESS,
	FIELD_IMPLEMENTATION,
	FIELD_SIGNATURE,
	FIELD_ECTAGS_LOOP_LAST = FIELD_SIGNATURE,

	/* Extension fields newly introduced in Universal Ctags. */
	FIELDS_UCTAGS_START,
	FIELD_REF_MARK = FIELDS_UCTAGS_START,
	FIELD_SCOPE_KEY,
	FIELD_SCOPE_KIND_LONG,
	FIELD_UCTAGS_LOOP_START,
	FIELD_ROLES = FIELD_UCTAGS_LOOP_START,
	FIELD_EXTRAS,
	FIELD_XPATH,
	FIELD_END_LINE,
	FIELD_EPOCH,
	FIELD_NTH,

	FIELD_BUILTIN_LAST = FIELD_NTH,
} fieldType ;

#define fieldDataTypeFalgs "sib" /* used in --list-fields */
typedef enum eFieldDataType {
	FIELDTYPE_STRING  = 1 << 0,
	FIELDTYPE_INTEGER = 1 << 1,
	FIELDTYPE_BOOL    = 1 << 2,

	/* used in --list-fields */
	FIELDTYPE_END_MARKER = 1 << 3,
} fieldDataType;

typedef const char* (*fieldRenderer)(const tagEntryInfo *const,
									 const char *,
									 vString *);

#define FIELD_LETTER_NO_USE '\0'
struct sFieldDefinition {
	/* letter, and ftype are initialized in the main part,
	   not in a parser. */
#define NUL_FIELD_LETTER '\0'
	unsigned char letter;
	const char* name;
	const char* description;
	bool enabled;

	fieldRenderer render;
	fieldRenderer renderNoEscaping;
	bool (* doesContainAnyChar) (const tagEntryInfo *const, const char*, const char *);

	bool (* isValueAvailable) (const tagEntryInfo *const);

	const char * getterValueType;
	struct _EsObject * (* getValueObject) (const tagEntryInfo *, const fieldDefinition *);
	const char * setterValueType;

	/* Return es_false if passed value is acceptable.
	   Return an error object is unacceptable. */
	struct _EsObject * (* checkValueForSetter) (const fieldDefinition *, const struct _EsObject *);
	struct _EsObject * (* setValueObject) (tagEntryInfo *, const fieldDefinition *, const struct _EsObject *);

	fieldDataType dataType; /* used in json output */

	unsigned int ftype;	/* Given from the main part */
};


/*
*   FUNCTION PROTOTYPES
*/

extern bool isFieldEnabled (fieldType type);

#endif	/* CTAGS_MAIN_FIELD_H */
