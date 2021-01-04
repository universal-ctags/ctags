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
#ifndef CTAGS_MAIN_FIELD_PRIVATE_H
#define CTAGS_MAIN_FIELD_PRIVATE_H

/*
*   INCLUDE FILES
*/
#include "general.h"
#include "colprint_p.h"
#include "field.h"
#include "optscript.h"

/*
*   DATA DECLARATIONS
*/


/*
*   FUNCTION PROTOTYPES
*/

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
extern bool enableField (fieldType type, bool state);
extern bool isCommonField (fieldType type);
extern int     getFieldOwner (fieldType type);
extern const char* getFieldDescription (fieldType type);
extern const char* getFieldName (fieldType type);
extern unsigned char getFieldLetter (fieldType type);
extern unsigned int getFieldDataType (fieldType type);
extern bool isFieldValueAvailableAlways (fieldType type);

/* Whether the field specified with TYPE has a
   method for rendering in the current format. */
extern bool doesFieldHaveRenderer (fieldType type, bool noEscaping);

extern bool doesFieldHaveValue (fieldType type, const tagEntryInfo *tag);

extern const char* renderField (fieldType type, const tagEntryInfo *tag, int index);
extern const char* renderFieldNoEscaping (fieldType type, const tagEntryInfo *tag, int index);
extern bool  doesFieldHaveTabOrNewlineChar (fieldType type, const tagEntryInfo *tag, int index);

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

/* tag is assumed that it is in the cork queue. */
extern EsObject * getFieldValue (fieldType type, const tagEntryInfo *tag);
extern bool hasFieldGetter (fieldType type);
extern const char * getFieldGetterValueType (fieldType type);
extern EsObject * setFieldValue (fieldType type, tagEntryInfo *tag, const EsObject *val);
extern bool hasFieldSetter (fieldType type);
extern const char * getFieldSetterValueType (fieldType type);
extern bool hasFieldValueCheckerForSetter (fieldType type);
extern EsObject *checkFieldValueForSetter (fieldType type, const EsObject *val);

#endif	/* CTAGS_MAIN_FIELD_PRIVATE_H */
