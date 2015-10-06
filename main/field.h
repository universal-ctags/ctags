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
#ifndef _FIELD_H
#define _FIELD_H

#include "general.h"

typedef enum eFieldType { /* extension field content control */
	FIELD_UNKNOWN = -1,
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
	FIELD_COUNT,
} fieldType ;

typedef struct sFieldDesc {
	boolean enabled;
	unsigned char letter;
	const char* name;         /* kind name */
	const char* description;  /* displayed in --help output */
} fieldDesc;

extern fieldDesc* getFieldDesc(fieldType type);
extern fieldType getFieldTypeForOption (char letter);
extern void printFields (void);

#endif	/* _FIELD_H */
