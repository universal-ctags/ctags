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

#include "general.h"  /* must always come first */

#include <errno.h>
#include <stdlib.h>
#include <string.h>

#include "debug.h"
#include "entry.h"
#include "field.h"
#include "options.h"
#include "routines.h"

#define DEFINE_FIELD_FULL(L,N, V, H, B) {			\
		.enabled       = V,				\
		.basic         = B,				\
		.letter        = L,				\
		.name          = N,				\
		.description   = H,				\
	}

#define DEFINE_BASIC_FIELD(L,N,V,H) \
    DEFINE_FIELD_FULL(L,N,V,H,TRUE)

#define DEFINE_FIELD(L,N,V,H) \
    DEFINE_FIELD_FULL(L,N,V,H,FALSE)

static fieldDesc fieldDescs [] = {
        /* BASIC FIELDS */
	DEFINE_BASIC_FIELD ('N', "name",     TRUE,
		      "tag name(fixed field)"),
	DEFINE_BASIC_FIELD ('F', "source",   TRUE,
		      "source file(fixed field)"),
	DEFINE_BASIC_FIELD ('P', "pattern",  TRUE,
		      "pattern(fixed field)"),

	/* EXTENSION FIELDS */
	DEFINE_FIELD ('a', "access",         FALSE,
		      "Access (or export) of class members"),
	DEFINE_FIELD ('f', "file",           TRUE,
		      "File-restricted scoping"),
	DEFINE_FIELD ('i', "inherits",       FALSE,
		      "Inheritance information"),
	DEFINE_FIELD ('K', NULL,             FALSE,
		      "Kind of tag as full name"),
	DEFINE_FIELD ('k', NULL,             TRUE,
		      "Kind of tag as a single letter"),
	DEFINE_FIELD ('l', "language",       FALSE,
			"Language of source file containing tag"),
	DEFINE_FIELD ('m', "implementation", FALSE,
			"Implementation information"),
	DEFINE_FIELD ('n', "line",           FALSE,
		      "Line number of tag definition"),
	DEFINE_FIELD ('S', "signature",	     FALSE,
		      "Signature of routine (e.g. prototype or parameter list)"),
	DEFINE_FIELD ('s', NULL,             TRUE,/* needs sanitizing */
		      "Scope of tag definition"),
	DEFINE_FIELD ('t', "typeref",        TRUE,
		      "Type and name of a variable or typedef"),
	DEFINE_FIELD ('z', "kind",           FALSE,
		      "Include the \"kind:\" key in kind field(use k or K)"),
};

extern fieldDesc* getFieldDesc(fieldType type)
{
	Assert ((0 <= type) && (type < FIELD_COUNT));
	return fieldDescs + type;
}
extern fieldType getFieldTypeForOption (char letter)
{
	int i;

	for (i = 0; i < FIELD_COUNT; i++)
	{
		if (fieldDescs [i].letter == letter)
			return i;
	}
	return FIELD_UNKNOWN;
}

static void printField (fieldType i)
{
	printf("%c\t%s\t%s\t%s\n",
	       fieldDescs[i].letter,
	       fieldDescs[i].name? fieldDescs[i].name: "NONE",
	       fieldDescs[i].description? fieldDescs[i].description: "NONE",
	       getFieldDesc (i)->enabled? "on": "off");
}

extern void printFields (void)
{
	int i;

	for (i = 0; i < sizeof (fieldDescs) / sizeof (fieldDescs [0]); i++)
		printField (i);
}
