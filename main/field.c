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

#define DEFINE_FIELD(L,N,H) {					\
		.letter        = L,				\
		.name          = N,				\
		.description   = H,				\
	}

fieldDesc fieldDescs [] = {
	DEFINE_FIELD ('a', "access",
		      "Access (or export) of class members"),
	DEFINE_FIELD ('f', "file",
		      "File-restricted scoping"),
	DEFINE_FIELD ('i', "inherits", /* needs sanitizing */
		      "Inheritance information"),
	DEFINE_FIELD ('K', NULL,
		      "Kind of tag as full name"),
	DEFINE_FIELD ('k', NULL,
		      "Kind of tag as a single letter"),
	DEFINE_FIELD ('l', "language",
			"Language of source file containing tag"),
	DEFINE_FIELD ('m', "implementation",
			"Implementation information"),
	DEFINE_FIELD ('n', "line",
		      "Line number of tag definition"),
	DEFINE_FIELD ('S', "signature",	/* needs sanitizing */
		      "Signature of routine (e.g. prototype or parameter list)"),
	DEFINE_FIELD ('s', NULL,/* needs sanitizing */
		      "Scope of tag definition"),
	DEFINE_FIELD ('t', "typeref",/* needs sanitizing */
		      "Type and name of a variable or typedef"),
	DEFINE_FIELD ('z', "kind",
		      "Include the \"kind:\" key in kind field(use k or K)"),
};

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
	       Option.extensionFields[i]? "on": "off");
}

extern void printFields (void)
{
	int i;

	for (i = 0; i < sizeof (fieldDescs) / sizeof (fieldDescs [0]); i++)
		printField (i);
}
