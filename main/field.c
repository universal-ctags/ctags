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

static const char *renderFieldName (const tagEntryInfo *const tag, vString* b);
static const char *renderFieldSource (const tagEntryInfo *const tag, vString* b);
static const char *renderFieldSignature (const tagEntryInfo *const tag, vString* b);
static const char *renderFieldScope (const tagEntryInfo *const tag, vString* b);
static const char *renderFieldTyperef (const tagEntryInfo *const tag, vString* b);
static const char *renderFieldInherits (const tagEntryInfo *const tag, vString* b);

#define DEFINE_FIELD_FULL(L,N, V, H, B, F) {			\
		.enabled       = V,				\
		.basic         = B,				\
		.letter        = L,				\
		.name          = N,				\
		.description   = H,				\
		.renderEscaped = F,				\
		.buffer        = NULL,  			\
	}

#define DEFINE_BASIC_FIELD(L,N,V,H,F)		\
	DEFINE_FIELD_FULL(L,N,V,H,TRUE, F)

#define DEFINE_FIELD(L,N,V,H, F)		\
	DEFINE_FIELD_FULL(L,N,V,H,FALSE, F)

static fieldDesc fieldDescs [] = {
        /* BASIC FIELDS */
	DEFINE_BASIC_FIELD ('N', "name",     TRUE,
			    "tag name(fixed field)",
			    renderFieldName),
	DEFINE_BASIC_FIELD ('F', "source",   TRUE,
			    "source file(fixed field)",
			    renderFieldSource),
	DEFINE_BASIC_FIELD ('P', "pattern",  TRUE,
			    "pattern(fixed field)",
			    NULL),

	/* EXTENSION FIELDS */
	DEFINE_FIELD ('a', "access",         FALSE,
		      "Access (or export) of class members",
		      NULL),
	DEFINE_FIELD ('f', "file",           TRUE,
		      "File-restricted scoping",
		      NULL),
	DEFINE_FIELD ('i', "inherits",       FALSE,
		      "Inheritance information",
		      renderFieldInherits),
	DEFINE_FIELD ('K', NULL,             FALSE,
		      "Kind of tag as full name",
		      NULL),
	DEFINE_FIELD ('k', NULL,             TRUE,
		      "Kind of tag as a single letter",
		      NULL),
	DEFINE_FIELD ('l', "language",       FALSE,
		      "Language of source file containing tag",
		      NULL),
	DEFINE_FIELD ('m', "implementation", FALSE,
		      "Implementation information",
		      NULL),
	DEFINE_FIELD ('n', "line",           FALSE,
		      "Line number of tag definition",
		      NULL),
	DEFINE_FIELD ('S', "signature",	     FALSE,
		      "Signature of routine (e.g. prototype or parameter list)",
		      renderFieldSignature),
	DEFINE_FIELD ('s', NULL,             TRUE,
		      "Scope of tag definition",
		      renderFieldScope),
	DEFINE_FIELD ('t', "typeref",        TRUE,
		      "Type and name of a variable or typedef",
		      renderFieldTyperef),
	DEFINE_FIELD ('z', "kind",           FALSE,
		      "Include the \"kind:\" key in kind field(use k or K)",
		      NULL),
	DEFINE_FIELD ('Z', "scope",          FALSE,
		      "Include the \"scope:\" key in scope field(use s)",
		      NULL),
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

static char valueToXDigit (int v)
{
	Assert (v >= 0 && v <= 0xF);

	if (v >= 0xA)
		return 'A' + (v - 0xA);
	else
		return '0' + v;
}

static const char *renderEscapedString (const char *s,
					const tagEntryInfo *const tag,
					vString* b)
{
	for(; *s; s++)
	{
		int c = *s;

		/* escape control characters (incl. \t) */
		if ((c > 0x00 && c <= 0x1F) || c == 0x7F || c == '\\')
		{
			vStringPut (b, '\\');

			switch (c)
			{
				/* use a short form for known escapes */
			case '\a':
				c = 'a'; break;
			case '\b':
				c = 'b'; break;
			case '\t':
				c = 't'; break;
			case '\n':
				c = 'n'; break;
			case '\v':
				c = 'v'; break;
			case '\f':
				c = 'f'; break;
			case '\r':
				c = 'r'; break;
			case '\\':
				c = '\\'; break;
			default:
				vStringPut (b, 'x');
				vStringPut (b, valueToXDigit ((c & 0xF0) >> 4));
				vStringPut (b, valueToXDigit (c & 0x0F));
				continue;
			}
		}
		vStringPut (b, c);
	}

	return vStringValue (b);
}

static const char *renderEscapedName (const char* s,
				      const tagEntryInfo *const tag,
				      vString* b)
{
	const char* base = s;

	for (; *s; s++)
	{
		int c = *s;
		if ((c > 0x00 && c <= 0x1F) || c == 0x7F)
		{
			notice("Unexpected character (0 < *c && *c < 0x20) included in a tagEntryInfo: %s", base);
			notice("File: %s, Line: %lu, Lang: %s, Kind: %c",
			       tag->sourceFileName, tag->lineNumber, tag->language, tag->kind->letter);
			if (Option.fatalWarnings)
				error (FATAL, "Aborting");
			notice("Escape the character");
			break;
		}
		else if (c == '\\')
			break;
		else
			continue;
	}

	if (*s == '\0')
		return base;

	vStringNCatS (b, base, s - base);

	return renderEscapedString (s, tag, b);
}

static const char *renderFieldName (const tagEntryInfo *const tag, vString* b)
{
	return renderEscapedName (tag->name, tag, b);
}

static const char *renderFieldSource (const tagEntryInfo *const tag, vString* b)
{
	return renderEscapedString (tag->sourceFileName, tag, b);
}

static const char *renderFieldSignature (const tagEntryInfo *const tag, vString* b)
{
	return renderEscapedString (tag->extensionFields.signature, tag, b);
}

static const char *renderFieldScope (const tagEntryInfo *const tag, vString* b)
{
	return renderEscapedName (tag->extensionFields.scopeName, tag, b);
}

static const char *renderFieldInherits (const tagEntryInfo *const tag, vString* b)
{
	return renderEscapedName (tag->extensionFields.inheritance, tag, b);
}

static const char *renderFieldTyperef (const tagEntryInfo *const tag, vString* b)
{
	return renderEscapedName (tag->extensionFields.typeRef [1], tag, b);
}


extern const char* renderFieldEscaped (fieldDesc *fdesc,
				       const tagEntryInfo *tag)
{
	Assert (fdesc);
	Assert (tag);
	Assert (fdesc->renderEscaped);

	if (fdesc->buffer == NULL)
		fdesc->buffer = vStringNew ();
	else
		vStringClear (fdesc->buffer);

	return fdesc->renderEscaped (tag, fdesc->buffer);
}

/* vi:set tabstop=4 shiftwidth=4: */
