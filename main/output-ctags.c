/*
*   Copyright (c) 1998-2002, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   External interface to entry.c
*/

#include "general.h"  /* must always come first */

#include "entry.h"
#include "mio.h"
#include "options.h"
#include "output.h"
#include "read.h"
#include "ptag.h"


static const char* escapeFieldValue (const tagEntryInfo * tag, fieldType ftype)
{
	return renderFieldEscaped (ftype, tag, NO_PARSER_FIELD);
}

static int renderExtensionFieldMaybe (int xftype, const tagEntryInfo *const tag, char sep[2], MIO *mio)
{
	if (isFieldEnabled (xftype) && doesFieldHaveValue (xftype, tag))
	{
		int len;
		len = mio_printf (mio, "%s\t%s:%s", sep,
				  getFieldName (xftype),
				  escapeFieldValue (tag, xftype));
		sep[0] = '\0';
		return len;
	}
	else
		return 0;
}

static int addParserFields (MIO * mio, const tagEntryInfo *const tag)
{
	unsigned int i;
	unsigned int ftype;
	int length = 0;

	for (i = 0; i < tag->usedParserFields; i++)
	{
		ftype = tag->parserFields [i].ftype;
		if (! isFieldEnabled (ftype))
			continue;

		length += mio_printf(mio, "\t%s:%s",
				     getFieldName (ftype),
				     renderFieldEscaped (tag->parserFields [i].ftype, tag, i));
	}
	return length;
}

static int writeLineNumberEntry (MIO * mio, const tagEntryInfo *const tag)
{
	if (Option.lineDirectives)
		return mio_printf (mio, "%s", escapeFieldValue (tag, FIELD_LINE_NUMBER));
	else
		return mio_printf (mio, "%lu", tag->lineNumber);
}

static int file_putc (char c, void *data)
{
	MIO *fp = data;
	mio_putc (fp, c);
	return 1;
}

static int file_puts (const char* s, void *data)
{
	MIO *fp = data;
	return mio_puts (fp, s);
}

static int addExtensionFields (MIO *mio, const tagEntryInfo *const tag)
{
	boolean isKindKeyEnabled = isFieldEnabled (FIELD_KIND_KEY);
	boolean isScopeEnabled = isFieldEnabled   (FIELD_SCOPE_KEY);

	const char* const kindKey = isKindKeyEnabled
		?getFieldName (FIELD_KIND_KEY)
		:"";
	const char* const kindFmt = isKindKeyEnabled
		?"%s\t%s:%s"
		:"%s\t%s%s";
	const char* const scopeKey = isScopeEnabled
		?getFieldName (FIELD_SCOPE_KEY)
		:"";
	const char* const scopeFmt = isScopeEnabled
		?"%s\t%s:%s:%s"
		:"%s\t%s%s:%s";

	char sep [] = {';', '"', '\0'};
	int length = 0;

	if (tag->kind->name != NULL && (isFieldEnabled (FIELD_KIND_LONG)  ||
		 (isFieldEnabled (FIELD_KIND)  && tag->kind == '\0')))
	{
		length += mio_printf (mio, kindFmt, sep, kindKey, tag->kind->name);
		sep [0] = '\0';
	}
	else if (tag->kind != '\0'  && (isFieldEnabled (FIELD_KIND) ||
			(isFieldEnabled (FIELD_KIND_LONG) &&  tag->kind->name == NULL)))
	{
		char str[2] = {tag->kind->letter, '\0'};
		length += mio_printf (mio, kindFmt, sep, kindKey, str);
		sep [0] = '\0';
	}

	if (isFieldEnabled (FIELD_LINE_NUMBER) &&  doesFieldHaveValue (FIELD_LINE_NUMBER, tag))
	{
		length += mio_printf (mio, "%s\t%s:%ld", sep,
				   getFieldName (FIELD_LINE_NUMBER),
				   tag->lineNumber);
		sep [0] = '\0';
	}

	length += renderExtensionFieldMaybe (FIELD_LANGUAGE, tag, sep, mio);

	if (isFieldEnabled (FIELD_SCOPE))
	{
		const char* k = NULL, *v = NULL;

		k = escapeFieldValue (tag, FIELD_SCOPE_KIND_LONG);
		v = escapeFieldValue (tag, FIELD_SCOPE);
		if (k && v)
		{
			length += mio_printf (mio, scopeFmt, sep, scopeKey, k, v);
			sep [0] = '\0';
		}
	}

	if (isFieldEnabled (FIELD_TYPE_REF) && doesFieldHaveValue (FIELD_TYPE_REF, tag))
	{
		length += mio_printf (mio, "%s\t%s:%s:%s", sep,
				      getFieldName (FIELD_TYPE_REF),
				      tag->extensionFields.typeRef [0],
				      escapeFieldValue (tag, FIELD_TYPE_REF));
		sep [0] = '\0';
	}

	if (isFieldEnabled (FIELD_FILE_SCOPE) &&  doesFieldHaveValue (FIELD_FILE_SCOPE, tag))
	{
		length += mio_printf (mio, "%s\t%s:", sep,
				      getFieldName (FIELD_FILE_SCOPE));
		sep [0] = '\0';
	}

	length += renderExtensionFieldMaybe (FIELD_INHERITANCE, tag, sep, mio);
	length += renderExtensionFieldMaybe (FIELD_ACCESS, tag, sep, mio);
	length += renderExtensionFieldMaybe (FIELD_IMPLEMENTATION, tag, sep, mio);
	length += renderExtensionFieldMaybe (FIELD_SIGNATURE, tag, sep, mio);
	length += renderExtensionFieldMaybe (FIELD_ROLE, tag, sep, mio);
	length += renderExtensionFieldMaybe (FIELD_EXTRA, tag, sep, mio);
	length += renderExtensionFieldMaybe (FIELD_XPATH, tag, sep, mio);
	length += renderExtensionFieldMaybe (FIELD_END, tag, sep, mio);

	return length;
}

static int writePatternEntry (MIO *mio, const tagEntryInfo *const tag)
{
	return makePatternStringCommon (tag, file_putc, file_puts, mio);
}

extern int writeCtagsEntry (MIO * mio, const tagEntryInfo *const tag, void *data CTAGS_ATTR_UNUSED)
{
	int length = mio_printf (mio, "%s\t%s\t",
			      escapeFieldValue (tag, FIELD_NAME),
			      escapeFieldValue (tag, FIELD_INPUT_FILE));

	if (tag->lineNumberEntry)
		length += writeLineNumberEntry (mio, tag);
	else if (tag->pattern)
		length += mio_printf(mio, "%s", tag->pattern);
	else
		length += writePatternEntry (mio, tag);

	if (includeExtensionFlags ())
	{
		length += addExtensionFields (mio, tag);
		length += addParserFields (mio, tag);
	}

	length += mio_printf (mio, "\n");

	return length;
}

extern int writeCtagsPtagEntry (MIO * mio, const ptagDesc *desc,
				const char *const fileName,
				const char *const pattern,
				const char *const parserName, void *data CTAGS_ATTR_UNUSED)
{
	return parserName

#define OPT(X) ((X)?(X):"")
		? mio_printf (mio, "%s%s%s%s\t%s\t%s\n",
			      PSEUDO_TAG_PREFIX, desc->name, PSEUDO_TAG_SEPARATOR, parserName,
			      OPT(fileName), OPT(pattern))
		: mio_printf (mio, "%s%s\t%s\t/%s/\n",
			      PSEUDO_TAG_PREFIX, desc->name,
			      OPT(fileName), OPT(pattern));
#undef OPT
}
