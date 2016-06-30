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


#define includeExtensionFlags()         (Option.tagFileFormat > 1)


static const char* escapeName (const tagEntryInfo * tag, fieldType ftype)
{
	return renderFieldEscaped (ftype, tag, NO_PARSER_FIELD);
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
		return mio_printf (mio, "%s", escapeName (tag, FIELD_LINE_NUMBER));
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

	boolean first = TRUE;
	const char* separator = ";\"";
	const char* const empty = "";
	int length = 0;

	boolean making_fq_tag =  (doesInputLanguageRequestAutomaticFQTag ()
				  && isXtagEnabled (XTAG_QUALIFIED_TAGS));

/* "sep" returns a value only the first time it is evaluated */
#define sep (first ? (first = FALSE, separator) : empty)

	if (tag->kind->name != NULL && (isFieldEnabled (FIELD_KIND_LONG)  ||
		 (isFieldEnabled (FIELD_KIND)  && tag->kind == '\0')))
		length += mio_printf (mio, kindFmt, sep, kindKey, tag->kind->name);
	else if (tag->kind != '\0'  && (isFieldEnabled (FIELD_KIND) ||
			(isFieldEnabled (FIELD_KIND_LONG) &&  tag->kind->name == NULL)))
	{
		char str[2] = {tag->kind->letter, '\0'};
		length += mio_printf (mio, kindFmt, sep, kindKey, str);
	}

	if (isFieldEnabled (FIELD_LINE_NUMBER))
		length += mio_printf (mio, "%s\t%s:%ld", sep,
				   getFieldName (FIELD_LINE_NUMBER),
				   tag->lineNumber);

	if (isFieldEnabled (FIELD_LANGUAGE)  &&  tag->language != NULL)
		length += mio_printf (mio, "%s\t%s:%s", sep,
				   getFieldName (FIELD_LANGUAGE),
				   escapeName (tag, FIELD_LANGUAGE));

	if (isFieldEnabled (FIELD_SCOPE) || making_fq_tag)
	{
		const char* k = NULL, *v = NULL;

		k = escapeName (tag, FIELD_SCOPE_KIND_LONG);
		v = escapeName (tag, FIELD_SCOPE);

		if (isFieldEnabled (FIELD_SCOPE) && k && v)
			length += mio_printf (mio, scopeFmt, sep, scopeKey, k, v);
	}

	if (isFieldEnabled (FIELD_TYPE_REF) &&
	    tag->extensionFields.typeRef [0] != NULL  &&
	    tag->extensionFields.typeRef [1] != NULL)
		length += mio_printf (mio, "%s\t%s:%s:%s", sep,
				      getFieldName (FIELD_TYPE_REF),
				      tag->extensionFields.typeRef [0],
				      escapeName (tag, FIELD_TYPE_REF));

	if (isFieldEnabled (FIELD_FILE_SCOPE) &&  tag->isFileScope)
		length += mio_printf (mio, "%s\t%s:", sep,
				      getFieldName (FIELD_FILE_SCOPE));

	if (isFieldEnabled (FIELD_INHERITANCE) &&
			tag->extensionFields.inheritance != NULL)
		length += mio_printf (mio, "%s\t%s:%s", sep,
				   getFieldName (FIELD_INHERITANCE),
				   escapeName (tag, FIELD_INHERITANCE));

	if (isFieldEnabled (FIELD_ACCESS) &&  tag->extensionFields.access != NULL)
		length += mio_printf (mio, "%s\t%s:%s", sep,
				   getFieldName (FIELD_ACCESS),
				   tag->extensionFields.access);

	if (isFieldEnabled (FIELD_IMPLEMENTATION) &&
			tag->extensionFields.implementation != NULL)
		length += mio_printf (mio, "%s\t%s:%s", sep,
				   getFieldName (FIELD_IMPLEMENTATION),
				   tag->extensionFields.implementation);

	if (isFieldEnabled (FIELD_SIGNATURE) &&
			tag->extensionFields.signature != NULL)
		length += mio_printf (mio, "%s\t%s:%s", sep,
				   getFieldName (FIELD_SIGNATURE),
				   escapeName (tag, FIELD_SIGNATURE));
	if (isFieldEnabled (FIELD_ROLE) && tag->extensionFields.roleIndex != ROLE_INDEX_DEFINITION)
		length += mio_printf (mio, "%s\t%s:%s", sep,
				   getFieldName (FIELD_ROLE),
				   escapeName (tag, FIELD_ROLE));

	if (isFieldEnabled (FIELD_EXTRA))
	{
		const char *value = escapeName (tag, FIELD_EXTRA);
		if (value)
			length += mio_printf (mio, "%s\t%s:%s", sep,
					   getFieldName (FIELD_EXTRA),
					   escapeName (tag, FIELD_EXTRA));
	}

#ifdef HAVE_LIBXML
	if (isFieldEnabled(FIELD_XPATH))
	{
		const char *value = escapeName (tag, FIELD_XPATH);
		if (value)
			length += mio_printf (mio, "%s\t%s:%s", sep,
					      getFieldName (FIELD_XPATH),
					      escapeName (tag, FIELD_XPATH));

	}
#endif

	return length;
#undef sep
}

static int writePatternEntry (MIO *mio, const tagEntryInfo *const tag)
{
	return makePatternStringCommon (tag, file_putc, file_puts, mio);
}

extern int writeCtagsEntry (MIO * mio, const tagEntryInfo *const tag, void *data __unused__)
{
	int length = mio_printf (mio, "%s\t%s\t",
			      escapeName (tag, FIELD_NAME),
			      escapeName (tag, FIELD_INPUT_FILE));

	if (tag->lineNumberEntry)
		length += writeLineNumberEntry (mio, tag);
	else if (tag->pattern)
		length += mio_printf(mio, "%s", tag->pattern);
	else
		length += writePatternEntry (mio, tag);

	if (includeExtensionFlags ())
		length += addExtensionFields (mio, tag);

	length += addParserFields (mio, tag);

	length += mio_printf (mio, "\n");

	return length;
}
