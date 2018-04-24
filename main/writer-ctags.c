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
#include "read.h"
#include "ptag.h"
#include "writer.h"


#define CTAGS_FILE  "tags"


static int writeCtagsEntry (tagWriter *writer CTAGS_ATTR_UNUSED,
							MIO * mio, const tagEntryInfo *const tag);
static int writeCtagsPtagEntry (tagWriter *writer CTAGS_ATTR_UNUSED,
								MIO * mio, const ptagDesc *desc,
								const char *const fileName,
								const char *const pattern,
								const char *const parserName);
static void buildCtagsFqTagCache (tagWriter *writer CTAGS_ATTR_UNUSED, tagEntryInfo *const tag);

struct rejection {
	bool rejectedInThisRendering;
	bool rejectedInThisInput;
};

tagWriter uCtagsWriter = {
	.writeEntry = writeCtagsEntry,
	.writePtagEntry = writeCtagsPtagEntry,
	.preWriteEntry = NULL,
	.postWriteEntry = NULL,
	.buildFqTagCache = buildCtagsFqTagCache,
	.defaultFileName = CTAGS_FILE,
};

static void *beginECtagsFile (tagWriter *writer CTAGS_ATTR_UNUSED, MIO * mio CTAGS_ATTR_UNUSED)
{
	static struct rejection rej;

	rej.rejectedInThisInput = false;

	return &rej;
}

static bool endECTagsFile (tagWriter *writer, MIO * mio CTAGS_ATTR_UNUSED, const char* filename CTAGS_ATTR_UNUSED)
{
	struct rejection *rej = writer->private;
	return rej->rejectedInThisInput;
}

tagWriter eCtagsWriter = {
	.writeEntry = writeCtagsEntry,
	.writePtagEntry = writeCtagsPtagEntry,
	.preWriteEntry = beginECtagsFile,
	.postWriteEntry = endECTagsFile,
	.buildFqTagCache = buildCtagsFqTagCache,
	.defaultFileName = CTAGS_FILE,
};

static const char* escapeFieldValue (tagWriter *writer, const tagEntryInfo * tag, fieldType ftype)
{
	bool *reject = NULL;

	if (writer->private)
	{
		struct rejection * rej = writer->private;
		reject = &rej->rejectedInThisRendering;
	}
	return renderFieldEscaped (writer->type, ftype, tag, NO_PARSER_FIELD, reject);
}

static int renderExtensionFieldMaybe (tagWriter *writer, int xftype, const tagEntryInfo *const tag, char sep[2], MIO *mio)
{
	if (isFieldEnabled (xftype) && doesFieldHaveValue (xftype, tag))
	{
		int len;
		len = mio_printf (mio, "%s\t%s:%s", sep,
				  getFieldName (xftype),
				  escapeFieldValue (writer, tag, xftype));
		sep[0] = '\0';
		return len;
	}
	else
		return 0;
}

static int addParserFields (tagWriter *writer, MIO * mio, const tagEntryInfo *const tag)
{
	unsigned int i;
	int length = 0;
	bool *reject = NULL;

	if (writer->private)
	{
		struct rejection *rej = writer->private;
		reject = &rej->rejectedInThisRendering;
	}

	for (i = 0; i < tag->usedParserFields; i++)
	{
		const tagField *f = getParserField(tag, i);
		if (! isFieldEnabled (f->ftype))
			continue;

		length += mio_printf(mio, "\t%s:%s",
							 getFieldName (f->ftype),
							 renderFieldEscaped (writer->type,
												 f->ftype, tag, i, reject));
	}
	return length;
}

static int writeLineNumberEntry (tagWriter *writer, MIO * mio, const tagEntryInfo *const tag)
{
	if (Option.lineDirectives)
		return mio_printf (mio, "%s", escapeFieldValue (writer, tag, FIELD_LINE_NUMBER));
	else
		return mio_printf (mio, "%lu", tag->lineNumber);
}

static int addExtensionFields (tagWriter *writer, MIO *mio, const tagEntryInfo *const tag)
{
	bool isKindKeyEnabled = isFieldEnabled (FIELD_KIND_KEY);
	bool isScopeEnabled = isFieldEnabled   (FIELD_SCOPE_KEY);

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

	const char *str = NULL;;
	kindDefinition *kdef = getLanguageKind(tag->langType, tag->kindIndex);
	const char kind_letter_str[2] = {kdef->letter, '\0'};

	if (kdef->name != NULL && (isFieldEnabled (FIELD_KIND_LONG)  ||
		 (isFieldEnabled (FIELD_KIND)  && kdef->letter == KIND_NULL)))
	{
		/* Use kind long name */
		str = kdef->name;
	}
	else if (kdef->letter != KIND_NULL  && (isFieldEnabled (FIELD_KIND) ||
			(isFieldEnabled (FIELD_KIND_LONG) &&  kdef->name == NULL)))
	{
		/* Use kind letter */
		str = kind_letter_str;
	}

	if (str)
	{
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

	length += renderExtensionFieldMaybe (writer, FIELD_LANGUAGE, tag, sep, mio);

	if (isFieldEnabled (FIELD_SCOPE))
	{
		const char* k, *v;

		k = escapeFieldValue (writer, tag, FIELD_SCOPE_KIND_LONG);
		v = escapeFieldValue (writer, tag, FIELD_SCOPE);
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
				      escapeFieldValue (writer, tag, FIELD_TYPE_REF));
		sep [0] = '\0';
	}

	if (isFieldEnabled (FIELD_FILE_SCOPE) &&  doesFieldHaveValue (FIELD_FILE_SCOPE, tag))
	{
		length += mio_printf (mio, "%s\t%s:", sep,
				      getFieldName (FIELD_FILE_SCOPE));
		sep [0] = '\0';
	}

	length += renderExtensionFieldMaybe (writer, FIELD_INHERITANCE, tag, sep, mio);
	length += renderExtensionFieldMaybe (writer, FIELD_ACCESS, tag, sep, mio);
	length += renderExtensionFieldMaybe (writer, FIELD_IMPLEMENTATION, tag, sep, mio);
	length += renderExtensionFieldMaybe (writer, FIELD_SIGNATURE, tag, sep, mio);
	length += renderExtensionFieldMaybe (writer, FIELD_ROLES, tag, sep, mio);
	length += renderExtensionFieldMaybe (writer, FIELD_EXTRAS, tag, sep, mio);
	length += renderExtensionFieldMaybe (writer, FIELD_XPATH, tag, sep, mio);
	length += renderExtensionFieldMaybe (writer, FIELD_END_LINE, tag, sep, mio);

	return length;
}

static int writeCtagsEntry (tagWriter *writer,
							MIO * mio, const tagEntryInfo *const tag)
{
	long origin = 0;

	if (writer->private)
	{
		struct rejection *rej = writer->private;

		origin = mio_tell (mio);
		rej->rejectedInThisRendering = false;

	}

	int length = mio_printf (mio, "%s\t%s\t",
			      escapeFieldValue (writer, tag, FIELD_NAME),
			      escapeFieldValue (writer, tag, FIELD_INPUT_FILE));

	if (tag->lineNumberEntry)
		length += writeLineNumberEntry (writer, mio, tag);
	else
		length += mio_puts(mio, escapeFieldValue(writer, tag, FIELD_PATTERN));

	if (includeExtensionFlags ())
	{
		length += addExtensionFields (writer, mio, tag);
		length += addParserFields (writer, mio, tag);
	}

	length += mio_printf (mio, "\n");

	if (writer->private
		&& ((struct rejection *)(writer->private))->rejectedInThisRendering)
	{
		mio_seek (mio, origin, SEEK_SET);

		/* Truncation is needed. */
		((struct rejection *)(writer->private))->rejectedInThisInput = true;

		length = 0;
	}
	return length;
}

static int writeCtagsPtagEntry (tagWriter *writer CTAGS_ATTR_UNUSED,
				MIO * mio, const ptagDesc *desc,
				const char *const fileName,
				const char *const pattern,
				const char *const parserName)
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

static void buildCtagsFqTagCache (tagWriter *writer CTAGS_ATTR_UNUSED, tagEntryInfo *const tag)
{
	escapeFieldValue (writer, tag, FIELD_SCOPE_KIND_LONG);
	escapeFieldValue (writer, tag, FIELD_SCOPE);
}
